%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2026, Anna-Sofia Kasierocka
%%% @doc
%%%   Admission control for MCP sessions.
%%%
%%%   One registered gen_server counts the live sessions, in total and
%%%   per tool command, and decides whether a new session may start.
%%%   The session process (mcp_server) asks from its init/1 and is the
%%%   slot holder from that moment on; the slot is released when the
%%%   process ends, so slots can not leak.
%%%
%%%   A start that does not fit may wait up to `wait_ms' for a slot to
%%%   free up; at most `max_waiting' starts wait at once, the rest are
%%%   refused at once. Waiters are served in arrival order, skipping
%%%   those whose tool is still at its own cap.
%%% @end
%%%-------------------------------------------------------------------
-module(mcp_admission).
-author("f104a").
-behaviour(gen_server).

-export([start/0, ensure_started/0, stop/0,
         acquire/2, release/1, status/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(waiter, {from, pid, tool, opts, ref, timer, mon}).
-record(st, {running = #{} :: #{pid() => {string(), reference()}},
             per_tool = #{} :: #{string() => pos_integer()},
             waiting = [] :: [#waiter{}]}).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% @doc Start the server unless it already runs. Safe to call from
%% several processes at the same time.
ensure_started() ->
  case whereis(?MODULE) of
    undefined ->
      case start() of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        Error -> Error
      end;
    _ -> ok
  end.

stop() ->
  case whereis(?MODULE) of
    undefined -> ok;
    Pid ->
      Ref = monitor(process, Pid),
      catch gen_server:stop(Pid),
      receive {'DOWN', Ref, process, Pid, _} -> ok after 2000 -> ok end
  end.

%% @doc Ask for a session slot for the calling process (the session).
%% Opts: see mcp_options:session_opts/1.
-spec acquire(string(), map()) -> ok | {error, busy}.
acquire(Tool, Opts) when is_map(Opts) ->
  ok = ensure_started(),
  WaitMs = maps:get(wait_ms, Opts, 0),
  try gen_server:call(?MODULE, {acquire, self(), Tool, Opts}, WaitMs + 5000)
  catch
    exit:{timeout, _} -> {error, busy};
    exit:{noproc, _} ->
      ok = ensure_started(),
      gen_server:call(?MODULE, {acquire, self(), Tool, Opts}, WaitMs + 5000)
  end.

%% @doc Give a slot back before the holder ends (normally not needed).
release(Pid) ->
  case whereis(?MODULE) of
    undefined -> ok;
    _ -> gen_server:call(?MODULE, {release, Pid})
  end.

%% @doc #{running => N, waiting => M, tools => #{Command => N}}.
status() ->
  case whereis(?MODULE) of
    undefined -> #{running => 0, waiting => 0, tools => #{}};
    _ -> gen_server:call(?MODULE, status)
  end.

%%--------------------------------------------------------------------
%% gen_server
%%--------------------------------------------------------------------

init([]) ->
  {ok, #st{}}.

handle_call({acquire, Pid, Tool, Opts}, From, St) ->
  case fits(Tool, Opts, St) of
    true ->
      {reply, ok, admit(Pid, Tool, St)};
    false ->
      WaitMs = maps:get(wait_ms, Opts, 0),
      MaxWaiting = maps:get(max_waiting, Opts, 0),
      case WaitMs > 0 andalso length(St#st.waiting) < MaxWaiting of
        true ->
          Ref = make_ref(),
          Timer = erlang:send_after(WaitMs, self(), {wait_timeout, Ref}),
          Mon = monitor(process, Pid),
          W = #waiter{from = From, pid = Pid, tool = Tool, opts = Opts,
                      ref = Ref, timer = Timer, mon = Mon},
          logging:debug("MCP session for ~s waits for a slot (~p running, ~p waiting)",
                        [Tool, map_size(St#st.running), length(St#st.waiting) + 1]),
          {noreply, St#st{waiting = St#st.waiting ++ [W]}};
        false ->
          logging:warn("Refusing MCP session for ~s: ~p sessions running, ~p waiting",
                       [Tool, map_size(St#st.running), length(St#st.waiting)]),
          {reply, {error, busy}, St}
      end
  end;

handle_call({release, Pid}, _From, St) ->
  {reply, ok, serve_waiters(forget(Pid, St))};

handle_call(status, _From, St) ->
  {reply, #{running => map_size(St#st.running),
            waiting => length(St#st.waiting),
            tools => St#st.per_tool}, St};

handle_call(Any, _From, St) ->
  logging:warn("Received unknown message ~p @ mcp_admission:handle_call/3", [Any]),
  {reply, {error, unknown_call}, St}.

handle_cast(_, St) ->
  {noreply, St}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, St) ->
  St1 = forget(Pid, St),
  St2 = drop_waiter(fun(#waiter{pid = P}) -> P =:= Pid end, none, St1),
  {noreply, serve_waiters(St2)};

handle_info({wait_timeout, Ref}, St) ->
  {noreply, drop_waiter(fun(#waiter{ref = R}) -> R =:= Ref end, {error, busy}, St)};

handle_info(Any, St) ->
  logging:warn("Received unknown message ~p @ mcp_admission:handle_info/2", [Any]),
  {noreply, St}.

terminate(_Reason, #st{waiting = Waiting}) ->
  lists:foreach(fun(#waiter{from = From}) -> catch gen_server:reply(From, {error, busy}) end,
                Waiting),
  ok.

%%--------------------------------------------------------------------
%% Internals
%%--------------------------------------------------------------------

fits(Tool, Opts, #st{running = Running, per_tool = PerTool}) ->
  below(map_size(Running), maps:get(global_max, Opts, infinity)) andalso
    below(maps:get(Tool, PerTool, 0), maps:get(tool_max, Opts, infinity)).

below(_, infinity) -> true;
below(N, Max) when is_integer(Max) -> N < Max;
below(_, _) -> true.

admit(Pid, Tool, #st{running = Running, per_tool = PerTool} = St) ->
  case maps:is_key(Pid, Running) of
    true -> St;
    false ->
      Ref = monitor(process, Pid),
      St#st{running = Running#{Pid => {Tool, Ref}},
            per_tool = PerTool#{Tool => maps:get(Tool, PerTool, 0) + 1}}
  end.

forget(Pid, #st{running = Running, per_tool = PerTool} = St) ->
  case maps:take(Pid, Running) of
    error -> St;
    {{Tool, Ref}, Rest} ->
      demonitor(Ref, [flush]),
      NewPerTool = case maps:get(Tool, PerTool, 0) of
                     N when N =< 1 -> maps:remove(Tool, PerTool);
                     N -> PerTool#{Tool => N - 1}
                   end,
      St#st{running = Rest, per_tool = NewPerTool}
  end.

%% Remove the first waiter matching Pred; Reply is sent to it unless none.
drop_waiter(Pred, Reply, #st{waiting = Waiting} = St) ->
  case lists:splitwith(fun(W) -> not Pred(W) end, Waiting) of
    {_, []} -> St;
    {Before, [W | After]} ->
      cancel_waiter(W),
      case Reply of
        none -> ok;
        _ -> gen_server:reply(W#waiter.from, Reply)
      end,
      St#st{waiting = Before ++ After}
  end.

cancel_waiter(#waiter{timer = Timer, mon = Mon}) ->
  erlang:cancel_timer(Timer),
  demonitor(Mon, [flush]),
  ok.

%% Admit every waiter that now fits, oldest first.
serve_waiters(#st{waiting = Waiting} = St) ->
  lists:foldl(
    fun(#waiter{pid = Pid, tool = Tool, opts = Opts, from = From} = W, Acc) ->
        case fits(Tool, Opts, Acc) of
          true ->
            cancel_waiter(W),
            gen_server:reply(From, ok),
            Acc1 = admit(Pid, Tool, Acc),
            Acc1#st{waiting = lists:delete(W, Acc1#st.waiting)};
          false -> Acc
        end
    end, St, Waiting).
