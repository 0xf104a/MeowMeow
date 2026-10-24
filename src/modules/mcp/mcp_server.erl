%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2026, Anna-Sofia Kasierocka
%%% @doc
%%%   MCP server controls the MCP stdio subprocess.
%%%
%%%   Before the tool is started the session asks mcp_admission for a
%%%   slot (session caps) and reserves its memory accounting with
%%%   mcp_meter (per-session memory limit). Memory is sampled every
%%%   `poll_ms'; with the /proc backend a sample over the limit ends the
%%%   session, with the cgroup backend the kernel enforces the limit and
%%%   an OOM kill inside the session's cgroup ends it.
%%% @end
%%%-------------------------------------------------------------------
-module(mcp_server).
-author("f104a").
-behaviour(gen_server).
-include("mcp.hrl").

-export([start/1, init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2,
  connect_to_mcp_session/1, disconnect_mcp_session/1, notify_mcp_session/2, terminate_session/1,
  memory_usage/1]).

%% Exit status of the cgroup wrapper shell when it could not join the cgroup
%% (see mcp_meter:spawn_spec/2).
-define(JOIN_FAILED, 97).

%% @doc Start a session: [Command, KeepAliveMs] or [Command, KeepAliveMs, RuleOpts]
%% where RuleOpts is the map mcp_options:parse_rule_args/1 produces.
%% Returns {error, {shutdown, busy}} when no session slot is available.
start([Cmd, KeepAliveMs]) ->
  start([Cmd, KeepAliveMs, #{}]);
start([Cmd, KeepAliveMs, RuleOpts]) ->
  gen_server:start(?MODULE, [Cmd, KeepAliveMs, RuleOpts], []).

init([Cmd, KeepAliveMs]) ->
  init([Cmd, KeepAliveMs, #{}]);

init([_, KeepAliveMs, _]) when not is_integer(KeepAliveMs) ->
  logging:err("KeepAliveMs=~p is not an integer", [KeepAliveMs]),
  {error, badarg};

init([Cmd, KeepAliveMs, RuleOpts]) when is_map(RuleOpts) ->
  Opts = mcp_options:session_opts(RuleOpts),
  case mcp_admission:acquire(Cmd, Opts) of
    {error, busy} ->
      {stop, {shutdown, busy}};
    ok ->
      start_tool(Cmd, KeepAliveMs, Opts)
  end.

start_tool(Cmd, KeepAliveMs, Opts) ->
  logging:info("Starting MCP tool: ~s", [Cmd]),
  Meter0 = mcp_meter:prepare(),
  {PortName, PortOpts} = mcp_meter:spawn_spec(Meter0, Cmd),
  Port = open_port(PortName, [
    binary,
    {line, 65536},
    use_stdio,
    exit_status | PortOpts
  ]),
  Meter = mcp_meter:attach(Meter0, Port),
  Limit = maps:get(memory_limit, Opts, none),
  case mcp_meter:set_limit(Meter, Limit) of
    ok -> ok;
    {error, Reason} ->
      logging:warn("Could not apply memory limit ~s to MCP tool ~s: ~p",
                   [mcp_options:format_size(Limit), Cmd, Reason])
  end,
  case Limit of
    none -> ok;
    _ -> logging:debug("MCP tool ~s limited to ~s (~p backend)",
                       [Cmd, mcp_options:format_size(Limit), mcp_meter:backend_name(Meter)])
  end,
  Timer = erlang:send_after(KeepAliveMs, self(), terminate),
  State = #state{
    port = Port,
    timer = Timer,
    keepalive_ms = KeepAliveMs,
    streams = [],
    buf = <<>>,
    cmd = Cmd,
    meter = Meter,
    mem_limit = Limit,
    mem_poll_ms = maps:get(poll_ms, Opts, 0)
  },
  {ok, schedule_poll(State)}.

schedule_poll(#state{mem_poll_ms = PollMs} = State) when is_integer(PollMs), PollMs > 0 ->
  erlang:send_after(PollMs, self(), poll_memory),
  State;
schedule_poll(State) ->
  State.

broadcast_msg(Msg, #state{streams = Streams}) ->
  lists:foreach(fun(Stream) -> Stream ! Msg end, Streams).

reset_timer(#state{timer = Timer, keepalive_ms = KeepAliveMs} = State) ->
  CnclResult = erlang:cancel_timer(Timer),
  logging:debug("CnclResult=~p", [CnclResult]),
  NewTimer = erlang:send_after(KeepAliveMs, self(), terminate),
  State#state{timer = NewTimer}.

%% Complete line — prepend any buffered partial, deliver, clear buffer
handle_info({Port, {data, {eol, Line}}}, #state{port = Port, buf = Buf} = State) ->
  FullLine = <<Buf/binary, Line/binary>>,
  broadcast_msg(FullLine, State),
  {noreply, reset_timer(State#state{buf = <<>>})};

%% Partial line — accumulate into buffer, don't deliver yet
handle_info({Port, {data, {noeol, Partial}}}, #state{port = Port, buf = Buf} = State) ->
  {noreply, State#state{buf = <<Buf/binary, Partial/binary>>}};

handle_info({Port, {exit_status, Code}}, #state{port = Port} = State0) ->
  logging:debug("Subprocess exited with code ~p", [Code]),
  explain_exit(Code, State0),
  %% The port may deliver the exit status before the last (unterminated)
  %% line of output, so give pending data a moment to arrive, then flush
  %% any buffered partial line before terminating.
  State = drain_port(Port, State0),
  case State#state.buf of
    <<>> -> ok;
    Remaining -> broadcast_msg(Remaining, State)
  end,
  broadcast_msg(terminated, State),
  {stop, {subprocess_exited, Code}, State};

handle_info(poll_memory, #state{meter = Meter, mem_limit = Limit, cmd = Cmd} = State0) ->
  Usage = mcp_meter:usage(Meter),
  OomKills = mcp_meter:oom_kills(Meter),
  State = State0#state{mem_current = Usage,
                       mem_peak = max(Usage, State0#state.mem_peak)},
  OverLimit = Limit =/= none andalso Usage > Limit
                andalso not mcp_meter:enforced_by_kernel(Meter),
  KernelKilled = OomKills > State0#state.oom_kills,
  if
    OverLimit orelse KernelKilled ->
      logging:warn("MCP tool ~s exceeded its memory limit (~s used, limit ~s), terminating session",
                   [Cmd, mcp_options:format_size(Usage), mcp_options:format_size(Limit)]),
      broadcast_msg(terminated, State),
      {stop, {shutdown, {memory_limit_exceeded, Usage}}, State#state{oom_kills = OomKills}};
    true ->
      {noreply, schedule_poll(State#state{oom_kills = OomKills})}
  end;

handle_info(terminate, State) ->
  logging:info("Stopping port ~p...", [self()]),
  broadcast_msg(terminated, State),
  %% Remember the process tree while the port is certainly still ours.
  KillList = mcp_meter:snapshot(State#state.meter),
  catch port_command(State#state.port, <<>>),
  util:sigterm_to_port(State#state.port),
  timer:sleep(300),
  {stop, normal, State#state{kill_list = KillList}};

handle_info(Any, State) ->
  logging:warn("Received unknown message ~p @ mcp_server:handle_info/2", [Any]),
  {noreply, State}.

%% Log why a tool ended when the status has a known meaning.
explain_exit(?JOIN_FAILED, #state{cmd = Cmd, meter = Meter}) ->
  case mcp_meter:backend_name(Meter) of
    cgroup -> logging:err("MCP tool ~s could not join its cgroup; check that the cgroup "
                          "hierarchy is writable or set MCPCgroup No", [Cmd]);
    _ -> ok
  end;
explain_exit(_Code, #state{cmd = Cmd, meter = Meter, mem_limit = Limit}) ->
  case mcp_meter:oom_kills(Meter) of
    0 -> ok;
    _ -> logging:warn("MCP tool ~s was killed by the kernel for exceeding its memory limit (~s)",
                      [Cmd, mcp_options:format_size(Limit)])
  end.

%% Deliver output that is still queued after the subprocess exited.
drain_port(Port, #state{buf = Buf} = State) ->
  receive
    {Port, {data, {eol, Line}}} ->
      broadcast_msg(<<Buf/binary, Line/binary>>, State),
      drain_port(Port, State#state{buf = <<>>});
    {Port, {data, {noeol, Partial}}} ->
      drain_port(Port, State#state{buf = <<Buf/binary, Partial/binary>>})
  after 100 ->
    State
  end.

handle_call({notify, Data}, _, State) ->
  port_command(State#state.port, [Data, <<"\n">>]),
  {reply, ok, reset_timer(State)};

handle_call({add_receiver, ConnPid}, _, State) ->
  logging:debug("Adding receiver ~p", [ConnPid]),
  {reply, ok, State#state{streams = State#state.streams ++ [ConnPid]}};

handle_call({remove_receiver, ConnPid}, _, State) ->
  {reply, ok, State#state{streams = lists:delete(ConnPid, State#state.streams)}};

handle_call(memory_usage, _, #state{meter = Meter} = State) ->
  {reply, #{current => State#state.mem_current,
            peak => State#state.mem_peak,
            limit => State#state.mem_limit,
            backend => mcp_meter:backend_name(Meter)}, State};

handle_call(terminate, _, State) ->
  logging:info("Stopping port ~p...", [self()]),
  broadcast_msg(terminated, State),
  KillList = mcp_meter:snapshot(State#state.meter),
  %% Ask to stop very politely(no tool yet recognized that)
  catch port_command(State#state.port, <<>>),
  %% Ask to stop politely but insistently(only few tools do that correctly)
  util:sigterm_to_port(State#state.port),
  %% terminate/2 kills whatever is left of the process tree after 300ms
  timer:sleep(300),
  {stop, normal, ok, State#state{kill_list = KillList}};

handle_call(Any, _, State) ->
  logging:warn("Received unknown message ~p @ mcp_server:handle_call/3", [Any]),
  {reply, {error, unknown_call}, State}.

handle_cast(_, State) ->
  {noreply, State}.

%% Every way out ends the whole process tree of the tool, not only the
%% process the port started, and releases the session's accounting.
terminate(_Reason, #state{port = Port, meter = Meter, kill_list = KillList}) ->
  PortOpen = erlang:port_info(Port) =/= undefined,
  catch mcp_meter:kill(Meter, PortOpen, KillList),
  catch port_close(Port),
  catch mcp_meter:detach(Meter),
  ok;
terminate(_Reason, _) ->
  ok.

connect_to_mcp_session(SessionPid) ->
  gen_server:call(SessionPid, {add_receiver, self()}).

disconnect_mcp_session(SessionPid) ->
  gen_server:call(SessionPid, {remove_receiver, self()}).

notify_mcp_session(SessionPid, Data) ->
  logging:debug("~p ~p", [SessionPid, Data]),
  gen_server:call(SessionPid, {notify, Data}).

terminate_session(SessionPid) ->
  gen_server:call(SessionPid, terminate).

%% @doc #{current, peak, limit, backend} of a session's memory accounting.
memory_usage(SessionPid) ->
  gen_server:call(SessionPid, memory_usage).
