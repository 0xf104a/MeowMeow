%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2026, Anna-Sofia Kasierocka
%%% @doc
%%%   MCP server controls the MCP stdio subprocess
%%% @end
%%%-------------------------------------------------------------------
-module(mcp_server).
-author("f104a").
-behaviour(gen_server).
-include("mcp.hrl").

-export([start/1, init/1, handle_info/2, handle_call/3, handle_cast/2,
  connect_to_mcp_session/1, disconnect_mcp_session/1, notify_mcp_session/2, terminate_session/1]).

start([Cmd, KeepAliveMs]) ->
  gen_server:start(?MODULE, [Cmd, KeepAliveMs], []).

init([_, KeepAliveMs]) when not is_integer(KeepAliveMs) ->
  logging:err("KeepAliveMs=~p is not an integer", [KeepAliveMs]),
  {error, badarg};

init([Cmd, KeepAliveMs]) ->
  Port = open_port({spawn, Cmd}, [
    binary,
    {line, 65536},
    use_stdio,
    exit_status
  ]),
  Timer = erlang:send_after(KeepAliveMs, self(), terminate),
  {ok, #state{
    port = Port,
    timer = Timer,
    keepalive_ms = KeepAliveMs,
    streams = [],
    buf = <<>>          %% add buf to your #state record in mcp.hrl
  }}.

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

handle_info({Port, exit_status, Code}, #state{port = Port} = State) ->
  logging:debug("Subprocess exited with code ~p", [Code]),
  %% Flush any buffered partial line before terminating
  case State#state.buf of
    <<>> -> ok;
    Remaining -> broadcast_msg(Remaining, State)
  end,
  broadcast_msg(terminated, State),
  {stop, {subprocess_exited, Code}, State};

handle_info(terminate, State) ->
  logging:info("Stopping port ~p...", [self()]),
  broadcast_msg(terminated, State),
  catch port_command(State#state.port, <<>>),
  catch port_close(State#state.port),
  {stop, normal, State};

handle_info(Any, _) ->
  logging:warn("Received unknown message ~p @ mcp_server:handle_info/2", [Any]).

handle_call({notify, Data}, _, State) ->
  port_command(State#state.port, [Data, <<"\n">>]),
  {reply, ok, reset_timer(State)};

handle_call({add_receiver, ConnPid}, _, State) ->
  logging:debug("Adding receiver ~p", [ConnPid]),
  {reply, ok, State#state{streams = State#state.streams ++ [ConnPid]}};

handle_call({remove_receiver, ConnPid}, _, State) ->
  {reply, ok, State#state{streams = lists:delete(ConnPid, State#state.streams)}};

handle_call(terminate, _, State) ->
  logging:info("Stopping port ~p...", [self()]),
  broadcast_msg(terminated, State),
  catch port_command(State#state.port, <<>>),
  util:sigterm_to_port(State#state.port),
  catch port_close(State#state.port),
  {stop, normal, ok, State};

handle_call(Any, _, _) ->
  logging:warn("Received unknown message ~p @ mcp_server:handle_call/3", [Any]).

handle_cast(_, State) ->
  {noreply, State}.

connect_to_mcp_session(SessionPid) ->
  gen_server:call(SessionPid, {add_receiver, self()}).

disconnect_mcp_session(SessionPid) ->
  gen_server:call(SessionPid, {remove_receiver, self()}).

notify_mcp_session(SessionPid, Data) ->
  logging:debug("~p ~p", [SessionPid, Data]),
  gen_server:call(SessionPid, {notify, Data}).

terminate_session(SessionPid) ->
  gen_server:call(SessionPid, terminate).