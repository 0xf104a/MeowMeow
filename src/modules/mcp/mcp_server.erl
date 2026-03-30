%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2026, Anna-Sofia Kasierocka
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(mcp_server).
-author("f104a").
-behaviour(gen_server).
-include("mcp.hrl").

%% API
-export([start/1, init/1, handle_info/2, handle_call/3,
  connect_to_mcp_session/1, disconnect_mcp_session/1, notify_mcp_session/2, terminate_session/1]).

start([Cmd, KeepAliveMs]) ->
  gen_server:start(?MODULE, [Cmd, KeepAliveMs], []).  %% NOT start_link — no link to caller

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
  {ok, #state{port = Port, timer = Timer, keepalive_ms = KeepAliveMs, streams = []}}.


broadcast_msg(Msg, #state{streams = Streams}) ->
  lists:foreach(fun(Stream) -> Stream ! Msg end, Streams).

reset_timer(#state{timer = Timer, keepalive_ms = Ms} = State) ->
  erlang:cancel_timer(Timer),
  NewTimer = erlang:send_after(Ms, self(), terminate),
  State#state{timer = NewTimer}.

handle_info({Port, {data, {eol, Line}}}, #state{port = Port, timer = Timer, keepalive_ms = KeepAliveMs} = State) ->
  broadcast_msg(Line, State),
  {noreply, reset_timer(State)};
handle_info({Port, exit_status, Code}, #state{port = Port} = State) ->
  logging:debug("Subprocess exited with code ~p", [Code]),
  broadcast_msg(terminated, State),
  {stop, {subprocess_exited, Code}, State};
handle_info(_Other, State) ->
  {noreply, State}.

handle_call({notify, Data}, _,  State) ->
  port_command(State#state.port, [Data, <<"\n">>]),
  {reply, ok, reset_timer(State)};

handle_call({add_receiver, ConnPid}, _, State) ->
  logging:debug("Adding receiver ~p", [ConnPid]),
  {reply, ok, State#state{streams = State#state.streams ++ [ConnPid]}};

handle_call({remove_receiver, ConnPid}, _, State) ->
  {reply, ok, State#state{streams = lists:delete(ConnPid, State#state.streams)}};

handle_call(terminate, _, State) ->
  broadcast_msg(terminate, State),
  logging:debug("Stopping port ~p...", [self()]),
  catch port_command(State#state.port, <<>>),  %% flush
  catch port_close(State#state.port),
  {stop, normal, ok, State}.

connect_to_mcp_session(SessionPid) ->
  gen_server:call(SessionPid, {add_receiver, self()}).

disconnect_mcp_session(SessionPid) ->
  gen_server:call(SessionPid, {remove_receiver, self()}).

notify_mcp_session(SessionPid, Data) ->
  logging:debug("~p ~p", [SessionPid, Data]),
  gen_server:call(SessionPid, {notify, Data}).

terminate_session(SessionPid) ->
  gen_server:call(SessionPid, terminate).
