%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2026, Anna-Sofia Kasierocka
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(mcp_session).
-author("f104a").
-behaviour(gen_server).
-include("mcp.hrl").

%% API
-export([start/1, init/1, handle_info/2, handle_call/3,
  connect_to_mcp_session/1, disconnect_mcp_session/1, notify_mcp_session/2]).

start(Cmd) ->
  gen_server:start(?MODULE, Cmd, []).  %% NOT start_link — no link to caller

init(Cmd) ->
  Port = open_port({spawn, Cmd}, [
    binary,
    {line, 65536},
    use_stdio,
    exit_status
  ]),
  {ok, #state{port = Port, streams = []}}.


broadcast_msg(Msg, #state{streams = Streams}) ->
  lists:foreach(fun(Stream) -> Stream ! Msg end, Streams).

handle_info({Port, {data, {eol, Line}}}, #state{port = Port} = State) ->
  broadcast_msg(Line, State),
  {noreply, State};
handle_info({Port, exit_status, Code}, #state{port = Port} = State) ->
  logging:debug("Subprocess exited with code ~p", [Code]),
  broadcast_msg(terminated, State),
  {stop, {subprocess_exited, Code}, State};
handle_info(_Other, State) ->
  {noreply, State}.

handle_call({notify, Data}, _,  State) ->
  logging:debug("notify ~p", [Data]),
  port_command(State#state.port, [Data, <<"\n">>]),
  logging:debug("cmd finished"),
  {reply, ok, State};
handle_call({add_receiver, ConnPid}, _, State) ->
  logging:debug("Adding receiver ~p", [ConnPid]),
  {reply, ok, State#state{streams = State#state.streams ++ [ConnPid]}};
handle_call({remove_receiver, ConnPid}, _, State) ->
  {reply, ok, State#state{streams = lists:delete(ConnPid, State#state.streams)}}.

connect_to_mcp_session(SessionPid) ->
  gen_server:call(SessionPid, {add_receiver, self()}).

disconnect_mcp_session(SessionPid) ->
  gen_server:call(SessionPid, {remove_receiver, self()}).

notify_mcp_session(SessionPid, Data) ->
  logging:debug("~p ~p", [SessionPid, Data]),
  gen_server:call(SessionPid, {notify, Data}).

