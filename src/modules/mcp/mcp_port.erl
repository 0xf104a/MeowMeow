%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2026, Anna-Sofia Kasierocka
%%% @doc
%%%  A MCP port allows to communicate with MCP running on stdio.
%%%  This module allows to create and manage MCP ports
%%% @end
%%%-------------------------------------------------------------------
-module(mcp_port).
-author("f104a").

%% API
-export([init_mcp/0, start_mcp_session/1, kill_mcp_session/1]).

open_mcp_port(Tool) ->
  logging:debug("Starting tool ~p", [Tool]),
  open_port({spawn, Tool}, [stream, binary, exit_status, use_stdio]).

init_mcp() ->
  ets:new(mcp_sessions, [set, public, named_table]),
  ok.

clamp_visible(Bin) ->
  << <<($A + (Byte rem ($z - $A)))>> || <<Byte>> <= Bin >>.

generate_session_id(ByteLength) ->
  Bytes = crypto:strong_rand_bytes(ByteLength),
  Encoded = base64:encode(Bytes),
  clamp_visible(Encoded).

is_mcp

generate_session_id() -> generate_session_id(64).

start_mcp_session(Tool) ->
  SessionId = generate_session_id(),
  Port = open_mcp_port(Tool),
  ets:insert(mcp_sessions, {SessionId, {Tool, Port}}).

kill_mcp_session(SessionId) ->
  case ets:lookup(mcp_sessions, SessionId) of
    [{SessionId, {_, Port}}] ->
      logging:info("Killing MCP session ~p", [SessionId]),
      Port ! {close, self()},
      ets:delete(mcp_sessions, SessionId),
      ok;
    [] ->
      not_found
  end.


