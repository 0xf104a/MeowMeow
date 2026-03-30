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
-export([init_mcp/0, start_mcp_session/2, terminate_mcp_session/1, get_mcp_session_by_id/1]).

init_mcp() ->
  ets:new(mcp_sessions, [set, public, named_table]),
  ok.

clamp_visible(Bin) ->
  << <<($A + (Byte rem ($Z - $A)))>> || <<Byte>> <= Bin >>.

generate_session_id(ByteLength) ->
  Bytes = crypto:strong_rand_bytes(ByteLength),
  Encoded = base64:encode(Bytes),
  clamp_visible(Encoded).

generate_session_id() -> generate_session_id(64).

%% @doc
%% Starts an MCP tool in session.
%% Tool is running until either explicitly
%% terminated or KeepAliveMs passes since last
%% message received from client.
%% @end
%% @param Tool: tool to run
%% @param KeepAliveMs: period for which session kept open in milliseconds
-spec start_mcp_session(string(), integer()) -> string().
start_mcp_session(Tool, KeepAliveMs) ->
  SessionId = generate_session_id(),
  Pid = mcp_server:start([Tool, KeepAliveMs]),
  ets:insert(mcp_sessions, {SessionId, {Tool, Pid}}),
  SessionId.

%% @doc
%% Finds MCP session PID by session ID.
%% @end
-spec get_mcp_session_by_id(string()) -> string() | none.
get_mcp_session_by_id(SessionId) ->
  case ets:lookup(mcp_sessions, SessionId) of
    [{SessionId, {_, Pid}}] ->
      Pid;
    [] ->
      none
  end.

%% @doc
%% Sends request to terminate MCP session process.
%% @end
-spec terminate_mcp_session(string()) -> ok | not_found.
terminate_mcp_session(SessionId) ->
  case get_mcp_session_by_id(SessionId) of
    none -> not_found;
    Pid ->
      gen_server:call(Pid, terminate),
      ets:remove(mcp_sessions, SessionId),
      ok
  end.
