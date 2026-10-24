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
-export([init_mcp/0, start_mcp_session/2, start_mcp_session/3, terminate_mcp_session/1, get_mcp_session_by_id/1]).

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
-spec start_mcp_session(string(), integer()) -> binary().
start_mcp_session(Tool, KeepAliveMs) ->
  {ok, SessionId} = start_mcp_session(Tool, KeepAliveMs, #{}),
  SessionId.

%% @doc
%% Like start_mcp_session/2 with the rule options of the `MCP-Tool-*'
%% rule (see mcp_options). Returns {error, busy} when the session caps
%% leave no slot for this tool within the configured wait time.
%% @end
-spec start_mcp_session(string(), integer(), map()) ->
        {ok, binary()} | {error, busy | term()}.
start_mcp_session(Tool, KeepAliveMs, RuleOpts) ->
  case mcp_server:start([Tool, KeepAliveMs, RuleOpts]) of
    {ok, Pid} ->
      SessionId = generate_session_id(),
      ets:insert(mcp_sessions, {SessionId, {Tool, Pid}}),
      {ok, SessionId};
    {error, {shutdown, busy}} ->
      {error, busy};
    {error, Reason} ->
      logging:err("Failed to start MCP tool ~s: ~p", [Tool, Reason]),
      {error, Reason}
  end.

%% @doc
%% Finds MCP session PID by session ID.
%% @end
%% A session whose process is gone (keep-alive expiry, tool exit, DELETE)
%% is forgotten here: its id must answer 404 afterwards, not crash the
%% caller with noproc. Nothing else removes entries when the process dies.
-spec get_mcp_session_by_id(binary()) -> {ok, pid()} | none.
get_mcp_session_by_id(SessionId) ->
  case ets:lookup(mcp_sessions, SessionId) of
    [{SessionId, {_, Pid}}] when is_pid(Pid) ->
      case is_process_alive(Pid) of
        true -> {ok, Pid};
        false ->
          ets:delete(mcp_sessions, SessionId),
          none
      end;
    [{SessionId, _}] ->
      ets:delete(mcp_sessions, SessionId),
      none;
    [] ->
      none
  end.

%% @doc
%% Sends request to terminate MCP session process.
%% @end
-spec terminate_mcp_session(binary()) -> ok | not_found.
terminate_mcp_session(SessionId) ->
  case get_mcp_session_by_id(SessionId) of
    none -> not_found;
    {ok, Pid} ->
      catch mcp_server:terminate_session(Pid),
      ets:delete(mcp_sessions, SessionId),
      ok
  end.
