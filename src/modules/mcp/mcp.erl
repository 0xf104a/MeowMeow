%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2026, Anna-Sofia Kasierocka
%%% @doc
%%%  A module which allows calling an arbitrary MCP tool gating stdio
%%%  JSON-RPC over HTTP.
%%% @end
%%% @see https://modelcontextprotocol.io/specification/2025-11-25/basic/transports
%%%-------------------------------------------------------------------
-module(mcp).
-author("f104a").
-behaviour(nya_module).
-include("../../response.hrl").
-include("../../request.hrl").
-include("mcp.hrl").


%% API
-export([init/0, terminate/1, get_custom_rules/0]).

mcp_request_state(Request) ->
  Headers = Request#request.header,
  AcceptHeader = maps:get("Accept", Headers, undefined),
  MCPVersion = maps:get("MCP-Protocol-Version", Headers, "2025-03-26"),
  MCPSessionID = maps:get("MCP-Session-Id", Headers, none),
  IsInitializeRequest = is_initialize_request(Request#request.body),
  ContentType = maps:get("Content-Type", Headers, none),
  {AcceptHeader, MCPVersion, MCPSessionID, IsInitializeRequest, ContentType}.

handle_mcp_session_init(Response, Tool, KeepAliveMs, DecodedRequest) ->
  RequestId = maps:get(<<"id">>, DecodedRequest),
  case RequestId of
    none -> {aborted, 422};
    _ ->
      ParsedID = list_to_integer(RequestId),
      MCPSessionID = mcp_port:start_mcp_session(Tool),
      Response#response{
        is_ready = true,
        code = 201,
        headers = maps:update(Response#response.headers,
          #{"Content-Type" => "application/json", "MCP-Session-ID" => MCPSessionID}
        ),
        body = iolist_to_binary(json:encode(#{
          <<"json_rpc">> => <<"2.0">>,
          <<"result">> => #{<<"protocolVersion">> => <<?mcp_version>>},
          <<"id">> => ParsedID
        }))}
  end.

%% No JSON lib — scan for the method field in the raw binary.
%% Safe enough: we only need this one check at the gateway level.
is_initialize_request(Body) when is_binary(Body) ->
  binary:match(Body, <<"\"method\"">>) =/= nomatch andalso
    binary:match(Body, <<"\"initialize\"">>) =/= nomatch;
is_initialize_request(Body) when is_list(Body) ->
  is_initialize_request(list_to_binary(Body));
is_initialize_request(_) ->
  false.

rule_mcp_call(_, _, {_, MCPVersion, _, _, _}, _) when MCPVersion < ?mcp_version -> {aborted, 501};
rule_mcp_call("POST", _, {_, _, MCPSessionID, IsInitializeRequest, _}, _)
  when MCPSessionID == none, not IsInitializeRequest -> {aborted, 400};
rule_mcp_call("POST", _, {_, _, _, _, ContentType}, _)
  when ContentType /= "application/json" -> {aborted, 415};
rule_mcp_call("POST", [Tool, KeepAliveMs], {_, _, _, IsInitializeRequest, _}, Response)
  when IsInitializeRequest ->
  DecodedRequest = json:decode(Response#response.request#request.body),
  handle_mcp_session_init(Response, Tool, KeepAliveMs, DecodedRequest);
rule_mcp_call("GET", Args, {AcceptHeader, MCPVersion, MCPSessionID, IsInitializeRequest, ContentType}, Response) -> {aborted, 501};
rule_mcp_call("DELETE", Args, {AcceptHeader, MCPVersion, MCPSessionID, IsInitializeRequest, ContentType}, Response) -> {aborted, 501};
rule_mcp_call(Method, _, _, _) ->
  logging:debug("Method not allowed ~p", [Method]),
  {aborted, 405}.

rule_mcp_call(Args, Response) ->
  Method = Response#response.request#request.method,

  rule_mcp_call(Method, Args,
    mcp_request_state(Response#response.request), Response).

get_custom_rules() ->
  [
    {"MCP-Tool", fun(Args, Response) -> rule_mcp_call(Args, Response) end}
  ].

init() ->
  mcp_port:init_mcp().

terminate(_) -> ok.
