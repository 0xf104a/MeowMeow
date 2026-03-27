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


%% API
-export([init/0, terminate/1, get_custom_rules/0]).

mcp_request_state(Request) ->
  AcceptHeader = maps:get(Request#request.header, "Accept"),
  MCPVersion = maps:get(Request#request.header, "")

rule_mcp_call("POST", Args, Response) -> ok;
rule_mcp_call("GET", Args, Response) -> ok;
rule_mcp_call("DELETE", Args, Response) -> ok;
rule_mcp_call(_, _, _) -> {aborted, 405}.

rule_mcp_call(Args, Response) ->
  Method = Response#response.request#request.method,

  rule_mcp_call(Method, Args, Response).

get_custom_rules() ->
  [
    {"MCP-Execute", fun(Args, Response) -> rule_mcp_call(Args, Response) end}
  ].

init() ->
  mcp_port:init_mcp().

terminate(_) -> ok.
