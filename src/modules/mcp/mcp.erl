%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2026, Anna-Sofia Kasierocka
%%% @doc
%%%  A module which allows calling an arbitrary MCP tool gating stdio
%%%  JSON-RPC over HTTP.
%%% @end
%%%-------------------------------------------------------------------
-module(mcp).
-author("f104a").
-behaviour(nya_module).
-include("mcp.hrl").


%% API
-export([init/0, terminate/1, get_custom_rules/0]).

get_custom_rules() ->
  [
    {
      "MCP-Tool-Streamable",
      fun(Args, Response) ->
        mcp_streamable:rule_mcp_streamable_call(Args, Response)
      end
    },
    {
      "MCP-Tool-SSE",
      fun(Args, Response) ->
        mcp_sse:rule_mcp_sse_call(Args, Response)
      end
    }
  ].

init() ->
  mcp_port:init_mcp().

terminate(_) -> ok.
