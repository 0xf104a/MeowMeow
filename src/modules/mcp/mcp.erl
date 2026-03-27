%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2026, Anna-Sofia Kasierocka
%%% @doc
%%%  A module which allows calling an arbitrary MCP tool
%%% @end
%%%-------------------------------------------------------------------
-module(mcp).
-author("f104a").
-behaviour(nya_module).


%% API
-export([init/0]).

init() ->
  mcp_port:init_mcp().
