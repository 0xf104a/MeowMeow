%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. черв. 2026 13:16
%%%-------------------------------------------------------------------
-module(route).
-author("f104a").
-behaviour(nya_module).
-include("../../request.hrl").
-include("../../response.hrl").
-include("../../config.hrl").

%% API
-export([init/0, terminate/1, get_custom_rules/0, get_context_rules/0]).

init() -> ok.

terminate(_State) -> ok.

get_custom_rules() -> [].

get_context_rules() ->
  [
    {"Route", fun(Args, Request) -> context_route(Args, Request) end}
  ].

context_route([Pattern], Request) ->
  Route = Request#request.route,
  util:check_wildcard(Route, Pattern);
context_route(_, _) -> false.
