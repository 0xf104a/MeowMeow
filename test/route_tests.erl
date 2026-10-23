%%%-------------------------------------------------------------------
%%% @doc EUnit suite for the route module (Route context rule)
%%% @end
%%%-------------------------------------------------------------------
-module(route_tests).
-include_lib("eunit/include/eunit.hrl").

rule() -> proplists:get_value("Route", route:get_context_rules()).
req(Path) -> test_support:request("GET", Path, [{"Host", "h"}]).

setup() -> test_support:setup_config(#{"LogLevel" => "0"}).

route_test_() ->
  {setup, fun setup/0, fun test_support:teardown_all/1,
   fun(_) -> [module_tests(), match_tests()] end}.

module_tests() ->
  [{"init/0 returns ok", ?_assertEqual(ok, route:init())},
   {"terminate/1 returns ok", ?_assertEqual(ok, route:terminate(state))},
   {"the module has no custom rules", ?_assertEqual([], route:get_custom_rules())},
   {"the module exports the Route context rule",
    ?_assertEqual(["Route"], [N || {N, _} <- route:get_context_rules()])}].

match_tests() ->
  [{"Route matches the request path against a wildcard (mimes.conf: Route /*.html)",
    ?_assert((rule())(["/*.html"], req("/index.html")))},
   {"Route matches an exact path", ?_assert((rule())(["/catnip"], req("/catnip")))},
   {"Route does not match a longer path", ?_assertNot((rule())(["/catnip"], req("/catnip_info")))},
   {"Route * matches everything", ?_assert((rule())(["*"], req("/anything/at/all")))},
   {"Route ignores the query string", ?_assert((rule())(["/a"], req("/a?x=1")))},
   {"Route with '?' matches a single character", ?_assert((rule())(["/?.txt"], req("/a.txt")))},
   {"Route with the wrong number of arguments fails",
    ?_assertNot((rule())(["/a", "/b"], req("/a")))},
   {"Route with no arguments fails", ?_assertNot((rule())([], req("/a")))}].
