%%%-------------------------------------------------------------------
%%% @doc EUnit suite for the env module (If-Env / If-Not-Env / If-Env-Def)
%%% Expectations come from the module's own doc comment.
%%% @end
%%%-------------------------------------------------------------------
-module(env_tests).
-include_lib("eunit/include/eunit.hrl").

-define(VAR, "MEOW_EUNIT_VAR").

rule(Name) -> proplists:get_value(Name, env:get_context_rules()).
req() -> test_support:request("GET", "/", [{"Host", "h"}]).

setup() -> test_support:setup_config(#{"LogLevel" => "0"}).
cleanup(_) -> os:unsetenv(?VAR), test_support:teardown_all(ok).

env_test_() ->
  {setup, fun setup/0, fun cleanup/1,
   fun(_) -> [module_tests(), if_env_tests(), if_not_env_tests(), if_env_def_tests()] end}.

module_tests() ->
  [{"init/0 returns ok", ?_assertEqual(ok, env:init())},
   {"terminate/1 returns ok", ?_assertEqual(ok, env:terminate(state))},
   {"the module has no custom rules", ?_assertEqual([], env:get_custom_rules())},
   {"the module exports exactly the three documented context rules",
    ?_assertEqual(["If-Env", "If-Not-Env", "If-Env-Def"],
                  [N || {N, _} <- env:get_context_rules()])}].

if_env_tests() ->
  [{"If-Env passes when the variable equals the value",
    ?_test(begin os:putenv(?VAR, "docker"), ?assert((rule("If-Env"))([?VAR, "docker"], req())) end)},
   {"If-Env fails when the value differs",
    ?_test(begin os:putenv(?VAR, "docker"), ?assertNot((rule("If-Env"))([?VAR, "podman"], req())) end)},
   {"If-Env fails when the variable is not set",
    ?_test(begin os:unsetenv(?VAR), ?assertNot((rule("If-Env"))([?VAR, "docker"], req())) end)},
   {"If-Env is case-sensitive on the value",
    ?_test(begin os:putenv(?VAR, "docker"), ?assertNot((rule("If-Env"))([?VAR, "Docker"], req())) end)},
   {"If-Env with one argument fails (invalid argument lists make the rule fail)",
    ?_assertNot((rule("If-Env"))([?VAR], req()))},
   {"If-Env with three arguments fails",
    ?_assertNot((rule("If-Env"))([?VAR, "a", "b"], req()))}].

if_not_env_tests() ->
  [{"If-Not-Env passes when the value differs",
    ?_test(begin os:putenv(?VAR, "docker"), ?assert((rule("If-Not-Env"))([?VAR, "podman"], req())) end)},
   {"If-Not-Env fails when the variable equals the value",
    ?_test(begin os:putenv(?VAR, "docker"), ?assertNot((rule("If-Not-Env"))([?VAR, "docker"], req())) end)},
   {"If-Not-Env passes when the variable is not set",
    ?_test(begin os:unsetenv(?VAR), ?assert((rule("If-Not-Env"))([?VAR, "docker"], req())) end)},
   {"If-Not-Env with one argument fails",
    ?_assertNot((rule("If-Not-Env"))([?VAR], req()))}].

if_env_def_tests() ->
  [{"If-Env-Def passes when the variable is defined",
    ?_test(begin os:putenv(?VAR, "1"), ?assert((rule("If-Env-Def"))([?VAR], req())) end)},
   {"If-Env-Def passes for a variable defined with an empty value",
    ?_test(begin os:putenv(?VAR, ""), ?assert((rule("If-Env-Def"))([?VAR], req())) end)},
   {"If-Env-Def fails when the variable is not set",
    ?_test(begin os:unsetenv(?VAR), ?assertNot((rule("If-Env-Def"))([?VAR], req())) end)},
   {"If-Env-Def with two arguments fails",
    ?_assertNot((rule("If-Env-Def"))([?VAR, "x"], req()))},
   {"If-Env-Def with no arguments fails",
    ?_assertNot((rule("If-Env-Def"))([], req()))}].
