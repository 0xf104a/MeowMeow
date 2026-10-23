%%%-------------------------------------------------------------------
%%% @doc EUnit suite for the header module (Host / If-Header context rules)
%%% @end
%%%-------------------------------------------------------------------
-module(header_tests).
-include_lib("eunit/include/eunit.hrl").

rule(Name) -> proplists:get_value(Name, header:get_context_rules()).
req(Headers) -> test_support:request("GET", "/", Headers).

setup() -> test_support:setup_config(#{"LogLevel" => "0"}).

header_test_() ->
  {setup, fun setup/0, fun test_support:teardown_all/1,
   fun(_) -> [module_tests(), host_tests(), if_header_tests()] end}.

module_tests() ->
  [{"init/0 returns ok", ?_assertEqual(ok, header:init())},
   {"terminate/1 returns ok", ?_assertEqual(ok, header:terminate(state))},
   {"the module has no custom rules", ?_assertEqual([], header:get_custom_rules())},
   {"the module exports the If-Header and Host context rules",
    ?_assertEqual(["If-Header", "Host"], [N || {N, _} <- header:get_context_rules()])}].

host_tests() ->
  [{"Host passes when the Host header matches the wildcard (routes.conf: Host \"127.0.0.1*\")",
    ?_assert((rule("Host"))(["127.0.0.1*"], req([{"Host", "127.0.0.1:80"}])))},
   {"Host fails for a different host",
    ?_assertNot((rule("Host"))(["127.0.0.1*"], req([{"Host", "localhost"}])))},
   {"Host matches exactly without wildcards",
    ?_assert((rule("Host"))(["localhost"], req([{"Host", "localhost"}])))},
   {"Host does not match a prefix without a wildcard",
    ?_assertNot((rule("Host"))(["localhost"], req([{"Host", "localhost:8080"}])))},
   {"Host fails when the request carries no Host header",
    ?_assertNot((rule("Host"))(["127.0.0.1*"], req([])))},
   {"Host is looked up case-insensitively",
    ?_assert((rule("Host"))(["localhost"], req([{"HOST", "localhost"}])))},
   {"Host with the wrong number of arguments fails",
    ?_assertNot((rule("Host"))(["a", "b"], req([{"Host", "a"}])))}].

if_header_tests() ->
  [{"If-Header <name> passes when the header is present",
    ?_assert((rule("If-Header"))(["X-Test"], req([{"X-Test", "1"}])))},
   {"If-Header <name> is case-insensitive",
    ?_assert((rule("If-Header"))(["x-test"], req([{"X-Test", "1"}])))},
   {"If-Header <name> fails when the header is absent",
    ?_assertNot((rule("If-Header"))(["X-Test"], req([])))},
   {"If-Header <name> <value> passes when the lower-cased name and the value match",
    ?_assert((rule("If-Header"))(["x-test", "1"], req([{"X-Test", "1"}])))},
   {"If-Header <name> <value> fails when the value differs",
    ?_assertNot((rule("If-Header"))(["x-test", "2"], req([{"X-Test", "1"}])))},
   {"If-Header <name> <value> fails when the header is absent",
    ?_assertNot((rule("If-Header"))(["x-test", "1"], req([])))},
   {"EXPECTATION: 'If-Header X-Test 1' (name written as in the config) matches a request "
    "carrying 'X-Test: 1', exactly like the one-argument form does. "
    "FAILS BECAUSE: the two-argument clause looks the name up verbatim with maps:find/2 while "
    "parse_http stores header names lower-cased, so any header name with upper-case letters "
    "in routes.conf never matches",
    ?_assert((rule("If-Header"))(["X-Test", "1"], req([{"X-Test", "1"}])))},
   {"If-Header with no arguments fails",
    ?_assertNot((rule("If-Header"))([], req([{"X-Test", "1"}])))}].
