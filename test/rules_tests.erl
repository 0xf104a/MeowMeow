%%%-------------------------------------------------------------------
%%% @doc EUnit suite for rules.erl
%%%
%%% Covers the rule registry (ETS), the five built-in rules documented
%%% in the README (Abort, No-Content, Disallow, Set-Header, Set-Code),
%%% error isolation in execute_rule/3 and the rule-chain executor
%%% including context (block) rules.
%%% @end
%%%-------------------------------------------------------------------
-module(rules_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/config.hrl").
-include("../src/request.hrl").
-include("../src/response.hrl").

setup() ->
  test_support:setup_config(#{"LogLevel" => "0"}),
  test_support:setup_rules(),
  rules:register_context_rule("Always", fun(_, _) -> true end),
  rules:register_context_rule("Never", fun(_, _) -> false end),
  rules:register_context_rule("RouteIs",
    fun([Route], Req) -> Req#request.route == list_to_binary(Route) end),
  ok.

resp() ->
  test_support:response_for(test_support:request("GET", "/x", [{"Host", "h"}])).

%% init_rules/0 creates named (protected) tables owned by the caller, so it
%% is exercised in its own fixture whose setup process owns the tables.
init_rules_test_() ->
  {setup,
   fun() -> ?assertEqual(undefined, ets:info(rules)), ?assertEqual(ok, rules:init_rules()) end,
   fun(_) -> catch ets:delete(rules), catch ets:delete(rules_context), ok end,
   [{"init_rules creates the rules table",
     ?_assertNotEqual(undefined, ets:info(rules))},
    {"init_rules creates the rules_context table",
     ?_assertNotEqual(undefined, ets:info(rules_context))},
    {"the tables start empty",
     ?_assertEqual({0, 0}, {ets:info(rules, size), ets:info(rules_context, size)})}]}.

rules_test_() ->
  {setup, fun setup/0, fun test_support:teardown_all/1,
   [registry_tests(), execute_tests(), abort_tests(), no_content_tests(),
    disallow_tests(), set_header_tests(), set_code_tests(), chain_tests(),
    context_tests()]}.

%%--------------------------------------------------------------------
registry_tests() ->
  [{"register_basic registers the five README rules",
    ?_assertEqual([true, true, true, true, true],
                  [ets:member(rules, R) || R <- ["Abort", "No-Content", "Disallow",
                                                  "Set-Header", "Set-Code"]])},
   {"register_rule stores a handler retrievable by name",
    ?_test(begin
             rules:register_rule("Custom", fun(_, R) -> {ok, R} end),
             ?assertMatch([{"Custom", F}] when is_function(F, 2), ets:lookup(rules, "Custom"))
           end)},
   {"register_context_rule stores into the context table, not the rule table",
    ?_test(begin
             ?assert(ets:member(rules_context, "Always")),
             ?assertNot(ets:member(rules, "Always"))
           end)}].

%%--------------------------------------------------------------------
execute_tests() ->
  [{"execute_rule calls the handler with Args and Response",
    ?_test(begin
             rules:register_rule("Echo", fun(Args, R) -> {ok, R#response{body = Args}} end),
             ?assertMatch({ok, #response{body = ["a", "b"]}},
                          rules:execute_rule("Echo", ["a", "b"], resp()))
           end)},
   {"execute_rule of an unregistered rule is {aborted, 500}",
    ?_assertEqual({aborted, 500}, rules:execute_rule("NoSuchRule", [], resp()))},
   {"a handler raising an error is isolated as {aborted, 502}",
    ?_test(begin
             rules:register_rule("Crash", fun(_, _) -> erlang:error(kaboom) end),
             ?assertEqual({aborted, 502}, rules:execute_rule("Crash", [], resp()))
           end)},
   {"a handler failing a pattern match is isolated as {aborted, 502}",
    ?_test(begin
             rules:register_rule("BadMatch", fun(_, _) -> {ok, _} = file:read_file_info("/nonexistent/meow") end),
             ?assertEqual({aborted, 502}, rules:execute_rule("BadMatch", [], resp()))
           end)},
   {"a handler that throws is isolated as {aborted, 502} like any other "
    "crash, so a buggy module cannot kill the connection handler. "
    "(regression: this used to fail because execute_rule/3 only catches class error (catch error:Error), "
    "throw and exit propagate to the handler process)",
    ?_test(begin
             rules:register_rule("Throw", fun(_, _) -> throw(oops) end),
             ?assertEqual({aborted, 502}, rules:execute_rule("Throw", [], resp()))
           end)}].

%%--------------------------------------------------------------------
abort_tests() ->
  [{"Abort <code> stops processing with that code (README)",
    ?_assertEqual({aborted, 418}, rules:execute_rule("Abort", ["418"], resp()))},
   {"Abort 402 (nonsense.conf)",
    ?_assertEqual({aborted, 402}, rules:execute_rule("Abort", ["402"], resp()))},
   {"Abort with a non-numeric code is a misconfiguration reported as 502",
    ?_assertEqual({aborted, 502}, rules:execute_rule("Abort", ["teapot"], resp()))},
   {"Abort without arguments is a misconfiguration reported as 502",
    ?_assertEqual({aborted, 502}, rules:execute_rule("Abort", [], resp()))}].

no_content_tests() ->
  [{"No-Content yields a ready-to-send 204 with an empty body (README)",
    ?_test(begin
             {Tag, R} = rules:execute_rule("No-Content", true, resp()),
             ?assertEqual(ready2send, Tag),
             ?assertEqual(204, R#response.code),
             ?assertEqual("", R#response.body),
             ?assert(R#response.is_ready2send)
           end)},
   {"No-Content adds Server and Date headers",
    ?_test(begin
             {ready2send, R} = rules:execute_rule("No-Content", true, resp()),
             ?assertEqual(?version, maps:get("Server", R#response.headers)),
             ?assert(maps:is_key("Date", R#response.headers))
           end)},
   {"No-Content keeps headers set by earlier rules (Set-Header before No-Content)",
    ?_test(begin
             {ok, R1} = rules:execute_rule("Set-Header", ["X-Powered-By", "Pusheen"], resp()),
             {ready2send, R2} = rules:execute_rule("No-Content", true, R1),
             ?assertEqual("Pusheen", maps:get("X-Powered-By", R2#response.headers))
           end)}].

disallow_tests() ->
  [{"Disallow aborts with 403 Forbidden (README)",
    ?_assertEqual({aborted, 403}, rules:execute_rule("Disallow", true, resp()))}].

set_header_tests() ->
  [{"Set-Header <header> <value> sets a response header (README)",
    ?_test(begin
             {ok, R} = rules:execute_rule("Set-Header", ["X-Powered-By", "Pusheen the Cat"], resp()),
             ?assertEqual("Pusheen the Cat", maps:get("X-Powered-By", R#response.headers))
           end)},
   {"Set-Header keeps the base headers (Server)",
    ?_test(begin
             {ok, R} = rules:execute_rule("Set-Header", ["X-A", "1"], resp()),
             ?assertEqual(?version, maps:get("Server", R#response.headers))
           end)},
   {"Set-Header overwrites an existing header (mimes.conf sets Content-Type twice)",
    ?_test(begin
             {ok, R1} = rules:execute_rule("Set-Header", ["Content-Type", "application/octet-stream"], resp()),
             {ok, R2} = rules:execute_rule("Set-Header", ["Content-Type", "text/html"], R1),
             ?assertEqual("text/html", maps:get("Content-Type", R2#response.headers))
           end)},
   {"Set-Header with a single argument is a misconfiguration reported as 502",
    ?_assertEqual({aborted, 502}, rules:execute_rule("Set-Header", ["X-Only"], resp()))}].

set_code_tests() ->
  [{"Set-Code <code> overrides the status code (README)",
    ?_test(begin
             {ok, R} = rules:execute_rule("Set-Code", ["451"], resp()),
             ?assertEqual(451, R#response.code),
             ?assert(R#response.status_code_override)
           end)},
   {"after Set-Code a module's set_code is ignored (Send-File after Set-Code 451 keeps 451)",
    ?_test(begin
             {ok, R} = rules:execute_rule("Set-Code", ["451"], resp()),
             ?assertEqual(451, (response:set_code(R, 200))#response.code)
           end)},
   {"Set-Code with a non-numeric code is a misconfiguration reported as 502",
    ?_assertEqual({aborted, 502}, rules:execute_rule("Set-Code", ["abc"], resp()))}].

%%--------------------------------------------------------------------
chain_tests() ->
  [{"an empty chain returns the response untouched as {ok, R}",
    ?_test(begin R = resp(), ?assertEqual({ok, R}, rules:rulechain_exec([], R)) end)},
   {"rules are applied in order and accumulate",
    ?_test(begin
             {ok, R} = rules:rulechain_exec([{"Set-Header", ["A", "1"]},
                                             {"Set-Header", ["B", "2"]}], resp()),
             ?assertEqual("1", maps:get("A", R#response.headers)),
             ?assertEqual("2", maps:get("B", R#response.headers))
           end)},
   {"Abort stops the chain: later rules are not run",
    ?_assertEqual({aborted, 418},
                  rules:rulechain_exec([{"Set-Header", ["A", "1"]},
                                        {"Abort", ["418"]},
                                        {"Set-Header", ["B", "2"]}], resp()))},
   {"a terminal rule (No-Content) stops the chain and the result is reported as {ok, R}",
    ?_test(begin
             {Tag, R} = rules:rulechain_exec([{"No-Content", true},
                                              {"Set-Header", ["B", "2"]}], resp()),
             ?assertEqual(ok, Tag),
             ?assertEqual(204, R#response.code),
             ?assertNot(maps:is_key("B", R#response.headers))
           end)},
   {"a rule reporting {sent, R} stops the chain and is passed through",
    ?_test(begin
             rules:register_rule("Sender", fun(_, R) -> {sent, R#response{is_sent = true}} end),
             ?assertMatch({sent, #response{is_sent = true}},
                          rules:rulechain_exec([{"Sender", []}, {"Abort", ["500"]}], resp()))
           end)},
   {"a rule returning a legacy/unknown shape aborts with 502",
    ?_test(begin
             rules:register_rule("Legacy", fun(_, R) -> R end),
             ?assertEqual({aborted, 502}, rules:rulechain_exec([{"Legacy", []}], resp()))
           end)},
   {"an unknown rule name in the chain aborts with 500",
    ?_assertEqual({aborted, 500}, rules:rulechain_exec([{"Nope", []}], resp()))}].

%%--------------------------------------------------------------------
context_tests() ->
  Inner = [{"Set-Header", ["X-In", "yes"]}],
  [{"a passing context block runs its inner rules",
    ?_test(begin
             {ok, R} = rules:rulechain_exec([{context, "Always", [], Inner}], resp()),
             ?assertEqual("yes", maps:get("X-In", R#response.headers))
           end)},
   {"a failing context block skips its inner rules",
    ?_test(begin
             {ok, R} = rules:rulechain_exec([{context, "Never", [], Inner}], resp()),
             ?assertNot(maps:is_key("X-In", R#response.headers))
           end)},
   {"rules after a skipped block still run",
    ?_test(begin
             {ok, R} = rules:rulechain_exec([{context, "Never", [], Inner},
                                             {"Set-Header", ["X-After", "1"]}], resp()),
             ?assertEqual("1", maps:get("X-After", R#response.headers))
           end)},
   {"context rules receive the request from the response record",
    ?_test(begin
             {ok, R} = rules:rulechain_exec([{context, "RouteIs", ["/x"], Inner}], resp()),
             ?assertEqual("yes", maps:get("X-In", R#response.headers)),
             {ok, R2} = rules:rulechain_exec([{context, "RouteIs", ["/other"], Inner}], resp()),
             ?assertNot(maps:is_key("X-In", R2#response.headers))
           end)},
   {"context blocks nest",
    ?_test(begin
             Nested = [{context, "Always", [], [{context, "Always", [], Inner}]}],
             {ok, R} = rules:rulechain_exec(Nested, resp()),
             ?assertEqual("yes", maps:get("X-In", R#response.headers)),
             Nested2 = [{context, "Always", [], [{context, "Never", [], Inner}]}],
             {ok, R2} = rules:rulechain_exec(Nested2, resp()),
             ?assertNot(maps:is_key("X-In", R2#response.headers))
           end)},
   {"an Abort inside a passing block aborts the whole chain",
    ?_assertEqual({aborted, 418},
                  rules:rulechain_exec([{context, "Always", [], [{"Abort", ["418"]}]},
                                        {"Set-Header", ["X", "1"]}], resp()))}].
