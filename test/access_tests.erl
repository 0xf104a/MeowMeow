%%%-------------------------------------------------------------------
%%% @doc EUnit suite for access.erl (routes.conf parser + rule lookup)
%%%
%%% The grammar under test is the one documented in the README:
%%%
%%%   Route <wildcard>          (any registered context rule opens a block)
%%%     Directive Args
%%%   End
%%%   Include "<file>"
%%%   # comments, blank lines and indentation are ignored
%%%
%%% Error paths call server:abort_init/1, which halts the VM, so those
%%% tests temporarily replace the `server' module with a stub that
%%% reports the call (see test_support:stub_server_abort_init/1).
%%% reload/0 reads the compiled-in /etc/nya/routes.conf and is not
%%% covered here.
%%% @end
%%%-------------------------------------------------------------------
-module(access_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/request.hrl").
-include("../src/response.hrl").

setup() ->
  test_support:setup_config(#{"LogLevel" => "0"}),
  test_support:setup_rules(),
  %% The shipped modules provide the Route/Host/If-* context rules and
  %% the Send-File/DocDir rules used by the real configuration files.
  [{ok, _} = nya_loader:nya_load(M) || M <- ["static", "header", "route", "env"]],
  test_support:tmp_dir().

cleanup(Dir) ->
  test_support:rm_rf(Dir),
  test_support:teardown_all(ok).

access_test_() ->
  {setup, fun setup/0, fun cleanup/1,
   fun(Dir) ->
     [get_cmd_tests(), parse_tests(Dir), shipped_routes_tests(Dir),
      error_path_tests(Dir), load_tests(Dir), get_rules_tests(Dir)]
   end}.

conf(Dir, Name, Content) ->
  test_support:write_file(filename:join(Dir, Name), Content).

%% Copies tests/config/*.conf into Dir rewriting the hard-coded
%% /etc/nya/ include paths so the real E2E configuration can be parsed.
shipped(Dir) ->
  lists:foreach(
    fun(F) ->
      {ok, Bin} = file:read_file(filename:join("tests/config", F)),
      Rewritten = binary:replace(Bin, <<"/etc/nya/">>, list_to_binary(Dir ++ "/"), [global]),
      ok = file:write_file(filename:join(Dir, F), Rewritten)
    end, ["routes.conf", "mimes.conf", "nonsense.conf", "bad_meow.conf"]),
  filename:join(Dir, "routes.conf").

%%--------------------------------------------------------------------
get_cmd_tests() ->
  [{"an empty line is 'pass'", ?_assertEqual(pass, access:get_cmd(""))},
   {"a directive with one argument", ?_assertEqual({"Abort", ["418"]}, access:get_cmd("Abort 418"))},
   {"a directive without arguments gets 'true' as its argument list",
    ?_assertEqual({"No-Content", true}, access:get_cmd("No-Content"))},
   {"quoted arguments with spaces are kept whole (README: strings with spaces MUST be quoted)",
    ?_assertEqual({"Set-Header", ["X-Powered-By", "Pusheen the Cat"]},
                  access:get_cmd("Set-Header X-Powered-By \"Pusheen the Cat\""))},
   {"Include takes a quoted path",
    ?_assertEqual({"Include", ["/etc/nya/mimes.conf"]},
                  access:get_cmd("Include \"/etc/nya/mimes.conf\""))},
   {"two unquoted arguments",
    ?_assertEqual({"Set-Header", ["Content-Type", "text/html"]},
                  access:get_cmd("Set-Header Content-Type text/html"))},
   {"EXPECTATION: extra whitespace between the directive and its argument is ignored "
    "('Abort  418' is the same as 'Abort 418'). "
    "FAILS BECAUSE: get_cmd/1 splits at the first space only and hands ' 418' to "
    "parse_arguments/1, which produces a leading empty argument",
    ?_assertEqual({"Abort", ["418"]}, access:get_cmd("Abort  418"))}].

%%--------------------------------------------------------------------
parse_tests(Dir) ->
  [{"a Route block becomes a context tuple wrapping its rules",
    ?_assertEqual([{context, "Route", ["*"],
                    [{"Set-Header", ["X-Powered-By", "Pusheen the Cat"]}]}],
                  access:parse_access(conf(Dir, "minimal.conf",
                    "Route *\n Set-Header X-Powered-By \"Pusheen the Cat\"\nEnd\n")))},
   {"a top-level rule outside any block is kept as is",
    ?_assertEqual([{"Abort", ["418"]}],
                  access:parse_access(conf(Dir, "toplevel.conf", "Abort 418\n")))},
   {"comments, blank lines, tabs and trailing comments are ignored",
    ?_assertEqual([{context, "Route", ["/a"], [{"Abort", ["418"]}]}],
                  access:parse_access(conf(Dir, "comments.conf",
                    "# leading comment\n\n\tRoute /a # trailing comment\n\t\tAbort 418\n\tEnd\n\n")))},
   {"blocks nest (Host containing Route, as in tests/config/routes.conf)",
    ?_assertEqual([{context, "Host", ["127.0.0.1*"],
                    [{"Set-Header", ["X-Different-Host", "true"]},
                     {context, "Route", ["/duckduck*"],
                      [{"Set-Header", ["X-Continuation", "goose"]}]},
                     {"Send-File", ["/var/www/index.html"]}]}],
                  access:parse_access(conf(Dir, "nested.conf",
                    "Host \"127.0.0.1*\"\n"
                    " Set-Header X-Different-Host true\n"
                    " Route /duckduck*\n"
                    "   Set-Header X-Continuation goose\n"
                    " End\n"
                    " Send-File /var/www/index.html\n"
                    "End\n")))},
   {"Include splices the included file's rules in place",
    ?_test(begin
             Inc = conf(Dir, "inc.conf", "Route /inc\n Abort 402\nEnd\n"),
             Main = conf(Dir, "main.conf",
                         "Abort 418\nInclude \"" ++ Inc ++ "\"\nAbort 403\n"),
             ?assertEqual([{"Abort", ["418"]},
                           {context, "Route", ["/inc"], [{"Abort", ["402"]}]},
                           {"Abort", ["403"]}],
                          access:parse_access(Main))
           end)},
   {"a quoted route pattern is unquoted (routes.conf: Route \"/quotes_418\")",
    ?_assertEqual([{context, "Route", ["/quotes_418"], [{"Abort", ["418"]}]}],
                  access:parse_access(conf(Dir, "quoted.conf",
                    "Route \"/quotes_418\"\n Abort 418\nEnd\n")))},
   {"the bare 'Section' keyword is ignored",
    ?_assertEqual([{"Abort", ["418"]}],
                  access:parse_access(conf(Dir, "section.conf", "Section\nAbort 418\n")))},
   {"a block that is not closed before EOF still yields its rules",
    ?_assertEqual([{context, "Route", ["/a"], [{"Abort", ["418"]}]}],
                  access:parse_access(conf(Dir, "noend.conf", "Route /a\n Abort 418\n")))},
   {"an empty block yields a context with no rules",
    ?_assertEqual([{context, "Route", ["/a"], []}],
                  access:parse_access(conf(Dir, "emptyblock.conf", "Route /a\nEnd\n")))},
   {"an empty file yields no rules",
    ?_assertEqual([], access:parse_access(conf(Dir, "empty.conf", "")))},
   {"a No-Content line inside a block carries no arguments",
    ?_assertMatch([{context, "Route", ["/generate_204"], [{"No-Content", _}]}],
                  access:parse_access(conf(Dir, "nocontent.conf",
                    "Route /generate_204\n No-Content\nEnd\n")))},
   {"If-Env blocks take two arguments",
    ?_assertEqual([{context, "If-Env", ["RUNTYPE", "docker"],
                    [{"Set-Header", ["X-Environment", "docker"]}]}],
                  access:parse_access(conf(Dir, "env.conf",
                    "If-Env RUNTYPE docker\n    Set-Header X-Environment docker\nEnd\n")))}].

%%--------------------------------------------------------------------
%% The real configuration files must parse into what the E2E tests
%% (tests/simple_test/*.rb) assume.
shipped_routes_tests(Dir) ->
  [{"tests/config/routes.conf parses into the rule list the E2E suite relies on",
    ?_test(begin
             Rules = access:parse_access(shipped(Dir)),
             ?assert(is_list(Rules)),
             %% mimes.conf is included first: default octet-stream, then html
             ?assertEqual({context, "Route", ["*"],
                           [{"Set-Header", ["Content-Type", "application/octet-stream"]}]},
                          hd(Rules)),
             ?assert(lists:member({context, "Route", ["/*.html"],
                                   [{"Set-Header", ["Content-Type", "text/html"]}]}, Rules)),
             ?assert(lists:member({context, "Route", ["*"],
                                   [{"Set-Header", ["X-Powered-By", "Pusheen the Cat"]}]}, Rules)),
             ?assert(lists:member({context, "Host", ["127.0.0.1*"],
                                   [{"Set-Header", ["X-Different-Host", "true"]},
                                    {context, "Route", ["/duckduck*"],
                                     [{"Set-Header", ["X-Continuation", "goose"]}]},
                                    {"Send-File", ["/var/www/index.html"]}]}, Rules)),
             ?assert(lists:member({context, "If-Env-Def", ["RUNTYPE"],
                                   [{"Set-Header", ["X-Environment-Present", "1"]}]}, Rules)),
             ?assert(lists:member({context, "If-Env", ["RUNTYPE", "docker"],
                                   [{"Set-Header", ["X-Environment", "docker"]}]}, Rules)),
             ?assert(lists:member({context, "If-Not-Env", ["RUNTYPE", "docker"],
                                   [{"Abort", ["500"]}]}, Rules)),
             ?assertMatch([_], [R || {context, "Route", ["/generate_204"], [{"No-Content", _}]} = R <- Rules]),
             ?assert(lists:member({context, "Route", ["/i_am_a_teapot"], [{"Abort", ["418"]}]}, Rules)),
             ?assert(lists:member({context, "Route", ["/catnip"],
                                   [{"Set-Code", ["451"]},
                                    {"Send-File", ["/var/www/catnip.html"]}]}, Rules)),
             ?assert(lists:member({context, "Route", ["/catnip_info"],
                                   [{"Send-File", ["/var/www/catnip.html"]}]}, Rules)),
             ?assert(lists:member({context, "Route", ["/FatCat"],
                                   [{"Send-File", ["/var/www/FatCat"]}]}, Rules)),
             ?assert(lists:member({context, "Route", ["/get_500"],
                                   [{"Send-File", ["/var/www/non-existent"]}]}, Rules)),
             ?assert(lists:member({context, "Route", ["/quotes_418"], [{"Abort", ["418"]}]}, Rules)),
             %% nested include of nonsense.conf
             ?assert(lists:member({context, "Route", ["/absolute_includable_nonsense"],
                                   [{"Abort", ["402"]}]}, Rules)),
             %% the catch-all DocDir must be last
             ?assertEqual({context, "Route", ["*"], [{"DocDir", ["/var/www"]}]}, lists:last(Rules))
           end)},
   {"config/routes.conf (the shipped default) parses",
    ?_test(begin
             {ok, Bin} = file:read_file("config/routes.conf"),
             Rewritten = binary:replace(Bin, <<"/etc/MeowMeow/">>, list_to_binary(Dir ++ "/"), [global]),
             ok = file:write_file(filename:join(Dir, "default_routes.conf"), Rewritten),
             {ok, Mimes} = file:read_file("config/mimes.conf"),
             ok = file:write_file(filename:join(Dir, "mimes.conf"), Mimes),
             Rules = access:parse_access(filename:join(Dir, "default_routes.conf")),
             ?assert(lists:member({context, "Route", ["*"],
                                   [{"Set-Header", ["X-Powered-By", "Pusheen the Cat"]},
                                    {"DocDir", ["/var/www/html"]}]}, Rules)),
             ?assertMatch([_], [R || {context, "Route", ["/generate_204"], [{"No-Content", _}]} = R <- Rules])
           end)},
   {"EXPECTATION: every Set-Header in config/mimes.conf has a header name AND a value, "
    "otherwise the rule crashes at request time (rule_set_header/2 needs [Header, Value] "
    "and execute_rule turns the crash into a 502 for e.g. every *.mp3 request). "
    "FAILS BECAUSE: the /*.mp3 and /*.mp4 blocks read 'Set-Header audio/mpeg' and "
    "'Set-Header video/mp4' - the 'Content-Type' name is missing",
    ?_test(begin
             {ok, Mimes} = file:read_file("config/mimes.conf"),
             F = filename:join(Dir, "mimes_only.conf"),
             ok = file:write_file(F, Mimes),
             Rules = access:parse_access(F),
             Bad = [{Pat, Args} || {context, "Route", [Pat], Inner} <- Rules,
                                   {"Set-Header", Args} <- Inner, length(Args) =/= 2],
             ?assertEqual([], Bad)
           end)}].

%%--------------------------------------------------------------------
with_stub(Fun) ->
  test_support:flush(),
  ok = test_support:stub_server_abort_init(self()),
  try Fun() after test_support:restore_server() end.

error_path_tests(Dir) ->
  [{"an unknown directive aborts initialisation with the directive name and parse_access reports it",
    ?_test(with_stub(fun() ->
             F = conf(Dir, "unknown.conf", "Bogus 1\n"),
             ?assertEqual({error, "Bogus"}, access:parse_access(F)),
             ?assertEqual({ok, {abort_init, "Bogus"}}, test_support:recv(500))
           end))},
   {"load_access refuses to load a configuration with an unknown directive",
    ?_test(with_stub(fun() ->
             F = conf(Dir, "unknown2.conf", "Bogus 1\n"),
             ?assertEqual({error, "Bogus"}, access:load_access(F)),
             ?assertEqual(undefined, ets:info(access))
           end))},
   {"Include of a missing file aborts initialisation with reason 'open' (tests/config/bad_meow.conf)",
    ?_test(with_stub(fun() ->
             F = filename:join(Dir, "bad_meow.conf"),
             shipped(Dir),
             ?assertEqual({error, open}, access:parse_access(F)),
             ?assertEqual({ok, {abort_init, open}}, test_support:recv(500))
           end))},
   {"EXPECTATION: an unknown directive inside a block is an error for the whole file, "
    "exactly like one at top level. "
    "FAILS BECAUSE: parse_custom_rule/3 stores whatever the nested parse_section "
    "returned - here {error, \"Bogus\"} - as the block's rule list instead of "
    "propagating the error, so parse_access returns a rule list containing garbage "
    "(only the init:stop in abort_init hides this in production)",
    ?_test(with_stub(fun() ->
             F = conf(Dir, "nested_unknown.conf", "Route /a\n Bogus\nEnd\n"),
             ?assertEqual({error, "Bogus"}, access:parse_access(F))
           end))},
   {"EXPECTATION: parse_access/1 of a missing routes file returns {error, Reason} so "
    "server:start/0 can print 'Refusing to start server' (it pattern-matches {error, _}). "
    "FAILS BECAUSE: parse_access/1 passes file:open's {error, enoent} straight into "
    "parse_section/2 which only has a clause for {ok, Dev}, so it crashes with function_clause",
    ?_assertMatch({error, _}, access:parse_access(filename:join(Dir, "does_not_exist.conf")))}].

%%--------------------------------------------------------------------
load_tests(Dir) ->
  [{"load_access returns ok and stores the parsed rules in the access table",
    ?_test(begin
             F = conf(Dir, "load.conf", "Abort 418\n"),
             ?assertEqual(ok, access:load_access(F)),
             try
               ?assertEqual([{table, [{"Abort", ["418"]}]}], ets:lookup(access, table))
             after
               access:unload()
             end
           end)},
   {"unload deletes the access table",
    ?_test(begin
             F = conf(Dir, "load2.conf", "Abort 418\n"),
             ok = access:load_access(F),
             access:unload(),
             ?assertEqual(undefined, ets:info(access))
           end)}].

%%--------------------------------------------------------------------
with_access(File, Fun) ->
  ok = access:load_access(File),
  try Fun() after access:unload() end.

req(Route, Host) ->
  test_support:request("GET", Route, [{"Host", Host}]).

names(Rules) -> [N || {N, _} <- Rules].

get_rules_tests(Dir) ->
  [{"get_rules flattens the blocks whose context matches the request",
    ?_test(with_access(shipped(Dir), fun() ->
             Rules = access:get_rules(req("/i_am_a_teapot", "localhost")),
             ?assert(lists:member({"Abort", ["418"]}, Rules)),
             ?assertNot(lists:keymember("No-Content", 1, Rules)),
             ?assertNot(lists:keymember("Send-File", 1, Rules))
           end))},
   {"rules keep configuration order: mimes default before specific mime, DocDir last",
    ?_test(with_access(shipped(Dir), fun() ->
             Rules = access:get_rules(req("/mime.html", "localhost")),
             Octet = {"Set-Header", ["Content-Type", "application/octet-stream"]},
             Html = {"Set-Header", ["Content-Type", "text/html"]},
             ?assert(lists:member(Octet, Rules)),
             ?assert(lists:member(Html, Rules)),
             ?assert(string:str(Rules, [Octet]) < string:str(Rules, [Html])),
             ?assertEqual({"DocDir", ["/var/www"]}, lists:last(Rules))
           end))},
   {"Host block applies only when the Host header matches; a nested Route block is kept as a "
    "context tuple for rules:rulechain_exec/2 to evaluate",
    ?_test(with_access(shipped(Dir), fun() ->
             Nested = {context, "Route", ["/duckduck*"], [{"Set-Header", ["X-Continuation", "goose"]}]},
             Duck = access:get_rules(req("/duckduck", "127.0.0.1")),
             ?assert(lists:member({"Set-Header", ["X-Different-Host", "true"]}, Duck)),
             ?assert(lists:member(Nested, Duck)),
             ?assert(lists:member({"Send-File", ["/var/www/index.html"]}, Duck)),
             Local = access:get_rules(req("/duckduck", "localhost")),
             ?assertNot(lists:member({"Set-Header", ["X-Different-Host", "true"]}, Local)),
             ?assertNot(lists:member(Nested, Local))
           end))},
   {"get_rules + rulechain_exec reproduce test_rules.rb test_different_host (nested Route inside Host)",
    ?_test(with_access(shipped(Dir), fun() ->
             os:putenv("RUNTYPE", "docker"),   %% as the E2E image does (else If-Not-Env aborts 500)
             %% Send-File/DocDir would hit the file system; only header rules are run here.
             Headers = fun(Route, Host) ->
                         Req = req(Route, Host),
                         Rules = [R || R <- access:get_rules(Req),
                                       not lists:member(element(1, R), ["Send-File", "DocDir"])],
                         {ok, Resp} = rules:rulechain_exec(Rules, test_support:response_for(Req)),
                         Resp#response.headers
                       end,
             H1 = Headers("/duckduck", "127.0.0.1"),
             ?assertEqual("true", maps:get("X-Different-Host", H1)),
             ?assertEqual("goose", maps:get("X-Continuation", H1)),
             ?assertEqual("Pusheen the Cat", maps:get("X-Powered-By", H1)),
             H2 = Headers("/", "127.0.0.1"),
             ?assertEqual("true", maps:get("X-Different-Host", H2)),
             ?assertNot(maps:is_key("X-Continuation", H2)),
             H3 = Headers("/duckduck", "localhost"),
             ?assertNot(maps:is_key("X-Different-Host", H3)),
             ?assertNot(maps:is_key("X-Continuation", H3))
           end))},
   {"If-Env / If-Env-Def / If-Not-Env blocks follow the environment (test_env.rb)",
    ?_test(with_access(shipped(Dir), fun() ->
             os:putenv("RUNTYPE", "docker"),
             Docker = access:get_rules(req("/", "localhost")),
             ?assert(lists:member({"Set-Header", ["X-Environment-Present", "1"]}, Docker)),
             ?assert(lists:member({"Set-Header", ["X-Environment", "docker"]}, Docker)),
             ?assertNot(lists:member({"Abort", ["500"]}, Docker)),
             os:unsetenv("RUNTYPE"),
             NoEnv = access:get_rules(req("/", "localhost")),
             ?assertNot(lists:member({"Set-Header", ["X-Environment-Present", "1"]}, NoEnv)),
             ?assertNot(lists:member({"Set-Header", ["X-Environment", "docker"]}, NoEnv)),
             ?assert(lists:member({"Abort", ["500"]}, NoEnv))
           end))},
   {"a request matching nothing special gets only the catch-all rules, DocDir last",
    ?_test(with_access(shipped(Dir), fun() ->
             os:putenv("RUNTYPE", "docker"),
             Rules = access:get_rules(req("/abstract_nonsense", "localhost")),
             ?assertNot(lists:keymember("Abort", 1, Rules)),
             ?assertNot(lists:keymember("Send-File", 1, Rules)),
             ?assertNot(lists:keymember("No-Content", 1, Rules)),
             ?assertEqual({"DocDir", ["/var/www"]}, lists:last(Rules)),
             ?assertEqual(["Set-Header"], lists:usort(names(lists:droplast(Rules))))
           end))},
   {"execute_context_rule evaluates a registered context rule against the request",
    ?_test(begin
             R = req("/x", "localhost"),
             ?assert(access:execute_context_rule(R, "Route", ["/x"])),
             ?assertNot(access:execute_context_rule(R, "Route", ["/y"])),
             ?assert(access:execute_context_rule(R, "Host", ["local*"]))
           end)}].
