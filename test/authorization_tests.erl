%%%-------------------------------------------------------------------
%%% @doc EUnit suite for the authorization module (Authorize / Authorize-Via)
%%% Expectations come from the module's own doc comment and from the E2E
%%% suite tests/simple_test/test_authorization.rb.
%%%
%%% Authorize-Via runs an external program, so the tests write small shell
%%% scripts into a temporary directory and point the rule at them.
%%% @end
%%%-------------------------------------------------------------------
-module(authorization_tests).
-include_lib("eunit/include/eunit.hrl").

-define(TOKEN, "secret-token-123").
%% A header value full of shell syntax; the tool must receive it verbatim.
-define(NASTY, "x; echo $(id) | cat \"q\" > /tmp/pwned").

rule(Name) -> proplists:get_value(Name, authorization:get_context_rules()).
req(Headers) -> test_support:request("GET", "/auth_via", [{"Host", "h"} | Headers]).

script(Dir, Name, Body) ->
  Path = filename:join(Dir, Name),
  test_support:write_file(Path, "#!/bin/sh\n" ++ Body),
  ok = file:change_mode(Path, 8#755),
  Path.

setup() ->
  test_support:setup_all(#{"LogLevel" => "0", "KeepAlive" => "100"}),
  {ok, authorization} = nya_loader:nya_load("authorization"),
  Dir = test_support:tmp_dir(),
  test_support:write_file(filename:join(Dir, "users.txt"),
                          "# users\nadmin:secret123\nalice:wonderland # trailing comment\n\nbob:builder\n"),
  %% Same tool as tests/config/auth_tool.sh
  script(Dir, "auth_tool.sh",
         "INPUT=$(cat)\n"
         "if [ \"$INPUT\" = \"" ?TOKEN "\" ] || [ \"$INPUT\" = \"Bearer " ?TOKEN "\" ]; then exit 0; else exit 1; fi\n"),
  script(Dir, "noisy.sh", "echo some stdout; echo some stderr >&2; cat >/dev/null; exit 0\n"),
  script(Dir, "exit3.sh", "cat >/dev/null; exit 3\n"),
  script(Dir, "verbatim.sh", "INPUT=$(cat)\n[ \"$INPUT\" = '" ?NASTY "' ]\n"),
  Dir.

cleanup(Dir) ->
  test_support:rm_rf(Dir),
  test_support:teardown_all(ok).

authorization_test_() ->
  {setup, fun setup/0, fun cleanup/1,
   fun(Dir) -> [module_tests(), bearer_tests(Dir), via_tests(Dir), chain_tests(Dir)] end}.

%%--------------------------------------------------------------------
module_tests() ->
  [{"init/0 returns ok", ?_assertEqual(ok, authorization:init())},
   {"terminate/1 returns ok", ?_assertEqual(ok, authorization:terminate(state))},
   {"the module has no custom rules", ?_assertEqual([], authorization:get_custom_rules())},
   {"the module exports the Authorize and Authorize-Via context rules",
    ?_assertEqual(["Authorize", "Authorize-Via"], [N || {N, _} <- authorization:get_context_rules()])}].

%%--------------------------------------------------------------------
bearer_tests(Dir) ->
  Users = filename:join(Dir, "users.txt"),
  Auth = fun(Headers) -> (rule("Authorize"))([Users], req(Headers)) end,
  B64 = fun(S) -> base64:encode_to_string(S) end,
  [{"a Bearer token base64(user:password) listed in the file is authorized (test_auth_bearer_valid_admin)",
    ?_assert(Auth([{"Authorization", "Bearer " ++ B64("admin:secret123")}]))},
   {"comments and blank lines in the credentials file are ignored",
    ?_assert(Auth([{"Authorization", "Bearer " ++ B64("alice:wonderland")}])
             andalso Auth([{"Authorization", "Bearer " ++ B64("bob:builder")}]))},
   {"the Basic scheme is accepted as well (test_auth_basic_valid)",
    ?_assert(Auth([{"Authorization", "Basic " ++ B64("admin:secret123")}]))},
   {"a wrong password is refused", ?_assertNot(Auth([{"Authorization", "Bearer " ++ B64("admin:nope")}]))},
   {"an unknown user is refused", ?_assertNot(Auth([{"Authorization", "Bearer " ++ B64("eve:secret123")}]))},
   {"a missing Authorization header is refused", ?_assertNot(Auth([]))},
   {"an empty Authorization header is refused", ?_assertNot(Auth([{"Authorization", ""}]))},
   {"malformed base64 is refused instead of crashing", ?_assertNot(Auth([{"Authorization", "Bearer %%%not-base64"}]))},
   {"a scheme without a token is refused", ?_assertNot(Auth([{"Authorization", "Bearer"}]))},
   {"an unsupported scheme is refused", ?_assertNot(Auth([{"Authorization", "Digest " ++ B64("admin:secret123")}]))},
   {"a missing credentials file refuses everyone",
    ?_assertNot((rule("Authorize"))([filename:join(Dir, "nope.txt")],
                                    req([{"Authorization", "Bearer " ++ B64("admin:secret123")}])))},
   {"Authorize with the wrong number of arguments fails", ?_assertNot((rule("Authorize"))([], req([])))}].

%%--------------------------------------------------------------------
via_tests(Dir) ->
  Tool = filename:join(Dir, "auth_tool.sh"),
  Via = fun(Args, Headers) -> (rule("Authorize-Via"))(Args, req(Headers)) end,
  [{"Authorize-Via <tool> passes the Authorization header to the tool's stdin; exit 0 authorizes "
    "(test_auth_via_default_header_success). (regression: this used to fail because run_auth_tool/2 "
    "called port_close/1 right after writing the input; that drops the port before its {exit_status, _} "
    "message can arrive (the receive also matched the wrong shape {Port, exit_status, S}), so every "
    "request waited the full 5 s timeout, and the connection's KeepAlive timer closed the socket first: "
    "the E2E client saw 'end of file reached' instead of any response)",
    {timeout, 10, ?_assert(Via([Tool], [{"Authorization", ?TOKEN}]))}},
   {"the tool also accepts the 'Bearer <token>' form", {timeout, 10, ?_assert(Via([Tool], [{"Authorization", "Bearer " ?TOKEN}]))}},
   {"exit 1 refuses (test_auth_via_default_header_failure)", {timeout, 10, ?_assertNot(Via([Tool], [{"Authorization", "wrong-token"}]))}},
   {"a missing header hands the tool an empty input, which it refuses (test_auth_via_missing_header)",
    {timeout, 10, ?_assertNot(Via([Tool], []))}},
   {"Authorize-Via <tool> <header> reads the named header instead (test_auth_via_custom_header_success)",
    {timeout, 10, ?_assert(Via([Tool, "X-Api-Key"], [{"X-Api-Key", ?TOKEN}]))}},
   {"the named header is refused when wrong or absent (test_auth_via_custom_header_failure / _missing)",
    {timeout, 10, ?_test(begin
       ?assertNot(Via([Tool, "X-Api-Key"], [{"X-Api-Key", "invalid-key"}])),
       ?assertNot(Via([Tool, "X-Api-Key"], [{"Authorization", ?TOKEN}]))
     end)}},
   {"the answer arrives well inside the KeepAlive window instead of after the 5 s tool timeout",
    {timeout, 10, ?_test(begin
       {Micros, true} = timer:tc(fun() -> Via([Tool], [{"Authorization", ?TOKEN}]) end),
       ?assert(Micros < 1000000)
     end)}},
   {"the tool's stdout and stderr are swallowed and no port message is left in the caller's mailbox "
    "(the caller is the connection handler, which answers 500 and exits on any message it does not know)",
    {timeout, 10, ?_test(begin
       test_support:flush(),
       ?assert(Via([filename:join(Dir, "noisy.sh")], [{"Authorization", "anything"}])),
       ?assertEqual([], test_support:collect(200))
     end)}},
   {"any non-zero exit code refuses, not only 1",
    {timeout, 10, ?_assertNot(Via([filename:join(Dir, "exit3.sh")], [{"Authorization", ?TOKEN}]))}},
   {"the header value reaches the tool verbatim: shell metacharacters are neither interpreted nor mangled",
    {timeout, 10, ?_assert(Via([filename:join(Dir, "verbatim.sh")], [{"Authorization", ?NASTY}]))}},
   {"a tool that does not exist refuses promptly",
    {timeout, 10, ?_test(begin
       {Micros, false} = timer:tc(fun() -> Via([filename:join(Dir, "missing.sh")], [{"Authorization", ?TOKEN}]) end),
       ?assert(Micros < 1000000)
     end)}},
   {"Authorize-Via with no arguments fails", ?_assertNot(Via([], [{"Authorization", ?TOKEN}]))},
   {"the rule is reachable through the context rule registry (access:execute_context_rule)",
    {timeout, 10, ?_assert(access:execute_context_rule(req([{"Authorization", ?TOKEN}]), "Authorize-Via", [Tool]))}}].

%%--------------------------------------------------------------------
%% The /auth_via route of tests/config/routes.conf, run through handle/2
%% with the test process playing the upstream (as handle_tests does).
chain_tests(Dir) ->
  Tool = filename:join(Dir, "auth_tool.sh"),
  %% The guarded block must end with a terminal rule (the real route uses
  %% Send-File); otherwise the chain continues into Abort 401 by design.
  Rules = [{context, "Authorize-Via", [Tool],
            [{"Set-Header", ["X-Auth-Via-Status", "authorized"]}, {"No-Content", true}]},
           {"Abort", ["401"]}],
  Run = fun(Headers) ->
          test_support:flush(),
          test_support:set_access(Rules),
          try
            handle:handle(test_support:response_for(req(Headers)), self()),
            [test_support:parse_response(W) || {send, W} <- test_support:collect(100)]
          after
            test_support:set_access([])
          end
        end,
  [{"an authorized request runs the guarded rules: 204 from No-Content carrying the X-Auth-Via-Status header",
    {timeout, 10, ?_test(begin
       [{204, H, _}] = Run([{"Authorization", ?TOKEN}]),
       ?assertEqual("authorized", maps:get("X-Auth-Via-Status", H))
     end)}},
   {"a refused request skips the guarded rules and hits Abort 401",
    {timeout, 10, ?_assertMatch([{401, _, _}], Run([{"Authorization", "wrong-token"}]))}},
   {"a request without the header is answered 401 too, not dropped",
    {timeout, 10, ?_assertMatch([{401, _, _}], Run([]))}}].
