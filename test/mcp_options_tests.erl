%%%-------------------------------------------------------------------
%%% @doc EUnit suite for mcp_options.erl (rule arguments, sizes,
%%% effective session options and the 503 response).
%%% @end
%%%-------------------------------------------------------------------
-module(mcp_options_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/response.hrl").

setup() -> test_support:setup_config(#{"LogLevel" => "0"}).

mcp_options_test_() ->
  {setup, fun setup/0, fun test_support:teardown_all/1,
   fun(_) -> [args_tests(), size_tests(), session_opts_tests(), busy_tests()] end}.

args_tests() ->
  [{"\"COMMAND\" KEEPALIVE_MS parses to the command, an integer and no options",
    ?_assertEqual({ok, "/bin/tool --x", 60000, #{}},
                  mcp_options:parse_rule_args(["/bin/tool --x", "60000"]))},
   {"MaxSessions= and MemoryLimit= become map entries",
    ?_assertEqual({ok, "tool", 1000, #{max_sessions => 2, memory_limit => 256 * 1024 * 1024}},
                  mcp_options:parse_rule_args(["tool", "1000", "MaxSessions=2", "MemoryLimit=256M"]))},
   {"option names are case-insensitive",
    ?_assertEqual({ok, "tool", 1000, #{max_sessions => 1}},
                  mcp_options:parse_rule_args(["tool", "1000", "maxsessions=1"]))},
   {"MemoryLimit=0 means no limit",
    ?_assertEqual({ok, "tool", 1000, #{memory_limit => none}},
                  mcp_options:parse_rule_args(["tool", "1000", "MemoryLimit=0"]))},
   {"a non-numeric keep-alive is an error",
    ?_assertMatch({error, {bad_keepalive, _}}, mcp_options:parse_rule_args(["tool", "soon"]))},
   {"a missing keep-alive is an error",
    ?_assertMatch({error, {bad_arguments, _}}, mcp_options:parse_rule_args(["tool"]))},
   {"an unknown option is an error",
    ?_assertMatch({error, {unknown_option, "colour"}},
                  mcp_options:parse_rule_args(["tool", "1000", "Colour=red"]))},
   {"an option without '=' is an error",
    ?_assertMatch({error, {bad_option, "fast"}}, mcp_options:parse_rule_args(["tool", "1000", "fast"]))},
   {"a bad option value is an error",
    ?_assertMatch({error, {bad_option_value, "MaxSessions", "many"}},
                  mcp_options:parse_rule_args(["tool", "1000", "MaxSessions=many"]))},
   {"with_rule_args/2 runs the fun with the parsed values",
    ?_assertEqual({"tool", 7, #{max_sessions => 3}},
                  mcp_options:with_rule_args(["tool", "7", "MaxSessions=3"],
                                             fun(T, K, O) -> {T, K, O} end))},
   {"with_rule_args/2 answers 500 for a misconfigured rule instead of crashing the request",
    ?_assertEqual({aborted, 500},
                  mcp_options:with_rule_args(["tool", "x"], fun(_, _, _) -> never end))}].

size_tests() ->
  [{"plain digits are bytes", ?_assertEqual({ok, 512}, mcp_options:parse_size("512"))},
   {"K, M and G are binary multiples",
    ?_assertEqual([{ok, 1024}, {ok, 256 * 1024 * 1024}, {ok, 2 * 1024 * 1024 * 1024}],
                  [mcp_options:parse_size(S) || S <- ["1K", "256M", "2G"]])},
   {"KB/MiB style suffixes and whitespace are accepted",
    ?_assertEqual([{ok, 10 * 1024 * 1024}, {ok, 3 * 1024}, {ok, 7}],
                  [mcp_options:parse_size(S) || S <- ["10MiB", " 3kb ", "7B"]])},
   {"0, none, off and an empty value mean no limit",
    ?_assert(lists:all(fun(S) -> mcp_options:parse_size(S) =:= {ok, none} end,
                       ["0", "none", "OFF", ""]))},
   {"binaries work too", ?_assertEqual({ok, 1024}, mcp_options:parse_size(<<"1k">>))},
   {"garbage, negative numbers and unknown suffixes are errors",
    ?_assert(lists:all(fun(S) -> mcp_options:parse_size(S) =:= error end,
                       ["x", "-5", "5T", "1.5M", 42]))},
   {"format_size/1 picks a readable unit",
    ?_assertEqual(["none", "512B", "1.0K", "256.0M", "2.0G"],
                  [lists:flatten(mcp_options:format_size(B))
                   || B <- [none, 512, 1024, 256 * 1024 * 1024, 2 * 1024 * 1024 * 1024]])}].

session_opts_tests() ->
  [{"the compiled-in defaults cap nothing, wait 5 s and sample every second",
    ?_test(begin
             test_support:setup_config(#{"LogLevel" => "0"}),
             ?assertEqual(#{global_max => infinity, tool_max => infinity, wait_ms => 5000,
                            max_waiting => 0, memory_limit => none, poll_ms => 1000},
                          mcp_options:session_opts(#{}))
           end)},
   {"MCPMaxSessions is the global cap and bounds the wait queue",
    ?_test(begin
             test_support:set_config("MCPMaxSessions", "3"),
             Opts = mcp_options:session_opts(#{}),
             ?assertEqual(3, maps:get(global_max, Opts)),
             ?assertEqual(3, maps:get(max_waiting, Opts)),
             test_support:set_config("MCPMaxSessions", "0")
           end)},
   {"a rule's MaxSessions is the per-tool cap and bounds the queue when there is no global cap",
    ?_test(begin
             Opts = mcp_options:session_opts(#{max_sessions => 2}),
             ?assertEqual(infinity, maps:get(global_max, Opts)),
             ?assertEqual(2, maps:get(tool_max, Opts)),
             ?assertEqual(2, maps:get(max_waiting, Opts))
           end)},
   {"MCPMemoryLimit is the default limit and a rule's MemoryLimit overrides it",
    ?_test(begin
             test_support:set_config("MCPMemoryLimit", "64M"),
             ?assertEqual(64 * 1024 * 1024, maps:get(memory_limit, mcp_options:session_opts(#{}))),
             ?assertEqual(1024, maps:get(memory_limit, mcp_options:session_opts(#{memory_limit => 1024}))),
             ?assertEqual(none, maps:get(memory_limit, mcp_options:session_opts(#{memory_limit => none}))),
             test_support:set_config("MCPMemoryLimit", "0")
           end)},
   {"MCPSessionWaitMs and MCPMemoryPollMs are read; negative values clamp to 0",
    ?_test(begin
             test_support:set_config("MCPSessionWaitMs", "250"),
             test_support:set_config("MCPMemoryPollMs", "-1"),
             Opts = mcp_options:session_opts(#{}),
             ?assertEqual(250, maps:get(wait_ms, Opts)),
             ?assertEqual(0, maps:get(poll_ms, Opts)),
             test_support:set_config("MCPSessionWaitMs", "5000"),
             test_support:set_config("MCPMemoryPollMs", "1000")
           end)},
   {"an unparsable MCPMemoryLimit is ignored rather than fatal",
    ?_test(begin
             test_support:set_config("MCPMemoryLimit", "lots"),
             ?assertEqual(none, maps:get(memory_limit, mcp_options:session_opts(#{}))),
             test_support:set_config("MCPMemoryLimit", "0")
           end)},
   {"config_int/2 and config_bool/2 fall back to the default for bad values",
    ?_test(begin
             test_support:set_config("MCPMaxSessions", "few"),
             test_support:set_config("MCPCgroup", "maybe"),
             ?assertEqual(9, mcp_options:config_int("MCPMaxSessions", 9)),
             ?assertEqual(false, mcp_options:config_bool("MCPCgroup", false)),
             ?assertEqual(42, mcp_options:config_int("NoSuchKey", 42)),
             ?assertEqual("d", mcp_options:config_string("NoSuchKey", "d")),
             test_support:set_config("MCPMaxSessions", "0"),
             test_support:set_config("MCPCgroup", "Yes")
           end)},
   {"without a config table the defaults apply (sessions started outside the server)",
    ?_test(begin
             catch ets:delete(config_storage),
             ?assertEqual(5000, maps:get(wait_ms, mcp_options:session_opts(#{}))),
             test_support:setup_config(#{"LogLevel" => "0"})
           end)}].

busy_tests() ->
  [{"busy_response/2 is a 503 with Retry-After derived from the wait time and an error page body",
    ?_test(begin
             Req = test_support:request("POST", "/mcp", [{"Host", "h"}]),
             Resp = test_support:response_for(Req),
             {ready2send, R} = mcp_options:busy_response(Resp, #{wait_ms => 5000}),
             ?assertEqual(503, R#response.code),
             ?assert(R#response.is_ready2send),
             ?assertEqual("5", maps:get("Retry-After", R#response.headers)),
             ?assertEqual("text/html", maps:get("Content-Type", R#response.headers)),
             ?assertNotEqual(nomatch, binary:match(R#response.body, <<"503">>)),
             %% headers already on the response are kept
             ?assert(maps:is_key("Server", R#response.headers))
           end)},
   {"Retry-After is at least one second even with no wait",
    ?_test(begin
             Resp = test_support:response_for(test_support:request("GET", "/sse", [{"Host", "h"}])),
             {ready2send, R} = mcp_options:busy_response(Resp, #{wait_ms => 0}),
             ?assertEqual("1", maps:get("Retry-After", R#response.headers))
           end)}].
