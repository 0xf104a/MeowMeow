%%%-------------------------------------------------------------------
%%% @doc EUnit suite for logging.erl
%%%
%%% The LogLevel semantics come from config/meow.conf:
%%%   0 - nothing, 1 - errors, 2 - +warnings, 3 - +info, 4 - everything.
%%% Output is captured by swapping the group leader (see test_support).
%%% @end
%%%-------------------------------------------------------------------
-module(logging_tests).
-include_lib("eunit/include/eunit.hrl").

setup() -> test_support:setup_config(#{"LogLevel" => "4"}).

logging_test_() ->
  {setup, fun setup/0, fun test_support:teardown_all/1,
   [level_filter_tests(), content_tests(), robustness_tests()]}.

at_level(Level, Fun) ->
  test_support:set_config("LogLevel", Level),
  {_, Out} = test_support:capture_io(Fun),
  Out.

level_filter_tests() ->
  [{"LogLevel 0 silences even errors",
    ?_assertEqual("", at_level("0", fun() -> logging:err("boom") end))},
   {"LogLevel 1 prints errors",
    ?_assertNotEqual("", at_level("1", fun() -> logging:err("boom") end))},
   {"LogLevel 1 hides warnings",
    ?_assertEqual("", at_level("1", fun() -> logging:warn("w") end))},
   {"LogLevel 2 prints warnings",
    ?_assertNotEqual("", at_level("2", fun() -> logging:warn("w") end))},
   {"LogLevel 2 hides info",
    ?_assertEqual("", at_level("2", fun() -> logging:info("i") end))},
   {"LogLevel 3 prints info",
    ?_assertNotEqual("", at_level("3", fun() -> logging:info("i") end))},
   {"LogLevel 3 hides debug",
    ?_assertEqual("", at_level("3", fun() -> logging:debug("d") end))},
   {"LogLevel 4 prints debug",
    ?_assertNotEqual("", at_level("4", fun() -> logging:debug("d") end))},
   {"filtering applies to the formatted variants too",
    ?_assertEqual("", at_level("0", fun() -> logging:debug("~p", [1]) end))}].

content_tests() ->
  [{"info output carries the INFO tag and the message",
    ?_test(begin
             Out = at_level("4", fun() -> logging:info("hello cat") end),
             ?assertNotEqual(nomatch, string:find(Out, "INFO")),
             ?assertNotEqual(nomatch, string:find(Out, "hello cat"))
           end)},
   {"debug output carries the DEBUG tag",
    ?_assertNotEqual(nomatch, string:find(at_level("4", fun() -> logging:debug("x") end), "DEBUG"))},
   {"warn output carries the WARN tag",
    ?_assertNotEqual(nomatch, string:find(at_level("4", fun() -> logging:warn("x") end), "WARN"))},
   {"err output carries the ERROR tag",
    ?_assertNotEqual(nomatch, string:find(at_level("4", fun() -> logging:err("x") end), "ERROR"))},
   {"formatted variant interpolates arguments",
    ?_assertNotEqual(nomatch,
                     string:find(at_level("4", fun() -> logging:info("x=~p y=~s", [42, "z"]) end),
                                 "x=42 y=z"))},
   {"every line is newline terminated",
    ?_assertEqual($\n, lists:last(at_level("4", fun() -> logging:info("x") end)))},
   {"all variants return ok so callers can use them in expression position",
    ?_test(begin
             test_support:set_config("LogLevel", "0"),
             ?assertEqual(ok, logging:info("a")),
             ?assertEqual(ok, logging:debug("a")),
             ?assertEqual(ok, logging:warn("a")),
             ?assertEqual(ok, logging:err("a")),
             ?assertEqual(ok, logging:info("~p", [a])),
             ?assertEqual(ok, logging:debug("~p", [a])),
             ?assertEqual(ok, logging:warn("~p", [a])),
             ?assertEqual(ok, logging:err("~p", [a]))
           end)}].

robustness_tests() ->
  [{"a logging call must never crash its caller, even with a "
    "format/argument mismatch, and especially when the level is filtered out. "
    "(regression: this used to fail because log_write/4 runs io_lib:format/2 before checking the level and "
    "lets the badarg escape. configuration:get/2 (bool) relies on exactly such a "
    "call (\"...~s...~p\" with []) and therefore crashes instead of reporting the error)",
    ?_test(begin
             Bad = fun() -> logging:err("~s ~p", []) end,
             ?assertEqual("", at_level("0", Bad)),
             Out = at_level("4", Bad),
             ?assertNotEqual(nomatch, string:find(Out, "bad log format"))
           end)}].
