%%%-------------------------------------------------------------------
%%% @doc EUnit suite for util.erl
%%%
%%% util is (almost) pure, but a few helpers call logging on error
%%% paths, and logging reads the LogLevel from the config ETS table, so
%%% the suite runs inside a config fixture.
%%%
%%% Every test title states the expectation and where it comes from
%%% (README, RFC, or the function's own doc comment) so that a failing
%%% test explains itself.
%%% @end
%%%-------------------------------------------------------------------
-module(util_tests).
-include_lib("eunit/include/eunit.hrl").

setup() -> test_support:setup_config(#{"LogLevel" => "0"}).

util_test_() ->
  {setup, fun setup/0, fun test_support:teardown_all/1,
   [wildcard_tests(), list_map_tests(), address_tests(), string_tests(),
    argument_tests(), time_tests(), port_signal_tests()]}.

%%--------------------------------------------------------------------
%% Wildcards (used by Route / Host / Accept matching)
%%--------------------------------------------------------------------
wildcard_tests() ->
  [{"wildcard2regex/1 returns a compiled regex",
    ?_assertMatch({ok, _}, util:wildcard2regex("/*.html"))},
   {"'*' expands to '.*': '/*.html' matches '/index.html' (README mimes.conf)",
    ?_assert(util:check_wildcard("/index.html", "/*.html"))},
   {"'/*.html' does not match '/index.htm' (pattern is anchored at the end)",
    ?_assertNot(util:check_wildcard("/index.htm", "/*.html"))},
   {"a pattern without wildcards is an exact, anchored match",
    ?_assert(util:check_wildcard("/catnip", "/catnip"))},
   {"'/catnip' must not match '/catnip_info' (anchored at the end, see tests/config/routes.conf)",
    ?_assertNot(util:check_wildcard("/catnip_info", "/catnip"))},
   {"'/catnip' must not match 'x/catnip' (anchored at the start)",
    ?_assertNot(util:check_wildcard("x/catnip", "/catnip"))},
   {"'*' alone matches anything",
    ?_assert(util:check_wildcard("/anything/at/all", "*"))},
   {"'*' alone matches the empty string",
    ?_assert(util:check_wildcard("", "*"))},
   {"'?' matches exactly one character",
    ?_assert(util:check_wildcard("/a.txt", "/?.txt"))},
   {"'?' does not match two characters",
    ?_assertNot(util:check_wildcard("/ab.txt", "/?.txt"))},
   {"'.' is escaped, so it only matches a literal dot",
    ?_assertNot(util:check_wildcard("/axhtml", "/a.html"))},
   {"trailing '*' is unanchored: Host pattern '127.0.0.1*' matches '127.0.0.1:8080'",
    ?_assert(util:check_wildcard("127.0.0.1:8080", "127.0.0.1*"))},
   {"Host pattern '127.0.0.1*' does not match 'localhost'",
    ?_assertNot(util:check_wildcard("localhost", "127.0.0.1*"))},
   {"'/duckduck*' matches '/duckduckgoose' (routes.conf nested Route)",
    ?_assert(util:check_wildcard("/duckduckgoose", "/duckduck*"))},
   {"binaries are accepted as the subject (routes are binaries after parsing)",
    ?_assert(util:check_wildcard(<<"/index.html">>, "/*.html"))},
   {"only '*' and '?' are wildcards, every other character is literal, "
    "so the pattern '/a+b' matches the path '/a+b'. "
    "(regression: this used to fail because wildcard/1 only escapes '.', every other regex metacharacter "
    "('+', '(', '[', '$' ...) is passed through to re:compile unescaped, so '/a+b' "
    "becomes the regex ^/a+b$ which matches '/aab' but not '/a+b')",
    ?_assert(util:check_wildcard("/a+b", "/a+b"))},
   {"'/a+b' must NOT match '/aab' (same root cause as above)",
    ?_assertNot(util:check_wildcard("/aab", "/a+b"))}].

%%--------------------------------------------------------------------
%% tup2list / sget / sget2 / split_list / parse_list
%%--------------------------------------------------------------------
list_map_tests() ->
  [{"tup2list converts a tuple to a list preserving order",
    ?_assertEqual([1, 2, 3], util:tup2list({1, 2, 3}))},
   {"tup2list of an empty tuple is []",
    ?_assertEqual([], util:tup2list({}))},
   {"sget returns the value for a present key",
    ?_assertEqual(1, util:sget(a, #{a => 1}))},
   {"sget returns {badkey, Key} for a missing key instead of raising",
    ?_assertEqual({badkey, b}, util:sget(b, #{a => 1}))},
   {"sget2 returns the value for a present key",
    ?_assertEqual("v", util:sget2("k", #{"k" => "v"}))},
   {"sget2 returns \"\" for a missing key",
    ?_assertEqual("", util:sget2("missing", #{}))},
   {"split_list splits on the token",
    ?_assertEqual(["a", "b", "c"], util:split_list("a b c", $\s))},
   {"split_list of [] is []",
    ?_assertEqual([], util:split_list([], $\s))},
   {"split_list keeps empty elements between consecutive tokens",
    ?_assertEqual(["a", "", "b"], util:split_list("a  b", $\s))},
   {"split_list drops a trailing empty element",
    ?_assertEqual(["a"], util:split_list("a ", $\s))},
   {"parse_list splits the LoadModules value on commas (README: LoadModules static,meow)",
    ?_assertEqual(["static", "header", "route", "env"],
                  util:parse_list("static,header,route,env"))},
   {"parse_list of a single module is a one element list",
    ?_assertEqual(["static"], util:parse_list("static"))}].

%%--------------------------------------------------------------------
%% Addresses
%%--------------------------------------------------------------------
address_tests() ->
  [{"str2addr parses dotted quad into an inet tuple",
    ?_assertEqual({127, 0, 0, 1}, util:str2addr("127.0.0.1"))},
   {"str2addr parses 0.0.0.0 (tests/config/meow.conf ListenHost)",
    ?_assertEqual({0, 0, 0, 0}, util:str2addr("0.0.0.0"))},
   {"str2addr with a non-numeric octet yields {error, bad_arg} in that position",
    ?_assertEqual({1, 2, 3, {error, bad_arg}}, util:str2addr("1.2.3.x"))},
   {"str2addr with fewer than four octets does not return an address tuple",
    ?_assertNotMatch({_, _, _, _}, util:str2addr("1.2.3"))},
   {"addr2str formats a list of octets",
    ?_assertEqual("127.0.0.1", util:addr2str([127, 0, 0, 1]))},
   {"addr2str accepts the address tuple socket:peername returns. "
    "(regression: this used to fail because io_lib:format/2 needs a list of arguments and addr2str passes "
    "the tuple straight through (badarg))",
    ?_assertEqual("127.0.0.1", util:addr2str({127, 0, 0, 1}))},
   {"pretty_addr formats a socket address map as ip:port",
    ?_assertEqual("127.0.0.1:8080",
                  util:pretty_addr(#{addr => {127, 0, 0, 1}, port => 8080}))},
   {"get_http_ver_pair parses HTTP/1.1 from a string",
    ?_assertEqual({1, 1}, util:get_http_ver_pair("HTTP/1.1"))},
   {"get_http_ver_pair parses HTTP/1.0 from a string",
    ?_assertEqual({1, 0}, util:get_http_ver_pair("HTTP/1.0"))},
   {"get_http_ver_pair parses a binary (the request record stores a binary)",
    ?_assertEqual({1, 1}, util:get_http_ver_pair(<<"HTTP/1.1">>))},
   {"get_http_ver_pair parses HTTP/2.0",
    ?_assertEqual({2, 0}, util:get_http_ver_pair("HTTP/2.0"))}].

%%--------------------------------------------------------------------
%% Strings
%%--------------------------------------------------------------------
string_tests() ->
  [{"bin2str of an empty binary is \"\"",
    ?_assertEqual("", util:bin2str(<<>>))},
   {"bin2str of [] is \"\"",
    ?_assertEqual("", util:bin2str([]))},
   {"bin2str converts a binary to a string",
    ?_assertEqual("abc", util:bin2str(<<"abc">>))},
   {"prettify_header_key capitalises a single token",
    ?_assertEqual("Host", util:prettify_header_key("host"))},
   {"prettify_header_key trims surrounding whitespace",
    ?_assertEqual("Host", util:prettify_header_key("  host "))},
   {"prettify_header_key capitalises both tokens of content-type",
    ?_assertEqual("Content-Type", util:prettify_header_key("CONTENT-type"))},
   {"every '-' separated token is capitalised: x-powered-by -> X-Powered-By. "
    "(regression: this used to fail because string:split/2 without 'all' only splits at the first '-', so the "
    "third token stays lower case (\"X-Powered-by\"))",
    ?_assertEqual("X-Powered-By", util:prettify_header_key("x-powered-by"))}].

%%--------------------------------------------------------------------
%% Config argument parsing (used by access.erl for every rule line)
%%--------------------------------------------------------------------
argument_tests() ->
  [{"single unquoted argument",
    ?_assertEqual(["/var/www"], util:parse_arguments("/var/www"))},
   {"several unquoted arguments split on spaces",
    ?_assertEqual(["Content-Type", "text/html"],
                  util:parse_arguments("Content-Type text/html"))},
   {"double-quoted argument keeps its spaces (README: strings with spaces MUST be quoted)",
    ?_assertEqual(["X-Powered-By", "Pusheen the Cat"],
                  util:parse_arguments("X-Powered-By \"Pusheen the Cat\""))},
   {"single-quoted argument keeps its spaces",
    ?_assertEqual(["/var/www/x y", "second"],
                  util:parse_arguments("'/var/www/x y' second"))},
   {"a quoted argument may contain a path with dots and slashes",
    ?_assertEqual(["/etc/nya/mimes.conf"],
                  util:parse_arguments("\"/etc/nya/mimes.conf\""))},
   {"double spaces between arguments do not produce empty arguments",
    ?_assertEqual(["a", "b"], util:parse_arguments("a  b"))},
   {"a quoted argument may contain any printable character, e.g. a comma. "
    "(regression: this used to fail because parse_quotes/1 whitelists characters with a regex that lacks ',', "
    "';', '(', ')' etc., returns nomatch, and parse_arguments crashes with badmatch)",
    ?_assertEqual(["a,b"], util:parse_arguments("\"a,b\""))}].

%%--------------------------------------------------------------------
%% Time formatting (Date / Last-Modified headers)
%%--------------------------------------------------------------------
-define(IMF_FIXDATE, "^(Mon|Tue|Wed|Thu|Fri|Sat|Sun), [0-9]{2} "
                     "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) "
                     "[0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2} GMT$").

time_tests() ->
  [{"format_file_time renders RFC 7231 IMF-fixdate (2026-10-23 is a Friday)",
    ?_assertEqual("Fri, 23 Oct 2026 14:33:18 GMT",
                  util:format_file_time({{2026, 10, 23}, {14, 33, 18}}))},
   {"format_file_time zero-pads day, hour, minute and second",
    ?_assertEqual("Sun, 01 Mar 2026 01:02:03 GMT",
                  util:format_file_time({{2026, 3, 1}, {1, 2, 3}}))},
   {"get_time returns a flat string ending in GMT",
    ?_test(begin
             T = util:get_time(),
             ?assert(io_lib:printable_list(T)),
             ?assertEqual("GMT", lists:sublist(T, length(T) - 2, 3))
           end)},
   {"get_time renders RFC 7231 IMF-fixdate ('Sun, 06 Nov 1994 08:49:37 GMT'), "
    "exactly like format_file_time does. "
    "(regression: this used to fail because the format string in get_time/0 has two spaces between the day "
    "and the month (\"~2..0w  ~s\"), so the Date header is malformed)",
    ?_assertMatch({match, _}, re:run(util:get_time(), ?IMF_FIXDATE))}].

%%--------------------------------------------------------------------
%% Port signalling (used to stop MCP subprocesses)
%%--------------------------------------------------------------------
port_signal_tests() ->
  [{"sigterm_to_port terminates the OS process behind a port",
    {timeout, 10, ?_test(begin
       Port = open_port({spawn_executable, test_support:tool("sleep")},
                        [{args, ["30"]}, exit_status]),
       ?assertEqual(ok, util:sigterm_to_port(Port)),
       receive
         {Port, {exit_status, Status}} -> ?assertNotEqual(0, Status)
       after 3000 -> ?assert(false)
       end
     end)}},
   {"sigkill_to_port terminates the OS process behind a port",
    {timeout, 10, ?_test(begin
       Port = open_port({spawn_executable, test_support:tool("sleep")},
                        [{args, ["30"]}, exit_status]),
       ?assertEqual(ok, util:sigkill_to_port(Port)),
       receive
         {Port, {exit_status, Status}} -> ?assertNotEqual(0, Status)
       after 3000 -> ?assert(false)
       end
     end)}},
   {"sigterm_to_port on an already closed port reports port_already_closed",
    {timeout, 10, ?_test(begin
       Port = open_port({spawn_executable, test_support:tool("true")}, [exit_status]),
       receive {Port, {exit_status, _}} -> ok after 3000 -> ?assert(false) end,
       ?assertEqual(port_already_closed, util:sigterm_to_port(Port))
     end)}},
   {"sigkill_to_port on an already closed port reports port_already_closed",
    {timeout, 10, ?_test(begin
       Port = open_port({spawn_executable, test_support:tool("true")}, [exit_status]),
       receive {Port, {exit_status, _}} -> ok after 3000 -> ?assert(false) end,
       ?assertEqual(port_already_closed, util:sigkill_to_port(Port))
     end)}}].
