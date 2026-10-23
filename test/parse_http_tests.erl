%%%-------------------------------------------------------------------
%%% @doc EUnit suite for parse_http.erl
%%%
%%% The incremental parser (make_request/1 + update_request/2) is fed
%%% the same binaries the socket handler receives. Expectations come
%%% from RFC 7230/7231 and from how the rest of the server consumes the
%%% #request{} record (binary method/route, lower-cased header names).
%%% @end
%%%-------------------------------------------------------------------
-module(parse_http_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/request.hrl").

setup() -> test_support:setup_config(#{"LogLevel" => "0"}).

parse_http_test_() ->
  {setup, fun setup/0, fun test_support:teardown_all/1,
   fun(_) ->
     [request_line_tests(), header_tests(), body_tests(), streaming_tests(),
      accessor_tests(), connection_tests(), accept_tests(), sanitize_tests(),
      query_tests(), legacy_tests()]
   end}.

fresh() -> parse_http:make_request({127, 0, 0, 1}).
feed(Req, Raw) -> parse_http:update_request(Req, list_to_binary(Raw)).
req(Raw) -> feed(fresh(), Raw).

%%--------------------------------------------------------------------
request_line_tests() ->
  [{"make_request starts with an empty record bound to the peer address",
    ?_test(begin
             R = fresh(),
             ?assertEqual({127, 0, 0, 1}, R#request.src_addr),
             ?assertEqual(nil, R#request.route),
             ?assertEqual(nil, R#request.method),
             ?assertEqual(#{}, R#request.header),
             ?assertNot(R#request.is_headers_accepted)
           end)},
   {"a complete GET request is parsed in one go",
    ?_test(begin
             R = req("GET / HTTP/1.1\r\nHost: example.com\r\n\r\n"),
             ?assertEqual(<<"GET">>, R#request.method),
             ?assertEqual(<<"/">>, R#request.route),
             ?assertEqual(<<"HTTP/1.1">>, R#request.http_ver),
             ?assert(R#request.is_headers_accepted),
             ?assertEqual(0, iolist_size(R#request.body))
           end)},
   {"the query string is split off the route into params",
    ?_test(begin
             R = req("GET /a?b=1&c=2 HTTP/1.1\r\n\r\n"),
             ?assertEqual(<<"/a">>, R#request.route),
             ?assertEqual(<<"b=1&c=2">>, R#request.params)
           end)},
   {"without a query string params is empty",
    ?_assertEqual(0, iolist_size((req("GET /a HTTP/1.1\r\n\r\n"))#request.params))},
   {"POST, HEAD and DELETE methods are preserved verbatim as binaries",
    ?_assertEqual([<<"POST">>, <<"HEAD">>, <<"DELETE">>],
                  [(req(M ++ " / HTTP/1.1\r\n\r\n"))#request.method
                   || M <- ["POST", "HEAD", "DELETE"]])},
   {"the HTTP version is preserved so handle:is_good/1 can reject HTTP/1.0",
    ?_assertEqual(<<"HTTP/1.0">>, (req("GET / HTTP/1.0\r\n\r\n"))#request.http_ver)},
   {"a request line with two tokens is a 400",
    ?_assertEqual({aborted, 400}, req("GET /\r\n\r\n"))},
   {"a request line with four tokens is a 400",
    ?_assertEqual({aborted, 400}, req("GET / HTTP/1.1 extra\r\n\r\n"))},
   {"an empty line before the request-line is ignored (RFC 7230 section 3.5: a "
    "server 'SHOULD ignore at least one empty line (CRLF) received prior to the request-line'). "
    "(regression: this used to fail because parse_lines/2 treats the first line as the request-line unconditionally; "
    "an empty line has no three tokens so the request is answered 400)",
    ?_assertEqual(<<"/">>, (req("\r\nGET / HTTP/1.1\r\n\r\n"))#request.route)},
   {"an empty chunk leaves the request unchanged (socket:recv/2 may return <<>>). "
    "(regression: this used to fail because parse_lines/2 only special-cases [] and [[]]; an empty binary falls into "
    "the string:split clause whose [L, T] match fails)",
    ?_test(begin R = fresh(), ?assertEqual(R, feed(R, "")) end)}].

%%--------------------------------------------------------------------
header_tests() ->
  [{"header names are stored lower-cased (lookups are case-insensitive)",
    ?_assertMatch(#{"host" := "example.com"},
                  (req("GET / HTTP/1.1\r\nHost: example.com\r\n\r\n"))#request.header)},
   {"header values are trimmed",
    ?_assertMatch(#{"host" := "example.com"},
                  (req("GET / HTTP/1.1\r\nHost:    example.com   \r\n\r\n"))#request.header)},
   {"a value may itself contain a colon (Host: localhost:8080)",
    ?_assertMatch(#{"host" := "localhost:8080"},
                  (req("GET / HTTP/1.1\r\nHost: localhost:8080\r\n\r\n"))#request.header)},
   {"all headers are collected",
    ?_assertEqual(#{"host" => "h", "user-agent" => "curl/8", "accept" => "*/*"},
                  (req("GET / HTTP/1.1\r\nHost: h\r\nUser-Agent: curl/8\r\nAccept: */*\r\n\r\n"))#request.header)},
   {"a header line without a colon is a 400",
    ?_assertEqual({aborted, 400}, req("GET / HTTP/1.1\r\nBroken\r\n\r\n"))},
   {"an empty header value is allowed",
    ?_assertMatch(#{"x-empty" := ""},
                  (req("GET / HTTP/1.1\r\nX-Empty:\r\n\r\n"))#request.header)},
   {"a request without any header is accepted",
    ?_assert((req("GET / HTTP/1.1\r\n\r\n"))#request.is_headers_accepted)}].

%%--------------------------------------------------------------------
body_tests() ->
  [{"bytes after the empty line are the body",
    ?_assertEqual(<<"hello">>,
                  iolist_to_binary((req("POST / HTTP/1.1\r\nContent-Length: 5\r\n\r\nhello"))#request.body))},
   {"the body is empty when nothing follows the headers",
    ?_assertEqual(0, iolist_size((req("GET / HTTP/1.1\r\n\r\n"))#request.body))},
   {"the body is a list after parsing (handle:handle_body_recv/1 measures it with length/1)",
    ?_assert(is_list((req("POST / HTTP/1.1\r\nContent-Length: 5\r\n\r\nhello"))#request.body))}].

%%--------------------------------------------------------------------
streaming_tests() ->
  [{"headers and the terminating empty line may arrive in separate chunks",
    ?_test(begin
             R1 = feed(fresh(), "GET / HTTP/1.1\r\nHost: x\r\n"),
             ?assertNot(R1#request.is_headers_accepted),
             ?assertMatch(#{"host" := "x"}, R1#request.header),
             R2 = feed(R1, "\r\n"),
             ?assert(R2#request.is_headers_accepted)
           end)},
   {"the request line may arrive alone first",
    ?_test(begin
             R1 = feed(fresh(), "GET /path HTTP/1.1\r\n"),
             ?assertEqual(<<"/path">>, R1#request.route),
             ?assertNot(R1#request.is_headers_accepted),
             R2 = feed(R1, "Host: x\r\n\r\n"),
             ?assert(R2#request.is_headers_accepted),
             ?assertMatch(#{"host" := "x"}, R2#request.header)
           end)},
   {"a header line split across two TCP reads is reassembled "
    "('Host: exa' + 'mple.com'). (regression: this used to fail because the unfinished fragment is kept as a binary "
    "and update_request/2 joins it with string:concat/2 (i.e. '++'), which raises badarg for "
    "a binary left operand)",
    ?_test(begin
             R1 = feed(fresh(), "GET / HTTP/1.1\r\nHost: exa"),
             R2 = feed(R1, "mple.com\r\n\r\n"),
             ?assertMatch(#{"host" := "example.com"}, R2#request.header)
           end)},
   {"a request line split across two TCP reads is reassembled. "
    "(regression: this used to fail because parse_lines/2 requires a CRLF in the very first chunk "
    "([L, T] = string:split(...) has nothing to match and raises badmatch))",
    ?_test(begin
             R1 = feed(fresh(), "GET / HT"),
             R2 = feed(R1, "TP/1.1\r\n\r\n"),
             ?assertEqual(<<"/">>, R2#request.route)
           end)}].

%%--------------------------------------------------------------------
accessor_tests() ->
  Raw = "GET / HTTP/1.1\r\nHost: example.com\r\nX-Pad:   padded   \r\n\r\n",
  [{"get_header/2 is case-insensitive in the header name",
    ?_assertEqual("example.com", parse_http:get_header("HOST", req(Raw)))},
   {"get_header/2 strips surrounding whitespace",
    ?_assertEqual("padded", parse_http:get_header("X-Pad", req(Raw)))},
   {"get_header/2 raises for a missing header",
    ?_assertError({badkey, "nope"}, parse_http:get_header("Nope", req(Raw)))},
   {"get_header/3 returns the value when present",
    ?_assertEqual("example.com", parse_http:get_header("host", req(Raw), none))},
   {"get_header/3 returns the default for a missing header",
    ?_assertEqual(none, parse_http:get_header("Nope", req(Raw), none))},
   {"has_header/2 is case-insensitive",
    ?_test(begin
             R = req(Raw),
             ?assert(parse_http:has_header("host", R)),
             ?assert(parse_http:has_header("HOST", R)),
             ?assertNot(parse_http:has_header("nope", R))
           end)}].

%%--------------------------------------------------------------------
connection_tests() ->
  Conn = fun(V) -> req("GET / HTTP/1.1\r\nConnection: " ++ V ++ "\r\n\r\n") end,
  [{"without a Connection header the server closes the connection",
    ?_assert(parse_http:is_close(req("GET / HTTP/1.1\r\n\r\n")))},
   {"Connection: close closes",
    ?_assert(parse_http:is_close(Conn("close")))},
   {"Connection: keep-alive keeps the connection open",
    ?_assertNot(parse_http:is_close(Conn("keep-alive")))},
   {"an unknown connection token closes (safe default)",
    ?_assert(parse_http:is_close(Conn("upgrade")))},
   {"connection options are case-insensitive (RFC 7230 section 6.1), so "
    "'Connection: Keep-Alive' keeps the connection open. (regression: this used to fail because is_close/1 compares "
    "the raw value with the lower-case literal \"keep-alive\" only and treats anything else "
    "as close)",
    ?_assertNot(parse_http:is_close(Conn("Keep-Alive")))}].

%%--------------------------------------------------------------------
accept_tests() ->
  Accept = fun(V) -> req("GET / HTTP/1.1\r\nAccept: " ++ V ++ "\r\n\r\n") end,
  [{"a single media type has q=1",
    ?_assertEqual([{"text/html", 1}], parse_http:parse_accept("text/html"))},
   {"a comma separated list keeps order and q-values",
    ?_assertEqual([{"text/html", 1}, {"application/json", 0.5}],
                  parse_http:parse_accept("text/html, application/json;q=0.5"))},
   {"whitespace around tokens is stripped",
    ?_assertEqual([{"a/b", 1}, {"c/d", 0.5}], parse_http:parse_accept(" a/b , c/d ; q=0.5 "))},
   {"*/* is a pattern like any other",
    ?_assertEqual([{"*/*", 0.1}], parse_http:parse_accept("*/*;q=0.1"))},
   {"'q=1' is a valid q-value (RFC 7231 section 5.3.1: qvalue = (\"0\" [\".\" 0*3DIGIT]) "
    "/ (\"1\" [\".\" 0*3(\"0\")])) so 'text/html;q=1' parses to [{\"text/html\", 1}]. "
    "(regression: this used to fail because parse_q_token/1 uses string:to_float/1, which rejects an integer without a "
    "decimal point; the error branch then makes parse_accept return the atom ok)",
    ?_test(begin
             [{"text/html", Q}] = parse_http:parse_accept("text/html;q=1"),
             ?assert(Q == 1)
           end)},
   {"acceptable/2 matches an exact type",
    ?_assert(parse_http:acceptable("application/json", [{"application/json", 1}]))},
   {"acceptable/2 matches */*",
    ?_assert(parse_http:acceptable("application/json", [{"*/*", 1}]))},
   {"acceptable/2 matches a subtype wildcard (text/*)",
    ?_assert(parse_http:acceptable("text/event-stream", [{"text/*", 1}]))},
   {"acceptable/2 rejects a non-matching type",
    ?_assertNot(parse_http:acceptable("application/json", [{"text/html", 1}]))},
   {"acceptable/2 finds a match later in the list",
    ?_assert(parse_http:acceptable("application/json", [{"text/html", 1}, {"application/json", 0.5}]))},
   {"acceptable/2 ignores patterns with q=0 (explicitly not acceptable)",
    ?_assertNot(parse_http:acceptable("text/html", [{"text/html", 0}]))},
   {"acceptable/2 of an empty pattern list is false",
    ?_assertNot(parse_http:acceptable("text/html", []))},
   {"acceptable_for_request/2 is true when the request has no Accept header",
    ?_assert(parse_http:acceptable_for_request("application/json", req("GET / HTTP/1.1\r\n\r\n")))},
   {"acceptable_for_request/2 is true when Accept lists the type",
    ?_assert(parse_http:acceptable_for_request("application/json", Accept("application/json")))},
   {"acceptable_for_request/2 is true for Accept: */*",
    ?_assert(parse_http:acceptable_for_request("application/json", Accept("*/*")))},
   {"acceptable_for_request/2 is false when Accept lists other types only "
    "('Accept: text/plain' must reject application/json - mcp_streamable relies on it to answer 406). "
    "(regression: this used to fail because it looks up the key \"Accept\" while parse_lines/2 stores header names "
    "lower-cased (\"accept\"), so the header is never found and everything is accepted)",
    ?_assertNot(parse_http:acceptable_for_request("application/json", Accept("text/plain")))}].

%%--------------------------------------------------------------------
sanitize_tests() ->
  [{"printable ASCII is kept",
    ?_assertEqual("text/html, */*;q=0.1", parse_http:sanitize_cors_unsafe("text/html, */*;q=0.1"))},
   {"tab (0x09) is kept",
    ?_assertEqual("a\tb", parse_http:sanitize_cors_unsafe("a\tb"))},
   {"control characters are removed",
    ?_assertEqual("ab", parse_http:sanitize_cors_unsafe([$a, 1, 2, 10, 13, $b]))},
   {"DEL (0x7F) is removed",
    ?_assertEqual("ab", parse_http:sanitize_cors_unsafe([$a, 127, $b]))},
   {"the empty string is unchanged",
    ?_assertEqual("", parse_http:sanitize_cors_unsafe(""))},
   {"0x19 is removed - the doc comment says 'Sanitizes string of 0x0-0x19 and 0x7F'. "
    "(regression: this used to fail because the guard is 'Character < 25' and 0x19 is 25, an off-by-one that keeps it)",
    ?_assertEqual("ab", parse_http:sanitize_cors_unsafe([$a, 25, $b]))}].

%%--------------------------------------------------------------------
query_tests() ->
  Q = fun(Path) -> req("GET " ++ Path ++ " HTTP/1.1\r\n\r\n") end,
  [{"a single pair",
    ?_assertEqual({ok, #{"x" => "1"}}, parse_http:parse_query_parameters(Q("/a?x=1")))},
   {"no query string yields an empty map",
    ?_assertEqual({ok, #{}}, parse_http:parse_query_parameters(Q("/a")))},
   {"an empty value is kept",
    ?_assertEqual({ok, #{"x" => ""}}, parse_http:parse_query_parameters(Q("/a?x=")))},
   {"a pair without '=' is an error",
    ?_assertEqual({error, badpair}, parse_http:parse_query_parameters(Q("/a?flag")))},
   {"several '&' separated pairs are all parsed ('x=1&y=2' -> #{x => 1, y => 2}). "
    "(regression: this used to fail because parse_query_params/2 splits the whole remaining list of pairs "
    "(string:split(Pairs, \"=\")) instead of the current Pair, so the first value swallows the "
    "following pairs (\"1y=2\"))",
    ?_assertEqual({ok, #{"x" => "1", "y" => "2"}}, parse_http:parse_query_parameters(Q("/a?x=1&y=2")))},
   {"get_query_parameter/3 returns the value (mcp_sse uses it for sessionId)",
    ?_assertEqual("ABC", parse_http:get_query_parameter("sessionId", Q("/sse?sessionId=ABC"), none))},
   {"get_query_parameter/3 returns the default when the parameter is absent",
    ?_assertEqual(none, parse_http:get_query_parameter("sessionId", Q("/sse"), none))},
   {"get_query_parameter/3 returns the default when there is no query string at all",
    ?_assertEqual(none, parse_http:get_query_parameter("sessionId", Q("/"), none))}].

%%--------------------------------------------------------------------
%% Older line-list based helpers that are still exported.
legacy_tests() ->
  [{"http2map/1 extracts method, route and version from the first line",
    ?_test(begin
             {ok, M} = parse_http:http2map([<<"GET /x HTTP/1.1">>, <<"Host: h">>]),
             ?assertEqual(<<"GET">>, maps:get(method, M)),
             ?assertEqual(<<"/x">>, maps:get(route, M)),
             ?assertEqual(<<"HTTP/1.1">>, maps:get(http_ver, M)),
             ?assert(maps:is_key("Host", M))
           end)},
   {"http2map/1 rejects a short request line with 400",
    ?_assertEqual({aborted, 400}, parse_http:http2map([<<"GET /x">>]))},
   {"parse_request/2 builds a request record and splits the query string",
    ?_test(begin
             {ok, R} = parse_http:parse_request({1, 2, 3, 4}, [<<"GET /x?y=1 HTTP/1.1">>, <<"Host: h">>]),
             ?assertEqual(<<"/x">>, R#request.route),
             ?assertEqual(<<"y=1">>, R#request.params),
             ?assertEqual(<<"GET">>, R#request.method),
             ?assertEqual({1, 2, 3, 4}, R#request.src_addr)
           end)},
   {"parse_request/2 reports bad_request for a malformed first line",
    ?_assertEqual(bad_request, parse_http:parse_request({1, 2, 3, 4}, [<<"GET">>]))},
   {"is_request_finished/1: two trailing empty lines mean the headers are complete",
    ?_assert(parse_http:is_request_finished([<<"GET / HTTP/1.1">>, <<>>, <<>>]))},
   {"is_request_finished/1: an empty line followed by body bytes",
    ?_assert(parse_http:is_request_finished([<<"POST / HTTP/1.1">>, <<>>, <<"body">>]))},
   {"is_request_finished/1: headers still open",
    ?_assertNot(parse_http:is_request_finished([<<"GET / HTTP/1.1">>, <<"Host: h">>]))},
   {"is_request_finished/1: fewer than two lines",
    ?_assertNot(parse_http:is_request_finished([<<"GET / HTTP/1.1">>]))},
   {"update_lines/2 joins the partial last line with the new chunk",
    ?_assertEqual(["GET / HTTP/1.1", "Host: example", "X: 1"],
                  parse_http:update_lines(["GET / HTTP/1.1", "Host: ex"], "ample\r\nX: 1"))},
   {"ensure_body/1 is true for a one byte binary",
    ?_assert(parse_http:ensure_body(<<"x">>))},
   {"ensure_body/1 is false for an empty binary",
    ?_assertNot(parse_http:ensure_body(<<>>))},
   {"ensure_body/1 is true for any non-empty binary. "
    "(regression: this used to fail because the clause pattern <<_>> only matches a binary of exactly one byte)",
    ?_assert(parse_http:ensure_body(<<"abc">>))}].
