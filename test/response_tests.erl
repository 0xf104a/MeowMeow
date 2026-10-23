%%%-------------------------------------------------------------------
%%% @doc EUnit suite for response.erl
%%%
%%% response.erl is pure: it renders status lines / headers to the
%%% wire format and manipulates the #response{} record. response_error/1
%%% calls util:get_time/0 only, so no fixtures are needed except for
%%% the Server header version string from config.hrl.
%%% @end
%%%-------------------------------------------------------------------
-module(response_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/config.hrl").
-include("../src/response.hrl").

desc_test_() ->
  [{"get_desc maps 200 to OK", ?_assertEqual("OK", response:get_desc("200"))},
   {"get_desc maps 204 to No Content", ?_assertEqual("No Content", response:get_desc("204"))},
   {"get_desc maps 404 to Not Found", ?_assertEqual("Not Found", response:get_desc("404"))},
   {"get_desc maps 418 to I'm a teapot (routes.conf /i_am_a_teapot)",
    ?_assertEqual("I'm a teapot", response:get_desc("418"))},
   {"get_desc maps 451 to Unavailable For Legal Reasons (routes.conf /catnip)",
    ?_assertEqual("Unavailable For Legal Reasons", response:get_desc("451"))},
   {"get_desc maps 502 to Bad Gateway (rules:execute_rule failure code)",
    ?_assertEqual("Bad Gateway", response:get_desc("502"))},
   {"get_desc accepts the code as an integer as well",
    ?_assertEqual("OK", response:get_desc(200))},
   {"any valid 3-digit status code renders (Set-Code accepts any integer, "
    "e.g. 'Set-Code 299' is a legal unregistered 2xx code), so get_desc must return a "
    "reason phrase - possibly a generic one - instead of raising. "
    "(regression: this used to fail because get_desc/1 does maps:get on a fixed table and raises badkey, which "
    "crashes the request handler after the rules have run)",
    ?_assert(is_list(response:get_desc("299")))}].

render_test_() ->
  [{"response/3 renders status line, headers, blank line and body",
    ?_assertEqual("HTTP/1.1 200 OK\r\nX-A: 1\r\n\r\nbody",
                  response:response(#{"X-A" => "1"}, 200, "body"))},
   {"response/3 renders every header as 'Key: Value'",
    ?_test(begin
             Wire = response:response(#{"Server" => "meow", "Content-Type" => "text/html"},
                                      404, "nope"),
             {Code, Headers, Body} = test_support:parse_response(Wire),
             ?assertEqual(404, Code),
             ?assertEqual(#{"Server" => "meow", "Content-Type" => "text/html"}, Headers),
             ?assertEqual("nope", Body)
           end)},
   {"response/3 with no headers still terminates the header block",
    ?_assertEqual("HTTP/1.1 204 No Content\r\n\r\n", response:response(#{}, 204, ""))},
   {"response/3 accepts a binary body (handle.erl passes binaries)",
    ?_assertEqual(<<"HTTP/1.1 200 OK\r\n\r\nbin">>,
                  iolist_to_binary(response:response(#{}, 200, <<"bin">>)))},
   {"response_headers/2 renders only the head, terminated by an empty line",
    ?_assertEqual("HTTP/1.1 200 OK\r\nX-A: 1\r\n\r\n",
                  response:response_headers(#{"X-A" => "1"}, 200))},
   {"do_response_headers/1 uses the record's headers and code",
    ?_assertEqual("HTTP/1.1 451 Unavailable For Legal Reasons\r\nX: y\r\n\r\n",
                  response:do_response_headers(#response{code = 451, headers = #{"X" => "y"}}))},
   {"response_error/1 builds an error response record with Server, Date and Content-Type",
    ?_test(begin
             R = response:response_error(500),
             ?assertEqual(500, R#response.code),
             ?assertEqual(?version, maps:get("Server", R#response.headers)),
             ?assertEqual("text/html", maps:get("Content-Type", R#response.headers)),
             ?assert(maps:is_key("Date", R#response.headers))
           end)}].

header_manipulation_test_() ->
  Base = #response{headers = #{"A" => "1", "B" => "2"}},
  [{"set_header adds a header",
    ?_assertEqual(#{"A" => "1", "B" => "2", "C" => "3"},
                  (response:set_header(Base, "C", "3"))#response.headers)},
   {"set_header overwrites an existing header",
    ?_assertEqual(#{"A" => "x", "B" => "2"},
                  (response:set_header(Base, "A", "x"))#response.headers)},
   {"set_headers merges a map, new values win",
    ?_assertEqual(#{"A" => "1", "B" => "y", "C" => "3"},
                  (response:set_headers(Base, #{"B" => "y", "C" => "3"}))#response.headers)},
   {"update_headers returns the merged map (not a record)",
    ?_assertEqual(#{"A" => "1", "B" => "2", "C" => "3"},
                  response:update_headers(Base, #{"C" => "3"}))},
   {"set_header leaves the rest of the record untouched",
    ?_assertEqual(418, (response:set_header(Base#response{code = 418}, "C", "3"))#response.code)}].

code_test_() ->
  R = #response{code = 200},
  [{"set_code changes the code when no override is active",
    ?_assertEqual(404, (response:set_code(R, 404))#response.code)},
   {"override_code sets the code and marks it overridden",
    ?_test(begin
             O = response:override_code(R, 451),
             ?assertEqual(451, O#response.code),
             ?assert(O#response.status_code_override)
           end)},
   {"set_code is ignored after override_code (Set-Code 451 survives Send-File's set_code 200)",
    ?_assertEqual(451, (response:set_code(response:override_code(R, 451), 200))#response.code)},
   {"a second override_code replaces the first (documented in override_code/2)",
    ?_assertEqual(403, (response:override_code(response:override_code(R, 451), 403))#response.code)}].
