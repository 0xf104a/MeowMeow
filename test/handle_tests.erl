%%%-------------------------------------------------------------------
%%% @doc EUnit suite for handle.erl
%%%
%%% The connection handler talks to the TCP control process ("upstream")
%%% only through messages ({send, Data}, close, recv, set_tmr), so the
%%% test process plays the upstream and inspects what it receives.
%%% handler_start/1 needs a real socket (it calls socket:peername), so
%%% those tests use a loopback socket pair.
%%% @end
%%%-------------------------------------------------------------------
-module(handle_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/config.hrl").
-include("../src/request.hrl").
-include("../src/response.hrl").

setup() ->
  test_support:setup_all(#{"LogLevel" => "0", "KeepAlive" => "2000"}),
  {ok, route} = nya_loader:nya_load("route"),
  ok.

handle_test_() ->
  {setup, fun setup/0, fun test_support:teardown_all/1,
   fun(_) ->
     [abort_tests(), is_good_tests(), misc_tests(), keepalive_tests(),
      rule_chain_tests(), dispatch_tests(), handler_process_tests(), body_recv_tests()]
   end}.

req(Route) -> req(Route, []).
req(Route, Headers) -> test_support:request("GET", Route, [{"Host", "h"} | Headers]).
keepalive(Route) -> req(Route, [{"Connection", "keep-alive"}]).
raw(Str) -> parse_http:update_request(parse_http:make_request({127, 0, 0, 1}), list_to_binary(Str)).

run_handle(Rules, Request) ->
  test_support:flush(),
  test_support:set_access(Rules),
  Result = handle:handle(test_support:response_for(Request), self()),
  {Result, test_support:collect(100)}.

run_handle3(Rules, Request) ->
  test_support:flush(),
  test_support:set_access(Rules),
  Result = handle:handle(fake_socket, self(), Request),
  {Result, test_support:collect(100)}.

sent(Msgs) -> [test_support:parse_response(W) || {send, W} <- Msgs].

%%--------------------------------------------------------------------
abort_tests() ->
  [{"abort/1 renders a complete error response with the headers a client needs",
    ?_test(begin
             {Code, H, Body} = test_support:parse_response(handle:abort(404)),
             ?assertEqual(404, Code),
             ?assertEqual("text/html", maps:get("Content-Type", H)),
             ?assertEqual("close", maps:get("Connection", H)),
             ?assertEqual(?version, maps:get("Server", H)),
             ?assertEqual(integer_to_list(length(Body)), maps:get("Content-Length", H)),
             ?assert(maps:is_key("Date", H)),
             ?assertNotEqual(nomatch, string:find(Body, "404 Not Found"))
           end)},
   {"abort/1 works for every code the rules can produce (403, 405, 411, 413, 500, 502, 505)",
    ?_assertEqual([403, 405, 411, 413, 500, 502, 505],
                  [element(1, test_support:parse_response(handle:abort(C)))
                   || C <- [403, 405, 411, 413, 500, 502, 505]])},
   {"abort(<<\"HEAD\">>, Code) sends the same headers but no body",
    ?_test(begin
             {Code, H, Body} = test_support:parse_response(handle:abort(<<"HEAD">>, 404)),
             {_, HGet, BodyGet} = test_support:parse_response(handle:abort(404)),
             ?assertEqual(404, Code),
             ?assertEqual("", Body),
             ?assertEqual(integer_to_list(length(BodyGet)), maps:get("Content-Length", H)),
             ?assertEqual(maps:remove("Date", HGet), maps:remove("Date", H))
           end)},
   {"abort(<<\"GET\">>, Code) is the same as abort(Code)",
    ?_test(begin
             {Code, _, Body} = test_support:parse_response(handle:abort(<<"GET">>, 418)),
             ?assertEqual(418, Code),
             ?assertNotEqual(nomatch, string:find(Body, "418 I'm a teapot"))
           end)},
   {"get_abort_body/1 mentions the code, the reason phrase and the server version",
    ?_test(begin
             B = handle:get_abort_body(451),
             ?assertNotEqual(nomatch, string:find(B, "451 Unavailable For Legal Reasons")),
             ?assertNotEqual(nomatch, string:find(B, ?version))
           end)}].

%%--------------------------------------------------------------------
is_good_tests() ->
  [{"HTTP/1.1 is good",
    ?_assertEqual(good, handle:is_good(raw("GET / HTTP/1.1\r\n\r\n")))},
   {"HTTP/2.0 is good (any version >= 1.1)",
    ?_assertEqual(good, handle:is_good(raw("GET / HTTP/2.0\r\n\r\n")))},
   {"HTTP/1.0 is rejected by default (AllowLegacyHttp No)",
    ?_test(begin
             test_support:set_config("AllowLegacyHttp", "No"),
             ?assertEqual({bad, old_http}, handle:is_good(raw("GET / HTTP/1.0\r\n\r\n")))
           end)},
   {"HTTP/1.0 is accepted when AllowLegacyHttp is Yes",
    ?_test(begin
             test_support:set_config("AllowLegacyHttp", "Yes"),
             try
               ?assertEqual(good, handle:is_good(raw("GET / HTTP/1.0\r\n\r\n")))
             after
               test_support:set_config("AllowLegacyHttp", "No")
             end
           end)}].

%%--------------------------------------------------------------------
misc_tests() ->
  [{"get_ua/1 reports an unknown agent when the header is absent",
    ?_assertEqual("<<Unknown UA>>", handle:get_ua(req("/")))},
   {"EXPECTATION: get_ua/1 returns the User-Agent header value. "
    "FAILS BECAUSE: it looks up the key \"User-Agent\" while parse_http stores header names "
    "lower-cased (\"user-agent\"), so every client is reported as unknown",
    ?_assertEqual("curl/8", handle:get_ua(req("/", [{"User-Agent", "curl/8"}])))},
   {"log_response/2 does not crash and returns ok",
    ?_assertEqual(ok, handle:log_response(req("/"), 200))},
   {"maybe_set_content_length/1 sets Content-Length from the binary body size",
    ?_assertEqual("5", maps:get("Content-Length",
                                (handle:maybe_set_content_length(#response{body = <<"hello">>}))#response.headers))},
   {"maybe_set_content_length/1 counts bytes, not characters",
    ?_assertEqual("6", maps:get("Content-Length",
                                (handle:maybe_set_content_length(#response{body = <<"héllo"/utf8>>}))#response.headers))},
   {"maybe_set_content_length/1 leaves a response with the default empty body alone",
    ?_assertEqual(#{}, (handle:maybe_set_content_length(#response{body = ""}))#response.headers)},
   {"maybe_set_content_length/1 leaves an empty binary body alone",
    ?_assertEqual(#{}, (handle:maybe_set_content_length(#response{body = <<>>}))#response.headers)},
   {"get_default_response/2 for HEAD is a 404 with an empty body",
    ?_assertMatch(#response{code = 404, body = <<>>},
                  handle:get_default_response(<<"HEAD">>, #response{}))},
   {"get_default_response/2 for GET is a 404 with the HTML error body",
    ?_test(begin
             R = handle:get_default_response(<<"GET">>, #response{}),
             ?assertEqual(404, R#response.code),
             ?assert(is_binary(R#response.body)),
             ?assertNotEqual(nomatch, binary:match(R#response.body, <<"404 Not Found">>))
           end)}].

%%--------------------------------------------------------------------
keepalive_tests() ->
  [{"set_keepalive/1 answers Connection: keep-alive to a keep-alive request",
    ?_assertEqual("keep-alive",
                  maps:get("Connection",
                           (handle:set_keepalive(test_support:response_for(keepalive("/"))))#response.headers))},
   {"set_keepalive/1 answers Connection: close otherwise",
    ?_assertEqual("close",
                  maps:get("Connection",
                           (handle:set_keepalive(test_support:response_for(req("/"))))#response.headers))},
   {"close_connection/2 tells the upstream to close when the request asks for it",
    ?_test(begin
             test_support:flush(),
             ?assertEqual(ok, handle:close_connection(req("/"), self())),
             ?assertEqual({ok, close}, test_support:recv(100))
           end)},
   {"close_connection/2 leaves a keep-alive connection open",
    ?_test(begin
             test_support:flush(),
             ?assertEqual(not_closed, handle:close_connection(keepalive("/"), self())),
             ?assertEqual(timeout, test_support:recv(100))
           end)}].

%%--------------------------------------------------------------------
rule_chain_tests() ->
  [{"with no matching rule the answer is 404 Not Found with an HTML body and the connection closes",
    ?_test(begin
             {Result, Msgs} = run_handle([], req("/nothing")),
             ?assertEqual(ok, Result),
             ?assertMatch([{send, _}, close], Msgs),
             [{404, H, Body}] = sent(Msgs),
             ?assertNotEqual(nomatch, string:find(Body, "404 Not Found")),
             ?assertEqual(integer_to_list(length(Body)), maps:get("Content-Length", H)),
             ?assertEqual("close", maps:get("Connection", H)),
             ?assertEqual(?version, maps:get("Server", H))
           end)},
   {"Abort 418 answers 418 and closes (test_rules.rb test_abort)",
    ?_test(begin
             {_, Msgs} = run_handle([{"Abort", ["418"]}], req("/i_am_a_teapot")),
             %% handle/2 sends 'close' itself and close_connection/2 sends it
             %% again; the control process exits on the first one.
             ?assertMatch([{send, _}, close | _], Msgs),
             ?assertMatch([{418, _, _}], sent(Msgs))
           end)},
   {"Set-Header adds the header to the response (test_rules.rb test_header_set)",
    ?_test(begin
             {_, Msgs} = run_handle([{"Set-Header", ["X-Powered-By", "Pusheen the Cat"]}], req("/")),
             [{404, H, _}] = sent(Msgs),
             ?assertEqual("Pusheen the Cat", maps:get("X-Powered-By", H))
           end)},
   {"No-Content answers 204 with an empty body (test_rules.rb test_204)",
    ?_test(begin
             {_, Msgs} = run_handle([{"No-Content", true}], req("/generate_204")),
             ?assertMatch([{204, _, ""}], sent(Msgs))
           end)},
   {"a keep-alive request is answered with Connection: keep-alive and the connection stays open",
    ?_test(begin
             {Result, Msgs} = run_handle([], keepalive("/")),
             ?assertEqual(not_closed, Result),
             ?assertMatch([{send, _}], Msgs),
             [{404, H, _}] = sent(Msgs),
             ?assertEqual("keep-alive", maps:get("Connection", H))
           end)},
   {"a Route block only applies to matching paths",
    ?_test(begin
             Rules = [{context, "Route", ["/tea"], [{"Abort", ["418"]}]}],
             {_, M1} = run_handle(Rules, req("/tea")),
             ?assertMatch([{418, _, _}], sent(M1)),
             {_, M2} = run_handle(Rules, req("/coffee")),
             ?assertMatch([{404, _, _}], sent(M2))
           end)},
   {"HEAD gets the status and headers but an empty body",
    ?_test(begin
             {_, Msgs} = run_handle([], test_support:request("HEAD", "/", [{"Host", "h"}])),
             ?assertMatch([{404, _, ""}], sent(Msgs))
           end)},
   {"a rule returning an unknown shape is answered with 502 and the connection closes",
    ?_test(begin
             rules:register_rule("Garbage", fun(_, _) -> garbage end),
             {_, Msgs} = run_handle([{"Garbage", []}], req("/")),
             ?assertMatch([{send, _}, close | _], Msgs),
             ?assertMatch([{502, _, _}], sent(Msgs))
           end)},
   {"a module-provided body gets a Content-Length in bytes",
    ?_test(begin
             rules:register_rule("Body", fun(_, R) -> {ok, R#response{code = 200, body = <<"héllo"/utf8>>}} end),
             {_, Msgs} = run_handle([{"Body", []}], req("/")),
             [{200, H, Body}] = sent(Msgs),
             ?assertEqual("6", maps:get("Content-Length", H)),
             ?assertEqual(<<"héllo"/utf8>>, list_to_binary(Body))
           end)}].

%%--------------------------------------------------------------------
dispatch_tests() ->
  Post = fun(Headers, Body) -> test_support:request("POST", "/", [{"Host", "h"} | Headers], Body) end,
  [{"an already-aborted request is answered with that code",
    ?_test(begin
             {Result, Msgs} = run_handle3([], {aborted, 400}),
             ?assertEqual(ok, Result),
             ?assertMatch([{400, _, _}], sent(Msgs))
           end)},
   {"HTTP/1.0 is rejected with 505 when AllowLegacyHttp is No",
    ?_test(begin
             test_support:set_config("AllowLegacyHttp", "No"),
             {_, Msgs} = run_handle3([], raw("GET / HTTP/1.0\r\nHost: h\r\n\r\n")),
             ?assertMatch([{send, _}, close], Msgs),
             ?assertMatch([{505, _, _}], sent(Msgs))
           end)},
   {"HTTP/1.0 is served when AllowLegacyHttp is Yes",
    ?_test(begin
             test_support:set_config("AllowLegacyHttp", "Yes"),
             try
               {_, Msgs} = run_handle3([], raw("GET / HTTP/1.0\r\nHost: h\r\n\r\n")),
               ?assertMatch([{404, _, _}], sent(Msgs))
             after
               test_support:set_config("AllowLegacyHttp", "No")
             end
           end)},
   {"an unknown method is rejected with 405 and the connection closes",
    ?_test(begin
             {_, Msgs} = run_handle3([], test_support:request("PATCH", "/", [{"Host", "h"}])),
             ?assertMatch([{send, _}, close], Msgs),
             ?assertMatch([{405, _, _}], sent(Msgs))
           end)},
   {"DELETE goes through the rule chain like GET",
    ?_test(begin
             {_, Msgs} = run_handle3([{"Abort", ["418"]}], test_support:request("DELETE", "/", [{"Host", "h"}])),
             ?assertMatch([{418, _, _}], sent(Msgs))
           end)},
   {"POST without Content-Length is rejected with 411 Length Required",
    ?_test(begin
             {_, Msgs} = run_handle3([{"Abort", ["418"]}], Post([], "")),
             ?assertMatch([{send, _}, close], Msgs),
             ?assertMatch([{411, _, _}], sent(Msgs))
           end)},
   {"POST larger than MaxPostSize (10240) is rejected with 413",
    ?_test(begin
             {_, Msgs} = run_handle3([{"Abort", ["418"]}], Post([{"Content-Length", "99999"}], "")),
             ?assertMatch([{413, _, _}], sent(Msgs))
           end)},
   {"POST within MaxPostSize runs the rule chain",
    ?_test(begin
             {_, Msgs} = run_handle3([{"Abort", ["418"]}], Post([{"Content-Length", "5"}], "hello")),
             ?assertMatch([{418, _, _}], sent(Msgs))
           end)},
   {"EXPECTATION: a POST to a path no rule handles is answered 404 like a GET. "
    "FAILS BECAUSE: handle_post_unwrapped/2 seeds the response body with the full text of "
    "abort(404) (a string) and maybe_set_content_length/1 then calls byte_size/1 on that string, "
    "so the handler crashes with badarg and the client never gets an answer",
    ?_test(begin
             {_, Msgs} = run_handle3([], Post([{"Content-Length", "5"}], "hello")),
             ?assertMatch([{send, _}, close], Msgs),
             ?assertMatch([{404, _, _}], sent(Msgs))
           end)}].

%%--------------------------------------------------------------------
start_handler() ->
  test_support:flush(),
  test_support:set_access([]),
  {S, C} = test_support:socket_pair(),
  Pid = spawn(fun() -> handle:handler_start(S) end),
  Ref = monitor(process, Pid),
  Pid ! {upstream, self()},
  {Pid, Ref, S, C}.

down(Ref, Timeout) ->
  receive {'DOWN', Ref, process, _, Reason} -> {down, Reason}
  after Timeout -> alive
  end.

data(Method, Route, Headers) ->
  list_to_binary(test_support:raw_request(Method, Route, [{"Host", "h"} | Headers], "")).

handler_process_tests() ->
  [{"handler_start/1 asks the upstream to receive once it knows the upstream pid",
    {timeout, 10, ?_test(begin
       {_, _, _, _} = start_handler(),
       ?assertEqual({ok, recv}, test_support:recv(1000))
     end)}},
   {"a keep-alive request is answered, then the handler asks for more data and a timer reset",
    {timeout, 10, ?_test(begin
       {Pid, Ref, _, _} = start_handler(),
       {ok, recv} = test_support:recv(1000),
       Pid ! {data, data("GET", "/", [{"Connection", "keep-alive"}])},
       Msgs = test_support:collect(300),
       ?assertMatch([{send, _}, recv, set_tmr], Msgs),
       ?assertMatch([{404, _, _}], sent(Msgs)),
       ?assert(is_process_alive(Pid)),
       Pid ! closed,
       ?assertEqual({down, closed}, down(Ref, 1000))
     end)}},
   {"a request without keep-alive is answered, the connection closed and the handler exits with 'done'",
    {timeout, 10, ?_test(begin
       {Pid, Ref, _, _} = start_handler(),
       {ok, recv} = test_support:recv(1000),
       Pid ! {data, data("GET", "/", [])},
       Msgs = test_support:collect(300),
       ?assertMatch([{send, _}, close | _], Msgs),
       ?assertEqual({down, done}, down(Ref, 1000))
     end)}},
   {"a request may arrive in several chunks split at a line boundary",
    {timeout, 10, ?_test(begin
       {Pid, _, _, _} = start_handler(),
       {ok, recv} = test_support:recv(1000),
       Pid ! {data, <<"GET / HTTP/1.1\r\n">>},
       ?assertEqual([], test_support:collect(100)),
       Pid ! {data, <<"Host: h\r\n\r\n">>},
       Msgs = test_support:collect(300),
       ?assertMatch([{404, _, _}], sent(Msgs))
     end)}},
   {"'timeout' from the upstream makes the handler exit with 'timeout'",
    {timeout, 10, ?_test(begin
       {Pid, Ref, _, _} = start_handler(),
       {ok, recv} = test_support:recv(1000),
       Pid ! timeout,
       ?assertEqual({down, timeout}, down(Ref, 1000))
     end)}},
   {"'closed' from the upstream makes the handler exit with 'closed'",
    {timeout, 10, ?_test(begin
       {Pid, Ref, _, _} = start_handler(),
       {ok, recv} = test_support:recv(1000),
       Pid ! closed,
       ?assertEqual({down, closed}, down(Ref, 1000))
     end)}},
   {"an unexpected message makes the handler answer 500 and exit with 'error'",
    {timeout, 10, ?_test(begin
       {Pid, Ref, _, _} = start_handler(),
       {ok, recv} = test_support:recv(1000),
       Pid ! bogus,
       ?assertEqual({down, error}, down(Ref, 1000)),
       ?assertMatch([{500, _, _}], sent(test_support:collect(100)))
     end)}},
   {"EXPECTATION: a malformed request line is answered with 400 Bad Request. "
    "FAILS BECAUSE: handler/3 reads ARequest#request.is_headers_accepted before checking "
    "for {aborted, 400}; on the tuple that raises badrecord and the handler dies without answering",
    {timeout, 10, ?_test(begin
       {Pid, _, _, _} = start_handler(),
       {ok, recv} = test_support:recv(1000),
       Pid ! {data, <<"GARBAGE\r\n\r\n">>},
       Msgs = test_support:collect(300),
       ?assertMatch([{400, _, _}], sent(Msgs))
     end)}}].

%%--------------------------------------------------------------------
body_recv_tests() ->
  Post = fun(Len, Body) ->
           test_support:request("POST", "/", [{"Host", "h"}, {"Content-Length", Len}], Body)
         end,
  [{"a body that arrived with the headers is converted to a binary without touching the socket",
    ?_test(begin
             {ok, R} = handle:handle_body_recv(test_support:response_for(Post("5", "hello"))),
             ?assertEqual(<<"hello">>, R#response.request#request.body)
           end)},
   {"a missing Content-Length is reported as unknown_length",
    ?_assertEqual({error, unknown_length},
                  handle:handle_body_recv(test_support:response_for(req("/"))))},
   {"the remainder of the body is read from the socket",
    {timeout, 10, ?_test(begin
       {S, C} = test_support:socket_pair(),
       Resp = (test_support:response_for(Post("10", "hello")))#response{socket = S},
       spawn(fun() -> timer:sleep(50), gen_tcp:send(C, "world") end),
       {ok, R} = handle:handle_body_recv(Resp),
       ?assertEqual(<<"helloworld">>, R#response.request#request.body)
     end)}},
   {"a peer closing before the body is complete is an error",
    {timeout, 10, ?_test(begin
       {S, C} = test_support:socket_pair(),
       Resp = (test_support:response_for(Post("10", "hello")))#response{socket = S},
       gen_tcp:close(C),
       ?assertMatch({error, _}, handle:handle_body_recv(Resp))
     end)}}].
