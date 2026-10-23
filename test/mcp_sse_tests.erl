%%%-------------------------------------------------------------------
%%% @doc EUnit suite for mcp_sse.erl (legacy HTTP+SSE transport, 2024-11-05)
%%%
%%% Spec: https://modelcontextprotocol.io/specification/2024-11-05/basic/transports
%%%   GET  /sse                 -> event-stream, first event 'endpoint' with the POST URL
%%%   POST /sse?sessionId=<id>  -> 202 Accepted, response arrives on the stream
%%% `cat' plays the MCP tool so every POSTed line is echoed back as an
%%% SSE 'message' event.
%%% @end
%%%-------------------------------------------------------------------
-module(mcp_sse_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/request.hrl").
-include("../src/response.hrl").

setup() ->
  test_support:setup_all(#{"LogLevel" => "0"}),
  test_support:setup_mcp(),
  ok.

cleanup(_) ->
  test_support:cleanup_mcp_sessions(),
  test_support:teardown_all(ok).

cat() -> test_support:tool("cat").
args() -> [cat(), "5000"].

json_post(Route, Body) ->
  test_support:request("POST", Route,
                       [{"Host", "h"}, {"Content-Type", "application/json"},
                        {"Content-Length", integer_to_list(length(Body))}], Body).

call(Req) -> mcp_sse:rule_mcp_sse_call(args(), test_support:response_for(Req)).

%% Run the rule in its own process (it blocks in sse_push_loop) with the
%% test process as upstream; the result comes back as {sse_done, R}.
spawn_call(Req) ->
  Self = self(),
  Resp = test_support:response_for(Req, Self),
  spawn(fun() -> Self ! {sse_done, mcp_sse:rule_mcp_sse_call(args(), Resp)} end).

new_session() ->
  Id = mcp_port:start_mcp_session(cat(), 5000),
  {ok, Pid} = mcp_port:get_mcp_session_by_id(Id),
  {binary_to_list(Id), Pid}.

mcp_sse_test_() ->
  {setup, fun setup/0, fun cleanup/1,
   fun(_) -> [pure_tests(), dispatch_tests(), flow_tests()] end}.

%%--------------------------------------------------------------------
pure_tests() ->
  [{"is_initialize_request/1 recognises an initialize request body (binary)",
    ?_assert(mcp_sse:is_initialize_request(<<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\"}">>))},
   {"is_initialize_request/1 accepts a string body",
    ?_assert(mcp_sse:is_initialize_request("{\"method\":\"initialize\"}"))},
   {"is_initialize_request/1 is false for other methods",
    ?_assertNot(mcp_sse:is_initialize_request(<<"{\"method\":\"tools/list\"}">>))},
   {"is_initialize_request/1 is false for a non-body value",
    ?_assertNot(mcp_sse:is_initialize_request(undefined))},
   {"EXPECTATION: only a request whose JSON-RPC method is 'initialize' is an initialize request; "
    "a tools/call that merely mentions the word is not. "
    "FAILS BECAUSE: is_initialize_request/1 checks for the substrings \"method\" and "
    "\"initialize\" anywhere in the body instead of decoding the JSON",
    ?_assertNot(mcp_sse:is_initialize_request(
                  <<"{\"method\":\"tools/call\",\"params\":{\"name\":\"initialize\"}}">>))},
   {"format_sse_event/2 renders an SSE frame",
    ?_assertEqual(<<"event: endpoint\ndata: /sse?sessionId=X\n\n">>,
                  mcp_sse:format_sse_event("endpoint", "/sse?sessionId=X"))},
   {"format_sse_event/2 accepts binary data",
    ?_assertEqual(<<"event: message\ndata: {}\n\n">>, mcp_sse:format_sse_event("message", <<"{}">>))},
   {"mcp_request_state/1 extracts sessionId, initialize flag and content type",
    ?_assertEqual({"ABC", true, "application/json"},
                  mcp_sse:mcp_request_state(json_post("/sse?sessionId=ABC", "{\"method\":\"initialize\"}")))},
   {"mcp_request_state/1 defaults to none/false/none",
    ?_assertEqual({none, false, none},
                  mcp_sse:mcp_request_state(test_support:request("GET", "/sse", [{"Host", "h"}])))}].

%%--------------------------------------------------------------------
dispatch_tests() ->
  [{"POST without sessionId is a 400",
    ?_assertEqual({aborted, 400}, call(json_post("/sse", "{}")))},
   {"POST with a sessionId but a non-JSON content type is a 415",
    ?_assertEqual({aborted, 415},
                  call(test_support:request("POST", "/sse?sessionId=X",
                                            [{"Host", "h"}, {"Content-Type", "text/plain"},
                                             {"Content-Length", "2"}], "{}")))},
   {"POST to an unknown session is a 404",
    ?_assertEqual({aborted, 404}, call(json_post("/sse?sessionId=UNKNOWN", "{}")))},
   {"GET with an unknown sessionId is a 404",
    ?_assertEqual({aborted, 404}, call(test_support:request("GET", "/sse?sessionId=UNKNOWN", [{"Host", "h"}])))},
   {"DELETE of an unknown session is a 404",
    ?_assertEqual({aborted, 404}, call(test_support:request("DELETE", "/sse?sessionId=UNKNOWN", [{"Host", "h"}])))},
   {"DELETE without a sessionId is rejected",
    ?_assertMatch({aborted, C} when C >= 400, call(test_support:request("DELETE", "/sse", [{"Host", "h"}])))},
   {"an unsupported method is a 405",
    ?_assertEqual({aborted, 405}, call(test_support:request("PUT", "/sse", [{"Host", "h"}])))}].

%%--------------------------------------------------------------------
flow_tests() ->
  [{"EXPECTATION: GET without a session opens an event stream, starts the tool and emits the "
    "'endpoint' event '<route>?sessionId=<id>' (2024-11-05 spec). "
    "FAILS BECAUSE: handle_sse_open/4 builds the URL with Route ++ \"?sessionId=\" where Route "
    "is the binary the parser stores, and '++' on a binary raises badarg - the rule process "
    "dies right after sending the headers",
    {timeout, 15, ?_test(begin
       test_support:flush(),
       spawn_call(test_support:request("GET", "/sse", [{"Host", "h"}])),
       Msgs = test_support:collect(500),
       ?assertMatch([cancel_tmr, {send, _}, {send, _} | _], Msgs),
       [{send, Headers}, {send, Endpoint} | _] = [M || {send, _} = M <- Msgs],
       {200, H, ""} = test_support:parse_response(Headers),
       ?assertEqual("text/event-stream", maps:get("Content-Type", H)),
       ?assertMatch({0, _}, binary:match(iolist_to_binary(Endpoint), <<"event: endpoint\ndata: /sse?sessionId=">>))
     end)}},
   {"POST to an existing session forwards the body to the tool and answers 202 with no body",
    {timeout, 15, ?_test(begin
       test_support:flush(),
       {Id, Pid} = new_session(),
       ok = mcp_server:connect_to_mcp_session(Pid),
       Body = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}",
       {Tag, R} = call(json_post("/sse?sessionId=" ++ Id, Body)),
       ?assertEqual(ready2send, Tag),
       ?assertEqual(202, R#response.code),
       ?assertEqual(<<>>, R#response.body),
       ?assertEqual("0", maps:get("Content-Length", R#response.headers)),
       ?assertEqual({ok, list_to_binary(Body)}, test_support:recv(2000)),
       ok = mcp_server:terminate_session(Pid)
     end)}},
   {"GET with an existing sessionId reconnects to the push channel and relays tool output as 'message' events",
    {timeout, 15, ?_test(begin
       test_support:flush(),
       {Id, Pid} = new_session(),
       spawn_call(test_support:request("GET", "/sse?sessionId=" ++ Id, [{"Host", "h"}])),
       Open = test_support:collect(300),
       ?assertMatch([cancel_tmr, {send, _}], Open),
       [{send, Headers}] = [M || {send, _} = M <- Open],
       ?assertMatch({200, #{"Content-Type" := "text/event-stream"}, ""}, test_support:parse_response(Headers)),
       ok = mcp_server:notify_mcp_session(Pid, <<"{\"id\":7}">>),
       ?assertEqual({ok, {send, <<"event: message\ndata: {\"id\":7}\n\n">>}}, test_support:recv(2000)),
       ok = mcp_server:terminate_session(Pid),
       Rest = test_support:collect(500),
       ?assert(lists:member(close, Rest)),
       ?assertMatch([{sse_done, {sent, _}}], [M || {sse_done, _} = M <- Rest])
     end)}},
   {"DELETE with a sessionId terminates the tool and answers 204",
    {timeout, 15, ?_test(begin
       {Id, Pid} = new_session(),
       {Tag, R} = call(test_support:request("DELETE", "/sse?sessionId=" ++ Id, [{"Host", "h"}])),
       ?assertEqual(ready2send, Tag),
       ?assertEqual(204, R#response.code),
       ?assertEqual(<<>>, R#response.body),
       ?assert(test_support:wait_until(fun() -> not is_process_alive(Pid) end, 2000))
     end)}}].
