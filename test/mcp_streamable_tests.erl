%%%-------------------------------------------------------------------
%%% @doc EUnit suite for mcp_streamable.erl (Streamable HTTP, 2025-11-25)
%%%
%%% Spec: https://modelcontextprotocol.io/specification/2025-11-25/basic/transports
%%% `cat' plays the tool: a POSTed JSON-RPC request is echoed back, so
%%% the echoed line is the "response" with the same id.
%%%
%%% The rule blocks in receive while waiting for the tool (wait_for_sse/3),
%%% so the fake upstream is a separate collector process rather than the
%%% test process itself.
%%% @end
%%%-------------------------------------------------------------------
-module(mcp_streamable_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/request.hrl").
-include("../src/response.hrl").

-define(V, {"Mcp-Protocol-Version", "2025-11-25"}).

setup() ->
  test_support:setup_all(#{"LogLevel" => "0"}),
  test_support:setup_mcp(),
  ok.

cleanup(_) ->
  test_support:cleanup_mcp_sessions(),
  test_support:teardown_all(ok).

cat() -> test_support:tool("cat").
args() -> [cat(), "5000"].

json_post(Route, Headers, Body) ->
  test_support:request("POST", Route,
                       [{"Host", "h"}, {"Content-Type", "application/json"},
                        {"Content-Length", integer_to_list(length(Body))} | Headers], Body).

call(Req) ->
  Upstream = test_support:spawn_collector(),
  try
    mcp_streamable:rule_mcp_streamable_call(args(), test_support:response_for(Req, Upstream))
  after
    test_support:stop_collector(Upstream)
  end.

spawn_call(Req) ->
  Self = self(),
  Resp = test_support:response_for(Req, Self),
  spawn(fun() -> Self ! {sse_done, mcp_streamable:rule_mcp_streamable_call(args(), Resp)} end).

new_session() ->
  Id = mcp_port:start_mcp_session(cat(), 5000),
  {ok, Pid} = mcp_port:get_mcp_session_by_id(Id),
  {binary_to_list(Id), Pid}.

%% Like call/1 but starting from the response handle:handle/2 really hands
%% to the rule chain: a 404 with the error page (get_default_response/2),
%% not the 200 response_for/2 defaults to.
call_as_handler(Req) ->
  Upstream = test_support:spawn_collector(),
  try
    Resp = (test_support:response_for(Req, Upstream))#response{code = 404, body = <<"<html>404</html>">>},
    mcp_streamable:rule_mcp_streamable_call(args(), Resp)
  after
    test_support:stop_collector(Upstream)
  end.

%% Like call/1 but also returns what the rule sent to the upstream.
call_collecting(Req) ->
  Upstream = test_support:spawn_collector(),
  try
    Result = mcp_streamable:rule_mcp_streamable_call(args(), test_support:response_for(Req, Upstream)),
    {Result, test_support:collected(Upstream)}
  after
    test_support:stop_collector(Upstream)
  end.

mcp_streamable_test_() ->
  {setup, fun setup/0, fun cleanup/1,
   fun(_) -> [state_tests(), dispatch_tests(), flow_tests(), limit_tests()] end}.

%%--------------------------------------------------------------------
state_tests() ->
  [{"mcp_request_state/1 defaults: no Accept, no version header, no session, not initialize, no content type",
    ?_assertEqual({undefined, none, none, false, none},
                  mcp_streamable:mcp_request_state(test_support:request("GET", "/mcp", [{"Host", "h"}])))},
   {"mcp_request_state/1 reads the MCP headers and strips Content-Type parameters",
    ?_assertEqual({"application/json, text/event-stream", "2025-11-25", "SID", true, "application/json"},
                  mcp_streamable:mcp_request_state(
                    test_support:request("POST", "/mcp",
                                         [{"Host", "h"}, ?V, {"Mcp-Session-Id", "SID"},
                                          {"Content-Type", "application/json; charset=utf-8"},
                                          {"Content-Length", "23"},
                                          {"Accept", "application/json, text/event-stream"}],
                                         "{\"method\":\"initialize\"}")))}].

%%--------------------------------------------------------------------
dispatch_tests() ->
  Init = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}",
  [{"a protocol version older than 2025-11-25 is refused with 400 (spec 2025-11-25 and 2026-07-28: "
    "an invalid or unsupported MCP-Protocol-Version MUST be answered with 400 Bad Request). "
    "(regression: this used to answer 501 Not Implemented)",
    ?_assertEqual({aborted, 400},
                  call(json_post("/mcp", [{"Mcp-Protocol-Version", "2025-06-18"}, {"Mcp-Session-Id", "X"}], "{}")))},
   {"DELETE without a session is a 400 like GET. (regression: this used to crash in "
    "list_to_binary(none) and surface as a 502 from rules:execute_rule/3)",
    ?_assertEqual({aborted, 400}, call(test_support:request("DELETE", "/mcp", [{"Host", "h"}, ?V])))},
   {"a non-initialize POST without a session is a 400",
    ?_assertEqual({aborted, 400}, call(json_post("/mcp", [?V], "{\"method\":\"ping\"}")))},
   {"a POST with a non-JSON content type is a 415",
    ?_assertEqual({aborted, 415},
                  call(test_support:request("POST", "/mcp",
                                            [{"Host", "h"}, ?V, {"Mcp-Session-Id", "X"},
                                             {"Content-Type", "text/plain"}, {"Content-Length", "2"}], "{}")))},
   {"a POST to an unknown session is a 404",
    ?_assertEqual({aborted, 404}, call(json_post("/mcp", [?V, {"Mcp-Session-Id", "UNKNOWN"}], "{\"method\":\"ping\"}")))},
   {"GET without a session is a 400",
    ?_assertEqual({aborted, 400}, call(test_support:request("GET", "/mcp", [{"Host", "h"}, ?V])))},
   {"DELETE of an unknown session is a 404",
    ?_assertEqual({aborted, 404},
                  call(test_support:request("DELETE", "/mcp", [{"Host", "h"}, ?V, {"Mcp-Session-Id", "UNKNOWN"}])))},
   {"an unsupported method is a 405",
    ?_assertEqual({aborted, 405}, call(test_support:request("PUT", "/mcp", [{"Host", "h"}, ?V])))},
   {"an initialize request without Mcp-Protocol-Version is accepted (spec: the header "
    "is only required on requests *after* initialization; a server that gets none SHOULD assume "
    "2025-03-26 and negotiate the version in the initialize response). "
    "(regression: this used to fail because the first clause of rule_mcp_call/4 rejects every request whose version "
    "(defaulting to 2025-03-26) sorts before 2025-11-25 with 501, so no client can ever initialize "
    "without sending the header up front)",
    {timeout, 15, ?_assertMatch({ready2send, _}, call(json_post("/mcp", [], Init)))}},
   {"'Content-Type: application/json; charset=utf-8' is JSON (RFC 9110 section 8.3: "
    "media type parameters). (regression: this used to fail because the content type is compared verbatim with "
    "\"application/json\" so any parameter yields 415)",
    {timeout, 15, ?_test(begin
       {Id, Pid} = new_session(),
       Body = "{\"jsonrpc\":\"2.0\",\"method\":\"ping\"}",
       Req = test_support:request("POST", "/mcp",
                                  [{"Host", "h"}, ?V, {"Mcp-Session-Id", Id},
                                   {"Content-Type", "application/json; charset=utf-8"},
                                   {"Content-Length", integer_to_list(length(Body))}],
                                  Body),
       try
         ?assertNotEqual({aborted, 415}, call(Req))
       after
         catch mcp_server:terminate_session(Pid)
       end
     end)}},
   {"an initialize request without an id is a 422 (that is what the 'none' branch of "
    "handle_mcp_session_init/4 intends). (regression: this used to fail because maps:get(<<\"id\">>, Decoded) raises "
    "badkey before the branch can run)",
    {timeout, 15, ?_assertEqual({aborted, 422},
                                call(json_post("/mcp", [?V], "{\"jsonrpc\":\"2.0\",\"method\":\"initialize\"}")))}}].

%%--------------------------------------------------------------------
flow_tests() ->
  Init = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}",
  Ping = "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"ping\"}",
  Note = "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}",
  [{"initialize -> request -> notification -> DELETE round trip",
    {timeout, 30, ?_test(begin
       test_support:flush(),
       %% initialize starts the tool and returns its answer with a session id
       {Tag1, R1} = call(json_post("/mcp", [?V], Init)),
       ?assertEqual(ready2send, Tag1),
       ?assert(R1#response.code >= 200 andalso R1#response.code < 300),
       ?assertEqual("application/json", maps:get("Content-Type", R1#response.headers)),
       Sid = maps:get("MCP-Session-Id", R1#response.headers),
       ?assert(is_list(Sid) andalso Sid =/= ""),
       ?assertEqual(list_to_binary(Init), iolist_to_binary(R1#response.body)),
       ?assertMatch({ok, _}, mcp_port:get_mcp_session_by_id(list_to_binary(Sid))),
       %% a request with an id gets the matching answer
       {Tag2, R2} = call(json_post("/mcp", [?V, {"Mcp-Session-Id", Sid}], Ping)),
       ?assertEqual(ready2send, Tag2),
       ?assertEqual(list_to_binary(Ping), iolist_to_binary(R2#response.body)),
       ?assertEqual("application/json", maps:get("Content-Type", R2#response.headers)),
       %% a notification (no id) is accepted with 202 and no body
       {Tag3, R3} = call(json_post("/mcp", [?V, {"Mcp-Session-Id", Sid}], Note)),
       ?assertEqual(ready2send, Tag3),
       ?assertEqual(202, R3#response.code),
       ?assertEqual(<<>>, iolist_to_binary(R3#response.body)),
       %% DELETE terminates the session
       {ok, Pid} = mcp_port:get_mcp_session_by_id(list_to_binary(Sid)),
       {Tag4, R4} = call(test_support:request("DELETE", "/mcp", [{"Host", "h"}, ?V, {"Mcp-Session-Id", Sid}])),
       ?assertEqual(ready2send, Tag4),
       ?assertEqual(204, R4#response.code),
       ?assert(test_support:wait_until(fun() -> not is_process_alive(Pid) end, 2000))
     end)}},
   {"a request inside a session is answered with status 200 (the handler starts every response "
    "as a 404). (regression: this used to fail because wait_for_sse/3 kept the default code, so real "
    "clients got the tool's JSON with a 404 status; invisible with response_for/2, which starts at 200)",
    {timeout, 15, ?_test(begin
       {Id, Pid} = new_session(),
       try
         {Tag, R} = call_as_handler(json_post("/mcp", [?V, {"Mcp-Session-Id", Id}], Ping)),
         ?assertEqual(ready2send, Tag),
         ?assertEqual(200, R#response.code),
         ?assertEqual(list_to_binary(Ping), iolist_to_binary(R#response.body))
       after
         catch mcp_server:terminate_session(Pid)
       end
     end)}},
   {"a request on a terminated session is a 404 (spec: after termination the server MUST answer "
    "404 for that id). (regression: this used to fail because the mcp_sessions entry outlived the "
    "process, so connect_to_mcp_session/1 died with noproc and the client got a 502)",
    {timeout, 15, ?_test(begin
       {Id, Pid} = new_session(),
       ok = mcp_server:terminate_session(Pid),
       ?assert(test_support:wait_until(fun() -> not is_process_alive(Pid) end, 2000)),
       ?assertEqual({aborted, 404}, call(json_post("/mcp", [?V, {"Mcp-Session-Id", Id}], Ping)))
     end)}},
   {"DELETE forgets the id at once: the next request with it is a 404, not a 502",
    {timeout, 15, ?_test(begin
       {Id, _Pid} = new_session(),
       {Tag, R} = call(test_support:request("DELETE", "/mcp", [{"Host", "h"}, ?V, {"Mcp-Session-Id", Id}])),
       ?assertEqual({ready2send, 204}, {Tag, R#response.code}),
       ?assertEqual(none, mcp_port:get_mcp_session_by_id(list_to_binary(Id))),
       ?assertEqual({aborted, 404}, call(json_post("/mcp", [?V, {"Mcp-Session-Id", Id}], Ping)))
     end)}},
   {"initialize disables the connection's keep-alive timer while the tool starts and answers "
    "(nya_tcp runs that timer during request handling too, and a tool may need longer than "
    "KeepAlive to start). (regression: this used to fail because nothing sent cancel_tmr, so "
    "with KeepAlive 100 a slow tool meant the connection was killed before the 201 went out)",
    {timeout, 15, ?_test(begin
       {{Tag, R}, Sent} = call_collecting(json_post("/mcp", [?V], Init)),
       ?assertEqual(ready2send, Tag),
       ?assert(lists:member(cancel_tmr, Sent)),
       {ok, Pid} = mcp_port:get_mcp_session_by_id(list_to_binary(maps:get("MCP-Session-Id", R#response.headers))),
       catch mcp_server:terminate_session(Pid)
     end)}},
   {"DELETE disables the keep-alive timer before terminating the session (mcp_server gives the "
    "tool a 300 ms SIGTERM grace). (regression: this used to fail because nothing sent cancel_tmr, so with "
    "KeepAlive 100 nya_tcp always killed the connection before the 204 went out)",
    {timeout, 15, ?_test(begin
       {Id, Pid} = new_session(),
       {{Tag, R}, Sent} = call_collecting(test_support:request("DELETE", "/mcp", [{"Host", "h"}, ?V, {"Mcp-Session-Id", Id}])),
       ?assertEqual(ready2send, Tag),
       ?assertEqual(204, R#response.code),
       ?assert(lists:member(cancel_tmr, Sent)),
       ?assert(test_support:wait_until(fun() -> not is_process_alive(Pid) end, 2000))
     end)}},
   {"GET with a session and 'Accept: application/json, text/event-stream' opens an "
    "event stream that is closed when the session terminates. "
    "(regression: this used to fail because handle_mcp_stream/2 passes the session id string instead of the session pid "
    "to mcp_server:connect_to_mcp_session/1 (gen_server:call on a string), so the stream process "
    "dies right after sending the headers; mcp_stream_loop/2 would also never relay data because "
    "it waits for {data, _} while mcp_server broadcasts bare binaries)",
    {timeout, 15, ?_test(begin
       test_support:flush(),
       {Id, Pid} = new_session(),
       spawn_call(test_support:request("GET", "/mcp",
                                       [{"Host", "h"}, ?V, {"Mcp-Session-Id", Id},
                                        {"Accept", "application/json, text/event-stream"}])),
       Open = test_support:collect(500),
       ?assertMatch([cancel_tmr, {send, _}], Open),
       [{send, Headers}] = [M || {send, _} = M <- Open],
       ?assertMatch({200, #{"Content-Type" := "text/event-stream"}, ""}, test_support:parse_response(Headers)),
       ok = mcp_server:terminate_session(Pid),
       Rest = test_support:collect(500),
       ?assert(lists:member(close, Rest)),
       ?assertMatch([{sse_done, {sent, _}}], [M || {sse_done, _} = M <- Rest])
     end)}},
   {"GET whose Accept header excludes the stream types is a 406 (spec: the client "
    "MUST include Accept: text/event-stream). (regression: this used to fail because parse_http:acceptable_for_request/2 "
    "never finds the Accept header (it looks up \"Accept\" while headers are stored lower-cased), "
    "so the request falls through to handle_mcp_stream/2 and crashes there)",
    {timeout, 15, ?_test(begin
       test_support:flush(),
       {Id, Pid} = new_session(),
       spawn_call(test_support:request("GET", "/mcp",
                                       [{"Host", "h"}, ?V, {"Mcp-Session-Id", Id}, {"Accept", "text/plain"}])),
       try
         ?assertEqual({ok, {sse_done, {aborted, 406}}}, test_support:recv(2000))
       after
         catch mcp_server:terminate_session(Pid)
       end
     end)}}].

%%--------------------------------------------------------------------
%% Session caps (issue #22): no slot -> 503 with Retry-After, misconfigured rule -> 500
%%--------------------------------------------------------------------
call(Args, Req) ->
  Upstream = test_support:spawn_collector(),
  try
    mcp_streamable:rule_mcp_streamable_call(Args, test_support:response_for(Req, Upstream))
  after
    test_support:stop_collector(Upstream)
  end.

init_request() ->
  json_post("/mcp", [?V, {"Accept", "application/json, text/event-stream"}],
            "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}").

capped(Fun) ->
  test_support:cleanup_mcp_sessions(),   %% earlier tests leave sessions running
  test_support:set_config("MCPMaxSessions", "1"),
  test_support:set_config("MCPSessionWaitMs", "2000"),
  {_, Pid} = new_session(),
  try Fun()
  after
    catch mcp_server:terminate_session(Pid),
    test_support:set_config("MCPMaxSessions", "0"),
    test_support:set_config("MCPSessionWaitMs", "5000"),
    catch mcp_admission:stop()
  end.

limit_tests() ->
  [{"an initialize request answers 503 with Retry-After when MCPMaxSessions leaves no slot",
    {timeout, 15, ?_test(capped(fun() ->
       {ready2send, R} = call(init_request()),
       ?assertEqual(503, R#response.code),
       ?assertEqual("2", maps:get("Retry-After", R#response.headers)),
       ?assertNotEqual(nomatch, binary:match(R#response.body, <<"503">>)),
       ?assertEqual(1, maps:get(running, mcp_admission:status()))
     end))}},
   {"MaxSessions=1 on the rule answers 503 for that command even without a global cap",
    {timeout, 15, ?_test(begin
       test_support:cleanup_mcp_sessions(),
       test_support:set_config("MCPSessionWaitMs", "0"),
       {_, Pid} = new_session(),
       try
         {ready2send, R} = call([cat(), "5000", "MaxSessions=1"], init_request()),
         ?assertEqual(503, R#response.code),
         ?assertEqual("1", maps:get("Retry-After", R#response.headers))
       after
         catch mcp_server:terminate_session(Pid),
         test_support:set_config("MCPSessionWaitMs", "5000"),
         catch mcp_admission:stop()
       end
     end)}},
   {"a rule with a bad keep-alive or an unknown option answers 500",
    ?_test(begin
             ?assertEqual({aborted, 500}, call([cat(), "later"], init_request())),
             ?assertEqual({aborted, 500}, call([cat(), "5000", "Colour=red"], init_request()))
           end)}].
