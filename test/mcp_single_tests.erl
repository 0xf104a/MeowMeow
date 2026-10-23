%%%-------------------------------------------------------------------
%%% @doc EUnit suite for mcp_single.erl (route-keyed single SSE session)
%%%
%%% One tool process per route: GET opens the stream and emits the
%%% endpoint event (the route itself), POST forwards JSON-RPC to the
%%% tool (the first initialize is remembered), DELETE stops the tool.
%%% @end
%%%-------------------------------------------------------------------
-module(mcp_single_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/request.hrl").
-include("../src/response.hrl").

-define(ROUTE_TABLE, mcp_route_sessions).

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

call(Req) -> mcp_single:rule_mcp_sse_single_call(args(), test_support:response_for(Req)).

spawn_call(Req) ->
  Self = self(),
  Resp = test_support:response_for(Req, Self),
  spawn(fun() -> Self ! {sse_done, mcp_single:rule_mcp_sse_single_call(args(), Resp)} end).

mcp_single_test_() ->
  {setup, fun setup/0, fun cleanup/1,
   fun(_) -> [dispatch_tests(), flow_tests()] end}.

dispatch_tests() ->
  [{"an unsupported method is a 405",
    ?_assertEqual({aborted, 405}, call(test_support:request("PUT", "/mcp", [{"Host", "h"}])))},
   {"DELETE of a route without a session is a 404",
    ?_test(begin
             ok = mcp_single:init_table(),
             ?assertEqual({aborted, 404}, call(test_support:request("DELETE", "/no-session", [{"Host", "h"}])))
           end)}].

flow_tests() ->
  Init = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}",
  Ping = "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"ping\"}",
  [{"GET opens the stream, POSTs are relayed as 'message' events, DELETE closes everything",
    {timeout, 20, ?_test(begin
       test_support:flush(),
       Stream = spawn_call(test_support:request("GET", "/mcp", [{"Host", "h"}])),
       Open = test_support:collect(500),
       ?assertMatch([cancel_tmr, {send, _}, {send, _}], Open),
       [{send, Headers}, {send, Endpoint}] = [M || {send, _} = M <- Open],
       ?assertMatch({200, #{"Content-Type" := "text/event-stream"}, ""}, test_support:parse_response(Headers)),
       ?assertEqual(<<"event: endpoint\ndata: /mcp\n\n">>, iolist_to_binary(Endpoint)),
       %% first initialize goes to the tool and is remembered
       {ready2send, R1} = call(json_post("/mcp", Init)),
       ?assertEqual(202, R1#response.code),
       ?assertEqual("0", maps:get("Content-Length", R1#response.headers)),
       ?assertEqual({ok, {send, iolist_to_binary(["event: message\ndata: ", Init, "\n\n"])}},
                    test_support:recv(2000)),
       [{<<"/mcp">>, Session}] = ets:lookup(?ROUTE_TABLE, <<"/mcp">>),
       ?assertEqual(true, maps:get(initialized, Session)),
       ?assertEqual(list_to_binary(Init), iolist_to_binary(maps:get(init_response, Session))),
       %% a normal request is relayed too
       {ready2send, R2} = call(json_post("/mcp", Ping)),
       ?assertEqual(202, R2#response.code),
       ?assertEqual({ok, {send, iolist_to_binary(["event: message\ndata: ", Ping, "\n\n"])}},
                    test_support:recv(2000)),
       %% DELETE stops the tool, the stream closes and the session is forgotten
       {ready2send, R3} = call(test_support:request("DELETE", "/mcp", [{"Host", "h"}])),
       ?assertEqual(204, R3#response.code),
       Rest = test_support:collect(1000),
       ?assert(lists:member(close, Rest)),
       ?assertMatch([{sse_done, {sent, _}}], [M || {sse_done, _} = M <- Rest]),
       ?assertNot(is_process_alive(Stream)),
       ?assertEqual([], ets:lookup(?ROUTE_TABLE, <<"/mcp">>))
     end)}},
   {"POST before any GET starts the tool on demand and answers 202",
    {timeout, 15, ?_test(begin
       {ready2send, R} = call(json_post("/lazy", Init)),
       ?assertEqual(202, R#response.code),
       [{<<"/lazy">>, Session}] = ets:lookup(?ROUTE_TABLE, <<"/lazy">>),
       ?assert(is_process_alive(maps:get(pid, Session))),
       ok = mcp_server:terminate_session(maps:get(pid, Session))
     end)}},
   {"a second initialize does not replace the remembered first one",
    {timeout, 15, ?_test(begin
       Init2 = "{\"jsonrpc\":\"2.0\",\"id\":9,\"method\":\"initialize\",\"params\":{\"x\":1}}",
       {ready2send, _} = call(json_post("/twice", Init)),
       {ready2send, R} = call(json_post("/twice", Init2)),
       ?assertEqual(202, R#response.code),
       [{<<"/twice">>, Session}] = ets:lookup(?ROUTE_TABLE, <<"/twice">>),
       ?assertEqual(list_to_binary(Init), iolist_to_binary(maps:get(init_response, Session))),
       ok = mcp_server:terminate_session(maps:get(pid, Session))
     end)}},
   {"a dead tool is restarted on the next request for the route",
    {timeout, 15, ?_test(begin
       {ready2send, _} = call(json_post("/restart", Init)),
       [{<<"/restart">>, S1}] = ets:lookup(?ROUTE_TABLE, <<"/restart">>),
       ok = mcp_server:terminate_session(maps:get(pid, S1)),
       {ready2send, _} = call(json_post("/restart", Ping)),
       [{<<"/restart">>, S2}] = ets:lookup(?ROUTE_TABLE, <<"/restart">>),
       ?assertNotEqual(maps:get(pid, S1), maps:get(pid, S2)),
       ?assert(is_process_alive(maps:get(pid, S2))),
       ok = mcp_server:terminate_session(maps:get(pid, S2))
     end)}},
   {"EXPECTATION: the route->session table outlives the connection handler that created it, "
    "otherwise sessions are lost (and their tool processes leaked) as soon as that connection "
    "closes. FAILS BECAUSE: init_table/0 creates the named ETS table in whichever request "
    "handler first runs the rule, and ETS tables are deleted when their owner process exits",
    {timeout, 15, ?_test(begin
       %% earlier tests created the table from this very process; drop it
       %% so that the handler below is the creator
       catch ets:delete(?ROUTE_TABLE),
       ?assertEqual(undefined, ets:info(?ROUTE_TABLE)),
       Self = self(),
       Handler = spawn(fun() ->
                         Self ! {first, call(json_post("/owner", Init))},
                         receive stop -> ok end
                       end),
       {ok, {first, {ready2send, _}}} = test_support:recv(5000),
       Handler ! stop,
       ?assert(test_support:wait_until(fun() -> not is_process_alive(Handler) end, 1000)),
       ?assertNotEqual(undefined, ets:info(?ROUTE_TABLE))
     end)}}].
