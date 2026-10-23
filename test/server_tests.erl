%%%-------------------------------------------------------------------
%%% @doc EUnit suite for server.erl
%%%
%%% listen/1 is exercised on an ephemeral loopback port with a real
%%% gen_tcp client, which drives the whole stack (accept -> nya_tcp ->
%%% handle -> rules -> response). run/1, start/0 and stop/0 read the
%%% compiled-in /etc/nya/* paths or stop the VM and are not covered;
%%% abort_init/1 calls init:stop/1 and is covered indirectly by the
%%% stub in access_tests.
%%% @end
%%%-------------------------------------------------------------------
-module(server_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  test_support:setup_all(#{"LogLevel" => "0", "KeepAlive" => "1000",
                           "ListenHost" => "127.0.0.1"}),
  {ok, route} = nya_loader:nya_load("route"),
  ok.

server_test_() ->
  {setup, fun setup/0, fun test_support:teardown_all/1,
   fun(_) -> [listen_tests(), connection_tests()] end}.

connect(Port) ->
  {ok, C} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}]),
  C.

http(C, Raw) ->
  ok = gen_tcp:send(C, Raw),
  test_support:parse_response(test_support:recv_all(C, 300)).

%% Pick a port that is free right now; run_synchronized/1 needs an explicit port.
free_port() ->
  {ok, L} = gen_tcp:listen(0, [{reuseaddr, true}]),
  {ok, Port} = inet:port(L),
  gen_tcp:close(L),
  Port.

%% run_synchronized/1 never returns (it runs the accept loop), so it gets its own process.
start_server() ->
  Port = free_port(),
  spawn(fun() -> server:run_synchronized(Port) end),
  true = test_support:wait_until(
           fun() ->
             case gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}]) of
               {ok, C} -> gen_tcp:close(C), true;
               _ -> false
             end
           end, 3000),
  Port.

listen_tests() ->
  [{"listen/1 (used by run/1) binds the port and returns the bound address. "
    "(regression: this used to fail because socket:bind/2 returns 'ok' since OTP 24 but listen/1 only matches "
    "{ok, _} (listen_synchronized/1 handles both), so it crashes with case_clause)",
    ?_assertMatch({ok, #{port := P}} when P > 0, server:listen(0))},
   {"listen_synchronized/1 reports a bind failure instead of crashing",
    ?_test(begin
             test_support:set_config("ListenHost", "192.0.2.1"), %% TEST-NET-1, never local
             try
               ?assertMatch({error, _}, server:run_synchronized(free_port()))
             after
               test_support:set_config("ListenHost", "127.0.0.1")
             end
           end)},
   {"a client connecting to the listening port is served through the full stack",
    {timeout, 10, ?_test(begin
       Port = start_server(),
       test_support:set_access([{"Abort", ["418"]}]),
       C = connect(Port),
       ?assertMatch({418, _, _}, http(C, "GET / HTTP/1.1\r\nHost: h\r\n\r\n")),
       gen_tcp:close(C)
     end)}},
   {"the connection is closed after a request without keep-alive",
    {timeout, 10, ?_test(begin
       Port = start_server(),
       test_support:set_access([]),
       C = connect(Port),
       {404, H, _} = http(C, "GET / HTTP/1.1\r\nHost: h\r\n\r\n"),
       ?assertEqual("close", maps:get("Connection", H)),
       %% recv_all/2 already observed the close; a further recv reports closed/enotconn
       ?assertMatch({error, E} when E == closed orelse E == enotconn, gen_tcp:recv(C, 0, 1000))
     end)}},
   {"a keep-alive connection serves several requests, each evaluated against the current rules",
    {timeout, 10, ?_test(begin
       Port = start_server(),
       test_support:set_access([]),
       C = connect(Port),
       KA = "GET / HTTP/1.1\r\nHost: h\r\nConnection: keep-alive\r\n\r\n",
       {404, H1, _} = http(C, KA),
       ?assertEqual("keep-alive", maps:get("Connection", H1)),
       test_support:set_access([{"Abort", ["418"]}]),
       ?assertMatch({418, _, _}, http(C, KA)),
       gen_tcp:close(C)
     end)}},
   {"an idle keep-alive connection is closed by the server after KeepAlive ms",
    {timeout, 10, ?_test(begin
       Port = start_server(),
       test_support:set_access([]),
       C = connect(Port),
       {404, _, _} = http(C, "GET / HTTP/1.1\r\nHost: h\r\nConnection: keep-alive\r\n\r\n"),
       ?assertEqual({error, closed}, gen_tcp:recv(C, 0, 3000))
     end)}},
   {"Route blocks select rules per path over a real socket",
    {timeout, 10, ?_test(begin
       Port = start_server(),
       test_support:set_access([{context, "Route", ["/tea"], [{"Abort", ["418"]}]}]),
       C1 = connect(Port),
       ?assertMatch({418, _, _}, http(C1, "GET /tea HTTP/1.1\r\nHost: h\r\n\r\n")),
       C2 = connect(Port),
       ?assertMatch({404, _, _}, http(C2, "GET /coffee HTTP/1.1\r\nHost: h\r\n\r\n"))
     end)}},
   {"several clients are served concurrently",
    {timeout, 10, ?_test(begin
       Port = start_server(),
       test_support:set_access([{"Abort", ["418"]}]),
       Self = self(),
       [spawn(fun() -> C = connect(Port),
                       Self ! {res, http(C, "GET / HTTP/1.1\r\nHost: h\r\n\r\n")} end)
        || _ <- lists:seq(1, 5)],
       Results = [receive {res, R} -> R after 3000 -> timeout end || _ <- lists:seq(1, 5)],
       ?assertEqual([418, 418, 418, 418, 418], [element(1, R) || R <- Results])
     end)}}].

connection_tests() ->
  [{"handle_connection/1 spawns the handler pair for a connected socket and serves it",
    {timeout, 10, ?_test(begin
       test_support:set_access([{"Abort", ["418"]}]),
       {S, C} = test_support:socket_pair(),
       ?assert(is_pid(server:handle_connection(S))),
       ?assertMatch({418, _, _}, http(C, "GET / HTTP/1.1\r\nHost: h\r\n\r\n"))
     end)}},
   {"handle_connection/1 on an already closed socket logs and returns instead of crashing",
    ?_test(begin
             {S, _C} = test_support:socket_pair(),
             socket:close(S),
             ?assertEqual(ok, server:handle_connection(S))
           end)}].
