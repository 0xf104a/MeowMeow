%%%-------------------------------------------------------------------
%%% @doc EUnit suite for sock.erl (legacy gen_tcp helpers)
%%%
%%% sock.erl is not used by the current request path (nya_tcp.erl is),
%%% but it is still exported, so its contract is pinned down here.
%%% @end
%%%-------------------------------------------------------------------
-module(sock_tests).
-include_lib("eunit/include/eunit.hrl").

split_list_test_() ->
  [{"split_list/2 chunks a list into pieces of at most Max elements; both the chunk list "
    "and each chunk come out reversed (socket_send_packets reverses them back)",
    ?_assertEqual([[5], [4, 3], [2, 1]], sock:split_list([1, 2, 3, 4, 5], 2))},
   {"split_list/2 of an exact multiple has no short chunk",
    ?_assertEqual([[4, 3], [2, 1]], sock:split_list([1, 2, 3, 4], 2))},
   {"split_list/2 of [] is []",
    ?_assertEqual([], sock:split_list([], 2))}].

socket_test_() ->
  [{"socket_send/3 delivers the whole packet in order when chunked",
    ?_test(begin
             {S, C} = test_support:gen_tcp_pair(),
             ?assertEqual(ok, sock:socket_send(S, "hello world!", 4)),
             ?assertEqual(<<"hello world!">>, test_support:recv_all(C, 200))
           end)},
   {"create_socket/1 returns a listening socket (port 0 = ephemeral)",
    ?_test(begin
             L = sock:create_socket(0),
             ?assert(is_port(L)),
             ?assertMatch({ok, P} when P > 0, inet:port(L)),
             gen_tcp:close(L)
           end)},
   {"socket_accept/1 returns the accepted connection",
    ?_test(begin
             L = sock:create_socket(0),
             {ok, Port} = inet:port(L),
             spawn(fun() -> gen_tcp:connect({127, 0, 0, 1}, Port, [binary]) end),
             S = sock:socket_accept(L),
             ?assert(is_port(S)),
             gen_tcp:close(L)
           end)},
   {"socket_recv/2 returns the received data",
    ?_test(begin
             {S, C} = test_support:gen_tcp_pair(),
             ok = gen_tcp:send(C, "x"),
             ?assertEqual({ok, <<"x">>}, sock:socket_recv(S, 1000))
           end)},
   {"socket_recv/2 reports a timeout",
    ?_test(begin
             {S, _C} = test_support:gen_tcp_pair(),
             ?assertEqual({error, timeout}, sock:socket_recv(S, 50))
           end)},
   {"socket_recv_all/2 collects everything until the peer goes quiet, as a string",
    ?_test(begin
             {S, C} = test_support:gen_tcp_pair(),
             ok = gen_tcp:send(C, "ab"),
             ok = gen_tcp:send(C, "cd"),
             timer:sleep(20),
             ?assertEqual("abcd", sock:socket_recv_all(S, ""))
           end)},
   {"socket_shutdown/2 half-closes the socket so the peer reads EOF. "
    "(regression: this used to fail because it calls gen_tcp:send(Sock, How) - trying to send the atom 'write' - "
    "instead of gen_tcp:shutdown/2)",
    ?_test(begin
             {S, C} = test_support:gen_tcp_pair(),
             ?assertEqual(ok, sock:socket_shutdown(S, write)),
             ?assertEqual({error, closed}, gen_tcp:recv(C, 0, 500))
           end)}].
