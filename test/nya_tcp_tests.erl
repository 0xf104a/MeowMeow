%%%-------------------------------------------------------------------
%%% @doc EUnit suite for nya_tcp.erl (TCP flow-control process)
%%%
%%% Uses a loopback socket pair: the server end is handed to the code
%%% under test, the client end is read/written by the test. The test
%%% process plays the request handler ("Handler") and receives the
%%% {data, _} / closed / timeout notifications. KeepAlive is set to
%%% 300 ms so timer behaviour can be observed quickly.
%%% @end
%%%-------------------------------------------------------------------
-module(nya_tcp_tests).
-include_lib("eunit/include/eunit.hrl").

-define(KEEPALIVE_MS, 300).

setup() ->
  test_support:setup_config(#{"LogLevel" => "0",
                              "KeepAlive" => integer_to_list(?KEEPALIVE_MS)}).

nya_tcp_test_() ->
  {setup, fun setup/0, fun test_support:teardown_all/1,
   fun(_) -> [send_tests(), recv_tests(), ctl_tests(), timer_tests()] end}.

start_ctl() ->
  test_support:flush(),
  {S, C} = test_support:socket_pair(),
  Self = self(),
  Pid = spawn(fun() -> nya_tcp:nya_tcp_ctl_start(S, Self) end),
  Ref = monitor(process, Pid),
  {Pid, Ref, S, C}.

down(Ref, Timeout) ->
  receive {'DOWN', Ref, process, _, Reason} -> {down, Reason}
  after Timeout -> alive
  end.

%% Stop a control process deterministically so that it does not die of its
%% own keep-alive timer in the middle of a later test.
stop_ctl(Pid, Ref) ->
  Pid ! close,
  down(Ref, 1000).

%%--------------------------------------------------------------------
send_tests() ->
  [{"tcp_send/2 sends a binary",
    ?_test(begin
             {S, C} = test_support:socket_pair(),
             ?assertEqual(ok, nya_tcp:tcp_send(S, <<"abc">>)),
             ?assertEqual({ok, <<"abc">>}, gen_tcp:recv(C, 0, 1000))
           end)},
   {"tcp_send/2 sends a string",
    ?_test(begin
             {S, C} = test_support:socket_pair(),
             ?assertEqual(ok, nya_tcp:tcp_send(S, "abc")),
             ?assertEqual({ok, <<"abc">>}, gen_tcp:recv(C, 0, 1000))
           end)},
   {"tcp_send/2 sends an iolist (response:response/3 yields nested lists and binaries)",
    ?_test(begin
             {S, C} = test_support:socket_pair(),
             ?assertEqual(ok, nya_tcp:tcp_send(S, ["a", <<"b">>, [$c]])),
             ?assertEqual({ok, <<"abc">>}, gen_tcp:recv(C, 0, 1000))
           end)},
   {"tcp_send/2 reports an error on a closed socket instead of crashing",
    ?_test(begin
             {S, _C} = test_support:socket_pair(),
             socket:close(S),
             ?assertMatch({error, _}, nya_tcp:tcp_send(S, <<"abc">>))
           end)}].

%%--------------------------------------------------------------------
recv_tests() ->
  [{"tcp_recv/2 reads exactly the requested number of bytes",
    ?_test(begin
             {S, C} = test_support:socket_pair(),
             ok = gen_tcp:send(C, "abcdef"),
             ?assertEqual({ok, <<"abc">>}, nya_tcp:tcp_recv(S, 3)),
             ?assertEqual({ok, <<"def">>}, nya_tcp:tcp_recv(S, 3))
           end)},
   {"tcp_recv/2 with size 0 returns an empty binary immediately",
    ?_assertEqual({ok, <<>>}, nya_tcp:tcp_recv(fake, 0))},
   {"tcp_recv/2 reassembles a payload larger than the 64 KiB chunk size",
    {timeout, 20, ?_test(begin
       {S, C} = test_support:socket_pair(),
       Payload = crypto:strong_rand_bytes(100000),
       spawn(fun() -> gen_tcp:send(C, Payload) end),
       {ok, Got} = nya_tcp:tcp_recv(S, 100000),
       ?assertEqual(100000, byte_size(Got)),
       ?assertEqual(Payload, Got)
     end)}},
   {"tcp_recv/2 reports an error when the peer closes early",
    ?_test(begin
             {S, C} = test_support:socket_pair(),
             ok = gen_tcp:send(C, "ab"),
             gen_tcp:close(C),
             ?assertMatch({error, _}, nya_tcp:tcp_recv(S, 10))
           end)}].

%%--------------------------------------------------------------------
ctl_tests() ->
  [{"the control process announces itself to the handler as upstream",
    ?_test(begin
             {Pid, Ref, _, _} = start_ctl(),
             ?assertEqual({ok, {upstream, Pid}}, test_support:recv(1000)),
             stop_ctl(Pid, Ref)
           end)},
   {"'recv' forwards whatever the client sends as {data, Binary}",
    ?_test(begin
             {Pid, Ref, _, C} = start_ctl(),
             {ok, {upstream, Pid}} = test_support:recv(1000),
             Pid ! cancel_tmr,
             ok = gen_tcp:send(C, "GET / HTTP/1.1\r\n\r\n"),
             Pid ! recv,
             ?assertEqual({ok, {data, <<"GET / HTTP/1.1\r\n\r\n">>}}, test_support:recv(1000)),
             stop_ctl(Pid, Ref)
           end)},
   {"{recv, N} reads exactly N bytes",
    ?_test(begin
             {Pid, Ref, _, C} = start_ctl(),
             {ok, {upstream, Pid}} = test_support:recv(1000),
             Pid ! cancel_tmr,
             ok = gen_tcp:send(C, "abcdef"),
             Pid ! {recv, 3},
             ?assertEqual({ok, {data, <<"abc">>}}, test_support:recv(1000)),
             stop_ctl(Pid, Ref)
           end)},
   {"{send, Binary} writes to the client",
    ?_test(begin
             {Pid, Ref, _, C} = start_ctl(),
             {ok, {upstream, Pid}} = test_support:recv(1000),
             Pid ! cancel_tmr,
             Pid ! {send, <<"hello">>},
             ?assertEqual({ok, <<"hello">>}, gen_tcp:recv(C, 0, 1000)),
             stop_ctl(Pid, Ref)
           end)},
   {"{send, IoList} writes to the client",
    ?_test(begin
             {Pid, Ref, _, C} = start_ctl(),
             {ok, {upstream, Pid}} = test_support:recv(1000),
             Pid ! cancel_tmr,
             Pid ! {send, ["hel", <<"lo">>]},
             ?assertEqual({ok, <<"hello">>}, gen_tcp:recv(C, 0, 1000)),
             stop_ctl(Pid, Ref)
           end)},
   {"{send, From, Data} writes to the client and acknowledges with 'sent'",
    ?_test(begin
             {Pid, Ref, _, C} = start_ctl(),
             {ok, {upstream, Pid}} = test_support:recv(1000),
             Pid ! cancel_tmr,
             Pid ! {send, self(), <<"ack">>},
             ?assertEqual({ok, sent}, test_support:recv(1000)),
             ?assertEqual({ok, <<"ack">>}, gen_tcp:recv(C, 0, 1000)),
             Pid ! {send, self(), "list"},
             ?assertEqual({ok, sent}, test_support:recv(1000)),
             ?assertEqual({ok, <<"list">>}, gen_tcp:recv(C, 0, 1000)),
             stop_ctl(Pid, Ref)
           end)},
   {"'close' closes the socket and the control process exits with 'requested'",
    ?_test(begin
             {Pid, Ref, _, C} = start_ctl(),
             {ok, {upstream, Pid}} = test_support:recv(1000),
             Pid ! close,
             ?assertEqual({down, requested}, down(Ref, 1000)),
             ?assertEqual({error, closed}, gen_tcp:recv(C, 0, 1000))
           end)},
   {"the handler is told 'closed' when the client hangs up during 'recv'",
    ?_test(begin
             {Pid, Ref, _, C} = start_ctl(),
             {ok, {upstream, Pid}} = test_support:recv(1000),
             Pid ! recv,
             gen_tcp:close(C),
             ?assertEqual({ok, closed}, test_support:recv(1000)),
             ?assertEqual({down, closed}, down(Ref, 1000))
           end)},
   {"an unknown command is logged and the control process keeps serving the "
    "connection (it logs 'Recieved unknown cmd' as a warning, not a fatal error). "
    "(regression: this used to fail because the catch-all receive clause does not recurse into nya_tcp_ctl/3, so the "
    "process simply returns and the connection is left without its I/O process)",
    ?_test(begin
             {Pid, Ref, _, C} = start_ctl(),
             {ok, {upstream, Pid}} = test_support:recv(1000),
             Pid ! cancel_tmr,
             Pid ! bogus_command,
             ?assertEqual(alive, down(Ref, 200)),
             Pid ! {send, <<"still here">>},
             ?assertEqual({ok, <<"still here">>}, gen_tcp:recv(C, 0, 1000)),
             stop_ctl(Pid, Ref)
           end)}].

%%--------------------------------------------------------------------
timer_tests() ->
  [{"a 'recv' that waits longer than KeepAlive tells the handler 'timeout' and closes",
    {timeout, 10, ?_test(begin
       {Pid, Ref, _, C} = start_ctl(),
       {ok, {upstream, Pid}} = test_support:recv(1000),
       Pid ! recv,
       ?assertEqual({ok, timeout}, test_support:recv(?KEEPALIVE_MS * 4)),
       ?assertEqual({down, normal}, down(Ref, 1000)),
       ?assertEqual({error, closed}, gen_tcp:recv(C, 0, 1000))
     end)}},
   {"the idle timer started at connect time closes a silent connection",
    {timeout, 10, ?_test(begin
       {Pid, Ref, _, C} = start_ctl(),
       {ok, {upstream, Pid}} = test_support:recv(1000),
       ?assertEqual({down, timeout}, down(Ref, ?KEEPALIVE_MS * 4)),
       ?assertEqual({error, closed}, gen_tcp:recv(C, 0, 1000))
     end)}},
   {"when the idle timer fires the handler is told 'timeout' (as the 'recv' "
    "timeout branch does) so it can exit instead of waiting forever. "
    "(regression: this used to fail because the 'timeout' receive clause closes the socket and exits without "
    "notifying the handler, leaking one blocked handler process per idle connection)",
    {timeout, 10, ?_test(begin
       {Pid, Ref, _, _} = start_ctl(),
       {ok, {upstream, Pid}} = test_support:recv(1000),
       {down, timeout} = down(Ref, ?KEEPALIVE_MS * 4),
       ?assertEqual({ok, timeout}, test_support:recv(500))
     end)}},
   {"'cancel_tmr' disables the idle timer (used while a module streams a file), 'set_tmr' re-arms it",
    {timeout, 10, ?_test(begin
       {Pid, Ref, _, _} = start_ctl(),
       {ok, {upstream, Pid}} = test_support:recv(1000),
       Pid ! cancel_tmr,
       ?assertEqual(alive, down(Ref, ?KEEPALIVE_MS * 3)),
       Pid ! set_tmr,
       ?assertEqual({down, timeout}, down(Ref, ?KEEPALIVE_MS * 4))
     end)}},
   {"a 'recv' issued while the idle timer is cancelled keeps waiting for the client "
    "and delivers data that arrives later (the process stays alive, so the pending recv must "
    "not be lost). (regression: this used to fail because when socket:recv/3 times out with the timer not set, the "
    "control process goes back to its command loop without retrying the recv, so the handler's "
    "request is silently dropped and late data is never delivered)",
    {timeout, 10, ?_test(begin
       {Pid, Ref, _, C} = start_ctl(),
       {ok, {upstream, Pid}} = test_support:recv(1000),
       Pid ! cancel_tmr,
       Pid ! recv,
       ?assertEqual(alive, down(Ref, ?KEEPALIVE_MS * 3)),
       ok = gen_tcp:send(C, "late"),
       try
         ?assertEqual({ok, {data, <<"late">>}}, test_support:recv(1000))
       after
         stop_ctl(Pid, Ref)
       end
     end)}}].
