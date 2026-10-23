%%%-------------------------------------------------------------------
%%% @doc EUnit suite for mcp_server.erl (gen_server around a stdio tool)
%%%
%%% `cat' is the fake MCP tool: every line written to it comes back,
%%% which makes request/response pairing observable. `printf' is used
%%% for a tool that exits on its own.
%%% @end
%%%-------------------------------------------------------------------
-module(mcp_server_tests).
-include_lib("eunit/include/eunit.hrl").

setup() -> test_support:setup_config(#{"LogLevel" => "0"}).

cat() -> test_support:tool("cat").

start(Cmd, KeepAlive) ->
  test_support:flush(),
  {ok, Pid} = mcp_server:start([Cmd, KeepAlive]),
  Ref = monitor(process, Pid),
  {Pid, Ref}.

stop(Pid) -> catch mcp_server:terminate_session(Pid), ok.

down(Ref, Timeout) ->
  receive {'DOWN', Ref, process, _, Reason} -> {down, Reason}
  after Timeout -> alive
  end.

mcp_server_test_() ->
  {setup, fun setup/0, fun test_support:teardown_all/1,
   fun(_) -> [lifecycle_tests(), io_tests(), timer_tests(), robustness_tests()] end}.

lifecycle_tests() ->
  [{"start/1 starts the subprocess and returns {ok, Pid}",
    {timeout, 10, ?_test(begin
       {Pid, _} = start(cat(), 5000),
       ?assert(is_process_alive(Pid)),
       stop(Pid)
     end)}},
   {"init rejects a non-integer keep-alive",
    ?_assertEqual({error, badarg}, mcp_server:start([cat(), "5000"]))},
   {"terminate_session/1 stops the server and tells every receiver 'terminated'",
    {timeout, 10, ?_test(begin
       {Pid, Ref} = start(cat(), 5000),
       ok = mcp_server:connect_to_mcp_session(Pid),
       ?assertEqual(ok, mcp_server:terminate_session(Pid)),
       ?assertEqual({ok, terminated}, test_support:recv(1000)),
       ?assertEqual({down, normal}, down(Ref, 1000))
     end)}}].

io_tests() ->
  [{"a connected receiver gets every line the tool prints",
    {timeout, 10, ?_test(begin
       {Pid, _} = start(cat(), 5000),
       ok = mcp_server:connect_to_mcp_session(Pid),
       ok = mcp_server:notify_mcp_session(Pid, <<"{\"id\":1}">>),
       ?assertEqual({ok, <<"{\"id\":1}">>}, test_support:recv(2000)),
       ok = mcp_server:notify_mcp_session(Pid, "{\"id\":2}"),
       ?assertEqual({ok, <<"{\"id\":2}">>}, test_support:recv(2000)),
       stop(Pid)
     end)}},
   {"disconnect_mcp_session/1 stops delivery to that receiver",
    {timeout, 10, ?_test(begin
       {Pid, _} = start(cat(), 5000),
       ok = mcp_server:connect_to_mcp_session(Pid),
       ok = mcp_server:disconnect_mcp_session(Pid),
       ok = mcp_server:notify_mcp_session(Pid, <<"silent">>),
       ?assertEqual(timeout, test_support:recv(300)),
       stop(Pid)
     end)}},
   {"every connected receiver gets the broadcast",
    {timeout, 10, ?_test(begin
       {Pid, _} = start(cat(), 5000),
       Self = self(),
       Other = spawn(fun() ->
                       ok = mcp_server:connect_to_mcp_session(Pid),
                       Self ! other_ready,
                       receive M -> Self ! {other_got, M} end
                     end),
       {ok, other_ready} = test_support:recv(1000),
       ok = mcp_server:connect_to_mcp_session(Pid),
       ok = mcp_server:notify_mcp_session(Pid, <<"both">>),
       Got = lists:sort(test_support:collect(500)),
       ?assertEqual([{other_got, <<"both">>}, <<"both">>], Got),
       ?assertNot(is_process_alive(Other)),
       stop(Pid)
     end)}},
   {"lines longer than the port line buffer (64 KiB) are reassembled into one message",
    {timeout, 20, ?_test(begin
       {Pid, _} = start(cat(), 5000),
       ok = mcp_server:connect_to_mcp_session(Pid),
       Long = binary:copy(<<"a">>, 70000),
       ok = mcp_server:notify_mcp_session(Pid, Long),
       ?assertEqual({ok, Long}, test_support:recv(5000)),
       stop(Pid)
     end)}},
   {"when the tool exits, buffered output is flushed, receivers get 'terminated' "
    "and the server stops with {subprocess_exited, Status} (that is what the exit_status "
    "clause of handle_info/2 is written to do). "
    "(regression: this used to fail because the clause matches {Port, exit_status, Code} but the port actually sends "
    "{Port, {exit_status, Code}}, so the message hits the catch-all clause, which returns "
    "the atom ok, and the gen_server crashes with {bad_return_value, ok})",
    {timeout, 10, ?_test(begin
       {Pid, Ref} = start(test_support:tool("printf") ++ " nonl", 5000),
       ok = mcp_server:connect_to_mcp_session(Pid),
       Msgs = test_support:collect(1000),
       ?assertEqual([<<"nonl">>, terminated], Msgs),
       ?assertMatch({down, {subprocess_exited, _}}, down(Ref, 1000))
     end)}}].

timer_tests() ->
  [{"the session terminates itself after KeepAliveMs without traffic",
    {timeout, 10, ?_test(begin
       {Pid, Ref} = start(cat(), 200),
       ok = mcp_server:connect_to_mcp_session(Pid),
       ?assertEqual({ok, terminated}, test_support:recv(2000)),
       ?assertEqual({down, normal}, down(Ref, 2000))
     end)}},
   {"notify resets the keep-alive timer",
    {timeout, 10, ?_test(begin
       {Pid, Ref} = start(cat(), 400),
       ok = mcp_server:connect_to_mcp_session(Pid),
       timer:sleep(250),
       ok = mcp_server:notify_mcp_session(Pid, <<"ping">>),
       {ok, <<"ping">>} = test_support:recv(1000),
       timer:sleep(300),               %% t = 550 ms > 400 ms: dead without the reset
       ?assert(is_process_alive(Pid)),
       ?assertEqual({down, normal}, down(Ref, 3000))
     end)}}].

robustness_tests() ->
  [{"handle_cast/2 ignores casts and keeps the server alive",
    {timeout, 10, ?_test(begin
       {Pid, Ref} = start(cat(), 5000),
       gen_server:cast(Pid, whatever),
       ?assertEqual(alive, down(Ref, 200)),
       stop(Pid)
     end)}},
   {"an unknown info message is logged as a warning and the server keeps running. "
    "(regression: this used to fail because the catch-all handle_info/2 clause returns the atom ok instead of "
    "{noreply, State}, so the gen_server exits with {bad_return_value, ok})",
    {timeout, 10, ?_test(begin
       {Pid, Ref} = start(cat(), 5000),
       Pid ! unexpected_message,
       ?assertEqual(alive, down(Ref, 300)),
       stop(Pid)
     end)}},
   {"an unknown call is logged as a warning and the server keeps running. "
    "(regression: this used to fail because the catch-all handle_call/3 clause returns the atom ok instead of a "
    "{reply, ...} tuple, so the gen_server exits with {bad_return_value, ok})",
    {timeout, 10, ?_test(begin
       {Pid, Ref} = start(cat(), 5000),
       catch gen_server:call(Pid, unexpected_call, 500),
       ?assertEqual(alive, down(Ref, 300)),
       stop(Pid)
     end)}}].
