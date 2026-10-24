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
   fun(_) -> [lifecycle_tests(), io_tests(), timer_tests(), robustness_tests(), limit_tests()] end}.

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

%%--------------------------------------------------------------------
%% Session caps and memory limits (issue #22)
%%--------------------------------------------------------------------

%% A process that holds ~40 MiB resident and then idles.
hog() ->
  case os:find_executable("perl") of
    false ->
      Erl = case os:find_executable("erl") of
              false -> filename:join([code:root_dir(), "bin", "erl"]);
              Path -> Path
            end,
      Erl ++ " -noshell -eval 'erlang:garbage_collect(), "
             "put(x, binary:copy(<<0>>, 40000000)), timer:sleep(60000)'";
    Perl -> "env LC_ALL=C " ++ Perl ++ " -e '$x = \"a\" x 40000000; sleep 60'"
  end.

os_pid_of(Pid) ->
  State = sys:get_state(Pid),
  Port = element(2, State),
  {os_pid, OsPid} = erlang:port_info(Port, os_pid),
  OsPid.

alive_os(OsPid) ->
  case file:read_file("/proc/" ++ integer_to_list(OsPid) ++ "/stat") of
    {ok, Bin} ->
      {Pos, _} = lists:last(binary:matches(Bin, <<")">>)),
      binary:part(Bin, Pos + 2, 1) =/= <<"Z">>;
    _ -> false
  end.

with_config(Pairs, Fun) ->
  test_support:cleanup_mcp_sessions(),   %% earlier tests may leave sessions running
  Old = [{K, mcp_options:config_string(K, "")} || {K, _} <- Pairs],
  lists:foreach(fun({K, V}) -> test_support:set_config(K, V) end, Pairs),
  try Fun()
  after
    lists:foreach(fun({K, V}) -> test_support:set_config(K, V) end, Old),
    catch mcp_admission:stop()
  end.

limit_tests() ->
  [{"MCPMaxSessions refuses the session beyond the cap with {error, {shutdown, busy}} and no crash report",
    {timeout, 10, ?_test(with_config([{"MCPMaxSessions", "1"}, {"MCPSessionWaitMs", "0"}], fun() ->
       {P1, _} = start(cat(), 5000),
       ?assertEqual({error, {shutdown, busy}}, mcp_server:start([cat(), 5000])),
       ?assertEqual(1, maps:get(running, mcp_admission:status())),
       stop(P1)
     end))}},
   {"the slot is released when the session stops, so the next start succeeds",
    {timeout, 10, ?_test(with_config([{"MCPMaxSessions", "1"}, {"MCPSessionWaitMs", "0"}], fun() ->
       {P1, Ref} = start(cat(), 5000),
       stop(P1),
       {down, normal} = down(Ref, 2000),
       ?assert(test_support:wait_until(fun() -> maps:get(running, mcp_admission:status()) =:= 0 end, 2000)),
       {P2, _} = start(cat(), 5000),
       stop(P2)
     end))}},
   {"a start that finds no slot waits up to MCPSessionWaitMs and proceeds once a session ends",
    {timeout, 15, ?_test(with_config([{"MCPMaxSessions", "1"}, {"MCPSessionWaitMs", "4000"}], fun() ->
       {P1, _} = start(cat(), 5000),
       Self = self(),
       spawn(fun() -> Self ! {second, mcp_server:start([cat(), 5000])} end),
       receive {second, _} -> ?assert(false) after 300 -> ok end,
       ?assertEqual(1, maps:get(waiting, mcp_admission:status())),
       stop(P1),
       P2 = receive {second, {ok, P}} -> P after 3000 -> ?assert(false) end,
       ?assert(is_process_alive(P2)),
       stop(P2)
     end))}},
   {"a rule's MaxSessions caps that command (the third start/1 element carries the rule options)",
    {timeout, 10, ?_test(with_config([{"MCPSessionWaitMs", "0"}], fun() ->
       {P1, _} = start(cat(), 5000),
       ?assertEqual({error, {shutdown, busy}}, mcp_server:start([cat(), 5000, #{max_sessions => 1}])),
       {ok, P2} = mcp_server:start([cat(), 5000, #{max_sessions => 2}]),
       stop(P1), stop(P2)
     end))}},
   {"memory_usage/1 reports the sampled tree size, its peak, the limit and the backend",
    {timeout, 10, ?_test(with_config([{"MCPMemoryPollMs", "50"}, {"MCPCgroup", "No"}], fun() ->
       mcp_meter:reset(),
       {P, _} = start(cat(), 5000),
       ?assert(test_support:wait_until(fun() -> maps:get(current, mcp_server:memory_usage(P)) > 0 end, 3000)),
       #{current := Cur, peak := Peak, limit := Limit, backend := Backend} = mcp_server:memory_usage(P),
       ?assert(Peak >= Cur),
       ?assertEqual(none, Limit),
       ?assertEqual(proc, Backend),
       stop(P)
     end))}},
   {"MCPMemoryLimit is the default per-session limit and a rule's MemoryLimit overrides it",
    {timeout, 10, ?_test(with_config([{"MCPMemoryLimit", "64M"}, {"MCPMemoryPollMs", "0"}], fun() ->
       {P1, _} = start(cat(), 5000),
       ?assertEqual(64 * 1024 * 1024, maps:get(limit, mcp_server:memory_usage(P1))),
       {ok, P2} = mcp_server:start([cat(), 5000, #{memory_limit => 1024 * 1024}]),
       ?assertEqual(1024 * 1024, maps:get(limit, mcp_server:memory_usage(P2))),
       stop(P1), stop(P2)
     end))}},
   {"with the /proc backend a tool over its MemoryLimit is killed, receivers get 'terminated' "
    "and the session stops with {shutdown, {memory_limit_exceeded, Bytes}}",
    {timeout, 30, ?_test(with_config([{"MCPMemoryPollMs", "100"}, {"MCPCgroup", "No"}], fun() ->
       mcp_meter:reset(),
       test_support:flush(),
       {ok, P} = mcp_server:start([hog(), 60000, #{memory_limit => 8 * 1024 * 1024}]),
       Ref = monitor(process, P),
       ok = mcp_server:connect_to_mcp_session(P),
       OsPid = os_pid_of(P),
       ?assertEqual({ok, terminated}, test_support:recv(15000)),
       receive {'DOWN', Ref, process, P, Reason} ->
                 ?assertMatch({shutdown, {memory_limit_exceeded, B}} when B > 8 * 1024 * 1024, Reason)
       after 5000 -> ?assert(false)
       end,
       ?assert(test_support:wait_until(fun() -> not alive_os(OsPid) end, 3000)),
       ?assert(test_support:wait_until(fun() -> maps:get(running, mcp_admission:status()) =:= 0 end, 2000))
     end))}},
   {"a tool under its limit keeps running",
    {timeout, 10, ?_test(with_config([{"MCPMemoryPollMs", "50"}, {"MCPCgroup", "No"}], fun() ->
       mcp_meter:reset(),
       {ok, P} = mcp_server:start([cat(), 5000, #{memory_limit => 512 * 1024 * 1024}]),
       Ref = monitor(process, P),
       ?assertEqual(alive, down(Ref, 400)),
       ?assert(maps:get(current, mcp_server:memory_usage(P)) > 0),
       stop(P)
     end))}},
   {"terminating a session kills the tool's whole process tree, not only the process the port started "
    "(regression: this used to fail because only the port's own pid got SIGTERM/SIGKILL, so children "
    "such as a background helper of the tool survived as orphans and kept their memory)",
    {timeout, 15, ?_test(with_config([{"MCPCgroup", "No"}], fun() ->
       mcp_meter:reset(),
       Sh = test_support:tool("sh"),
       {ok, P} = mcp_server:start([Sh ++ " -c 'sleep 30 & exec cat'", 5000]),
       Ref = monitor(process, P),
       OsPid = os_pid_of(P),
       ?assert(test_support:wait_until(fun() -> length(mcp_meter:descendants(OsPid)) >= 2 end, 3000)),
       [OsPid, Child] = mcp_meter:descendants(OsPid),
       ok = mcp_server:terminate_session(P),
       {down, normal} = down(Ref, 3000),
       ?assert(test_support:wait_until(fun() -> not alive_os(OsPid) end, 3000)),
       ?assert(test_support:wait_until(fun() -> not alive_os(Child) end, 3000))
     end))}}].
