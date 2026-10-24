%%%-------------------------------------------------------------------
%%% @doc EUnit suite for mcp_admission.erl (session slots and the wait queue)
%%%
%%% No subprocesses here: a slot holder is a plain process that calls
%%% acquire/2, reports the answer and lives until told to stop or
%%% killed, which is how a session releases its slot.
%%% @end
%%%-------------------------------------------------------------------
-module(mcp_admission_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  test_support:setup_config(#{"LogLevel" => "0"}),
  ok = mcp_admission:ensure_started(),
  ok.

cleanup(_) ->
  mcp_admission:stop(),
  test_support:teardown_all(ok).

opts(GlobalMax, ToolMax, WaitMs, MaxWaiting) ->
  #{global_max => GlobalMax, tool_max => ToolMax,
    wait_ms => WaitMs, max_waiting => MaxWaiting}.

%% A slot holder: acquires, reports {acquired, Pid, Result}, then idles.
holder(Tool, Opts) ->
  Self = self(),
  spawn(fun() ->
          Result = mcp_admission:acquire(Tool, Opts),
          Self ! {acquired, self(), Result},
          receive stop -> ok end
        end).

result(Pid, Timeout) ->
  receive {acquired, Pid, R} -> R after Timeout -> timeout end.

kill(Pid) ->
  Ref = monitor(process, Pid),
  exit(Pid, kill),
  receive {'DOWN', Ref, process, Pid, _} -> ok after 1000 -> ok end.

running() -> maps:get(running, mcp_admission:status()).
waiting() -> maps:get(waiting, mcp_admission:status()).

wait_running(N) ->
  test_support:wait_until(fun() -> running() =:= N end, 2000).

mcp_admission_test_() ->
  {setup, fun setup/0, fun cleanup/1,
   fun(_) -> [lifecycle_tests(), cap_tests(), queue_tests()] end}.

lifecycle_tests() ->
  [{"ensure_started/0 is idempotent and registers the server",
    ?_test(begin
             ?assertEqual(ok, mcp_admission:ensure_started()),
             ?assertEqual(ok, mcp_admission:ensure_started()),
             ?assert(is_pid(whereis(mcp_admission)))
           end)},
   {"status/0 of an idle server is all zeros",
    ?_assertEqual(#{running => 0, waiting => 0, tools => #{}}, mcp_admission:status())},
   {"acquire/2 starts the server lazily after stop/0",
    ?_test(begin
             mcp_admission:stop(),
             ?assertEqual(undefined, whereis(mcp_admission)),
             H = holder("t", opts(infinity, infinity, 0, 0)),
             ?assertEqual(ok, result(H, 1000)),
             ?assert(is_pid(whereis(mcp_admission))),
             kill(H), wait_running(0)
           end)},
   {"status/0 without a server reports zeros instead of crashing",
    ?_test(begin
             mcp_admission:stop(),
             ?assertEqual(#{running => 0, waiting => 0, tools => #{}}, mcp_admission:status()),
             ok = mcp_admission:ensure_started()
           end)},
   {"the slot goes back when the holder dies",
    ?_test(begin
             H = holder("t", opts(1, infinity, 0, 0)),
             ok = result(H, 1000),
             ?assertEqual(1, running()),
             ?assertEqual(#{"t" => 1}, maps:get(tools, mcp_admission:status())),
             kill(H),
             ?assert(wait_running(0)),
             ?assertEqual(#{}, maps:get(tools, mcp_admission:status()))
           end)},
   {"release/1 gives the slot back early; a second release is harmless",
    ?_test(begin
             H = holder("t", opts(1, infinity, 0, 0)),
             ok = result(H, 1000),
             ?assertEqual(ok, mcp_admission:release(H)),
             ?assertEqual(0, running()),
             ?assertEqual(ok, mcp_admission:release(H)),
             kill(H)
           end)},
   {"acquiring twice from the same process holds one slot",
    ?_test(begin
             Self = self(),
             H = spawn(fun() ->
                         ok = mcp_admission:acquire("t", opts(5, infinity, 0, 0)),
                         ok = mcp_admission:acquire("t", opts(5, infinity, 0, 0)),
                         Self ! {acquired, self(), ok},
                         receive stop -> ok end
                       end),
             ok = result(H, 1000),
             ?assertEqual(1, running()),
             kill(H), wait_running(0)
           end)}].

cap_tests() ->
  [{"with no caps every start is admitted at once",
    ?_test(begin
             Hs = [holder("t", opts(infinity, infinity, 0, 0)) || _ <- lists:seq(1, 5)],
             ?assert(lists:all(fun(H) -> result(H, 1000) =:= ok end, Hs)),
             ?assertEqual(5, running()),
             lists:foreach(fun kill/1, Hs), wait_running(0)
           end)},
   {"the global cap refuses the start beyond it when no wait is allowed",
    ?_test(begin
             O = opts(2, infinity, 0, 0),
             [H1, H2, H3] = [holder("t", O) || _ <- [1, 2, 3]],
             ok = result(H1, 1000), ok = result(H2, 1000),
             ?assertEqual({error, busy}, result(H3, 1000)),
             ?assertEqual(2, running()),
             lists:foreach(fun kill/1, [H1, H2, H3]), wait_running(0)
           end)},
   {"the per-tool cap only counts sessions of the same command",
    ?_test(begin
             O = opts(infinity, 1, 0, 0),
             A1 = holder("a", O), ok = result(A1, 1000),
             A2 = holder("a", O), ?assertEqual({error, busy}, result(A2, 1000)),
             B1 = holder("b", O), ?assertEqual(ok, result(B1, 1000)),
             ?assertEqual(#{"a" => 1, "b" => 1}, maps:get(tools, mcp_admission:status())),
             lists:foreach(fun kill/1, [A1, A2, B1]), wait_running(0)
           end)},
   {"the cap is the one the asking rule carries: a stricter rule sees sessions a looser one started",
    ?_test(begin
             A1 = holder("a", opts(infinity, infinity, 0, 0)), ok = result(A1, 1000),
             A2 = holder("a", opts(infinity, 1, 0, 0)), ?assertEqual({error, busy}, result(A2, 1000)),
             lists:foreach(fun kill/1, [A1, A2]), wait_running(0)
           end)},
   {"a cap of 0 sessions admits nothing",
    ?_test(begin
             H = holder("t", opts(0, infinity, 0, 0)),
             ?assertEqual({error, busy}, result(H, 1000)),
             kill(H)
           end)}].

queue_tests() ->
  [{"a start that does not fit waits and is admitted when a slot frees up",
    {timeout, 10, ?_test(begin
       O = opts(1, infinity, 3000, 1),
       H1 = holder("t", O), ok = result(H1, 1000),
       H2 = holder("t", O),
       ?assertEqual(timeout, result(H2, 300)),
       ?assertEqual(1, waiting()),
       kill(H1),
       ?assertEqual(ok, result(H2, 2000)),
       ?assertEqual(0, waiting()),
       ?assertEqual(1, running()),
       kill(H2), wait_running(0)
     end)}},
   {"a waiting start gives up with {error, busy} after wait_ms",
    {timeout, 10, ?_test(begin
       O = opts(1, infinity, 300, 1),
       H1 = holder("t", O), ok = result(H1, 1000),
       T0 = erlang:monotonic_time(millisecond),
       H2 = holder("t", O),
       ?assertEqual({error, busy}, result(H2, 2000)),
       ?assert(erlang:monotonic_time(millisecond) - T0 >= 250),
       ?assertEqual(0, waiting()),
       lists:foreach(fun kill/1, [H1, H2]), wait_running(0)
     end)}},
   {"only max_waiting starts may wait; the next one is refused at once",
    {timeout, 10, ?_test(begin
       O = opts(1, infinity, 3000, 1),
       H1 = holder("t", O), ok = result(H1, 1000),
       H2 = holder("t", O), ?assertEqual(timeout, result(H2, 200)),
       H3 = holder("t", O), ?assertEqual({error, busy}, result(H3, 1000)),
       ?assertEqual(1, waiting()),
       lists:foreach(fun kill/1, [H1, H2, H3]), wait_running(0)
     end)}},
   {"a waiter that dies leaves the queue",
    {timeout, 10, ?_test(begin
       O = opts(1, infinity, 3000, 2),
       H1 = holder("t", O), ok = result(H1, 1000),
       H2 = holder("t", O), ?assertEqual(timeout, result(H2, 200)),
       ?assertEqual(1, waiting()),
       kill(H2),
       ?assert(test_support:wait_until(fun() -> waiting() =:= 0 end, 2000)),
       kill(H1), wait_running(0)
     end)}},
   {"waiters are served oldest first",
    {timeout, 10, ?_test(begin
       O = opts(1, infinity, 3000, 2),
       H1 = holder("t", O), ok = result(H1, 1000),
       H2 = holder("t", O), ?assertEqual(timeout, result(H2, 100)),
       H3 = holder("t", O), ?assertEqual(timeout, result(H3, 100)),
       kill(H1),
       ?assertEqual(ok, result(H2, 2000)),
       ?assertEqual(timeout, result(H3, 200)),
       kill(H2),
       ?assertEqual(ok, result(H3, 2000)),
       kill(H3), wait_running(0)
     end)}},
   {"a waiter still blocked by its tool cap is skipped in favour of one that now fits",
    {timeout, 10, ?_test(begin
       Loose = opts(2, infinity, 3000, 2),
       A1 = holder("a", Loose), ok = result(A1, 1000),
       C1 = holder("c", Loose), ok = result(C1, 1000),
       %% W1 waits because "a" is at its own cap, W2 because of the global cap
       W1 = holder("a", opts(2, 1, 3000, 2)), ?assertEqual(timeout, result(W1, 100)),
       W2 = holder("b", Loose), ?assertEqual(timeout, result(W2, 100)),
       ?assertEqual(2, waiting()),
       kill(C1),
       ?assertEqual(ok, result(W2, 2000)),
       ?assertEqual(timeout, result(W1, 200)),
       ?assertEqual(1, waiting()),
       lists:foreach(fun kill/1, [A1, W1, W2]), wait_running(0)
     end)}},
   {"stop/0 answers every waiter with {error, busy}",
    {timeout, 10, ?_test(begin
       O = opts(1, infinity, 5000, 2),
       H1 = holder("t", O), ok = result(H1, 1000),
       H2 = holder("t", O), ?assertEqual(timeout, result(H2, 100)),
       mcp_admission:stop(),
       ?assertEqual({error, busy}, result(H2, 2000)),
       lists:foreach(fun kill/1, [H1, H2]),
       ok = mcp_admission:ensure_started()
     end)}}].
