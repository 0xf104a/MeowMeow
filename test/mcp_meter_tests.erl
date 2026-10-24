%%%-------------------------------------------------------------------
%%% @doc EUnit suite for mcp_meter.erl (process-tree memory accounting)
%%%
%%% The /proc backend is exercised with real `sleep' trees. The cgroup
%%% backend is exercised against a plain directory that mimics a cgroup
%%% v2 directory (the files are regular files, so the kernel enforces
%%% nothing, but every read and write the module does is observable).
%%% @end
%%%-------------------------------------------------------------------
-module(mcp_meter_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/modules/mcp/mcp.hrl").

setup() ->
  test_support:setup_config(#{"LogLevel" => "0", "MCPCgroup" => "No"}),
  test_support:setup_mcp(),
  mcp_meter:reset(),
  ok.

cleanup(_) ->
  test_support:cleanup_mcp_sessions(),
  mcp_meter:reset(),
  test_support:teardown_all(ok).

mcp_meter_test_() ->
  {setup, fun setup/0, fun cleanup/1,
   fun(_) -> [proc_tests(), cgroup_tests()] end}.

%% A tree of two processes: `sleep' (the exec'd shell) with a `sleep' child.
tree() ->
  Port = open_port({spawn, "sh -c 'sleep 30 & exec sleep 30'"}, [exit_status]),
  {os_pid, Pid} = erlang:port_info(Port, os_pid),
  ?assert(test_support:wait_until(fun() -> length(mcp_meter:descendants(Pid)) >= 2 end, 3000)),
  {Port, Pid}.

gone(OsPid) ->
  test_support:wait_until(fun() -> not is_running(OsPid) end, 3000).

%% /proc/PID exists for zombies too; a zombie is "gone" for our purpose.
is_running(OsPid) ->
  case file:read_file("/proc/" ++ integer_to_list(OsPid) ++ "/stat") of
    {ok, Bin} ->
      {Pos, _} = lists:last(binary:matches(Bin, <<")">>)),
      binary:part(Bin, Pos + 2, 1) =/= <<"Z">>;
    _ -> false
  end.

fake_root(Controllers) ->
  Root = test_support:tmp_dir(),
  test_support:write_file(filename:join(Root, "cgroup.controllers"), Controllers),
  test_support:write_file(filename:join(Root, "cgroup.subtree_control"), ""),
  test_support:write_file(filename:join(Root, "cgroup.procs"), "1234\n"),
  Root.

use_fake_root(Root) ->
  test_support:set_config("MCPCgroup", "Yes"),
  test_support:set_config("MCPCgroupRoot", Root),
  mcp_meter:reset().

no_cgroup() ->
  test_support:set_config("MCPCgroup", "No"),
  test_support:set_config("MCPCgroupRoot", ""),
  mcp_meter:reset().

read(Path) ->
  {ok, Bin} = file:read_file(Path),
  string:trim(binary_to_list(Bin)).

session_dirs(Sessions) ->
  {ok, Entries} = file:list_dir(Sessions),
  [E || "session-" ++ _ = E <- Entries].

proc_tests() ->
  [{"MCPCgroup No selects the /proc backend",
    ?_test(begin
             ?assertEqual(proc, mcp_meter:init()),
             ?assertEqual(proc, mcp_meter:backend()),
             ?assertEqual({proc, undefined}, mcp_meter:prepare())
           end)},
   {"stat_ppid/1 reads the parent pid even when the command name has spaces and parentheses",
    ?_assertEqual(45, mcp_meter:stat_ppid(<<"123 (a (we)ird) name) S 45 1 1 0 -1 4194560 0\n">>))},
   {"stat_ppid/1 of a malformed line is undefined",
    ?_assertEqual(undefined, mcp_meter:stat_ppid(<<"garbage">>))},
   {"descendants/1 lists the port's process first and its children after it",
    {timeout, 15, ?_test(begin
       {Port, Pid} = tree(),
       [First | Rest] = mcp_meter:descendants(Pid),
       ?assertEqual(Pid, First),
       ?assertEqual(1, length(Rest)),
       ?assert(mcp_meter:tree_rss(Pid) > 0),
       ?assertEqual(mcp_meter:descendants(Pid), mcp_meter:snapshot({proc, Pid})),
       mcp_meter:kill_pids(mcp_meter:descendants(Pid)),
       receive {Port, {exit_status, _}} -> ok after 3000 -> ?assert(false) end,
       ?assert(gone(Pid)),
       ?assert(gone(hd(Rest)))
     end)}},
   {"descendants/1 of a pid that does not exist is just that pid; tree_rss/1 is 0",
    ?_test(begin
             ?assertEqual([4194304], mcp_meter:descendants(4194304)),
             ?assertEqual(0, mcp_meter:tree_rss(4194304))
           end)},
   {"usage/1 of a /proc handle is the tree's resident memory, 0 before attach",
    {timeout, 15, ?_test(begin
       {Port, Pid} = tree(),
       ?assertEqual(0, mcp_meter:usage({proc, undefined})),
       ?assert(mcp_meter:usage({proc, Pid}) > 0),
       ?assertNot(mcp_meter:enforced_by_kernel({proc, Pid})),
       ?assertEqual(0, mcp_meter:oom_kills({proc, Pid})),
       ?assertEqual(ok, mcp_meter:set_limit({proc, Pid}, 1024)),
       mcp_meter:kill({proc, Pid}, true, []),
       receive {Port, {exit_status, _}} -> ok after 3000 -> ?assert(false) end,
       ?assert(gone(Pid))
     end)}},
   {"kill/3 still reaches a snapshot child after the port's own process is gone "
    "(the child was re-parented away, so it can no longer be found from that pid; note that the "
    "port stays open until the child, which inherited the tool's stdout, is gone too)",
    {timeout, 15, ?_test(begin
       {Port, Pid} = tree(),
       [Pid, Child] = Snapshot = mcp_meter:snapshot({proc, Pid}),
       mcp_meter:kill_pids([Pid]),
       ?assert(gone(Pid)),
       ?assert(is_running(Child)),
       ?assertNotEqual(Pid, mcp_meter:stat_ppid(element(2, file:read_file(
                                "/proc/" ++ integer_to_list(Child) ++ "/stat")))),
       ?assertEqual(ok, mcp_meter:kill({proc, Pid}, false, Snapshot)),
       ?assert(gone(Child)),
       receive {Port, {exit_status, _}} -> ok after 3000 -> ?assert(false) end
     end)}},
   {"kill_pids/1 ignores init, this VM and non-pids",
    ?_test(begin
             Self = list_to_integer(os:getpid()),
             ?assertEqual(ok, mcp_meter:kill_pids([1, 0, Self, bogus])),
             ?assert(is_process_alive(self()))
           end)},
   {"spawn_spec/2 of a /proc handle is the plain {spawn, Cmd}",
    ?_assertEqual({{spawn, "cat"}, []}, mcp_meter:spawn_spec({proc, undefined}, "cat"))},
   {"attach/2 records the port's OS pid",
    ?_test(begin
             Port = open_port({spawn, "cat"}, [exit_status]),
             {os_pid, OsPid} = erlang:port_info(Port, os_pid),
             ?assertEqual({proc, OsPid}, mcp_meter:attach({proc, undefined}, Port)),
             port_close(Port)
           end)},
   {"detach/1 and snapshot/1 of a /proc handle are no-ops",
    ?_test(begin
             ?assertEqual(ok, mcp_meter:detach({proc, 1})),
             ?assertEqual([], mcp_meter:snapshot({proc, undefined}))
           end)},
   {"own_cgroup_dir/0 returns {ok, Dir} under /sys/fs/cgroup on a cgroup v2 host, or an error",
    ?_test(case mcp_meter:own_cgroup_dir() of
             {ok, "/sys/fs/cgroup" ++ _} -> ok;
             {error, _} -> ok;
             Other -> ?assert({unexpected, Other})
           end)}].

cgroup_tests() ->
  [{"init/0 enables the memory controller under MCPCgroupRoot and creates the sessions cgroup",
    ?_test(begin
             Root = fake_root("cpuset cpu io memory pids\n"),
             use_fake_root(Root),
             Sessions = filename:join(Root, "meowmeow-mcp"),
             ?assertEqual({cgroup, Sessions}, mcp_meter:init()),
             ?assertEqual({cgroup, Sessions}, mcp_meter:backend()),
             ?assertEqual("+memory", read(filename:join(Root, "cgroup.subtree_control"))),
             ?assertEqual("+memory", read(filename:join(Sessions, "cgroup.subtree_control"))),
             no_cgroup(), test_support:rm_rf(Root)
           end)},
   {"without the memory controller the backend falls back to /proc",
    ?_test(begin
             Root = fake_root("cpu pids\n"),
             use_fake_root(Root),
             ?assertEqual(proc, mcp_meter:init()),
             no_cgroup(), test_support:rm_rf(Root)
           end)},
   {"a root that is not a cgroup directory falls back to /proc",
    ?_test(begin
             use_fake_root("/nonexistent/cgroup"),
             ?assertEqual(proc, mcp_meter:init()),
             no_cgroup()
           end)},
   {"cgroup_setup/1 reports why it failed",
    ?_test(begin
             ?assertMatch({error, {no_cgroup_controllers, enoent}}, mcp_meter:cgroup_setup("/nonexistent")),
             Root = fake_root("cpu\n"),
             ?assertEqual({error, no_memory_controller}, mcp_meter:cgroup_setup(Root)),
             test_support:rm_rf(Root)
           end)},
   {"move_procs/2 puts every listed process into the leaf cgroup",
    ?_test(begin
             Root = fake_root("memory\n"),
             Main = filename:join(Root, "meowmeow-main"),
             ?assertEqual(ok, mcp_meter:move_procs(Root, Main)),
             ?assertEqual("1234", read(filename:join(Main, "cgroup.procs"))),
             test_support:rm_rf(Root)
           end)},
   {"a session joins its own cgroup before exec, limits are written, usage and OOM kills are read, "
    "kill writes cgroup.kill and detach removes the directory",
    {timeout, 15, ?_test(begin
       Root = fake_root("memory\n"),
       use_fake_root(Root),
       {cgroup, Sessions} = mcp_meter:init(),
       {cgroup, Dir, undefined} = H0 = mcp_meter:prepare(),
       ?assertEqual(Sessions, filename:dirname(Dir)),
       ?assert(filelib:is_dir(Dir)),
       {Name, PortOpts} = mcp_meter:spawn_spec(H0, "cat"),
       ?assertMatch({spawn_executable, _}, Name),
       [{args, ["-c", Script]}] = PortOpts,
       ?assertNotEqual(nomatch, string:find(Script, "exec cat")),
       Port = open_port(Name, [binary, use_stdio, exit_status | PortOpts]),
       {cgroup, Dir, OsPid} = H = mcp_meter:attach(H0, Port),
       ?assert(is_integer(OsPid)),
       Procs = filename:join(Dir, "cgroup.procs"),
       ?assert(test_support:wait_until(fun() -> filelib:is_file(Procs) end, 3000)),
       ?assertEqual(integer_to_list(OsPid), read(Procs)),
       ?assert(mcp_meter:enforced_by_kernel(H)),
       ?assertEqual([], mcp_meter:snapshot(H)),
       ?assertEqual(ok, mcp_meter:set_limit(H, 1048576)),
       ?assertEqual("1048576", read(filename:join(Dir, "memory.max"))),
       ?assertEqual(ok, mcp_meter:set_limit(H, none)),
       ?assertEqual("max", read(filename:join(Dir, "memory.max"))),
       ?assertEqual(0, mcp_meter:usage(H)),
       test_support:write_file(filename:join(Dir, "memory.current"), "4096\n"),
       ?assertEqual(4096, mcp_meter:usage(H)),
       ?assertEqual(0, mcp_meter:oom_kills(H)),
       test_support:write_file(filename:join(Dir, "memory.events"),
                               "low 0\nhigh 0\nmax 3\noom 1\noom_kill 2\n"),
       ?assertEqual(2, mcp_meter:oom_kills(H)),
       ?assertEqual(ok, mcp_meter:kill(H, true, [])),
       ?assertEqual("1", read(filename:join(Dir, "cgroup.kill"))),
       port_close(Port),
       ?assertEqual(ok, mcp_meter:detach(H)),
       ?assertNot(filelib:is_dir(Dir)),
       no_cgroup(), test_support:rm_rf(Root)
     end)}},
   {"when cgroup.kill cannot be written the listed processes are killed instead",
    {timeout, 15, ?_test(begin
       {Port, Pid} = tree(),
       Dir = test_support:tmp_dir(),
       test_support:write_file(filename:join(Dir, "cgroup.procs"), integer_to_list(Pid) ++ "\n"),
       ok = file:make_dir(filename:join(Dir, "cgroup.kill")),   %% a directory: the write fails
       ?assertEqual(ok, mcp_meter:kill({cgroup, Dir, Pid}, true, [])),
       receive {Port, {exit_status, _}} -> ok after 3000 -> ?assert(false) end,
       ?assert(gone(Pid)),
       test_support:rm_rf(Dir)
     end)}},
   {"the wrapper exits with status 97 when the cgroup cannot be joined, instead of running the tool unlimited",
    {timeout, 15, ?_test(begin
       H0 = {cgroup, "/nonexistent/meowmeow-mcp/session-1", undefined},
       {Name, PortOpts} = mcp_meter:spawn_spec(H0, "cat"),
       Port = open_port(Name, [binary, use_stdio, exit_status, stderr_to_stdout | PortOpts]),
       Msgs = test_support:collect(2000),
       ?assert(lists:member({Port, {exit_status, 97}}, Msgs))
     end)}},
   {"mcp_server runs the tool in a session cgroup and removes it when the session ends",
    {timeout, 15, ?_test(begin
       Root = fake_root("memory\n"),
       use_fake_root(Root),
       test_support:set_config("MCPMemoryPollMs", "50"),
       {ok, Pid} = mcp_server:start([test_support:tool("cat"), 5000, #{memory_limit => 2048}]),
       Ref = monitor(process, Pid),
       Sessions = filename:join(Root, "meowmeow-mcp"),
       [Session] = session_dirs(Sessions),
       Dir = filename:join(Sessions, Session),
       ?assertEqual("2048", read(filename:join(Dir, "memory.max"))),
       ?assertMatch(#{backend := cgroup, limit := 2048}, mcp_server:memory_usage(Pid)),
       %% the kernel would account here; make the file say so and watch the sample
       test_support:write_file(filename:join(Dir, "memory.current"), "1000\n"),
       ?assert(test_support:wait_until(
                 fun() -> maps:get(current, mcp_server:memory_usage(Pid)) =:= 1000 end, 2000)),
       ok = mcp_server:terminate_session(Pid),
       receive {'DOWN', Ref, process, Pid, normal} -> ok after 3000 -> ?assert(false) end,
       ?assertEqual([], session_dirs(Sessions)),
       test_support:set_config("MCPMemoryPollMs", "1000"),
       no_cgroup(), test_support:rm_rf(Root)
     end)}},
   {"an OOM kill recorded in memory.events ends the session as a memory limit violation",
    {timeout, 15, ?_test(begin
       Root = fake_root("memory\n"),
       use_fake_root(Root),
       test_support:set_config("MCPMemoryPollMs", "50"),
       test_support:flush(),
       {ok, Pid} = mcp_server:start([test_support:tool("cat"), 5000, #{memory_limit => 2048}]),
       Ref = monitor(process, Pid),
       ok = mcp_server:connect_to_mcp_session(Pid),
       Sessions = filename:join(Root, "meowmeow-mcp"),
       [Session] = session_dirs(Sessions),
       test_support:write_file(filename:join([Sessions, Session, "memory.events"]), "oom_kill 1\n"),
       ?assertEqual({ok, terminated}, test_support:recv(3000)),
       receive {'DOWN', Ref, process, Pid, Reason} ->
                 ?assertMatch({shutdown, {memory_limit_exceeded, _}}, Reason)
       after 3000 -> ?assert(false)
       end,
       ?assert(test_support:wait_until(fun() -> session_dirs(Sessions) =:= [] end, 3000)),
       test_support:set_config("MCPMemoryPollMs", "1000"),
       no_cgroup(), test_support:rm_rf(Root)
     end)}}].
