%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2026, Anna-Sofia Kasierocka
%%% @doc
%%%   Memory accounting and limits for MCP tool subprocesses.
%%%
%%%   A tool command usually starts a process tree (`sh' -> `npx' ->
%%%   `node' -> ...), so the memory of the port's own OS pid says little.
%%%   Two backends account for the whole tree:
%%%
%%%   cgroup (preferred): every session gets its own cgroup v2 under the
%%%     server's cgroup. The tool is started through `sh -c' which joins
%%%     the cgroup before `exec', so every descendant inherits it, even
%%%     daemonised ones. `memory.max' makes the kernel enforce the limit
%%%     and `cgroup.kill' ends the whole tree. Needs a writable cgroup v2
%%%     hierarchy with the memory controller - the usual case for a
%%%     container running as root.
%%%
%%%   proc (fallback): the tree is found by scanning `/proc' for parent
%%%     pids and its resident set sizes are summed. The limit is then
%%%     enforced by mcp_server, which kills the tree when a sample is
%%%     over it. Processes that re-parent to init escape this view.
%%%
%%%   The chosen backend is kept in persistent_term; init/0 is called
%%%   once from mcp:init/0 and lazily on first use otherwise.
%%% @end
%%%-------------------------------------------------------------------
-module(mcp_meter).
-author("f104a").

-export([init/0, reset/0, backend/0, backend_name/1,
         prepare/0, spawn_spec/2, attach/2,
         set_limit/2, enforced_by_kernel/1, usage/1, oom_kills/1,
         snapshot/1, kill/3, detach/1,
         own_cgroup_dir/0, cgroup_setup/1, descendants/1, tree_rss/1,
         stat_ppid/1, kill_pids/1]).

-define(KEY, {?MODULE, backend}).
-define(SESSIONS_DIR, "meowmeow-mcp").
-define(MAIN_DIR, "meowmeow-main").
-define(CGROUP_FS, "/sys/fs/cgroup").
%% Exit status of the wrapper shell when it could not join the cgroup.
-define(JOIN_FAILED, 97).

-type handle() :: {proc, OsPid :: integer() | undefined}
                | {cgroup, Dir :: string(), OsPid :: integer() | undefined}.
-export_type([handle/0]).

%%--------------------------------------------------------------------
%% Backend selection
%%--------------------------------------------------------------------

%% @doc Pick the backend from meow.conf (`MCPCgroup', `MCPCgroupRoot')
%% and what the host offers; remembered until reset/0.
-spec init() -> proc | {cgroup, string()}.
init() ->
  Backend =
    case mcp_options:config_bool("MCPCgroup", true) of
      false ->
        logging:info("MCP memory limits: cgroups disabled, using /proc polling"),
        proc;
      true ->
        Root = case mcp_options:config_string("MCPCgroupRoot", "") of
                 "" -> own_cgroup_dir();
                 Dir -> {ok, Dir}
               end,
        case Root of
          {ok, RootDir} ->
            case cgroup_setup(RootDir) of
              {ok, Sessions} ->
                logging:info("MCP memory limits: cgroup v2 under ~s", [Sessions]),
                {cgroup, Sessions};
              {error, Reason} ->
                logging:warn("MCP memory limits: cgroup v2 unavailable at ~s (~p), "
                             "falling back to /proc polling", [RootDir, Reason]),
                proc
            end;
          {error, Reason} ->
            logging:warn("MCP memory limits: no cgroup v2 (~p), falling back to /proc polling",
                         [Reason]),
            proc
        end
    end,
  persistent_term:put(?KEY, Backend),
  Backend.

reset() ->
  persistent_term:erase(?KEY),
  ok.

backend() ->
  case persistent_term:get(?KEY, undefined) of
    undefined -> init();
    Backend -> Backend
  end.

backend_name({proc, _}) -> proc;
backend_name({cgroup, _, _}) -> cgroup;
backend_name(proc) -> proc;
backend_name({cgroup, _}) -> cgroup.

%% @doc The cgroup v2 directory this VM runs in, from /proc/self/cgroup.
own_cgroup_dir() ->
  case file:read_file("/proc/self/cgroup") of
    {ok, Bin} ->
      Lines = binary:split(Bin, <<"\n">>, [global, trim_all]),
      case [P || <<"0::", P/binary>> <- Lines] of
        [Path | _] -> {ok, ?CGROUP_FS ++ binary_to_list(Path)};
        [] -> {error, no_cgroup_v2}
      end;
    {error, Reason} -> {error, {no_proc_cgroup, Reason}}
  end.

%% @doc Make Root able to host per-session cgroups with the memory
%% controller and return the directory sessions are created in.
%%
%% cgroup v2 forbids a cgroup to have both member processes and
%% controllers enabled for its children, so when enabling `memory' is
%% refused with EBUSY the processes currently in Root (this VM and its
%% ports) are moved into a leaf `meowmeow-main' first.
-spec cgroup_setup(string()) -> {ok, string()} | {error, term()}.
cgroup_setup(Root) ->
  case file:read_file(filename:join(Root, "cgroup.controllers")) of
    {ok, Controllers} ->
      Available = binary:split(Controllers, [<<" ">>, <<"\n">>], [global, trim_all]),
      case lists:member(<<"memory">>, Available) of
        false -> {error, no_memory_controller};
        true ->
          case enable_memory(Root) of
            ok ->
              Sessions = filename:join(Root, ?SESSIONS_DIR),
              case ensure_dir(Sessions) of
                ok ->
                  case write(filename:join(Sessions, "cgroup.subtree_control"), "+memory") of
                    ok -> {ok, Sessions};
                    {error, Reason} -> {error, {subtree_control, Sessions, Reason}}
                  end;
                {error, Reason} -> {error, {mkdir, Sessions, Reason}}
              end;
            {error, Reason} -> {error, {enable_memory, Reason}}
          end
      end;
    {error, Reason} -> {error, {no_cgroup_controllers, Reason}}
  end.

enable_memory(Root) ->
  Ctl = filename:join(Root, "cgroup.subtree_control"),
  case write(Ctl, "+memory") of
    ok -> ok;
    {error, ebusy} ->
      case move_procs(Root, filename:join(Root, ?MAIN_DIR)) of
        ok -> write(Ctl, "+memory");
        Error -> Error
      end;
    Error -> Error
  end.

%% Move every process listed in Root/cgroup.procs into Dest.
move_procs(Root, Dest) ->
  case ensure_dir(Dest) of
    ok ->
      case file:read_file(filename:join(Root, "cgroup.procs")) of
        {ok, Bin} ->
          Target = filename:join(Dest, "cgroup.procs"),
          %% A pid that exited meanwhile is not an error worth stopping for.
          lists:foreach(fun(Pid) -> write(Target, Pid) end,
                        binary:split(Bin, <<"\n">>, [global, trim_all])),
          ok;
        {error, Reason} -> {error, {read_procs, Reason}}
      end;
    Error -> Error
  end.

%%--------------------------------------------------------------------
%% Per-session handles
%%--------------------------------------------------------------------

%% @doc Reserve the accounting for a session about to be spawned.
-spec prepare() -> handle().
prepare() ->
  case backend() of
    proc -> {proc, undefined};
    {cgroup, Sessions} ->
      Dir = filename:join(Sessions, "session-" ++ integer_to_list(erlang:unique_integer([positive]))),
      case file:make_dir(Dir) of
        ok -> {cgroup, Dir, undefined};
        {error, Reason} ->
          logging:warn("Cannot create session cgroup ~s (~p); the session runs without "
                       "a kernel memory limit", [Dir, Reason]),
          {proc, undefined}
      end
  end.

%% @doc What to give open_port/2: the port name and extra options.
%% `{spawn, Cmd}' already runs `sh -c "exec Cmd"', so the cgroup variant
%% behaves the same apart from joining the cgroup first.
-spec spawn_spec(handle(), string()) -> {term(), list()}.
spawn_spec({proc, _}, Cmd) ->
  {{spawn, Cmd}, []};
spawn_spec({cgroup, Dir, _}, Cmd) ->
  Procs = filename:join(Dir, "cgroup.procs"),
  Script = lists:flatten(
             ["echo $$ > '", Procs, "' || { echo 'MeowMeow: cannot join cgroup ", Dir,
              "' >&2; exit ", integer_to_list(?JOIN_FAILED), "; }; exec ", Cmd]),
  {{spawn_executable, shell()}, [{args, ["-c", Script]}]}.

shell() ->
  case os:find_executable("sh") of
    false -> "/bin/sh";
    Path -> Path
  end.

%% @doc Record the OS pid of the port just opened.
-spec attach(handle(), port()) -> handle().
attach({proc, _}, Port) -> {proc, os_pid(Port)};
attach({cgroup, Dir, _}, Port) -> {cgroup, Dir, os_pid(Port)}.

os_pid(Port) ->
  case erlang:port_info(Port, os_pid) of
    {os_pid, Pid} -> Pid;
    _ -> undefined
  end.

%% @doc Apply a byte limit (or none). Only the cgroup backend can make
%% the kernel enforce it; see enforced_by_kernel/1.
-spec set_limit(handle(), none | pos_integer()) -> ok | {error, term()}.
set_limit({cgroup, Dir, _}, none) ->
  write(filename:join(Dir, "memory.max"), "max");
set_limit({cgroup, Dir, _}, Bytes) when is_integer(Bytes) ->
  write(filename:join(Dir, "memory.max"), integer_to_list(Bytes));
set_limit({proc, _}, _) ->
  ok.

enforced_by_kernel({cgroup, _, _}) -> true;
enforced_by_kernel(_) -> false.

%% @doc Bytes currently used by the session's process tree.
-spec usage(handle()) -> non_neg_integer().
usage({cgroup, Dir, _}) ->
  read_int(filename:join(Dir, "memory.current"), 0);
usage({proc, undefined}) -> 0;
usage({proc, Pid}) -> tree_rss(Pid).

%% @doc How many times the kernel OOM-killed a process of this session.
-spec oom_kills(handle()) -> non_neg_integer().
oom_kills({cgroup, Dir, _}) ->
  case file:read_file(filename:join(Dir, "memory.events")) of
    {ok, Bin} ->
      Lines = binary:split(Bin, <<"\n">>, [global, trim_all]),
      case [V || <<"oom_kill ", V/binary>> <- Lines] of
        [V | _] -> to_int(V, 0);
        [] -> 0
      end;
    _ -> 0
  end;
oom_kills(_) -> 0.

%% @doc The process tree right now, to be killed later even if the top
%% process exits first (its children are then re-parented away and can
%% not be found from the port's pid any more). The cgroup backend needs
%% no snapshot: cgroup.kill reaches every member.
-spec snapshot(handle()) -> [integer()].
snapshot({proc, Pid}) when is_integer(Pid) -> descendants(Pid);
snapshot(_) -> [].

%% @doc Kill the whole process tree: the snapshot taken earlier plus, if
%% the port is still open (so its recorded OS pid is still the tool's
%% and not a reused number), whatever is below that pid now.
-spec kill(handle(), boolean(), [integer()]) -> ok.
kill({cgroup, Dir, Pid}, PortOpen, Snapshot) ->
  case write(filename:join(Dir, "cgroup.kill"), "1") of
    ok -> ok;
    {error, _} ->
      %% Kernel older than 5.14 has no cgroup.kill: kill what is listed.
      Listed = read_pids(filename:join(Dir, "cgroup.procs")),
      kill_pids(lists:usort(Listed ++ Snapshot ++ live_tree(Pid, PortOpen)))
  end;
kill({proc, Pid}, PortOpen, Snapshot) ->
  kill_pids(lists:usort(Snapshot ++ live_tree(Pid, PortOpen))).

live_tree(Pid, true) when is_integer(Pid) -> descendants(Pid);
live_tree(_, _) -> [].

%% @doc Release what prepare/0 reserved.
-spec detach(handle()) -> ok | {error, term()}.
detach({cgroup, Dir, _}) -> remove_cgroup(Dir, 20);
detach(_) -> ok.

%% The directory can only go once every process in it is gone, which
%% takes a moment after cgroup.kill.
remove_cgroup(Dir, 0) ->
  logging:warn("Could not remove session cgroup ~s", [Dir]),
  {error, ebusy};
remove_cgroup(Dir, Tries) ->
  case file:del_dir(Dir) of
    ok -> ok;
    {error, enoent} -> ok;
    {error, E} when E =:= eexist; E =:= enotempty ->
      %% Regular files inside: not a real cgroup (tests use a plain dir).
      case file:del_dir_r(Dir) of
        ok -> ok;
        _ -> timer:sleep(50), remove_cgroup(Dir, Tries - 1)
      end;
    {error, _} ->
      timer:sleep(50),
      remove_cgroup(Dir, Tries - 1)
  end.

%%--------------------------------------------------------------------
%% /proc process trees
%%--------------------------------------------------------------------

%% @doc RootPid and every process below it, parents before children.
-spec descendants(integer()) -> [integer()].
descendants(RootPid) when is_integer(RootPid) ->
  walk([RootPid], children_map(), []).

walk([], _, Acc) -> lists:reverse(Acc);
walk([P | Rest], Children, Acc) ->
  walk(maps:get(P, Children, []) ++ Rest, Children, [P | Acc]).

children_map() ->
  case file:list_dir("/proc") of
    {ok, Entries} ->
      lists:foldl(
        fun(Entry, Acc) ->
            case string:to_integer(Entry) of
              {Pid, []} ->
                case ppid(Pid) of
                  undefined -> Acc;
                  PPid -> maps:update_with(PPid, fun(L) -> [Pid | L] end, [Pid], Acc)
                end;
              _ -> Acc
            end
        end, #{}, Entries);
    _ -> #{}
  end.

%% Parent pid from /proc/PID/stat: "pid (comm) state ppid ..." where
%% comm may itself contain spaces and parentheses, hence the last ")".
ppid(Pid) ->
  case file:read_file("/proc/" ++ integer_to_list(Pid) ++ "/stat") of
    {ok, Bin} -> stat_ppid(Bin);
    _ -> undefined
  end.

%% @doc Parent pid from the content of a /proc/PID/stat file.
-spec stat_ppid(binary()) -> integer() | undefined.
stat_ppid(Bin) ->
  case binary:matches(Bin, <<")">>) of
    [] -> undefined;
    Matches ->
      {Pos, _} = lists:last(Matches),
      Rest = binary:part(Bin, Pos + 1, byte_size(Bin) - Pos - 1),
      case binary:split(Rest, <<" ">>, [global, trim_all]) of
        [_State, PPid | _] -> to_int(PPid, undefined);
        _ -> undefined
      end
  end.

%% @doc Sum of the resident set sizes (bytes) of a process tree.
-spec tree_rss(integer()) -> non_neg_integer().
tree_rss(RootPid) ->
  lists:sum([rss(P) || P <- descendants(RootPid)]).

rss(Pid) ->
  case file:read_file("/proc/" ++ integer_to_list(Pid) ++ "/status") of
    {ok, Bin} ->
      Lines = binary:split(Bin, <<"\n">>, [global, trim_all]),
      case [V || <<"VmRSS:", V/binary>> <- Lines] of
        [V | _] ->
          case binary:split(string:trim(V), <<" ">>) of
            [KB | _] -> to_int(KB, 0) * 1024;
            _ -> 0
          end;
        [] -> 0
      end;
    _ -> 0
  end.

%% @doc SIGKILL the given OS pids, never this VM or init.
-spec kill_pids([integer()]) -> ok.
kill_pids(Pids0) ->
  Self = list_to_integer(os:getpid()),
  Pids = [P || P <- Pids0, is_integer(P), P > 1, P =/= Self],
  case Pids of
    [] -> ok;
    _ ->
      logging:debug("Sending SIGKILL to ~p", [Pids]),
      os:cmd("kill -9 " ++ lists:join(" ", [integer_to_list(P) || P <- Pids])),
      ok
  end.

%%--------------------------------------------------------------------
%% Small file helpers
%%--------------------------------------------------------------------

write(Path, Data) ->
  file:write_file(Path, Data).

ensure_dir(Dir) ->
  case file:make_dir(Dir) of
    ok -> ok;
    {error, eexist} -> ok;
    Error -> Error
  end.

read_int(Path, Default) ->
  case file:read_file(Path) of
    {ok, Bin} -> to_int(string:trim(Bin), Default);
    _ -> Default
  end.

read_pids(Path) ->
  case file:read_file(Path) of
    {ok, Bin} ->
      [P || L <- binary:split(Bin, <<"\n">>, [global, trim_all]),
            P <- [to_int(string:trim(L), undefined)], is_integer(P)];
    _ -> []
  end.

to_int(Bin, Default) when is_binary(Bin) ->
  try binary_to_integer(Bin) catch _:_ -> Default end;
to_int(_, Default) -> Default.
