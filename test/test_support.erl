%%%-------------------------------------------------------------------
%%% @doc
%%%  Shared helpers for the EUnit suites.
%%%
%%%  Most MeowMeow modules depend on process-less global state:
%%%  the `config_storage', `rules', `rules_context' and `access' ETS
%%%  tables.  ETS tables die with their owner, so every suite creates
%%%  them inside an EUnit `setup' fixture (which runs setup, tests and
%%%  cleanup in one dedicated process) and drops them in cleanup.
%%%
%%%  The helpers here also build realistic `#request{}' records by
%%%  pushing raw HTTP bytes through `parse_http:update_request/2' - the
%%%  same path the socket handler uses - so unit tests see exactly the
%%%  record shape the production code sees.
%%% @end
%%%-------------------------------------------------------------------
-module(test_support).

-include("../src/config.hrl").
-include("../src/request.hrl").
-include("../src/response.hrl").

%% 'DOWN' messages are left in the mailbox for the test that monitors.
-define(IS_DOWN(M), (is_tuple(M) andalso tuple_size(M) == 5 andalso element(1, M) == 'DOWN')).

-export([setup_config/0, setup_config/1, set_config/2,
         setup_rules/0, setup_access/1, set_access/1, setup_mcp/0,
         setup_all/0, setup_all/1,
         teardown_all/1, default_config/0,
         raw_request/4, request/2, request/3, request/4, response_for/1,
         response_for/2, flush/0, collect/1, recv/1, parse_response/1,
         socket_pair/0, gen_tcp_pair/0, tmp_dir/0, rm_rf/1, write_file/2,
         capture_io/1, stub_server_abort_init/1, restore_server/0,
         load_source/2, wait_until/2, wait_until/3, is_alive/1,
         tool/1, recv_all/2, cleanup_mcp_sessions/0,
         spawn_collector/0, collected/1, stop_collector/1]).

%%--------------------------------------------------------------------
%% Global-state fixtures
%%--------------------------------------------------------------------

default_config() -> ?defconf.

%% @doc Create the `config_storage' ETS table with the compiled-in
%% defaults (see src/config.hrl) merged with Overrides.
%%
%% EUnit runs every test in its own process while the fixture (setup)
%% process owns the tables, so the tables are created `public' here;
%% the production code reads them from arbitrary processes anyway.
setup_config() -> setup_config(#{}).
setup_config(Overrides) ->
  new_public(config_storage),
  true = ets:insert(config_storage, {table, maps:merge(?defconf, Overrides)}),
  ok.

set_config(Key, Value) ->
  [{table, Map}] = ets:lookup(config_storage, table),
  true = ets:insert(config_storage, {table, maps:put(Key, Value, Map)}),
  ok.

%% @doc Same tables rules:init_rules/0 creates, but public, then the
%% built-in rules are registered exactly like the server does.
setup_rules() ->
  new_public(rules),
  new_public(rules_context),
  rules:register_basic(),
  ok.

%% @doc Create the `access' table holding an already-parsed rule list
%% (what access:parse_access/1 would have produced).
setup_access(RuleList) ->
  new_public(access),
  true = ets:insert(access, {table, RuleList}),
  ok.

set_access(RuleList) ->
  true = ets:insert(access, {table, RuleList}),
  ok.

%% @doc The session table mcp_port:init_mcp/0 creates (already public).
setup_mcp() ->
  case ets:info(mcp_sessions) of
    undefined -> ok = mcp_port:init_mcp();
    _ -> ok
  end.

setup_all() -> setup_all(#{}).
setup_all(Overrides) ->
  setup_config(Overrides),
  setup_rules(),
  setup_access([]),
  ok.

teardown_all(_) ->
  lists:foreach(fun(T) -> catch ets:delete(T) end,
                [config_storage, rules, rules_context, access,
                 mcp_sessions, mcp_route_sessions]),
  ok.

new_public(Name) ->
  case ets:info(Name) of
    undefined -> ets:new(Name, [set, public, named_table]);
    _ -> ets:delete_all_objects(Name)
  end,
  ok.

%%--------------------------------------------------------------------
%% Request / response builders
%%--------------------------------------------------------------------

raw_request(Method, Route, Headers, Body) ->
  lists:flatten([Method, " ", Route, " HTTP/1.1\r\n",
                 [[K, ": ", V, "\r\n"] || {K, V} <- Headers],
                 "\r\n", Body]).

request(Method, Route) -> request(Method, Route, []).
request(Method, Route, Headers) -> request(Method, Route, Headers, "").
%% @doc Build a #request{} exactly the way handle:handler/3 does: the
%% raw bytes arrive as one binary and go through update_request/2.
request(Method, Route, Headers, Body) ->
  Raw = list_to_binary(raw_request(Method, Route, Headers, Body)),
  Req = parse_http:update_request(parse_http:make_request({127, 0, 0, 1}), Raw),
  case Req of
    #request{} -> Req;
    Other -> erlang:error({request_builder_failed, Other})
  end.

%% @doc Build the #response{} handle:handle_by_method/3 creates before
%% running the rule chain. Upstream defaults to the calling process so
%% tests can observe what rules send "to the socket".
response_for(Request) -> response_for(Request, self()).
response_for(Request, Upstream) ->
  #response{socket = fake_socket, code = 200, request = Request,
            upstream = Upstream, headers = ?base_headers}.

%%--------------------------------------------------------------------
%% Mailbox helpers (tests act as the fake upstream / handler)
%%--------------------------------------------------------------------

flush() ->
  receive _ -> flush() after 0 -> ok end.

%% @doc Collect every message that arrives until Timeout ms of silence.
%% EUnit runs all tests of a fixture in one process, so 'DOWN' messages
%% from monitors set up by earlier tests may arrive at any time; they are
%% skipped here (tests that care about them receive on the monitor ref).
collect(Timeout) -> collect(Timeout, []).
collect(Timeout, Acc) ->
  receive
    M when not ?IS_DOWN(M) -> collect(Timeout, [M | Acc])
  after Timeout -> lists:reverse(Acc)
  end.

recv(Timeout) ->
  receive
    M when not ?IS_DOWN(M) -> {ok, M}
  after Timeout -> timeout
  end.

%% @doc A process that records every message it receives; used as the
%% fake upstream when the code under test itself blocks in `receive'
%% (e.g. mcp_streamable:wait_for_sse/3), so that upstream traffic does
%% not land in the mailbox the code under test is reading.
spawn_collector() -> spawn(fun() -> collector_loop([]) end).

collector_loop(Acc) ->
  receive
    {get_collected, From} -> From ! {collected, lists:reverse(Acc)}, collector_loop(Acc);
    stop_collector -> ok;
    M -> collector_loop([M | Acc])
  end.

collected(Pid) ->
  Pid ! {get_collected, self()},
  receive {collected, L} -> L after 1000 -> erlang:error(collector_timeout) end.

stop_collector(Pid) -> Pid ! stop_collector, ok.

%% @doc Parse a wire-format HTTP/1.1 response into {Code, HeadersMap, Body}.
parse_response(IoData) ->
  Str = binary_to_list(iolist_to_binary(IoData)),
  case string:split(Str, "\r\n\r\n") of
    [Head, Body] ->
      [StatusLine | HeaderLines] = string:split(Head, "\r\n", all),
      [_Ver, CodeStr | _] = string:split(StatusLine, " ", all),
      Headers = maps:from_list(
                  [begin [K, V] = string:split(L, ":"), {K, string:trim(V)} end
                   || L <- HeaderLines, L =/= ""]),
      {list_to_integer(CodeStr), Headers, Body};
    _ ->
      {error, {no_header_terminator, Str}}
  end.

%%--------------------------------------------------------------------
%% Sockets
%%--------------------------------------------------------------------

%% @doc A connected pair: server side is a `socket' module socket (what
%% server.erl hands to the handlers), client side is a passive gen_tcp
%% socket the test can read/write with.
socket_pair() ->
  {ok, L} = socket:open(inet, stream, tcp),
  ok = socket:bind(L, #{family => inet, addr => {127, 0, 0, 1}, port => 0}),
  ok = socket:listen(L),
  {ok, #{port := Port}} = socket:sockname(L),
  {ok, C} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}]),
  {ok, S} = socket:accept(L),
  socket:close(L),
  {S, C}.

%% @doc Read from a passive gen_tcp socket until Idle ms of silence or close.
recv_all(C, Idle) -> recv_all(C, Idle, <<>>).
recv_all(C, Idle, Acc) ->
  case gen_tcp:recv(C, 0, Idle) of
    {ok, D} -> recv_all(C, Idle, <<Acc/binary, D/binary>>);
    {error, _} -> Acc
  end.

%% @doc A connected pair of gen_tcp sockets (for the legacy sock module).
gen_tcp_pair() ->
  {ok, L} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
  {ok, Port} = inet:port(L),
  {ok, C} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}]),
  {ok, S} = gen_tcp:accept(L),
  gen_tcp:close(L),
  {S, C}.

%%--------------------------------------------------------------------
%% Files
%%--------------------------------------------------------------------

tmp_dir() ->
  Base = case os:getenv("TMPDIR") of
           false -> "/tmp";
           B -> B
         end,
  Dir = filename:join(Base, "meow_eunit_" ++ integer_to_list(erlang:unique_integer([positive]))),
  ok = filelib:ensure_path(Dir),
  Dir.

rm_rf(Dir) -> file:del_dir_r(Dir).

write_file(Path, Content) ->
  ok = filelib:ensure_dir(Path),
  ok = file:write_file(Path, Content),
  Path.

%%--------------------------------------------------------------------
%% IO capture (for the logging module)
%%--------------------------------------------------------------------

%% @doc Run Fun with the group leader replaced by a collector and return
%% {Result, Output} where Output is everything written with io:fwrite &co.
capture_io(Fun) ->
  Old = group_leader(),
  Self = self(),
  Collector = spawn(fun() -> io_collector(Self, []) end),
  group_leader(Collector, self()),
  Result = try Fun() after group_leader(Old, self()) end,
  Collector ! {get, self()},
  Out = receive {io_output, O} -> O after 2000 -> erlang:error(io_capture_timeout) end,
  {Result, Out}.

io_collector(Owner, Acc) ->
  receive
    {io_request, From, ReplyAs, Req} ->
      {Reply, NewAcc} = handle_io(Req, Acc),
      From ! {io_reply, ReplyAs, Reply},
      io_collector(Owner, NewAcc);
    {get, Pid} ->
      Pid ! {io_output, lists:flatten(lists:reverse(Acc))}
  end.

handle_io({put_chars, _Enc, Chars}, Acc) -> {ok, [chars(Chars) | Acc]};
handle_io({put_chars, _Enc, M, F, A}, Acc) -> {ok, [chars(apply(M, F, A)) | Acc]};
handle_io({put_chars, Chars}, Acc) -> {ok, [chars(Chars) | Acc]};
handle_io({put_chars, M, F, A}, Acc) -> {ok, [chars(apply(M, F, A)) | Acc]};
handle_io({requests, Reqs}, Acc) ->
  lists:foldl(fun(R, {_, A}) -> handle_io(R, A) end, {ok, Acc}, Reqs);
handle_io(_, Acc) -> {{error, request}, Acc}.

chars(Chars) ->
  case unicode:characters_to_list(Chars) of
    L when is_list(L) -> L;
    _ -> binary_to_list(iolist_to_binary(Chars))
  end.

%%--------------------------------------------------------------------
%% Module stubbing (server:abort_init/1 calls init:stop/1 which would
%% halt the test VM, so suites covering those error paths swap the
%% `server' module for a stub that reports the call instead).
%%--------------------------------------------------------------------

stub_server_abort_init(Collector) when is_pid(Collector) ->
  Src = lists:flatten(io_lib:format(
    "-module(server).~n"
    "-export([abort_init/1]).~n"
    "abort_init(Reason) -> ~s ! {abort_init, Reason}, ok.~n",
    [pid_src(Collector)])),
  load_source(server, Src).

pid_src(Pid) ->
  "list_to_pid(\"" ++ pid_to_list(Pid) ++ "\")".

restore_server() ->
  code:purge(server),
  code:delete(server),
  code:purge(server),
  {module, server} = code:load_file(server),
  ok.

%% @doc Compile Erlang source text and load it as Mod.
load_source(Mod, Src) ->
  {ok, Tokens, _} = erl_scan:string(Src),
  Forms = parse_forms(split_forms(Tokens, [], [])),
  {ok, Mod, Bin} = compile:forms(Forms, [return_errors]),
  code:purge(Mod),
  {module, Mod} = code:load_binary(Mod, atom_to_list(Mod) ++ ".erl", Bin),
  ok.

split_forms([], [], Acc) -> lists:reverse(Acc);
split_forms([], Cur, Acc) -> lists:reverse([lists:reverse(Cur) | Acc]);
split_forms([{dot, _} = D | T], Cur, Acc) ->
  split_forms(T, [], [lists:reverse([D | Cur]) | Acc]);
split_forms([H | T], Cur, Acc) -> split_forms(T, [H | Cur], Acc).

parse_forms(TokenForms) ->
  [begin {ok, F} = erl_parse:parse_form(Ts), F end || Ts <- TokenForms].

%%--------------------------------------------------------------------
%% MCP: stop every subprocess started through mcp_port so suites do not
%% leak `cat' processes when a test fails half way.
%%--------------------------------------------------------------------
cleanup_mcp_sessions() ->
  case ets:info(mcp_sessions) of
    undefined -> ok;
    _ ->
      lists:foreach(
        fun({Id, {_, Pid}}) when is_pid(Pid) ->
              case is_process_alive(Pid) of
                true -> catch mcp_server:terminate_session(Pid);
                false -> ok
              end,
              catch ets:delete(mcp_sessions, Id);
           ({Id, _}) -> catch ets:delete(mcp_sessions, Id)
        end, ets:tab2list(mcp_sessions))
  end,
  case ets:info(mcp_route_sessions) of
    undefined -> ok;
    _ -> catch ets:delete_all_objects(mcp_route_sessions)
  end,
  catch mcp_admission:stop(),
  catch mcp_meter:reset(),
  ok.

%%--------------------------------------------------------------------
%% Misc
%%--------------------------------------------------------------------

wait_until(Fun, Timeout) -> wait_until(Fun, Timeout, 10).
wait_until(Fun, Timeout, _Step) when Timeout =< 0 -> Fun();
wait_until(Fun, Timeout, Step) ->
  case Fun() of
    true -> true;
    _ -> timer:sleep(Step), wait_until(Fun, Timeout - Step, Step)
  end.

is_alive(Pid) when is_pid(Pid) -> erlang:is_process_alive(Pid);
is_alive(_) -> false.

%% @doc Absolute path of a shell tool used as a fake MCP subprocess.
tool(Name) ->
  case os:find_executable(Name) of
    false -> erlang:error({tool_not_found, Name});
    Path -> Path
  end.
