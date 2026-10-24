%%%-------------------------------------------------------------------
%%% @doc EUnit suite for mcp_port.erl (MCP session registry)
%%% Sessions are backed by a real subprocess; `cat' is used as the tool.
%%% @end
%%%-------------------------------------------------------------------
-module(mcp_port_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  test_support:setup_config(#{"LogLevel" => "0"}),
  test_support:setup_mcp(),
  ok.

cleanup(_) ->
  test_support:cleanup_mcp_sessions(),
  test_support:teardown_all(ok).

cat() -> test_support:tool("cat").

mcp_port_test_() ->
  {setup, fun setup/0, fun cleanup/1,
   fun(_) -> [table_tests(), id_tests(), session_tests()] end}.

table_tests() ->
  [{"init_mcp/0 creates a public named session table (other handlers must see sessions)",
    ?_assertEqual(public, ets:info(mcp_sessions, protection))}].

id_tests() ->
  [{"generate_session_id/0 returns a binary (callers use binary_to_list/1 on it)",
    ?_assert(is_binary(mcp_port:generate_session_id()))},
   {"generate_session_id/0 encodes 64 random bytes (88 base64 characters)",
    ?_assertEqual(88, byte_size(mcp_port:generate_session_id()))},
   {"generate_session_id/0 only yields the letters A-Y (clamp_visible maps every byte to $A + Byte rem 25)",
    ?_assert(lists:all(fun(C) -> C >= $A andalso C =< $Y end,
                       binary_to_list(mcp_port:generate_session_id())))},
   {"generate_session_id/1 honours the byte length",
    ?_assertEqual(16, byte_size(mcp_port:generate_session_id(12)))},
   {"two session ids differ",
    ?_assertNotEqual(mcp_port:generate_session_id(), mcp_port:generate_session_id())}].

session_tests() ->
  [{"start_mcp_session/3 returns {ok, Id} and {error, busy} once the caps are reached",
    {timeout, 10, ?_test(begin
       test_support:set_config("MCPMaxSessions", "1"),
       test_support:set_config("MCPSessionWaitMs", "0"),
       try
         {ok, Id} = mcp_port:start_mcp_session(cat(), 5000, #{}),
         ?assert(is_binary(Id)),
         {ok, Pid} = mcp_port:get_mcp_session_by_id(Id),
         ?assertEqual({error, busy}, mcp_port:start_mcp_session(cat(), 5000, #{})),
         ok = mcp_port:terminate_mcp_session(Id),
         ?assertNot(is_process_alive(Pid))
       after
         test_support:set_config("MCPMaxSessions", "0"),
         test_support:set_config("MCPSessionWaitMs", "5000"),
         catch mcp_admission:stop()
       end
     end)}},
   {"start_mcp_session/2 starts the tool and registers it under a new id",
    {timeout, 10, ?_test(begin
       Id = mcp_port:start_mcp_session(cat(), 5000),
       ?assert(is_binary(Id)),
       Found = mcp_port:get_mcp_session_by_id(Id),
       %% The -spec says pid() | none but every caller pattern-matches
       %% {ok, Pid}, which is what is actually stored.
       ?assertMatch({ok, Pid} when is_pid(Pid), Found),
       {ok, Pid} = Found,
       ?assert(is_process_alive(Pid)),
       ok = mcp_server:terminate_session(Pid)
     end)}},
   {"get_mcp_session_by_id/1 of an unknown id is none",
    ?_assertEqual(none, mcp_port:get_mcp_session_by_id(<<"NOPE">>))},
   {"get_mcp_session_by_id/1 forgets a session whose process is gone and answers none (a session "
    "ends by keep-alive expiry, tool exit or mcp_server:terminate_session/1 without anyone removing "
    "its entry). (regression: this used to fail because the entry stayed in mcp_sessions forever, so "
    "the transports called gen_server:call/2 on a dead pid and the client got a 502 instead of the 404 the spec requires)",
    {timeout, 10, ?_test(begin
       Id = mcp_port:start_mcp_session(cat(), 5000),
       {ok, Pid} = mcp_port:get_mcp_session_by_id(Id),
       ok = mcp_server:terminate_session(Pid),
       ?assert(test_support:wait_until(fun() -> not is_process_alive(Pid) end, 2000)),
       ?assertEqual(none, mcp_port:get_mcp_session_by_id(Id)),
       ?assertEqual([], ets:lookup(mcp_sessions, Id))
     end)}},
   {"terminate_mcp_session/1 of an unknown id is not_found",
    ?_assertEqual(not_found, mcp_port:terminate_mcp_session(<<"NOPE">>))},
   {"terminate_mcp_session/1 stops the tool and forgets the session (its -spec says ok). "
    "(regression: this used to fail because it calls gen_server:call/2 on the stored {ok, Pid} tuple instead of the pid, "
    "and afterwards calls the non-existent ets:remove/2 (the function is ets:delete/2))",
    {timeout, 10, ?_test(begin
       Id = mcp_port:start_mcp_session(cat(), 5000),
       {ok, Pid} = mcp_port:get_mcp_session_by_id(Id),
       try
         ?assertEqual(ok, mcp_port:terminate_mcp_session(Id)),
         ?assertEqual(none, mcp_port:get_mcp_session_by_id(Id)),
         ?assert(test_support:wait_until(fun() -> not is_process_alive(Pid) end, 2000))
       after
         catch mcp_server:terminate_session(Pid)
       end
     end)}}].
