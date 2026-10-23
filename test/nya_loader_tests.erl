%%%-------------------------------------------------------------------
%%% @doc EUnit suite for nya_loader.erl and the nya_module behaviour
%%% @end
%%%-------------------------------------------------------------------
-module(nya_loader_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  test_support:setup_config(#{"LogLevel" => "0",
                              "LoadModules" => "static,header,route,env"}),
  test_support:setup_rules(),
  ok.

nya_loader_test_() ->
  {setup, fun setup/0, fun test_support:teardown_all/1,
   fun(_) -> [load_tests(), behaviour_tests()] end}.

load_tests() ->
  [{"nya_load registers a module's context rules (env: If-Env, If-Not-Env, If-Env-Def)",
    ?_test(begin
             ?assertEqual({ok, env}, nya_loader:nya_load("env")),
             ?assert(ets:member(rules_context, "If-Env")),
             ?assert(ets:member(rules_context, "If-Not-Env")),
             ?assert(ets:member(rules_context, "If-Env-Def"))
           end)},
   {"nya_load registers a module's custom rules (static: Send-File, DocDir)",
    ?_test(begin
             ?assertEqual({ok, static}, nya_loader:nya_load("static")),
             ?assert(ets:member(rules, "Send-File")),
             ?assert(ets:member(rules, "DocDir"))
           end)},
   {"header provides the Host and If-Header context rules",
    ?_test(begin
             ?assertEqual({ok, header}, nya_loader:nya_load("header")),
             ?assert(ets:member(rules_context, "Host")),
             ?assert(ets:member(rules_context, "If-Header"))
           end)},
   {"route provides the Route context rule",
    ?_test(begin
             ?assertEqual({ok, route}, nya_loader:nya_load("route")),
             ?assert(ets:member(rules_context, "Route"))
           end)},
   {"mcp provides the three MCP-Tool-* rules and creates its session table",
    ?_test(begin
             ?assertEqual({ok, mcp}, nya_loader:nya_load("mcp")),
             ?assert(ets:member(rules, "MCP-Tool-Streamable")),
             ?assert(ets:member(rules, "MCP-Tool-SSE")),
             ?assert(ets:member(rules, "MCP-Tool-SSE-Single")),
             ?assertNotEqual(undefined, ets:info(mcp_sessions))
           end)},
   {"nya_load of a module that does not exist reports nofile",
    ?_assertEqual({error, nofile}, nya_loader:nya_load("no_such_module_xyz"))},
   {"nya_load of a module that is not a nya_module is rejected",
    ?_assertEqual({error, not_a_nya_module}, nya_loader:nya_load("lists"))},
   {"nya_load_all loads every module listed in LoadModules, in order",
    ?_assertEqual([{ok, static}, {ok, header}, {ok, route}, {ok, env}],
                  nya_loader:nya_load_all())}].

behaviour_tests() ->
  Callbacks = [{init, 0}, {get_custom_rules, 0}, {get_context_rules, 0}, {terminate, 1}],
  [{"is_nya_module recognises the behaviour attribute",
    ?_test(begin
             ?assert(nya_loader:is_nya_module(static)),
             ?assert(nya_loader:is_nya_module(env)),
             ?assertNot(nya_loader:is_nya_module(util))
           end)},
   {"nya_module declares init/0, get_custom_rules/0, get_context_rules/0 and terminate/1",
    ?_assertEqual([], Callbacks -- nya_module:behaviour_info(callbacks))},
   {"every shipped module implements all nya_module callbacks",
    ?_assertEqual([],
                  [{M, F, A} || M <- [static, header, route, env, mcp],
                                {module, _} <- [code:ensure_loaded(M)],
                                {F, A} <- Callbacks,
                                not erlang:function_exported(M, F, A)])},
   {"every shipped module's rule lists have the documented shape",
    ?_test(lists:foreach(
             fun(M) ->
               {module, M} = code:ensure_loaded(M),
               ?assertEqual(ok, M:init()),
               [?assert(is_list(N) andalso is_function(F, 2)) || {N, F} <- M:get_custom_rules()],
               [?assert(is_list(N) andalso is_function(F, 2)) || {N, F} <- M:get_context_rules()],
               ?assertEqual(ok, M:terminate(state))
             end, [static, header, route, env]))}].
