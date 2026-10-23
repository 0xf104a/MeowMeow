%%%-------------------------------------------------------------------
%%% @doc EUnit suite for mcp.erl (the nya_module facade of the MCP gateway)
%%% mcp_supervisor.erl is marked Work-In-Progress and exports nothing,
%%% so it has no tests.
%%% @end
%%%-------------------------------------------------------------------
-module(mcp_tests).
-include_lib("eunit/include/eunit.hrl").

setup() -> test_support:setup_config(#{"LogLevel" => "0"}).

mcp_test_() ->
  {setup, fun setup/0, fun test_support:teardown_all/1,
   fun(_) ->
     [{"the module exports the three transport rules",
       ?_assertEqual(["MCP-Tool-Streamable", "MCP-Tool-SSE", "MCP-Tool-SSE-Single"],
                     [N || {N, _} <- mcp:get_custom_rules()])},
      {"every rule is a 2-arity fun",
       ?_assert(lists:all(fun({_, F}) -> is_function(F, 2) end, mcp:get_custom_rules()))},
      {"the module has no context rules", ?_assertEqual([], mcp:get_context_rules())},
      {"terminate/1 returns ok", ?_assertEqual(ok, mcp:terminate(state))},
      {"init/0 creates the session registry",
       ?_test(begin
                ?assertEqual(ok, mcp:init()),
                ?assertNotEqual(undefined, ets:info(mcp_sessions))
              end)},
      {"the rules delegate to the transport modules (unsupported method -> 405 from each)",
       ?_test(begin
                Req = test_support:request("PUT", "/mcp", [{"Host", "h"}, {"Mcp-Protocol-Version", "2025-11-25"}]),
                Resp = test_support:response_for(Req),
                [?assertEqual({aborted, 405}, F([test_support:tool("cat"), "5000"], Resp))
                 || {_, F} <- mcp:get_custom_rules()]
              end)}]
   end}.
