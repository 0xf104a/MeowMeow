%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%  Work-In-Progress
%%% @end
%%%-------------------------------------------------------------------
-module(mcp_supervisor).
-author("f104a").

%% API
-export([]).
-record(mcp_supervisor_state, {
  mcp_tool_queue=[],
  max_tools=none,
  min_free_memory_mb=512
}).

supervise(State) ->
  receive
    {wait_for_start, From, Tool} -> pass;
    {tool_terminated, From, Tool} -> pass
  end.
