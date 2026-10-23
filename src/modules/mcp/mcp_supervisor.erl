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
-export([supervise/1]).
-record(mcp_supervisor_state, {
  mcp_tool_queue=[],
  max_tools=none,
  min_free_memory_mb=512
}).

%% @doc Not wired in yet: accepts the planned messages and keeps its state.
supervise(#mcp_supervisor_state{} = State) ->
  receive
    {wait_for_start, _From, _Tool} -> supervise(State);
    {tool_terminated, _From, _Tool} -> supervise(State)
  end.
