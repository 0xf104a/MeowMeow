%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2026, Anna-Sofia Kasierocka
%%% @doc
%%%  A static configuration and records for mcp module
%%% @end
%%%-------------------------------------------------------------------
-author("f104a").
-define(mcp_version, "2025-11-25").
-record(state, {
  port,          %% the stdio subprocess
  timer,
  keepalive_ms,
  streams = [],
  event_cursor = 0   %% monotonic int for SSE event IDs
}).



