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
  buf = <<>>,
  event_cursor = 0,  %% monotonic int for SSE event IDs
  cmd,               %% the tool command line (admission key, logging)
  meter,             %% mcp_meter:handle() of the subprocess tree
  mem_limit = none,  %% bytes or none
  mem_poll_ms = 0,   %% memory sampling period, 0 = off
  mem_current = 0,   %% last sample, bytes
  mem_peak = 0,      %% highest sample, bytes
  oom_kills = 0,     %% kernel OOM kills seen so far (cgroup backend)
  kill_list = []     %% process tree captured before asking the tool to stop
}).



