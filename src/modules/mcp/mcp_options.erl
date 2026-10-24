%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2026, Anna-Sofia Kasierocka
%%% @doc
%%%   Argument and option handling shared by the MCP transports.
%%%
%%%   Every `MCP-Tool-*' rule takes `"COMMAND" KEEPALIVE_MS' followed by
%%%   optional `Key=Value' options:
%%%
%%%     MaxSessions=N      at most N live sessions of this command
%%%     MemoryLimit=SIZE   per-session memory cap (e.g. 256M), 0 = none
%%%
%%%   `meow.conf' supplies the defaults and the global knobs
%%%   (`MCPMaxSessions', `MCPSessionWaitMs', `MCPMemoryLimit',
%%%   `MCPMemoryPollMs', `MCPCgroup', `MCPCgroupRoot').
%%% @end
%%%-------------------------------------------------------------------
-module(mcp_options).
-author("f104a").
-include("../../response.hrl").

-export([parse_rule_args/1, with_rule_args/2, session_opts/1,
         parse_size/1, format_size/1,
         config_int/2, config_bool/2, config_string/2,
         busy_response/2]).

%%--------------------------------------------------------------------
%% Rule arguments
%%--------------------------------------------------------------------

%% @doc Parse the argument list of an `MCP-Tool-*' rule.
-spec parse_rule_args([string()]) ->
        {ok, Tool :: string(), KeepAliveMs :: integer(), Opts :: map()} |
        {error, term()}.
parse_rule_args([Tool, KeepAlive | Rest]) when is_list(Tool), is_list(KeepAlive) ->
  case string:to_integer(string:trim(KeepAlive)) of
    {Ms, []} when is_integer(Ms), Ms > 0 ->
      case parse_options(Rest, #{}) of
        {ok, Opts} -> {ok, Tool, Ms, Opts};
        Error -> Error
      end;
    _ -> {error, {bad_keepalive, KeepAlive}}
  end;
parse_rule_args(Args) ->
  {error, {bad_arguments, Args}}.

parse_options([], Opts) -> {ok, Opts};
parse_options([Arg | Rest], Opts) when is_list(Arg) ->
  case string:split(Arg, "=") of
    [Key, Value] ->
      case parse_option(string:lowercase(string:trim(Key)), string:trim(Value)) of
        {ok, K, V} -> parse_options(Rest, Opts#{K => V});
        Error -> Error
      end;
    _ -> {error, {bad_option, Arg}}
  end;
parse_options([Arg | _], _) ->
  {error, {bad_option, Arg}}.

parse_option("maxsessions", Value) ->
  case string:to_integer(Value) of
    {N, []} when N >= 0 -> {ok, max_sessions, N};
    _ -> {error, {bad_option_value, "MaxSessions", Value}}
  end;
parse_option("memorylimit", Value) ->
  case parse_size(Value) of
    {ok, Size} -> {ok, memory_limit, Size};
    error -> {error, {bad_option_value, "MemoryLimit", Value}}
  end;
parse_option(Key, _Value) ->
  {error, {unknown_option, Key}}.

%% @doc Run Fun(Tool, KeepAliveMs, Opts) with the parsed arguments, or
%% answer 500 when the rule is misconfigured (the error is logged).
with_rule_args(Args, Fun) ->
  case parse_rule_args(Args) of
    {ok, Tool, KeepAliveMs, Opts} -> Fun(Tool, KeepAliveMs, Opts);
    {error, Reason} ->
      logging:err("Bad MCP-Tool rule arguments ~p: ~p", [Args, Reason]),
      {aborted, 500}
  end.

%%--------------------------------------------------------------------
%% Effective per-session options (rule options over meow.conf defaults)
%%--------------------------------------------------------------------

%% @doc The options mcp_server:init/1 works with:
%%   global_max   - total number of live sessions (infinity = no cap)
%%   tool_max     - live sessions started with the same command
%%   wait_ms      - how long a session start may wait for a free slot
%%   max_waiting  - how many starts may wait at the same time
%%   memory_limit - bytes, or none
%%   poll_ms      - memory sampling period, 0 disables sampling
-spec session_opts(map()) -> map().
session_opts(RuleOpts) when is_map(RuleOpts) ->
  GlobalMax = cap(config_int("MCPMaxSessions", 0)),
  ToolMax = cap(maps:get(max_sessions, RuleOpts, 0)),
  DefaultLimit = case parse_size(config_string("MCPMemoryLimit", "0")) of
                   {ok, L} -> L;
                   error ->
                     logging:err("MCPMemoryLimit in meow.conf is not a size, ignoring"),
                     none
                 end,
  MaxWaiting = case {GlobalMax, ToolMax} of
                 {infinity, infinity} -> 0;
                 {infinity, T} -> T;
                 {G, _} -> G
               end,
  #{global_max => GlobalMax,
    tool_max => ToolMax,
    wait_ms => max(0, config_int("MCPSessionWaitMs", 5000)),
    max_waiting => MaxWaiting,
    memory_limit => maps:get(memory_limit, RuleOpts, DefaultLimit),
    poll_ms => max(0, config_int("MCPMemoryPollMs", 1000))}.

cap(0) -> infinity;
cap(N) when is_integer(N), N > 0 -> N;
cap(_) -> infinity.

%%--------------------------------------------------------------------
%% Sizes
%%--------------------------------------------------------------------

%% @doc "0", "none" and "off" mean no limit; otherwise bytes with an
%% optional K/M/G suffix (binary multiples), e.g. "512M", "2G", "1048576".
-spec parse_size(string() | binary()) -> {ok, none | pos_integer()} | error.
parse_size(Bin) when is_binary(Bin) -> parse_size(binary_to_list(Bin));
parse_size(Str) when is_list(Str) ->
  case string:lowercase(string:trim(Str)) of
    "" -> {ok, none};
    "0" -> {ok, none};
    "none" -> {ok, none};
    "off" -> {ok, none};
    S ->
      case string:to_integer(S) of
        {N, Suffix} when is_integer(N), N > 0 ->
          case multiplier(Suffix) of
            error -> error;
            M -> {ok, N * M}
          end;
        _ -> error
      end
  end;
parse_size(_) -> error.

multiplier("") -> 1;
multiplier("b") -> 1;
multiplier("k") -> 1024;
multiplier("kb") -> 1024;
multiplier("kib") -> 1024;
multiplier("m") -> 1024 * 1024;
multiplier("mb") -> 1024 * 1024;
multiplier("mib") -> 1024 * 1024;
multiplier("g") -> 1024 * 1024 * 1024;
multiplier("gb") -> 1024 * 1024 * 1024;
multiplier("gib") -> 1024 * 1024 * 1024;
multiplier(_) -> error.

format_size(none) -> "none";
format_size(B) when B >= 1024 * 1024 * 1024 -> io_lib:format("~.1fG", [B / (1024 * 1024 * 1024)]);
format_size(B) when B >= 1024 * 1024 -> io_lib:format("~.1fM", [B / (1024 * 1024)]);
format_size(B) when B >= 1024 -> io_lib:format("~.1fK", [B / 1024]);
format_size(B) -> io_lib:format("~pB", [B]).

%%--------------------------------------------------------------------
%% meow.conf access that survives a missing table or value
%%--------------------------------------------------------------------

config_string(Key, Default) ->
  try configuration:get(Key, string) of
    V when is_list(V) -> V;
    _ -> Default
  catch _:_ -> Default
  end.

config_int(Key, Default) ->
  case string:to_integer(string:trim(config_string(Key, integer_to_list(Default)))) of
    {N, []} -> N;
    _ ->
      logging:err("~s in meow.conf is not an integer, using ~p", [Key, Default]),
      Default
  end.

config_bool(Key, Default) ->
  DefaultStr = case Default of true -> "yes"; false -> "no" end,
  case string:lowercase(string:trim(config_string(Key, DefaultStr))) of
    "yes" -> true;
    "no" -> false;
    _ ->
      logging:err("~s in meow.conf must be Yes or No, using ~p", [Key, Default]),
      Default
  end.

%%--------------------------------------------------------------------
%% 503 for a request that could not get a session slot
%%--------------------------------------------------------------------

%% @doc A `503 Service Unavailable' with `Retry-After' set from the wait
%% time, so clients back off instead of hammering the gateway.
busy_response(Response, Opts) ->
  WaitMs = maps:get(wait_ms, Opts, 0),
  RetryAfter = max(1, WaitMs div 1000),
  Body = list_to_binary(handle:get_abort_body(503)),
  Headers = maps:merge(Response#response.headers,
                       #{"Content-Type" => "text/html",
                         "Retry-After" => integer_to_list(RetryAfter)}),
  {ready2send, Response#response{is_ready2send = true, code = 503,
                                 headers = Headers, body = Body}}.
