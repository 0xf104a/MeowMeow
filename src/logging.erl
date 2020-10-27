-module(logging).
-export([info/1, debug/1, warn/1, err/1, info/2, debug/2, warn/2, err/2]).
-define(GREEN, "\033[32m").
-define(BLUE, "\033[34m").
-define(ORANGE, "\033[33m").
-define(RED, "\033[31m").
-define(ENDC, "\033[39m\033[0m").
-define(BOLD, "\033[1m").

level2str(info) -> "INFO ";
level2str(debug) -> "DEBUG";
level2str(warn) -> "WARN ";
level2str(error) -> "ERROR".

get_verbosity(debug) -> 4;
get_verbosity(info) -> 3;
get_verbosity(warn) -> 2;
get_verbosity(error) -> 1.

log_write(Level, Colour, Msg) ->
  %%io:fwrite("Entered log()~n"),
  LogVerbose = get_verbosity(Level),
  LogLevel = configuration:get("LogLevel",int),
  if LogVerbose =< LogLevel ->
    io:fwrite("\r~s~s~s~s~s|~s:~s~s~n", [Colour, ?BOLD, level2str(Level), ?ENDC, ?BOLD, util:get_time(), ?ENDC, Msg]);
    true -> ok
  end.

log_write(Level, Colour, MsgFmt, Args) ->
  %%io:fwrite("Entered log()~n"),
  LogVerbose = get_verbosity(Level),
  LogLevel = configuration:get("LogLevel",int),
  Msg = lists:flatten(io_lib:format(MsgFmt, Args)),
  if LogVerbose =< LogLevel ->
    io:fwrite("\r~s~s~s~s~s|~s:~s~s~n", [Colour, ?BOLD, level2str(Level), ?ENDC, ?BOLD, util:get_time(), ?ENDC, Msg]);
    true -> ok
  end.

info(Msg) ->
  log_write(info, ?BLUE, Msg).

debug(Msg) ->
  log_write(debug, ?GREEN, Msg).

warn(Msg) ->
  log_write(warn, ?ORANGE, Msg).

err(Msg) ->
  log_write(error, ?RED, Msg).

info(Msg, Fmt) ->
  log_write(info, ?BLUE, Msg, Fmt).

debug(Msg, Fmt) ->
  log_write(debug, ?GREEN, Msg, Fmt).

warn(Msg, Fmt) ->
  log_write(warn, ?ORANGE, Msg, Fmt).

err(Msg, Fmt) ->
  log_write(error, ?RED, Msg, Fmt).
