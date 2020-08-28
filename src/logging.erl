-module(logging).
-export([info/1, debug/1, warn/1, err/1, info/2, debug/2, warn/2, err/2]).
-define(GREEN, "\033[32m").
-define(BLUE, "\033[34m").
-define(ORANGE, "\033[33m").
-define(RED, "\033[31m").
-define(ENDC, "\033[39m\033[0m").
-define(BOLD, "\033[1m").

log_write(Level, Colour, Msg) ->
  io:fwrite("~s~s~s~s~s|~s:~s~s~n", [Colour, ?BOLD, Level, ?ENDC, ?BOLD, util:get_time(), ?ENDC, Msg]).

log_write(Level, Colour, MsgFmt, Args) ->
  Msg = lists:flatten(io_lib:format(MsgFmt, Args)),
  io:fwrite("~s~s~s~s~s|~s:~s~s~n", [Colour, ?BOLD, Level, ?ENDC, ?BOLD, util:get_time(), ?ENDC, Msg]).

info(Msg) ->
  log_write("INFO ", ?BLUE, Msg).

debug(Msg) ->
  log_write("DEBUG", ?GREEN, Msg).

warn(Msg) ->
  log_write("WARN ", ?ORANGE, Msg).

err(Msg) ->
  log_write("ERROR", ?RED, Msg).

info(Msg, Fmt) ->
  log_write("INFO ", ?BLUE, Msg, Fmt).

debug(Msg, Fmt) ->
  log_write("DEBUG", ?GREEN, Msg, Fmt).

warn(Msg, Fmt) ->
  log_write("WARN ", ?ORANGE, Msg, Fmt).

err(Msg, Fmt) ->
  log_write("ERROR", ?RED, Msg, Fmt).
