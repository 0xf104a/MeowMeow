-module(access).
-export([parse_access/1, load_access/1, get_rules/1, unload/0, reload/0]).
-include("config.hrl").

get_cmd("") -> pass;
get_cmd(Cmd) ->
  L = string:split(Cmd, " "),
  if length(L) == 0 -> pass;
    length(L) == 1 -> {lists:nth(1, L), true};
    length(L) > 1 -> [K | V] = L,
      {K, V}
  end.

parse_line(Dev, {ok, Line}) ->
  Cmd = get_cmd(lists:nth(1, string:split(string:trim(Line), "#"))),
  case Cmd of
    pass -> {ok, []};
    {"Section", true} -> {ok, []};
    {"Route", [Name]} -> {ok, [{Name, parse_section({ok, Dev}, [], Name)}]};
    {"End", _} -> finish;
    {Key, Value} -> {ok, [{Key, Value}]};
    Any -> logging:err("get_cmd/1 returned unexpected result ~p @ access:parse_line/2", [Any])
  end;

parse_line(_, eof) -> finish.

parse_section({ok, Dev}, R, SectionName) ->
  Line = file:read_line(Dev),
  case parse_line(Dev, Line) of
    {ok, Data} -> parse_section({ok, Dev}, R ++ Data, SectionName);
    finish -> R;
    Any -> logging:err("parse_line/2 retuned unexpected result: ~p @ access:parse_section/3", [Any])
  end.

parse_access(FName) ->
  Dev = file:open(FName, read),
  R = parse_section(Dev, [], global),
  file:close(Dev),
  R.

load_access(FName) ->
  logging:info("Loading access table from ~s", [FName]),
  Access = parse_access(FName),
  access = ets:new(access, [set, named_table]),
  logging:debug("Created ETS access table"),
  true = ets:insert(access, {table, Access}),
  ok.

unload() ->
  logging:info("Unloading access table"),
  ets:delete(access).

reload() ->
  logging:info("Reloading access table"),
  unload(),
  load_access(?accessfile).

get_rules(_, [], Rules) -> Rules;
get_rules(Route, Array, Rules) ->
  [{Pattern, List} | T] = Array,
  Stat = util:check_wildcard(Route, Pattern),
  if Stat -> get_rules(Route, T, Rules ++ List);
    true -> get_rules(Route, T, Rules)
  end.

get_rules(Route) ->
  [{table, Array}] = ets:lookup(access, table),
  get_rules(Route, Array, []).
