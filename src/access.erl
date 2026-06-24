-module(access).
-export([parse_access/1, load_access/1, get_rules/1, unload/0, reload/0, execute_context_rule/3]).
-include("config.hrl").
-include("request.hrl").

get_cmd("") -> pass;
get_cmd(Cmd) ->
  L = string:split(Cmd, " "),
  if length(L) == 0 -> pass;
    length(L) == 1 -> {string:trim(lists:nth(1, L)), true};
    length(L) > 1 -> [K | V] = L,
      Args = lists:nth(1, V),
      {K, util:parse_arguments(Args)}
  end.

include_file(FName) ->
  logging:debug("Including ~p", [FName]),
  Result = file:open(FName, read),
  case Result of
    {ok, _} -> {ok, parse_section(Result, [])};
    Error -> logging:err("Failed to open ~p: ~p @ access:include_file/1", [FName, Error]),
      {error, open}
  end.

is_context_rule(RuleName) ->
  ets:member(rules_context, RuleName).

is_custom_rule(RuleName) ->
  ets:member(rules, RuleName).

parse_custom_rule(Dev, Key, Args) ->
  IsContextRule = is_context_rule(Key),
  IsCustomRule = is_custom_rule(Key),
  if IsContextRule -> {ok, [{context, Key, Args, parse_section({ok, Dev}, [])}]};
     IsCustomRule -> {ok, [{Key, Args}]};
     true -> {error, Key}
  end.

parse_line(Dev, {ok, Line}) ->
  Cmd = get_cmd(string:trim(lists:nth(1, string:split(string:trim(Line), "#")))),
  case Cmd of
    pass -> {ok, []};
    {"Section", true} -> {ok, []};
    {"End", _} -> finish;
    {"Include", [FName]} -> include_file(FName);
    {error, Reason} -> server:abort_init(Reason),
      {error, Reason};
    {Key, Args} -> parse_custom_rule(Dev, Key, Args);
    Any -> logging:err("get_cmd/1 returned unexpected result ~p @ access:parse_line/2", [Any])
  end;

parse_line(_, eof) -> finish.

parse_section({ok, Dev}, R) ->
  Line = file:read_line(Dev),
  case parse_line(Dev, Line) of
    {ok, Data} -> parse_section({ok, Dev}, R ++ Data);
    {error, Err} -> server:abort_init(Err),
      {error, Err};
    finish -> R;
    Any -> logging:err("parse_line/2 retuned unexpected result: ~p @ access:parse_section/3", [Any])
  end.

parse_access(FName) ->
  Dev = file:open(FName, read),
  R = parse_section(Dev, []),
  %%logging:debug("R=~p",[R]),
  file:close(Dev),
  R.

load_access(FName) ->
  logging:info("Loading access table from ~s", [FName]),
  WrappedAccess = parse_access(FName),
  case WrappedAccess of
    {error, Err} -> logging:err("Refusing to load access due to error"),
      {error, Err};
    Access ->
      access = ets:new(access, [set, named_table]),
      logging:debug("Created ETS access table"),
      true = ets:insert(access, {table, Access}),
      ok
  end.

unload() ->
  logging:info("Unloading access table"),
  ets:delete(access).

reload() ->
  logging:info("Reloading access table"),
  unload(),
  load_access(?accessfile).

get_rules(_, [], Rules) -> Rules;
get_rules(Request, Array, Rules) ->
  [H | T] = Array,
  case H of
    {context, _, _, _} ->
      get_context_rules(Request, H, Rules, T);
    _ ->
      get_rules(Request, T, Rules ++ [H])
  end.

get_context_rules(Request, {context, Name, Args, RuleList}, Rules,  RulesTail) ->
  [{Name, ContextFun}] = ets:lookup(rules_context, Name),
  ContextPassed = ContextFun(Args, Request),
  if ContextPassed -> get_rules(Request, RulesTail, Rules ++ RuleList);
     true -> get_rules(Request, RulesTail, Rules)
  end.

execute_context_rule(Request, Name, Args) ->
  [{Name, ContextFun}] = ets:lookup(rules_context, Name),
  ContextFun(Args, Request).

get_rules(Request) ->
  [{table, Array}] = ets:lookup(access, table),
  get_rules(Request, Array, []).
