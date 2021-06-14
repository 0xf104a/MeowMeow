%%%-------------------------------------------------------------------
%%% @author p01ar
%%% @copyright (C) 2020, Polar Group
%%% @doc
%%%   Configuration storage
%%% @end
%%% Created : 30. сент. 2020 13:31
%%%-------------------------------------------------------------------
-module(configuration).
-author("p01ar").

%% API
-export([load/0, get/1, get/2]).

-include("config.hrl").

parse_line(eof) -> eof;
parse_line("") -> {ok, #{}};
parse_line({ok, RLine}) ->
  %%io:fwrite("RLine=~p~n", [RLine]),
  Line = lists:nth(1, string:split(string:trim(RLine), "#")),
  if length(Line)>1 ->
        [Cmd | Args] = string:split(Line, " "),
        %%io:fwrite("Args=~s~n",[Args]),
        {ok, #{Cmd => string:trim(Args)}};
     true -> {ok, #{}}
  end.
     

parse2map(Dev, Map) ->
  RawLine = file:read_line(Dev),
  case parse_line(RawLine) of
    eof ->
      {ok, Map};
    {ok, NewMap} ->
      parse2map(Dev, maps:merge(Map, NewMap))
  end.

load() ->
  {ok, Dev} = file:open(?configfile, read),
  ets:new(config_storage, [set, named_table]),
  {ok, Config} = parse2map(Dev, ?defconf),
  true = ets:insert(config_storage, {table, Config}),
  logging:info("Loading config from ~s", [?configfile]),
  ok.


get(VarName, string) ->
  [{table, Map}] = ets:lookup(config_storage, table),
  maps:get(VarName, Map);

get(VarName, bool) ->
  [{table, Map}] = ets:lookup(config_storage, table),
  StrValue = string:trim(string:lowercase(maps:get(VarName, Map))),
  if StrValue == "no" -> false;
     StrValue == "yes" -> true;
     true -> logging:err("Can not parse config: ~s option requires Yes/No values, but got ~p", []),
             {error, parse_error}
  end; 

get(VarName, int) ->
  [{table, Map}] = ets:lookup(config_storage, table),
  %%io:fwrite("VarName=~s,Map=~p,tp=int~n", [VarName, Map]),
  Arg=maps:get(VarName, Map),
  list_to_integer(Arg).

get(VarName) -> get(VarName, string).
