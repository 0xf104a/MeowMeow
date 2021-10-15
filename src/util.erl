-module(util).
-export([addr2str/1, get_http_ver_pair/1, str2addr/1, 
         get_time/0, wildcard2regex/1, check_wildcard/2, 
         tup2list/1, sget/2, sget2/2, pretty_addr/1,
         bin2str/1, get_addr/1, prettify_header_key/1,
         split_list/2, parse_options_line/1]).
-include("config.hrl").

%% This part of code converts wildcards to regex
%% It is required since erlang can not match wildcards to strings
%% Source: https://stackoverflow.com/questions/17077850/how-to-convert-a-wildcard-pattern-to-regex-in-erlang#17086860
replace(L) when is_list(L) -> lists:reverse(replace(L, wildcard(hd(L)))).

% take care of the first character
replace(L, W = {true, _}) -> replace(L, W, []);
replace(L, W = {false, _}) -> replace(L, W, [$^]).

% take care of the last character
replace([_], {true, R}, Res) -> R ++ Res;
replace([_], {false, R}, Res) -> [$$ | R] ++ Res;
% middle characters
replace([_ | Q], {_, R}, Res) -> replace(Q, wildcard(hd(Q)), R ++ Res).

wildcard($*) -> {true, [$*, $.]};
wildcard($?) -> {true, [$.]};
wildcard($.) -> {true, [$., $\\]};
wildcard(C) -> {false, [C]}.

wildcard2regex(Wildcard) ->
  Regex = replace(Wildcard),
  re:compile(Regex).

tup2list(T) -> tup2list(T, size(T), []).

tup2list(T, 0, Acc) -> Acc;
tup2list(T, N, Acc) -> tup2list(T, N - 1, [element(N, T) | Acc]).

check_wildcard(String, Wildcard) ->
  {ok, Regex} = wildcard2regex(Wildcard),
  case re:run(String, Regex) of
    {match, _} -> true;
    nomatch -> false
  end.
%% Gets standartized time for HTTP
get_time() ->
  Months = #{1 => "Jan",
    2 => "Feb",
    3 => "Mar",
    4 => "Apr",
    5 => "May",
    6 => "Jun",
    7 => "Jul",
    8 => "Aug",
    9 => "Sep",
    10 => "Oct",
    11 => "Nov",
    12 => "Dec"},
  Days = #{1 => "Mon",
    2 => "Tue",
    3 => "Wed",
    4 => "Thu",
    5 => "Fri",
    6 => "Sat",
    7 => "Sun"},
  {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:universaltime(),
  WeekDay = maps:get(calendar:day_of_the_week(Year, Month, Day), Days),
  lists:flatten(io_lib:format("~s, ~2..0w  ~s ~4..0w ~2..0w:~2..0w:~2..0w GMT", [WeekDay, Day, maps:get(Month, Months), Year, Hour, Minute, Second])).
%% Secure map get
sget(Key, Map) ->
  IsKey = maps:is_key(Key, Map),
  if IsKey ->
    maps:get(Key, Map);
    true ->
      {badkey, Key}
  end.

sget2(Key, Map) ->
  IsKey = maps:is_key(Key, Map),
  if IsKey ->
    maps:get(Key, Map);
    true ->
      ""
  end.
str2int(Str) ->
    case string:to_integer(Str) of
         {R, []} -> R;
         {R, <<>>} -> R;
         Any -> logging:err("Failed to parse to int: ~p", [Str]),
                {error, bad_arg}
    end.

str2addr(Str) ->
  IP = string:split(Str,".",all),
  if length(IP) == 4 ->
     {str2int(lists:nth(1,IP)),
      str2int(lists:nth(2,IP)),
      str2int(lists:nth(3,IP)),
      str2int(lists:nth(4,IP))};
     true -> logging:err("Failed to parse IP ~s",[Str])
  end.

get_http_ver_pair(Str) ->
   Ver = lists:nth(2,string:split(Str,"/")),
   [VerMajor, VerMinor] = string:split(Ver, "."),
   {str2int(VerMajor),str2int(VerMinor)}.

addr2str(Addr)->
   lists:flatten(io_lib:format("~p.~p.~p.~p",Addr)).

get_addr(Sock)->
  Result = socket:peername(Sock),
  case Result of
    {ok, AddrInfo} -> maps:get(addr, AddrInfo);
    Any -> logging:err("Failed to get peername: ~p @ util:get_addr/1",[Any]),
           {error, Any}
  end.
%% Safe conversion of binaries to strings
bin2str(<<>>) -> "";
bin2str([]) -> "";
bin2str(Bin) -> binary_to_list(Bin).

prettify_header_token(<<>>) -> "";
prettify_header_token(S) ->
  string:to_upper(string:slice(S,0,1))++string:slice(string:to_lower(S),1).

prettify_apply(S,[T]) -> S++prettify_header_token(T);
prettify_apply(S, A) ->
  [H|T] = A,
  prettify_apply(S++prettify_header_token(H)++"-",T).

prettify_header_key(K)->
  Klower = string:lowercase(string:trim(K)),
  Tokens = string:split(Klower, "-"),
  prettify_apply("",Tokens).

split_list([], _, SuperList, []) -> SuperList;
split_list([], _, SuperList, RList) -> SuperList++[RList];
split_list(List, Token, SuperList, RList) ->
  [H|T] = List,
  case H of 
    Token -> split_list(T, Token, SuperList++[RList], []);
    Any -> split_list(T, Token, SuperList, RList++[Any])
  end.

split_list(List, Token) ->
  split_list(List, Token, [],[]).

%% clean_list remove from list unneccessary tokens(like [] returned by string:split)
%% and returns new list without them
clean_list([], _, NewList) -> NewList;
clean_list(List, Token, NewList) ->
  [H|T] = List,
  case H of 
    Token -> clean_list(T, Token, NewList);
    Any -> clean_list(T, Token, NewList++[Any])
  end.

clean_list(List, Token) -> clean_list(List, Token, []).

parse_options_line(L) ->
  split_list(clean_list(string:split(L, "'", all), []), " ").

pretty_addr(Addr) ->
  lists:flatten(io_lib:format("~p.~p.~p.~p:~p", tup2list(maps:get(addr, Addr)) ++ [maps:get(port, Addr)])).
