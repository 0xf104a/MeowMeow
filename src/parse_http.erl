-module(parse_http).
-export([http2map/1, mime_by_ext/1, mime_by_fname/1, update_lines/2, 
         is_request_finished/1, make_request/1, parse_request/2, 
         is_close/1, get_header/2, get_header/3, ensure_body/1,
        update_request/2]).
-import(util,[sget2/2]).
-include("config.hrl").
-include("request.hrl").

param2map(List) ->
  if length(List) >= 2 ->
    #{binary_to_list(lists:nth(1, List)) => string:lowercase(binary_to_list(lists:nth(2, List)))};
    length(List) == 1 -> #{body => lists:nth(1, List)};
    true -> #{}
  end.

parse_params(Params, Parsed) ->
  if length(Params) < 1 -> Parsed;
    true -> CurParsed = string:split(lists:nth(1, Params), ":", all),
      parse_params(lists:delete(lists:nth(1, Params), Params), maps:merge(Parsed, param2map(CurParsed)))
  end.

http2map(Lines) ->
  Header = string:split(lists:nth(1, Lines), " ", all),
  if length(Header) < 3 ->
    logging:debug("Bad header: ~p from ~p", [Header, Lines]),
    {aborted, 400};
  true -> Params = lists:delete(lists:nth(1, Lines), Lines),
          Parsed = #{method => lists:nth(1, Header), route => lists:nth(2, Header), 
                     http_ver => lists:nth(3, Header), body => ""},
      {ok, parse_params(Params, Parsed)}
  end.

parse_mimes(Data, Index, Parsed) ->
  if Index > length(Data) -> Parsed;
    true -> Params = string:split(lists:nth(Index, Data), " ", all),
      if length(Params) < 2 -> parse_mimes(Data, Index + 1, Parsed);
        true ->
          RawType = lists:nth(1, Params),
          RawExts = lists:nth(2, Params),
          Exts = string:split(lists:nth(2, string:split(RawExts, "=", all)), ",", all),
          Type = lists:nth(2, string:split(RawType, "=", all)),
          parse_mimes(Data, Index + 1, maps:merge(Parsed, #{Type => Exts}))
      end
  end.

read_mimes(FName) ->
  {ok, File} = file:read_file(FName),
  Content = unicode:characters_to_list(File),
  Preprocessed = string:split(Content, "\n", all),
  parse_mimes(Preprocessed, 1, #{}).

find_mime(Ext, Data) ->
  if length(Data) < 1 -> "application/octet-stream";
    true -> [H | T] = Data,
      if H /= <<>> ->
        Head = binary_to_list(H),
        Params = string:split(Head, " "),
        RawType = lists:nth(1, Params),
        RawExts = lists:nth(2, Params),
        Exts = string:split(lists:nth(2, string:split(RawExts, "=", all)), ",", all),
        Type = lists:nth(2, string:split(RawType, "=", all)),
        Found = lists:member(Ext, Exts),
        if Found -> Type;
          true -> find_mime(Ext, T)
        end;
        true -> "application/octet-stream"
      end
  end.

mime_by_ext(Ext) ->
  FName = ?mime_types_file,
  {ok, File} = file:read_file(FName),
  Content = unicode:characters_to_list(File),
  Preprocessed = re:split(Content, "\n\n|\r\n|\n|\r|\r\n|\032", []),
  find_mime(Ext, Preprocessed).

mime_by_fname(FName) ->
  Splitted = string:split(FName, ".", all),
  mime_by_ext(lists:nth(length(Splitted), Splitted)).

magic_merge(Cat1, "", Cat2) ->
  Cat1 ++ Cat2;
magic_merge(Cat1, Any, Cat2) ->
  lists:sublist(Cat1, length(Cat1) - 1) ++ [Any] ++ lists:sublist(Cat2, 2, length(Cat2) - 1).
magic_merge(List1, List2) ->
%% Merges 2 lists with concatting last element from List1
%% and first element from List2
  Cat1 = lists:last(List1),
  Cat2 = lists:nth(1, List2),
  New = Cat1 ++ Cat2,
  magic_merge(List1, New, List2).

update_lines(OldLines, Raw) ->
  Lines = string:split(Raw, "\r\n", all),
  magic_merge(OldLines, Lines).

ensure_body(T1) -> 
  case T1 of
    <<_>> -> true;
    _ -> false
  end.

is_request_finished(Lines) when length(Lines) < 2 -> false;
is_request_finished(Lines) ->
  T0 = lists:nth(length(Lines), Lines),
  T1 = lists:nth(length(Lines) - 1, Lines),
  BodySize = length(binary_to_list(T0)),
  logging:debug("--is_request_finished variable dump--"),
  logging:debug("T0 = ~p",[T0]),
  logging:debug("T1 = ~p, ~p",[T1, T1==<<>>]),
  logging:debug("BodySize = ~p <= 0, ~p",[BodySize, BodySize > 0]),
  if (T0 == <<>>) and (T1 == <<>>) -> true;
     (T1 == <<>>) and (BodySize > 0) -> true;
     true -> false
  end.

make_request(SrcAddr) ->
  #request{src_addr = SrcAddr}.

parse_request(SrcAddr, Lines) ->
  XMap = http2map(Lines),
  logging:debug("XMap=~p", [XMap]),
  case XMap of
    {aborted, 400} ->
      logging:info("~p.~p.~p.~p -- 400 Bad Request", util:tup2list(SrcAddr)),
      bad_request;
    {ok, Map} ->
      Method = maps:get(method, Map),
      BinRoute = maps:get(route, Map),
      HttpVer = maps:get(http_ver, Map),
      Body = maps:get(body, Map),
      logging:debug("BinRoute=~p", [BinRoute]),
      SRoute = binary:split(BinRoute,<<"?">>),
      case SRoute of
           [CRoute] -> Route = CRoute, Params = "";
           [CRoute, CParams] -> Route = CRoute, Params = CParams
      end,
      {ok, #request{src_addr = SrcAddr, http_ver = HttpVer, 
                    route = Route, header = Map, method = Method, params = Params, body = Body}}
  end.

guard_str(<<>>) -> "";
guard_str([]) -> "";
guard_str("") -> "";
guard_str(S) -> S.

guard_parse_lines(Request, Lines)->
  Result = string:find(Lines, "\r\n"),
  case Result of
    nomatch -> Request#request{unfinished_line = guard_str(Lines)};
    _ -> parse_lines(Request, Lines)
  end.

unfinished_body(Request, Tail)->
  string:concat(util:bin2str(Request#request.unfinished_line),util:bin2str(Tail)).

parse_route(Route) ->
  [R|P] = string:split(Route, "?"),
  ARoute = guard_str(R),
  Params = guard_str(P),
  {ARoute, Params}.

parse_lines(Request, []) -> Request;
parse_lines(Request, [[]]) -> Request;
parse_lines(Request, Lines) ->
  [L, T] = string:split(Lines, "\r\n"), 
%%  logging:debug("L=~p, Lines=~p",[L, Lines]),
  case Request#request.route of
    nil -> 
      Header = string:trim(L),
      Params = string:split(Header, " ", all),
      if length(Params) /= 3 -> {aborted, 400};
         true-> {Route, GetParams} = parse_route(lists:nth(2,Params)),
                guard_parse_lines(Request#request{method=lists:nth(1,Params),
                                            route=Route,
                                            params=GetParams,
                                            http_ver=lists:nth(3,Params)}, T)
      end;
    _ -> 
      case L of
        <<>> -> Request#request{is_headers_accepted = true, body = unfinished_body(Request, T)};
        [] -> Request#request{is_headers_accepted = true, body = unfinished_body(Request, T)};
        "" -> Request#request{is_headers_accepted = true, body = unfinished_body(Request, T)};
        BLine ->
          Line = BLine,
          Params = string:split(Line, ":"),
          if length(Params) /= 2 ->
               logging:err("Bad header: ~p", [Params]),
               {aborted, 400};
             true->
               [K, V] = string:split(Line, ":"),
               Headers = Request#request.header,
               guard_parse_lines(Request#request{header = maps:merge(Headers, 
                                                                     #{binary_to_list(K)=>string:trim(binary_to_list(V))})}, T)
          end
      end
  end.

update_request(Request, Lines) ->
  parse_lines(Request, string:concat(Request#request.unfinished_line,Lines)).

is_close(Request) ->
  logging:debug("Header=~p",[Request#request.header]),
  case string:trim(sget2("Connection", Request#request.header)) of
    {badkey, "Connection"} -> true;
    "" -> true;
    "close" -> true;
    "keep-alive" -> false;
    Any -> logging:err("Unrecognized connection type: ~p. Either a bug or protocol violation",[Any]),
           true
  end.

get_header(Header, Request) ->
  Result = util:sget(Header, Request#request.header),
  case Result of
    {badkey, Key} -> 
      {no_header, Key};
    Value -> string:trim(Value)
  end.

get_header(Header, Request, int) ->
  list_to_integer(get_header(Header, Request)).



