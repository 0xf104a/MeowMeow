-module(parse_http).
-export([http2map/1, update_lines/2,
  is_request_finished/1, make_request/1, parse_request/2,
  is_close/1, get_header/2, get_header/3, ensure_body/1,
  update_request/2, parse_accept/1, sanitize_cors_unsafe/1,
  acceptable/2, acceptable_for_request/2, has_header/2,
  parse_query_parameters/1, get_query_parameter/3]).
-import(util, [sget2/2]).
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

%% @doc
%%  Tells whether client has sent off a request to server.
%%  In HTTP terms means whether "\r\n\r\n" received
%% @end
is_request_finished(Lines) when length(Lines) < 2 -> false;
is_request_finished(Lines) ->
  T0 = lists:nth(length(Lines), Lines),
  T1 = lists:nth(length(Lines) - 1, Lines),
  BodySize = length(binary_to_list(T0)),
  %%logging:debug("--is_request_finished variable dump--"),
  %%logging:debug("T0 = ~p",[T0]),
  %%logging:debug("T1 = ~p, ~p",[T1, T1==<<>>]),
  %%logging:debug("BodySize = ~p <= 0, ~p",[BodySize, BodySize > 0]),
  if (T0 == <<>>) and (T1 == <<>>) -> true;
    (T1 == <<>>) and (BodySize > 0) -> true;
    true -> false
  end.

make_request(SrcAddr) ->
  #request{src_addr = SrcAddr}.

parse_request(SrcAddr, Lines) ->
  XMap = http2map(Lines),
  %%logging:debug("XMap=~p", [XMap]),
  case XMap of
    {aborted, 400} ->
      logging:info("~p.~p.~p.~p -- 400 Bad Request", util:tup2list(SrcAddr)),
      bad_request;
    {ok, Map} ->
      Method = maps:get(method, Map),
      BinRoute = maps:get(route, Map),
      HttpVer = maps:get(http_ver, Map),
      Body = maps:get(body, Map),
      %%logging:debug("BinRoute=~p", [BinRoute]),
      SRoute = binary:split(BinRoute, <<"?">>),
      case SRoute of
        [CRoute] -> Route = CRoute, Params = "";
        [CRoute, CParams] -> Route = CRoute, Params = CParams
      end,
      {ok, #request{src_addr = SrcAddr, http_ver = HttpVer,
        route = Route, header = Map, method = Method, params = Params, body = Body}}
  end.

guard_str(<<>>) -> "";
guard_str([]) -> "";
guard_str(S) -> S.

guard_parse_lines(Request, Lines) ->
  Result = string:find(Lines, "\r\n"),
  case Result of
    nomatch -> Request#request{unfinished_line = guard_str(Lines)};
    _ -> parse_lines(Request, Lines)
  end.

unfinished_body(Request, Tail) ->
  string:concat(util:bin2str(Request#request.unfinished_line), util:bin2str(Tail)).

first_or([], Default) -> Default;
first_or(List, _) ->
  lists:nth(1, List).

parse_route(Route) ->
  [R | P] = string:split(Route, "?"),
  ARoute = guard_str(R),
  Params = guard_str(first_or(P, "")),
  {ARoute, Params}.

parse_lines(Request, []) -> Request;
parse_lines(Request, [[]]) -> Request;
parse_lines(Request, Lines) ->
  [L, T] = string:split(Lines, "\r\n"),
  %%logging:debug("L=~p, Lines=~p",[L, Lines]),
  case Request#request.route of
    nil ->
      Header = string:trim(L),
      Params = string:split(Header, " ", all),
      if length(Params) /= 3 -> {aborted, 400};
        true -> {Route, GetParams} = parse_route(lists:nth(2, Params)),
          guard_parse_lines(Request#request{method = lists:nth(1, Params),
            route = Route,
            params = GetParams,
            http_ver = lists:nth(3, Params)}, T)
      end;
    _ ->
      case L of
        <<>> -> Request#request{is_headers_accepted = true, body = unfinished_body(Request, T)};
        [] -> Request#request{is_headers_accepted = true, body = unfinished_body(Request, T)};
        BLine ->
          Line = BLine,
          Params = string:split(Line, ":"),
          if length(Params) /= 2 ->
            logging:err("Bad header: ~p", [Params]),
            {aborted, 400};
            true ->
              [K, V] = string:split(Line, ":"),
              Headers = Request#request.header,
              guard_parse_lines(Request#request{header = maps:merge(Headers,
                #{string:to_lower(binary_to_list(K)) => string:trim(binary_to_list(V))})}, T)
          end
      end
  end.

update_request(Request, Lines) ->
  parse_lines(Request, string:concat(Request#request.unfinished_line, Lines)).

get_header(Header, Request, Default) ->
  Result = maps:get(string:to_lower(Header), Request#request.header, Default),
  IsString = is_list(Result),
  if IsString-> string:strip(Result);
     true -> Result
  end.

get_header(Header, Request) ->
  string:strip(maps:get(string:to_lower(Header), Request#request.header)).

-spec has_header(string(), any()) -> true | false.
has_header(Header, Request) ->
  maps:is_key(string:to_lower(Header), Request#request.header).

is_close(Request) ->
  %%logging:debug("Header=~p",[Request#request.header]),
  case get_header("Connection", Request, badheader) of
    badheader -> true;
    "" -> true;
    "close" -> true;
    "keep-alive" -> false;
    Any -> logging:err("Unrecognized connection type: ~p. Either a bug or protocol violation", [Any]),
      true
  end.

parse_q_token(QToken) ->
  case string:split(QToken, "=") of
    ["q", FloatValue] ->
      case string:to_float(FloatValue) of
        {Float, ""} -> {ok, Float};
        Any -> logging:err("Can not parse q-value: ~p", [Any]), error
      end;
    Any ->
      logging:err("Invalid q-value: ~p", [Any]),
      error
  end.

sanitize_cors_unsafe([], Acc) -> Acc;
sanitize_cors_unsafe(Line, Acc) ->
  [Character | Tail] = Line,
  if Character == 9 -> sanitize_cors_unsafe(Tail, Acc ++ [Character]);
    (Character < 25) or (Character >= 127) -> sanitize_cors_unsafe(Tail, Acc);
    true -> sanitize_cors_unsafe(Tail, Acc ++ [Character])
  end.

%% @doc
%% Sanitizes string of 0x0-0x19 and 0x7F characters, allowing all others and Tab(0x09)
%% @end
%% @param Line: string to sanitize
sanitize_cors_unsafe(Line) ->
  sanitize_cors_unsafe(Line, "").

parse_accept([], Acc) -> Acc;
parse_accept(Tokens, Acc) ->
  [Token | Tail] = Tokens,
  StrippedToken = string:strip(Token),
  case string:split(StrippedToken, ";") of
    [StrippedToken] -> parse_accept(Tail, Acc ++ [{StrippedToken, 1}]);
    [MimeToken, QToken] ->
      case parse_q_token(string:strip(QToken)) of
        {ok, Value} -> parse_accept(Tail, Acc ++ [{string:strip(MimeToken), Value}]);
        Any -> logging:err("Can not parse q token: ~p", [Any])
      end;
    Any ->
      logging:err("Invalid Accept token: ~p", [Any]),
      invalid
  end.

%% @doc
%% Parses Accept header value into list of form [{MimePatter, QValue}...]
%% @end
parse_accept(Line) ->
  Tokens = string:split(sanitize_cors_unsafe(Line), ","),
  parse_accept(Tokens, []).

%% @doc
%% Checks if give Target mime-type can be accept among given patterns.
%% @end
acceptable(_, []) -> false;
acceptable(Target, ParsedPatterns) ->
  [{Pattern, QValue} | Tail] = ParsedPatterns,
  ValidToWildcard = util:check_wildcard(Target, Pattern),
  if QValue =< 0 -> acceptable(Target, Tail);
    ValidToWildcard -> true;
    true -> acceptable(Target, Tail)
  end.

%% @doc
%% Checks if given What mime-type can be accepted by client which sent the request
%% @end
%% @param What: mime-time of request
%% @param Request: clients request
%% @see acceptable/2
acceptable_for_request(What, Request) ->
  HasAcceptHeader = maps:is_key("Accept", Request#request.header),
  if HasAcceptHeader ->
    Allowed = parse_accept(maps:get("Accept", Request#request.header)),
    acceptable(What, Allowed);
    true -> true
  end.

parse_query_params([], Accumulator) -> {ok, Accumulator};
parse_query_params(Pairs, Accumulator) ->
  [Pair | Tail] = Pairs,
  PairParsed = string:split(Pairs, "="),
  case PairParsed of
    [Key, Value] -> parse_query_params(Tail, maps:merge(Accumulator, #{Key => Value}));
    Any -> logging:err("Invalid query parameters pair: ~p(from ~s)", [Any, Pair]), {error, badpair}
  end.

%% @doc
%% Parses query parameters, returns them as map
%% @end
%% @param Request: request to parse parameters from
parse_query_parameters(Request) when length(Request#request.params) == 0 -> {ok, #{}};
parse_query_parameters(Request) ->
  parse_query_params(string:split(binary_to_list(Request#request.params), "&"), #{}).


%% @doc
%% Gets value of given query parameter or returns given default value if parameter is undefined.
%% @end
%% @param Key: key of parameter
%% @param Request: a request record to get query parameter
%% @param Default: default value to return if requested parameter is absent
get_query_parameter(Key, Request, Default) ->
  {ok, Result} = parse_query_parameters(Request),
  maps:get(Key, Result, Default).


