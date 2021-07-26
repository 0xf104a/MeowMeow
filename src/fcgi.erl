%% FastCGI wrapper
-module(fcgi).
-author("p01ar").
-import(util,[sget2/2,pretty_addr/1]).
-export([fcgi_exec/2, fcgi_dir_exec/2]).
-include("response.hrl").
-include("request.hrl").
-include("config.hrl").

recover_tail("", [X])->X;
recover_tail(S,[])->S;
recover_tail(S,A)->
   [H|T]=A,
   recover_tail(S++"\r\n"++H, T).

do_headers([], Response)-> Response;
do_headers(Lines, Response) -> 
   [Line|Tail] = Lines,
   case Line of 
        [] -> {done, Response, recover_tail("",Tail)};
        L -> 
	  [K|V] = string:split(Line,":"),
          case K of 
               "Status" -> 
                       [Code|_] = string:split(string:trim(V)," "),
                       NewResponse=Response#response{code=list_to_integer(Code)},
                       do_headers(Tail, NewResponse);
               Key->
                       NewResponse=response:set_header(Response, Key, V),
                       do_headers(Tail, NewResponse)
          end
    end.
update_response(Out, Response)->
   Lines=string:split(Out,"\r\n",all),
   do_headers(Lines,Response). 

fcgi_send(Response) ->
  receive 
    {fastcgi_request_done, _, _} -> Response;
    {fast_cgi_done, _} -> Response;
    {fast_cgi_stdout, 600, Out} ->
      io_proxy:tcp_send(Response#response.socket, binary_to_list(Out)),
      fcgi_send(Response)
  end.

fcgi_proxy(FastCGIConnection,Response) ->
  Upstream = Response#response.upstream,
  logging:debug("FCGI: entered PROXY"),
  receive
    {fast_cgi_done, _} -> Upstream ! set_tmr,
                          ok;
    {fast_cgi_stdout, 600, ROut} ->
      Out=binary_to_list(ROut),
      io:format("Got: ~p~n", [Out]),
      Updated=update_response(Out, Response),
      logging:debug("FCGI: Updated = ~p",[Updated]),
      case Updated of
           {done, NewResponse, Tail}->
                 %%logging:debug("Sending ~p, then ~p",[NewResponse,Tail]),
                 io_proxy:tcp_send(Response#response.socket, response:do_response_headers(NewResponse)),
                 io_proxy:tcp_send(Response#response.socket, Tail),
                 fcgi_send(NewResponse),
                 Upstream ! set_tmr,
                 NewResponse#response{is_finished=true};
           NewResponse->
                 fcgi_proxy(FastCGIConnection, NewResponse)
      end;
    {fast_cgi_stderr, 600, Msg}->
      logging:err("FastCGI error: ~p",[Msg]),
      fcgi_proxy(FastCGIConnection, Response);
    Unhandled->
      logging:debug("Got unhandled FastCGI data: ~p @ fcgi:fcgi_proxy/2",[Unhandled]),
      io_proxy:tcp_send(Response#response.socket, handle:abort(502)),
      Response#response{code=502, is_finished=true}
  after
    5000 -> erl_fastcgi:close(FastCGIConnection)
  end,
  Response#response{is_finished=true}.

fcgi_get_content_length(Request) when Request#request.method == <<"POST">> ->
  parse_http:get_header("Content-Length", Request);
fcgi_get_content_length(_) -> "0".

fcgi_query_params(Request) ->
  Q = string:split(Request#request.route, "?"),
  if length(Q) == 1 -> "";
     true->
       Params = lists:nth(2, Q),
       case Params of
         [] -> "";
         Str -> Str
        end
  end.

fcgi_get_content_type(Request) ->
  case parse_http:get_header("Content-Type", Request) of
    {no_header, _} -> "";
    Value -> Value 
  end.

fcgi_get_body(Body) when length(Body) == 0 -> <<>>;
fcgi_get_body(Body) -> list_to_binary(Body).

fcgi_to_str([]) -> "";
fcgi_to_str(<<>>) -> "";
fcgi_to_str(S) -> binary:bin_to_list(S).

fcgi_safe_filename(XRoute, BaseDir) ->
  Route = fcgi_to_str(XRoute),
  SafeFName = filelib:safe_relative_path(Route, BaseDir),
  FileName = filename:join([BaseDir, filelib:safe_relative_path(Route, BaseDir)]),
  if (SafeFName == unsafe) -> unsafe;
    true ->
      FNameIsRegular = filelib:is_regular(FileName),
      if FNameIsRegular -> FileName; 
         true -> enoent
      end
  end.

fcgi_sjoin(<<>>, []) -> "";
fcgi_sjoin([], []) -> "";
fcgi_sjoin("", []) -> "";
fcgi_sjoin(S, []) -> S;
fcgi_sjoin(S, A) ->
  [H|T] = A,
  case H of
    <<>> -> fcgi_sjoin(S,T);
    "" -> fcgi_sjoin(S,T);
    [] -> fcgi_sjoin(S,T);
    X -> fcgi_sjoin(S++X,T)
  end.

fcgi_sjoin(Array) -> fcgi_sjoin("",Array).

fcgi_dir_exec(Arg, Response) ->
  [BaseDir,RemovePrefix,Host,Port,TryReconnectEveryMillis] = string:split(Arg, " ",all),
  FName = fcgi_safe_filename(fcgi_sjoin(string:replace(Response#response.request#request.route, RemovePrefix, "")), BaseDir),
  logging:info("About to execute ~s",[FName]),
  case FName of
    enoent -> {aborted, 404};
    Name -> fcgi_exec(Name, Host, Port, TryReconnectEveryMillis, Response)
  end.

fcgi_exec(Arg, Response) ->
  [Script,Host,Port,TryReconnectEveryMillis] = string:split(Arg, " ",all),
  Request = Response#response.request,
  fcgi_exec(Script, Host, Port, TryReconnectEveryMillis, Response).

fcgi_get_scriptname(Path) ->
  Tokens = string:split(Path, "/", all),
  "/"++lists:nth(length(Tokens), Tokens).
fcgi_exec(Script, Host, Port, TryReconnectEveryMillis, Response) ->
  {ok, FastCGIConnection} = erl_fastcgi:start_link(Host, list_to_integer(Port), list_to_integer(TryReconnectEveryMillis)),
  Upstream = Response#response.upstream,
  Upstream ! cancel_tmr,
  RequestId = 600,
  Request = handle:get_request(Response),
  Body = Request#request.body,
  logging:debug("Body = ~p", [Body]),
  logging:debug("Request = ~p", [Request]),
  ContentLength = fcgi_get_content_length(Request),
  ContentType = fcgi_get_content_type(Request),
  {ok, XAddr}=socket:peername(Response#response.socket),
  SAddr = pretty_addr(XAddr),
  Method = binary:bin_to_list(Response#response.request#request.method),
  [RAddr|RPort]=string:split(SAddr,":"),
  ok=erl_fastcgi:run(FastCGIConnection, RequestId, [
    {"SCRIPT_FILENAME", Script},
    {"SCRIPT_NAME", fcgi_get_scriptname(Script)},
    {"QUERY_STRING", Request#request.params},
    {"REQUEST_METHOD", Method},
    {"REQUEST_URI", binary:bin_to_list(Request#request.route)},
    {"CONTENT_LENGTH", ContentLength},
    {"HTTP_CONTENT_LENGTH", ContentLength},
    {"CONTENT_TYPE", ContentType},
    {"HTTP_CONTENT_TYPE", ContentType},
    {"GATEWAY_INTERFACE", "CGI/1.1"},
    {"REMOTE_ADDR", RAddr},
    {"SERVER_PROTOCOL", binary_to_list(Request#request.http_ver)},
    {"REMOTE_PORT", RPort},
    {"SERVER_ADDR", "0.0.0.0"},
    {"SERVER_PORT", configuration:get("ListenPort",string)},
    {"SERVER_NAME", sget2("Host", Request#request.header)},
    {"SERVER_SOFTWARE", ?version}
  ], fcgi_get_body(Body)),
  fcgi_proxy(FastCGIConnection, Response).
