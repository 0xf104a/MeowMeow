%% FastCGI wrapper
-module(fcgi).
-author("p01ar").
-import(util,[sget2/2,pretty_addr/1]).
-export([fcgi_exec/2]).
-include("response.hrl").
-include("request.hrl").

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
    {fast_cgi_done, _} -> ok;
    {fast_cgi_stdout, 600, Out} ->
      io_proxy:tcp_send(Response#response.socket, binary_to_list(Out)),
      fcgi_send(Response)
  end.
fcgi_proxy(FastCGIConnection,Response) ->
  logging:debug("FCGI: entered PROXY"),
  receive
    {fast_cgi_done, _} -> ok;
    {fast_cgi_stdout, 600, ROut} ->
      Out=binary_to_list(ROut),
      io:format("Got: ~p~n", [Out]),
      Updated=update_response(Out, Response),
      logging:debug("FCGI: Updated = ~p",[Updated]),
      case Updated of
           {done, NewResponse, Tail}->
                 logging:debug("Sending ~p, then ~p",[NewResponse,Tail]),
                 io_proxy:tcp_send(Response#response.socket, response:do_response_headers(NewResponse)),
                 io_proxy:tcp_send(Response#response.socket, Tail),
                 fcgi_send(NewResponse);
           NewResponse->
                 fcgi_proxy(FastCGIConnection, NewResponse)
      end 
  after
    5000 -> erl_fastcgi:close(FastCGIConnection)
  end,
  Response#response{is_finished=true}.

fcgi_exec(Arg, Response) ->
  [Script,Host,Port,TryToReconnectEveryMillis] = string:split(Arg, " ",all),
  {ok, FastCGIConnection} = erl_fastcgi:start_link(Host, list_to_integer(Port), list_to_integer(TryToReconnectEveryMillis)),
  RequestId = 600,
  Request = Response#response.request,
  Body = "",
  {ok, XAddr}=socket:peername(Response#response.socket),
  SAddr = pretty_addr(XAddr),
  Method = binary:bin_to_list(Response#response.request#request.method),
  [RAddr|RPort]=string:split(SAddr,":"),
  erl_fastcgi:run(FastCGIConnection, RequestId, [
    {"SCRIPT_FILENAME", Script},
    {"QUERY_STRING", Body},
    {"REQUEST_METHOD", Method},
    {"CONTENT_LENGTH", "0"},
    {"HTTP_CONTENT_LENGTH", "0"},
    {"GATEWAY_INTERFACE", "CGI/1.1"},
    {"REMOTE_ADDR", RAddr},
    {"SERVER_PROTOCOL", "HTTP/1.1"},
    {"REMOTE_PORT", RPort},
    {"SERVER_ADDR", "0.0.0.0"},
    {"SERVER_PORT", configuration:get("ListenPort",string)},
    {"SERVER_NAME", sget2("Host", Request#request.header)}
  ], <<>>),
  fcgi_proxy(FastCGIConnection, Response).
