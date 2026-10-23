%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2018, 2020, 2026, Anna-Sofia Kasierocka
%%% @doc
%%% This module contains a process responsible for parsing HTTP headers.
%%% @end
%%%-------------------------------------------------------------------
-module(handle).
-author("f104a").
-export([abort/1, close_connection/2, set_keepalive/1,
  handler_start/1, get_ua/1, abort/2, log_response/2, handle_body_recv/1]).
-import(response, [response/3, get_desc/1, set_header/3]).
-import(parse_http, [http2map/1, mime_by_fname/1, is_close/1]).
-import(util, [get_time/0]).
-include_lib("kernel/include/file.hrl").
-include("config.hrl").
-include("request.hrl").
-include("response.hrl").

%% Checks that request is good
is_good(Request) ->
  AllowLegacy = configuration:get("AllowLegacyHttp", bool),
  if not AllowLegacy ->
    {Major, Minor} = util:get_http_ver_pair(Request#request.http_ver),
    if ((Major == 1) and (Minor >= 1)) or (Major > 1) -> good;
      true -> {bad, old_http}
    end;
    true -> good
  end.

get_ua(Request) ->
  parse_http:get_header("User-Agent", Request, "<<Unknown UA>>").

log_response(Request, Code) ->
  logging:info("~p.~p.~p.~p ~s ~s -- ~p ~s", util:tup2list(Request#request.src_addr) ++ [Request#request.method, Request#request.route, Code, get_desc(integer_to_list(Code))]).

%% If we passed with response being ready2send we may calculate Content-Length
maybe_set_content_length(Response) when Response#response.body /= []->
  RespLen = iolist_size(Response#response.body),
  if RespLen > 0 -> set_header(Response, "Content-Length", integer_to_list(RespLen));
    true -> Response
  end;
maybe_set_content_length(Response) -> Response.

%% @doc
%% Gets body with error description
%% @end
get_abort_body(Code) ->
  lists:flatten(io_lib:format("<html><head><title>~p ~s</title></head><body><h1><i>~p ~s</i></h1><hr><i> ~s </i></body></html>",
    [Code, get_desc(integer_to_list(Code)), Code, get_desc(integer_to_list(Code)), ?version])).

%% RFC 7230 section 3.3: 1xx, 204 and 304 responses never carry a message
%% body, and section 3.3.2 forbids Content-Length on 1xx and 204 responses.
has_body(Code) when Code >= 100, Code < 200 -> false;
has_body(204) -> false;
has_body(304) -> false;
has_body(_) -> true.

%% Headers and body of an abort response for Code: the HTML error page
%% with its Content-Type / Content-Length, or nothing at all for codes
%% that must not carry a body.
abort_parts(Code) ->
  Base = #{"Date" => get_time(),
           "Connection" => "close",
           "Server" => ?version},
  case has_body(Code) of
    true ->
      Body = get_abort_body(Code),
      {Base#{"Content-Type" => "text/html",
             "Content-Length" => integer_to_list(length(Body))}, Body};
    false ->
      {Base, ""}
  end.

%%% @doc
%%%  Function that generates error responses.
%%%  Should not be used to generate 1xx, 2xx and 3xx responses,
%%%  beyond legacy exemption of 204 No Content (and 304), which are
%%%  rendered as headers only.
%%% @end
abort(Code) ->
  {Headers, Body} = abort_parts(Code),
  response:response(Headers, Code, Body).

%% HEAD carries the headers a GET would get, but never a body (RFC 7231 4.3.2)
abort(<<"HEAD">>, Code) ->
  {Headers, _Body} = abort_parts(Code),
  response:response_headers(Headers, Code);

abort(_, Code) ->
  abort(Code).

close_connection(Request, Upstream) ->
  NeedsClose = is_close(Request),
  if NeedsClose ->
    logging:info("Close mark set. Closing connection with ~p", [Request#request.src_addr]),
    Upstream ! close,
    ok;
    true ->
      not_closed
  end.

set_keepalive(Response) ->
  Request = Response#response.request,
  NeedsClose = is_close(Request),
  if NeedsClose ->
    set_header(Response, "Connection", "close");
    true ->
      set_header(Response, "Connection", "keep-alive")
  end.

handle_abort(Code, Request, Upstream) ->
  logging:info("~p.~p.~p.~p ~s ~s -- ~p ~s", util:tup2list(Request#request.src_addr) ++ [Request#request.method, Request#request.route, Code, get_desc(integer_to_list(Code))]),
  Upstream ! {send, abort(Code)},
  Upstream ! close,
  ok.

get_default_response(<<"HEAD">>, InitialResponse) ->
  InitialResponse#response{code=404,
          body = <<"">>};

get_default_response(_, InitialResponse) ->
  InitialResponse#response{code=404,
    body = list_to_binary(get_abort_body(404))}.

handle(Resp, Upstream) ->
  Request = Resp#response.request,
  Method = Request#request.method,
  Rules = access:get_rules(Request),
  ResponseWithKeepAlive = set_keepalive(Resp),
  logging:debug("ResponseWithKeepAlive=~p", [ResponseWithKeepAlive]),
  DefaultResponse = get_default_response(Method, ResponseWithKeepAlive),
  logging:debug("DefaultResp=~p", [DefaultResponse]),
  Result = rules:rulechain_exec(Rules, DefaultResponse),
  logging:debug("Result=~p", [Result]),
  case Result of
    %% Aborted response: an error happend
    {aborted, Code} ->
      logging:info("~p.~p.~p.~p ~s ~s -- ~p ~s", util:tup2list(Request#request.src_addr) ++ [Request#request.method, Request#request.route, Code, get_desc(integer_to_list(Code))]),
      Upstream ! {send, abort(Method, Code)},
      Upstream ! close;
    %% Response maybe send by our side
    {ready2send, ResponseWithoutCL} ->
      Response = maybe_set_content_length(ResponseWithoutCL),
      Headers = Response#response.headers,
      Code = Response#response.code,
      Body = Response#response.body,
      logging:info("~p.~p.~p.~p ~s ~s -- ~p ~s", util:tup2list(Request#request.src_addr) ++ [Request#request.method, Request#request.route, Code, get_desc(integer_to_list(Code))]),
      Upstream ! {send, response:response(Headers, Code, Body)};
    %% Response was sent off by module, no need to care about data
    {sent, Response} ->
      Code = Response#response.code,
      logging:info("~p.~p.~p.~p ~s ~s -- ~p ~s", util:tup2list(Request#request.src_addr) ++ [Request#request.method, Request#request.route, Code, get_desc(integer_to_list(Code))]);
    %% Module provided string to respond with
    {ok, ResponseWithoutCL} ->
      Response = maybe_set_content_length(ResponseWithoutCL),
      Headers = Response#response.headers,
      Code = Response#response.code,
      Body = Response#response.body,
      logging:info("~p.~p.~p.~p ~s ~s -- ~p ~s", util:tup2list(Request#request.src_addr) ++ [Request#request.method, Request#request.route, Code, get_desc(integer_to_list(Code))]),
      Upstream ! {send, response:response(Headers, Code, Body)};
    %% Module has given non-standard result: it is actually gateway problem
    Any ->
      logging:err("Unhandled rules result: ~p @ handle:handle/2", [Any]),
      Upstream ! {send, abort(502)},
      Upstream ! close
  end,
  close_connection(Request, Upstream).

handle_post(Resp, Upstream) ->
  %% Here we verify the POST request
  %% It should definitely have
  %% Content-Length header
  %% And of course the Content-Length
  %% should be less or equal to MaxPostSize
  logging:debug("Entered handle:handle_post/2"),
  Request = Resp#response.request,
  HasLength = parse_http:has_header("Content-Length", Request),
  if not HasLength ->
    logging:warn("POST request without Content-Length"),
    handle_abort(411, Request, Upstream);
    HasLength ->
      Length = list_to_integer(parse_http:get_header("Content-Length", Request, "0")),
      MaxLength = configuration:get("MaxPostSize", int),
      if Length > MaxLength ->
        logging:warn("POST request payload is too big(~p>~p)", [Length, MaxLength]),
        handle_abort(413, Request, Upstream);
        true -> handle_post_unwrapped(Resp, Upstream)
      end
  end.

handle_post_unwrapped(Resp, Upstream) ->
  Request = Resp#response.request,
  Rules = access:get_rules(Request),
  AResponse = set_keepalive(Resp),
  logging:debug("Before do_rules @ handle:handle_post_unwrapped/2"),
  Result = rules:rulechain_exec(Rules, get_default_response(Request#request.method, AResponse)),
  %%logging:debug("Result = ~p", [Result]),
  case Result of
    {aborted, Code} ->
      logging:info("~p.~p.~p.~p ~s ~s -- ~p ~s", util:tup2list(Request#request.src_addr) ++ [Request#request.method, Request#request.route, Code, get_desc(integer_to_list(Code))]),
      Upstream ! {send, abort(Code)},
      Upstream ! close;
    {sent, Response} ->
      Code = Response#response.code,
      logging:info("~p.~p.~p.~p ~s ~s -- ~p ~s", util:tup2list(Request#request.src_addr) ++ [Request#request.method, Request#request.route, Code, get_desc(integer_to_list(Code))]);
    {ready2send, ResponseWithoutCL} ->
      Response = maybe_set_content_length(ResponseWithoutCL),
      Headers = Response#response.headers,
      Code = Response#response.code,
      Body = Response#response.body,
      logging:info("~p.~p.~p.~p ~s ~s -- ~p ~s", util:tup2list(Request#request.src_addr) ++ [Request#request.method, Request#request.route, Code, get_desc(integer_to_list(Code))]),
      Upstream ! {send, response:response(Headers, Code, Body)};
    {ok, ResponseWithoutCL} ->
      Response = maybe_set_content_length(ResponseWithoutCL),
      Headers = Response#response.headers,
      Code = Response#response.code,
      Body = Response#response.body,
      logging:info("~p.~p.~p.~p ~s ~s -- ~p ~s", util:tup2list(Request#request.src_addr) ++ [Request#request.method, Request#request.route, Code, get_desc(integer_to_list(Code))]),
      Upstream ! {send, response:response(Headers, Code, Body)};
    Any ->
      logging:err("Unhandled rules result: ~p @ handle:handle_post_unwrapped/2", [Any])
  end,
  close_connection(Request, Upstream).

handle(Sock, Upstream, ARequest) ->
  case ARequest of
    {aborted, Code} ->
      logging:debug("Bad request -- responding"),
      Upstream ! {send, abort(Code)},
      ok;
    Request ->
      Result = is_good(Request),
      case Result of
        good ->
          handle_by_method(Request, Upstream, Sock);
        {bad, old_http} ->
          log_response(Request, 505),
          Upstream ! {send, abort(505)},
          Upstream ! close,
          ok
      end
  end.

handle_by_method(Request, Upstream, Sock) ->
  case Request#request.method of
    <<"GET">> ->
      Response = #response{socket = Sock, code = 200, request = Request, upstream = Upstream, headers = ?base_headers},
      %%logging:debug("Response=~p", [Response]),
      handle(Response, Upstream);
    <<"DELETE">> ->
      Response = #response{socket = Sock, code = 200, request = Request, upstream = Upstream, headers = ?base_headers},
      %%logging:debug("Response=~p", [Response]),
      handle(Response, Upstream);
    <<"HEAD">> ->
      Response = #response{socket = Sock, code = 200, request = Request, upstream = Upstream, headers = ?base_headers},
      %%logging:debug("Response=~p", [Response]),
      handle(Response, Upstream);
    <<"POST">> ->
      Response = #response{socket = Sock, code = 200, request = Request, upstream = Upstream, headers = ?base_headers},
      handle_post(Response, Upstream);
    _ ->
      logging:warn("Requested unknown method ~s, just rejecting request", [Request#request.method]),
      log_response(Request, 405),
      Upstream ! {send, abort(405)},
      Upstream ! close,
      ok
  end.

handler(_, Upstream, {aborted, Code}) ->
  Upstream ! {send, abort(Code)},
  Upstream ! close,
  exit(done);

handler(Sock, Upstream, Request) ->
  receive
    {data, Data} ->
      case parse_http:update_request(Request, Data) of
        {aborted, Code} ->
          %% malformed request line or header: answer and give up on the connection
          handler(Sock, Upstream, {aborted, Code});
        ARequest ->
          Finished = ARequest#request.is_headers_accepted,
          %%logging:debug("Request finished = ~p", [Finished]),
          if Finished ->
            logging:debug("Finished processing request"),
            case handle(Sock, Upstream, ARequest) of
              not_closed ->
                Upstream ! recv, %% Notify that connection is keep-alive -- need wait for packet
                Upstream ! set_tmr, %% ask to reset keep-alive timer
                handler(Sock, Upstream, parse_http:make_request(util:get_addr(Sock)));
              ok ->
                exit(done)
            end;
            true -> ok
          end,
          handler(Sock, Upstream, ARequest)
      end;
    closed -> %% Connection was closed, does not need to do anything
      exit(closed);
    timeout -> %% Timed-out waiting. Exit gracefully
      exit(timeout);
    Any ->
      logging:err("Recieved bad message @ handle:handler/3: ~p", [Any]),
      Upstream ! {send, abort(500)},
      exit(error)
  end.

handler_start(Sock) ->
  receive
    {upstream, Upstream} ->
      logging:debug("Recieved upstream PID. Starting handling."),
      Upstream ! recv,
      handler(Sock, Upstream,
        parse_http:make_request(util:get_addr(Sock)))
  end.

handle_body_recv_residual(_, Response, 0) ->
  %% From 2.0 body must always be binary, but part of it may be received with headers,
  %% so we need still conver it to binary
  CurrentBody = Response#response.request#request.body,
  BinaryBody = list_to_binary(CurrentBody),
  Request = Response#response.request#request{body = BinaryBody},
  {ok, Response#response{request = Request}};

handle_body_recv_residual(Socket, Response, ResidualLen) ->
  CurrentBody = Response#response.request#request.body,
  BinaryBody = list_to_binary(CurrentBody),
  case nya_tcp:tcp_recv(Socket, ResidualLen) of
    {ok, Recvd} -> {ok,
      Response#response{
        request = Response#response.request#request{
          body = <<BinaryBody/binary, Recvd/binary>>
        }
      }
    };
    Any -> logging:err("Failed receiving data ~p @ handle:handle_body_recv_residual/3", [Any]),
      {error, Any}
  end.

%% @doc
%% Receives request body into memory.
%% Suitable for small requests which have body fitting into memory well, otherwise
%% should be handled by module itself to keep body parts, written, cached, processed
%% online, etc.
%% @end
handle_body_recv(Response) ->
  ContentLen = list_to_integer(parse_http:get_header("Content-Length", Response#response.request, "0")),
  Socket = Response#response.socket,
  case ContentLen of
    _ when ContentLen =< 0 -> {error, unknown_length};
    Len -> handle_body_recv_residual(Socket, Response,
      Len - length(Response#response.request#request.body))
  end.