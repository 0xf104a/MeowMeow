-module(handle).
-export([abort/1, get_request/1, close_connection/2, set_keepalive/1, handler_start/1, send_file/3, get_filename/1, stat_file/1]).
-import(response, [response/3, get_desc/1, set_header/3]).
-import(parse_http, [http2map/1, mime_by_fname/1, is_close/1]).
-import(util, [get_time/0]).
-include_lib("kernel/include/file.hrl").
-include("config.hrl").
-include("request.hrl").
-include("response.hrl").

%% Reads request body if it is needed
%% Should be used by rule handlers
read_body(Request, Sock) when Request#request.method == <<"POST">> ->
  ContentLength = parse_http:get_header("Content-Length", Request, int) - length(Request#request.body),
  Body = Request#request.body,
  Request#request{body = string:concat(Body, io_proxy:tcp_recv(Sock, ContentLength))};
read_body(Request, _) -> Request.

%% Extracts request from response record
%% an receives body if needed
get_request(Response) -> 
  Sock = Response#response.socket,
  Request = Response#response.request,
  read_body(Request, Sock).

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
   IsKey = maps:is_key("User-Agent", Request#request.header),
   if IsKey -> maps:get("User-Agent", Request#request.header);
      true -> "<<Unknown UA>>"
   end.

log_response(Request, Code) ->
  logging:info("~p.~p.~p.~p ~s ~s -- ~p ~s", util:tup2list(Request#request.src_addr) ++ [Request#request.method, Request#request.route, Code, get_desc(integer_to_list(Code))]).

send_chunks(Dev, Sock, Sz) ->
  case file:read(Dev, Sz) of
    {ok, Data} ->
      case io_proxy:tcp_send(Sock, Data) of
           ok -> send_chunks(Dev, Sock, Sz);
           Error -> logging:err("Failed to send chunk: ~p @ handle:send_chunks/3",[Error]),
                    {failed, Error}
      end;
    eof -> ok
  end.

send_file(FName, Sock, ChunkSz) ->
  logging:debug("Sending file: ~p", [FName]),
  case file:open(FName, read) of
    {ok, Dev} -> send_chunks(Dev, Sock, ChunkSz);
    Any -> logging:err("Unexpected result while opening the file ~s: ~p",[FName, Any]),
           {failed, Any}
  end.


abort(Code) ->
  Body = lists:flatten(io_lib:format("<html><head><title>~p ~s</title></head><body><h1><i>~p ~s</i></h1><hr><i> ~s </i></body></html>", [Code, get_desc(integer_to_list(Code)), Code, get_desc(integer_to_list(Code)), ?version])),
  StrTime = get_time(),
  response:response(
    #{"Date" => StrTime,
      "Content-Type" => "text/html",
      "Content-Length" => integer_to_list(length(Body)),
      "Connection" => "close",
      "Server" => ?version}, Code, Body).

abort(<<"HEAD">>, Code) ->
  Body = lists:flatten(io_lib:format("<html><head><title>~p ~s</title></head><body><h1><i>~p ~s</i></h1><hr><i> ~s </i></body></html>", [Code, get_desc(integer_to_list(Code)), Code, get_desc(integer_to_list(Code)), ?version])),
  StrTime = get_time(),
  response:response_headers(
    #{"Date" => StrTime,
      "Content-Type" => "text/html",
      "Content-Length" => integer_to_list(length(Body)),
      "Connection" => "close",
      "Server" => ?version}, Code);

abort(_, Code) ->
  abort(Code).

do_rules(_, Response) when Response#response.is_finished -> {finished, Response}; %% Response was sent
do_rules(_, Response) when Response#response.is_done -> {done, Response}; %% Response is ready to be sent by handler
do_rules([], Response) -> {ok, Response};
do_rules(Rules, Response) ->
  [{Rule, Args} | T] = Rules,
  NewResponse = rules:execute_rule(Rule, Args, Response),
%%  logging:debug("New response: ~p", [NewResponse]),
  case NewResponse of
    {aborted, Code} -> {abort, Code};
    Any -> do_rules(T, Any)
  end.

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

wrap_fname_stat(FName) ->
  case file:read_file_info(FName) of
       {error, Err} -> Err;
       {ok, FInfo} -> {FName, FInfo}
  end.

get_filename(XRoute) ->
  Route = binary:bin_to_list(XRoute, {1, string:length(XRoute) - 1}),
  SafeFName = filelib:safe_relative_path(Route, ?docdir),
  SafeIName = filelib:safe_relative_path(Route ++ "index.html", ?docdir),
  FileName = filename:join([?docdir, filelib:safe_relative_path(Route, ?docdir)]),
  IndexName = filename:join([?docdir, filelib:safe_relative_path(Route ++ "index.html", ?docdir)]),
  if (SafeIName == unsafe) or (SafeFName == unsafe) -> unsafe;
    true ->
      IndexExists = filelib:is_regular(IndexName),
      FNameIsRegular = filelib:is_regular(FileName),
      if IndexExists -> wrap_fname_stat(IndexName);
         FNameIsRegular -> wrap_fname_stat(FileName);
         true -> enoent
      end
  end.

stat_file({error, enoent}) -> {0, no_file};
stat_file(enoent) -> {0, no_file};
stat_file(no_file) -> {0, no_file};
stat_file(unsafe) -> {0, no_access};
stat_file(eacces) -> {0,no_access};
stat_file({FName,FInfo}) ->
  logging:debug("Stat: ~p, FInfo: ~p", [FName,FInfo]),
  Access = FInfo#file_info.access,
  FSize = FInfo#file_info.size,
  if (Access /= read) and (Access /= read_write) -> {0, no_access};
    FSize == 0 -> {0, empty_file};
    FInfo#file_info.type /= regular -> logging:warn("Attempting to send irregular file: ~s",[FName]),
			               {0, no_file};
    true -> {FSize, ok}
  end.

handle_file(Response, Upstream, _FName) when Response#response.request#request.method == "HEAD" ->
  log_response(Response#response.request, 200),
  Upstream ! {send, response:response_headers(Response#response.headers, Response#response.code)};
handle_file(Response, Upstream, FName) ->
  logging:debug("Response=~p", [Response]),
  log_response(Response#response.request, 200),
  Upstream ! {send, response:response_headers(Response#response.headers, Response#response.code)},
  Upstream ! cancel_tmr,
  case send_file(FName, Response#response.socket, ?chunk_size) of
       ok -> pass;
       {failed, _} -> 
	logging:warn("Failed to send file, so telling upstream to close connection"),
        Upstream ! close
  end,
  Upstream ! set_tmr.

handle_file(Response, Upstream) ->
  Route = Response#response.request#request.route,
  Request = Response#response.request,
  Method = Response#response.request#request.method,
  FStat = get_filename(Route),
  case stat_file(FStat) of
    {0, no_file} ->
      Upstream ! {send, abort(Method, 404)},
      log_response(Request, 404),
      Upstream ! close;
    {0, no_access} ->
      Upstream ! {send, abort(Method, 403)},
      log_response(Request, 403),
      Upstream ! close;
    {0, empty_file} ->
      Upstream ! {send, abort(<<"HEAD">>, 204)},
      log_response(Request, 204),
      Upstream ! close;
    {ContentSize, ok} ->
      StrTime = util:get_time(),
      ResponseHeadered = response:set_headers(Response, #{
        "Content-Length" => integer_to_list(ContentSize),
        "Date" => StrTime,
        "Server" => ?version}),
      {FName, _} = FStat,
      case Method of
           <<"GET">> -> handle_file(ResponseHeadered, Upstream, FName);
           <<"HEAD">> ->  Upstream ! {send, response:response_headers(Response#response.headers, Response#response.code)};
           Any -> logging:err("Bad method handling: ~p. Probably a bug @ handle:handle_file/2",[Any]),
                  Upstream ! {send, abort(500)}
      end
  end.

handle_abort(Code, Request, Upstream) ->
  logging:info("~p.~p.~p.~p ~s ~s -- ~p ~s", util:tup2list(Request#request.src_addr) ++ [Request#request.method, Request#request.route, Code, get_desc(integer_to_list(Code))]),
  Upstream ! {send, abort(Code)},
  Upstream ! close,
  ok.

handle(Resp, Upstream) ->
  Request = Resp#response.request,
  Rules = access:get_rules(Request),
  R = set_keepalive(Resp),
%%  logging:debug("R=~p", [R]),
%%  logging:debug("Rules=~p", [Rules]),
  Result = do_rules(Rules, R),
  case Result of
    {abort, Code} ->
      logging:info("~p.~p.~p.~p ~s ~s -- ~p ~s", util:tup2list(Request#request.src_addr) ++ [Request#request.method, Request#request.route, Code, get_desc(integer_to_list(Code))]),
      Upstream ! {send, abort(Code)},
      Upstream ! close;
    {finished, Response} -> 
      Code = Response#response.code,
      logging:info("~p.~p.~p.~p ~s ~s -- ~p ~s", util:tup2list(Request#request.src_addr) ++ [Request#request.method, Request#request.route, Code, get_desc(integer_to_list(Code))]);
    {done, Response} ->
      Headers = Response#response.headers,
      Code = Response#response.code,
      Body = Response#response.body,
      logging:info("~p.~p.~p.~p ~s ~s -- ~p ~s", util:tup2list(Request#request.src_addr) ++ [Request#request.method, Request#request.route, Code, get_desc(integer_to_list(Code))]),
      Upstream ! {send, response:response(Headers, Code, Body)};
    {ok, Response} ->
      handle_file(Response, Upstream);
    Any -> logging:err("Unhandled rules result: ~p @ handle:handle/2", [Any])
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
  HasLength = maps:is_key("Content-Length",Request#request.header),
  if not HasLength ->
       logging:warn("POST request without Content-Length"),
       handle_abort(411, Request, Upstream);
     HasLength ->
       Length = parse_http:get_header("Content-Length", Request,int),
       MaxLength = configuration:get("MaxPostSize", int),
       if Length > MaxLength ->
            logging:warn("POST request payload is too big(~p>~p)",[Length, MaxLength]),
            handle_abort(413, Request, Upstream);
          true -> handle_post_unwrapped(Resp, Upstream)
      end
  end.
  
handle_post_unwrapped(Resp, Upstream) ->
  Request = Resp#response.request,
  Rules = access:get_rules(Request),
  AResponse = set_keepalive(Resp),
  logging:debug("Before do_rules @ handle:handle_post_unwrapped/2"),
  Result = do_rules(Rules, AResponse),
  logging:debug("Result = ~p", [Result]),
  case Result of
    {abort, Code} ->
      logging:info("~p.~p.~p.~p ~s ~s -- ~p ~s", util:tup2list(Request#request.src_addr) ++ [Request#request.method, Request#request.route, Code, get_desc(integer_to_list(Code))]),
      Upstream ! {send, abort(Code)},
      Upstream ! close;
    {finished, Response} ->
      Code = Response#response.code,
      logging:info("~p.~p.~p.~p ~s ~s -- ~p ~s", util:tup2list(Request#request.src_addr) ++ [Request#request.method, Request#request.route, Code, get_desc(integer_to_list(Code))]);
    {done, Response} ->
      Headers = Response#response.headers,
      Code = Response#response.code,
      Body = Response#response.body,
      logging:info("~p.~p.~p.~p ~s ~s -- ~p ~s", util:tup2list(Request#request.src_addr) ++ [Request#request.method, Request#request.route, Code, get_desc(integer_to_list(Code))]),
      Upstream ! {send, response:response(Headers, Code, Body)};
    {ok, _} ->
      %% There is no default proocedure of handling POST requests
      %% So they should be handled by rules engine. But if they are
      %% not we should return HTTP/1.1 405 Method Not Allowed.
      logging:info("~p.~p.~p.~p ~s ~s -- ~p ~s", util:tup2list(Request#request.src_addr) ++ [Request#request.method, Request#request.route, 405, get_desc(integer_to_list(405))]),
      Upstream ! {send, abort(405)},
      Upstream ! close;
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
      Result =  is_good(Request),
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
           <<"GET">>->
      		      Response = #response{socket = Sock, code = 200, request = Request, upstream = Upstream},
      		      logging:debug("Response=~p", [Response]),
      		      handle(Response, Upstream);
           <<"HEAD">>->
                Response = #response{socket = Sock, code = 200, request = Request, upstream = Upstream},
                logging:debug("Response=~p", [Response]),
                handle(Response, Upstream);
           <<"POST">>->
                Response = #response{socket = Sock, code = 200, request = Request, upstream = Upstream},
		            handle_post(Response, Upstream);
           _ -> 
                logging:warn("Requested unknown method ~s, just rejecting request", [Request#request.method]),
                log_response(Request,405),
                Upstream ! {send, abort(405)},
                Upstream ! close,
                ok
      end.

handler(_, Upstream, {aborted, Code})->
  Upstream ! {send, abort(Code)},
  Upstream ! close,
  exit(done);
handler(Sock, Upstream, Request) ->
  receive
    {data, Data} ->
      ARequest = parse_http:update_request(Request,Data),
      Finished = ARequest#request.is_headers_accepted,
      logging:debug("Request finished = ~p", [Finished]),
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
      handler(Sock, Upstream, ARequest);
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
      handler(Sock, Upstream, parse_http:make_request(util:get_addr(Sock)))
  end.
