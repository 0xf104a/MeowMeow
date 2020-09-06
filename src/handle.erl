-module(handle).
-export([abort/1, handler_start/1, send_file/3]).
-import(response, [response/3, get_desc/1, set_header/3]).
-import(parse_http, [http2map/1, mime_by_fname/1]).
-import(util, [get_time/0]).
-include_lib("kernel/include/file.hrl").
-include("config.hrl").
-include("request.hrl").
-include("response.hrl").

send_chunks(Dev, Upstream, Sz) ->
  case file:read(Dev, Sz) of
    {ok, Data} ->
      Upstream ! {send, Data},
      send_chunks(Dev, Upstream, Sz);
    eof -> ok
  end.

send_file(FName, Upstream, ChunkSz) ->
  {ok, Dev} = file:open(FName, read),
  send_chunks(Dev, Upstream, ChunkSz).


abort(Code) ->
  Body = lists:flatten(io_lib:format("<html><head><title>~p ~s</title></head><body><h1><i>~p ~s</i></h1><hr><i> ~s </i></body></html>", [Code, get_desc(integer_to_list(Code)), Code, get_desc(integer_to_list(Code)), ?version])),
  StrTime = get_time(),
  response:response(#{"Date" => StrTime,
    "Content-Type" => "text/html",
    "Connection" => "close",
    "Server" => ?version}, Code, Body).

do_rules(_, Response) when Response#response.is_finished -> {finished, Response}; %% Response was sent
do_rules(_, Response) when Response#response.is_done -> {done, Response}; %% Response is ready to be sent by handler
do_rules([], Response) -> {ok, Response};
do_rules(Rules, Response) ->
  [{Rule, Args} | T] = Rules,
  NewResponse = rules:execute_rule(Rule, Args, Response),
  logging:debug("New response: ~p", [NewResponse]),
  case NewResponse of
    {aborted, Code} -> {abort, Code};
    Any -> do_rules(T, Any)
  end.

handle(R, Upstream) ->
  Route = R#response.request#request.route,
  Request = R#response.request,
  Rules = access:get_rules(Route),
  logging:debug("Rules=~p", [Rules]),
  Result = do_rules(Rules, R),
  case Result of
    {abort, Code} ->
      logging:info("~p.~p.~p.~p ~s ~s -- ~p ~s", util:tup2list(Request#request.src_addr) ++ [Request#request.method, Request#request.route, Code, get_desc(integer_to_list(Code))]),
      Upstream ! {send, abort(Code)};
    {finished, Response} ->
      Code = Response#response.code,
      logging:info("~p.~p.~p.~p ~s ~s -- ~p ~s", util:tup2list(Request#request.src_addr) ++ [Request#request.method, Request#request.route, Code, get_desc(integer_to_list(Code))]);
    {done, Response} ->
      Headers = Response#response.headers,
      Code = Response#response.code,
      Body = Response#response.body,
      logging:info("~p.~p.~p.~p ~s ~s -- ~p ~s", util:tup2list(Request#request.src_addr) ++ [Request#request.method, Request#request.route, Code, get_desc(integer_to_list(Code))]),
      Upstream ! {send, response:response(Headers, Code, Body)}

  end.


handle(Sock, Upstream, RequestLines) ->
  {ok, Peer} = socket:peername(Sock),
  Parsed = parse_http:parse_request(maps:get(addr, Peer), RequestLines),
  case Parsed of
    bad_request ->
      logging:debug("Bad request -- responding"),
      Upstream ! {send, abort(400)};
    {ok, Request} ->
      Response = #response{code = 200, request = Request, upstream = Upstream},
      logging:debug("Response=~p", [Response]),
      handle(Response, Upstream)
  end.
handler(Sock, Upstream, RequestLines) ->
  receive
    {data, Data} ->
      Lines = parse_http:update_lines(RequestLines, Data),
      logging:debug("Updated lines: ~p", [Lines]),
      Finished = parse_http:is_request_finished(Lines),
      if Finished ->
        logging:debug("Finished processing request"),
        handle(Sock, Upstream, Lines),
        Upstream ! close,
        exit(done);
        true -> ok
      end,
      handler(Sock, Upstream, Lines);
    Any ->
      logging:err("Recieved bad message @ handle:handler/3: ~p", [Any])
  end.

handler_start(Sock) ->
  receive
    {upstream, Upstream} ->
      logging:debug("Recieved upstream PID. Starting handling."),
      Upstream ! recv,
      handler(Sock, Upstream, [""])
  end.
