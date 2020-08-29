-module(handle).
-export([abort/1, handler_start/1]).
-import(response, [response/3, get_desc/1]).
-import(parse_http, [http2map/1, mime_by_fname/1]).
-include_lib("kernel/include/file.hrl").
-include("config.hrl").
-include("request.hrl").
-include("response.hrl").
-import(util, [get_time/0]).

abort(Code) ->
  Body = lists:flatten(io_lib:format("<html><head><title>~p ~s</title></head><body><h1><i>~p ~s</i></h1><hr><i> ~s </i></body></html>", [Code, get_desc(integer_to_list(Code)), Code, get_desc(integer_to_list(Code)), ?version])),
  StrTime = get_time(),
  response:response(#{"Date" => StrTime,
    "Content-Type" => "text/html",
    "Connection" => "close",
    "Server" => ?version}, Code, Body).

handle(R, Upstream)->
  Route=R#response.request#request.route,
  Rules=access:get_rules(Route),

handle(Sock, Upstream, RequestLines) ->
  {ok, Peer} = socket:peername(Sock),
  case parse_http:parse_request(maps:get(addr, Peer), RequestLines) of
    bad_request ->
      logging:debug("Bad request -- responding"),
      Upstream ! {send, abort(400)};
    {ok, Request} ->
      Response = #response{code = 200, request = Request},
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
