-module(server).
-export([run/1, run_synchronized/1, start/0, stop/0]).
-import(socket, [create_socket/1, socket_recv/2, socket_accept/3, socket_send/2, socket_recv_all/2]).
-import(handle, [handle_http11/1, abort/1]).
-import(parse_http, [http2map/1]).
-include_lib("kernel/include/inet.hrl").
-include("config.hrl").

tup2list(Tuple) -> tup2list(Tuple, 1, tuple_size(Tuple)).

tup2list(Tuple, Pos, Size) when Pos =< Size ->
  [element(Pos, Tuple) | tup2list(Tuple, Pos + 1, Size)];
tup2list(_Tuple, _Pos, _Size) -> [].

handle_connection(Sock) ->
  case socket:peername(Sock) of
    {ok, Addr} ->
      logging:debug("Addr = ~p @server.erl:19", [Addr]),
      PidHandler = spawn(fun() -> handle:handler_start(Sock) end),
      spawn(fun() -> io_proxy:io_proxy_tcp_start(Sock, PidHandler) end);
    {error, enotconn} ->
      logging:warn("Unexpected disconnect of a client. Maybe a port scan?");
    {error, Any} ->
      logging:err("Error getting peername: ~p", [Any])
  end.

loop(Sock) ->
  logging:debug("Entered loop"),
  case socket:accept(Sock) of
    {ok, Socket} ->
  	logging:debug("Entering handle"),
  	handle_connection(Socket),
  	loop(Sock);
    Any ->
        logging:err("Accept failed: ~p @ server:loop/1", [Any])
   end.

listen(Port) ->
  Addr = #{addr => {0, 0, 0, 0}, family => inet, port => Port},
  {ok, Sock} = socket:open(inet, stream, tcp),
  case socket:bind(Sock, Addr) of
    {ok, _} ->
      R = socket:sockname(Sock),
      ok = socket:listen(Sock),
      socket:setopt(Sock, socket, reuseaddr, true),
      socket:setopt(Sock, socket, reuseport, true),
      logging:info("Listening on port ~p", [Port]),
      spawn(fun() -> loop(Sock) end),
      R;
    {error, Reason} ->
      logging:err("Failed to bind to 0.0.0.0:~p. Reason: ~s", [Port, Reason]),
      {error, Reason}
  end.

listen_synchronized(Port) ->
  %% Does not create new process
  Addr = #{addr => {0, 0, 0, 0}, family => inet, port => Port},
  {ok, Sock} = socket:open(inet, stream, tcp),
  case socket:bind(Sock, Addr) of
    {ok, _} ->
      R = socket:sockname(Sock),
      ok = socket:listen(Sock),
      socket:setopt(Sock, socket, reuseaddr, true),
      socket:setopt(Sock, socket, reuseport, true),
      logging:info("Listening on port ~p", [Port]),
      loop(Sock),
      R;
    {error, Reason} ->
      logging:err("Failed to bind to 0.0.0.0:~p. Reason: ~s", [Port, Reason]),
      {error, Reason}
  end.

run(Port) ->
  configuration:load(),
  rules:init_rules(),
  rules:register_basic(),
  access:load_access(?accessfile),
  listen(Port).

run_synchronized(Port) ->
  listen_synchronized(Port).

start() ->
  configuration:load(),
  rules:init_rules(),
  rules:register_basic(),
  access:load_access(?accessfile),
  run_synchronized(configuration:get("ListenPort", int)).

stop() ->
  init:stop().
