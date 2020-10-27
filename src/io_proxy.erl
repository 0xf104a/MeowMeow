-module(io_proxy).
-export([io_proxy_tcp_start/2, tcp_send/2]).
-import(timer, [send_after/3]).
-include("config.hrl").

tcp_send(Sock, Data) when length(Data) < ?chunk_size ->
  ok = socket:send(Sock, Data);
tcp_send(Sock, Data) ->
  {H, T} = lists:split(?chunk_size, Data),
  ok = socket:send(Sock, H),
  tcp_send(Sock, T).
cancel_ref(Ref) when Ref == not_set -> not_set;
cancel_ref(Ref) ->
  timer:cancel(Ref).
io_proxy_tcp(Sock, Handler, TmRef) ->
  receive
    {recv, Size} ->
      {ok, Data} = socket:recv(Sock, Size),
      cancel_ref(TmRef),
      Handler ! {data, Data},
      {ok, TRef} = send_after(?timeout, self(), timeout),
      io_proxy_tcp(Sock, Handler, TRef);
    {send, Data} ->
      cancel_ref(TmRef),
      tcp_send(Sock, Data),
      {ok, TRef} = send_after(?timeout, self(), timeout),
      io_proxy_tcp(Sock, Handler, TRef);
    recv ->
      case socket:recv(Sock, 0, ?timeout) of
        {ok, Data} ->
          Handler ! {data, Data};
        {error, closed} ->
          Handler ! closed,
          logging:warn("Remote has closed conntection @ io_proxy_tcp/3"),
          exit(closed);
        {error, timeout} ->
          Handler ! timeout,
          logging:debug("Timed-out waiting packets from remote. Exiting. @ io_proxy_tcp/3");
        {error, Other} ->
          logging:err("Error while receiving: ~p", [Other])
      end;
    cancel_tmr ->
      cancel_ref(TmRef);
    set_tmr ->
      {ok, TRef} = send_after(?timeout, self(), timeout),
      io_proxy_tcp(Sock, Handler, TRef);
    close ->
      {ok, Addr} = socket:peername(Sock),
      logging:debug("Closing connection with ~p", [util:pretty_addr(Addr)]),
      socket:close(Sock),
      cancel_ref(TmRef),
      exit(requested);
    timeout ->
      {ok, Addr} = socket:peername(Sock),
      logging:info("Killing connection with ~p", [util:pretty_addr(Addr)]),
      socket:close(Sock),
      exit(timeout);
    Any ->
      logging:warn("Recieved unknown cmd: ~p @ io_proxy_tcp/2", [Any])
  end,
  io_proxy_tcp(Sock, Handler, TmRef).

io_proxy_tcp_start(Sock, Handler) ->
  Handler ! {upstream, self()},
  io_proxy_tcp(Sock, Handler, not_set).
