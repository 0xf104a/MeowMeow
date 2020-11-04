-module(io_proxy).
-export([io_proxy_tcp_start/2, tcp_send/2]).
-import(erlang, [send_after/3]).
-include("config.hrl").

tcp_send(Sock, Data) when length(Data) < ?chunk_size ->
  case socket:send(Sock, Data) of
       ok -> ok;
       Any -> logging:err("Failed to send packet: ~p @ io_proxy:tcp_send/2",[Any]),
              {error,Any}
  end;
tcp_send(Sock, Data) ->
  {H, T} = lists:split(?chunk_size, Data),
  case socket:send(Sock, H) of
       ok -> tcp_send(Sock, T);
       Any -> logging:err("Failed to send packet: ~p @ io_proxy:tcp_send/2",[Any]),
              {error, Any}
  end.

%%send_after(Time, Dest, Msg)->
%%    Ref=erlang:send_after(Time, Dest, Msg),
%%    logging:debug("Setted ref=~p",[Ref]),
%%    Ref.

cancel_ref(Ref) when Ref == not_set -> not_set;
cancel_ref(Ref) ->
%%  logging:debug("Cancelled ref=~p",[Ref]),
  erlang:cancel_timer(Ref).
io_proxy_tcp(Sock, Handler, TmRef) ->
  receive
    {recv, Size} ->
      {ok, Data} = socket:recv(Sock, Size),
      cancel_ref(TmRef),
      Handler ! {data, Data},
      TRef = send_after(?timeout, self(), timeout),
      io_proxy_tcp(Sock, Handler, TRef);
    {send, Data} ->
      cancel_ref(TmRef),
      ok=tcp_send(Sock, Data),
      TRef = send_after(?timeout, self(), timeout),
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
      cancel_ref(TmRef),
      TRef = send_after(?timeout, self(), timeout),
      logging:debug("Setted TmRef @ io_proxy_tcp/3"),
      io_proxy_tcp(Sock, Handler, TRef);
    close ->
      case socket:peername(Sock) of
           {ok, Addr} -> ok;
           Any ->Addr=#{addr=>{nan, nan, nan, nan},port=>nan}, 
                 logging:err("Failed to get peername while closing connection: ~p @ io_proxy_tcp/3",[Any])
      end,
      logging:info("Closing connection with ~p", [util:pretty_addr(Addr)]),
      socket:close(Sock),
      cancel_ref(TmRef),
      exit(requested);
    timeout ->
      case socket:peername(Sock) of
           {ok, Addr} ->
      		logging:info("Killing connection with ~p due to timeout", [util:pretty_addr(Addr)]),
      		socket:close(Sock),
      		cancel_ref(TmRef),
      		exit(timeout);
           {error, Err} ->
 		logging:err("Peername error while handling timeout: ~p @ io_porxy_tcp/3", [Err])
      end;
    Any ->
      logging:warn("Recieved unknown cmd: ~p @ io_proxy_tcp/3", [Any])
  end,
  io_proxy_tcp(Sock, Handler, TmRef).

io_proxy_tcp_start(Sock, Handler) ->
  Handler ! {upstream, self()},
  TRef = send_after(?timeout, self(), timeout),
  io_proxy_tcp(Sock, Handler, TRef).
