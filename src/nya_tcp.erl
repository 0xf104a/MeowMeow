-module(nya_tcp).
-export([nya_tcp_ctl_start/2, tcp_send/2, tcp_recv/2]).
-import(erlang, [send_after/3]).
-include("config.hrl").

tcp_send(Sock, Data) when is_binary(Data) ->
  case socket:send(Sock, Data) of
    ok -> ok;
    {error, Reason} ->
      logging:err("Send failed: ~p", [Reason]),
      {error, Reason}
  end;
tcp_send(Sock, Data) when is_list(Data) ->
  % Convert to binary once, then send
  tcp_send(Sock, list_to_binary(Data)).

tcp_recv(_, 0, Data) -> Data;
tcp_recv(Sock, Size, Data) when Size =< ?chunk_size ->
  Result = socket:recv(Sock, Size),
  case Result of
    {ok, Payload} -> string:concat(Data, Payload);
    Any -> logging:err("Recieve packet failed: ~p @ io_proxy:tcp_send/3", [Any]),
      {error, Any}
  end;
tcp_recv(Sock, Size, Data) ->
  Result = socket:recv(Sock, ?chunk_size),
  case Result of
    {ok, Payload} -> tcp_recv(Sock, Size - ?chunk_size, string:concat(Data, Payload));
    Any -> logging:err("Recieve packet failed: ~p @ nya_tcp:tcp_recv/3", [Any]),
      {error, Any}
  end.

tcp_recv(Sock, Size) ->
  %%logging:debug("Sz = ~p", [Size]),
  tcp_recv(Sock, Size, "").

cancel_ref(Ref) when Ref == not_set -> not_set;
cancel_ref(Ref) ->
  erlang:cancel_timer(Ref).
nya_tcp_ctl(Sock, Handler, TmRef) ->
  receive
    {recv, Size} ->
      {ok, Data} = socket:recv(Sock, Size),
      cancel_ref(TmRef),
      Handler ! {data, Data},
      TRef = send_after(?timeout, self(), timeout),
      nya_tcp_ctl(Sock, Handler, TRef);
    {send, From, Data} when is_binary(Data) ->
      cancel_ref(TmRef),
      % Direct send. Let the OS handle the chunking.
      ok = socket:send(Sock, Data),
      TRef = send_after(?timeout, self(), timeout),
      From ! sent,
      nya_tcp_ctl(Sock, Handler, TRef);
    {send, From, Data} ->
      cancel_ref(TmRef),
      ok = tcp_send(Sock, Data),
      TRef = send_after(?timeout, self(), timeout),
      From ! sent,
      nya_tcp_ctl(Sock, Handler, TRef);
    {send, Data} when is_binary(Data) ->
      cancel_ref(TmRef),
      % Direct send. Let the OS handle the chunking.
      ok = socket:send(Sock, Data),
      TRef = send_after(?timeout, self(), timeout),
      %%From ! sent,
      nya_tcp_ctl(Sock, Handler, TRef);
    {send, Data} ->
      cancel_ref(TmRef),
      ok = tcp_send(Sock, Data),
      TRef = send_after(?timeout, self(), timeout),
      %%From ! sent,
      nya_tcp_ctl(Sock, Handler, TRef);
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
      nya_tcp_ctl(Sock, Handler, TRef);
    close ->
      case socket:peername(Sock) of
        {ok, Addr} -> ok;
        Any -> Addr = #{addr => {nan, nan, nan, nan}, port => nan},
          logging:err("Failed to get peername while closing connection: ~p @ io_proxy_tcp/3", [Any])
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
          logging:err("Peername error while handling timeout: ~p @ io_proxy_tcp/3", [Err])
      end;
    Any ->
      logging:warn("Recieved unknown cmd: ~p @ io_proxy_tcp/3", [Any])
  end,
  nya_tcp_ctl(Sock, Handler, TmRef).

nya_tcp_ctl_start(Sock, Handler) ->
  Handler ! {upstream, self()},
  socket:setopt(Sock, socket, active, once),
  TRef = send_after(?timeout, self(), timeout),
  nya_tcp_ctl(Sock, Handler, TRef).
