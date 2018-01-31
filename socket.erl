-module(socket).
-export([create_socket/1, socket_recv/2, socket_send/3, socket_shutdown/2,socket_accept/1,socket_recv_all/2,split_list/2]).
-define(chunk_wait_timeout,10).

split_list(List, Max) ->
    element(1, lists:foldl(fun
        (E, {[Buff|Acc], C}) when C < Max ->
            {[[E|Buff]|Acc], C+1};
        (E, {[Buff|Acc], _}) ->
            {[[E],Buff|Acc], 1};
        (E, {[], _}) ->
            {[[E]], 1}
    end, {[], 0}, List)).

create_socket(Port) ->
   {ok, Sock} = gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}]),
   Sock.

socket_recv(Sock, Timeout) -> %%Returns data
  {ok,Data} = gen_tcp:recv(Sock, 0,Timeout),
  Data.

socket_recv_all(Sock, Received) -> %%FIXME:Fix timeout
  case gen_tcp:recv(Sock, 0,?chunk_wait_timeout) of
  {ok,Data} -> socket_recv_all(Sock, Received ++ binary_to_list(Data));
  {error,closed} -> Received;
  {error,timeout} -> Received
  end.

socket_accept(LSock) ->
   {ok, Sock} = gen_tcp:accept(LSock),
   Sock.

socket_send_packets(Sock,[])->
   ok;
socket_send_packets(Sock,Packets)->
   [H|T]=Packets,
   ok=gen_tcp:send(Sock,lists:reverse(H)),
   socket_send_packets(Sock,T).

socket_send(Sock, Packet, ChunkSize) ->
   Chunks=split_list(Packet, ChunkSize),
   socket_send_packets(Sock,lists:reverse(Chunks)).

socket_shutdown(Sock, How) ->
   gen_tcp:send(Sock, How).
   

