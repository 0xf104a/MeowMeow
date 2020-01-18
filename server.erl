-module(server).
-export([run/1,loop/1,start/0]).
-import(socket,[create_socket/1,socket_recv/2,socket_accept/3,socket_send/2,socket_recv_all/2]).
-import(handle,[handle_http11/1,abort/1]).
-import(parse_http,[http2map/1]).

-define(CHUNK_SIZE,2048).
-define(TIMEOUT, 1000).

tup2list(Tuple) -> tup2list(Tuple, 1, tuple_size(Tuple)).

tup2list(Tuple, Pos, Size) when Pos =< Size ->
    [element(Pos,Tuple) | tup2list(Tuple, Pos+1, Size)];
tup2list(_Tuple,_Pos,_Size) -> [].

run(Port) ->
    io:fwrite("Starting up on port ~p ~n",[Port]),
    Sock = sock:create_socket(Port),
    spawn(fun() -> accept(Sock) end).

accept(Sock) ->
    MSock = sock:socket_accept(Sock),
    Pid = spawn(fun() ->
                     loop(MSock)
                     end),
    gen_tcp:controlling_process(MSock, Pid),
    accept(Sock).
loop(Sock) ->
    %%inet:setopts(Sock, [{active, once}]),
    Data=sock:socket_recv_all(Sock,""),
    {ok, {Address, Port}} = inet:peername(Sock),
    io:fwrite("[~p.~p.~p.~p:~p]:",tup2list(Address) ++ [Port]),
    case handle_http11(Data) of
         {aborted,Code} -> sock:socket_send(Sock,abort(Code),?CHUNK_SIZE);
         {ok,Headers,head} -> sock:socket_send(Sock,Headers,?CHUNK_SIZE);
         {ok,Headers,FileName} -> sock:socket_send(Sock,Headers,?CHUNK_SIZE),
                              {ok, FContent}=file:read_file(FileName),
                              Content=unicode:characters_to_list(binary_to_list(FContent)),
                              sock:socket_send(Sock,Content,?CHUNK_SIZE)
    end.

    %%receive
    %%{tcp, Socket, Data} ->
    %%    Response=Handler(Data,Socket),
    %%    io:fwrite("~p~n",[Response]),
    %%   socket:socket_send(Socket, Response);
    %%{tcp_closed, Socket}->
    %%    io:format("Socket ~p closed~n", [Socket]);
    %%{tcp_error, Socket, Reason} ->
    %%    io:format("Error on socket ~p reason: ~p~n", [Socket, Reason])
   %% end.

start() -> run(8888).
