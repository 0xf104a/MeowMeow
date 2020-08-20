-module(io_proxy).
-export([io_proxy_tcp_start/2]).
-define(chunk_size,1400). %%TODO:Config file
-define(timeout, 10000).
tcp_send(Sock, Data) when length(Data)<?chunk_size -> 
        logging:debug("Data=~p.Finished.",[Data]),
        ok = socket:send(Sock, Data);
tcp_send(Sock, Data) ->
        {H,T} = lists:split(?chunk_size, Data),
        ok = socket:send(Sock, H),
        logging:debug("Data=~p, recalling...",[H]),
        tcp_send(Sock, T).

io_proxy_tcp(Sock, Handler)->
	receive
	  {recv, Size} ->
		{ok, Data} = socket:recv(Sock, Size),
		Handler ! {data, Data};
	  {send, Data} ->
	        tcp_send(Sock, Data);
	   recv ->
		case socket:recv(Sock) of
		     {ok, Data}->
			Handler ! {data, Data};
		     {error, closed}->
			logging:err("Recv from closed socket");
                     {error, Other}->
                        logging:err("Error while receiving: ~p", [Other])
		end;
	  close ->
		socket:close(Sock),
		exit(requested)
	end,
	io_proxy_tcp(Sock, Handler).

io_proxy_tcp_start(Sock, Handler)->
	Handler ! {upstream, self()},
	io_proxy_tcp(Sock, Handler).
