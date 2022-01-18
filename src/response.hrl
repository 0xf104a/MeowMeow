%%%-------------------------------------------------------------------
%%% @author p01ar
%%% @copyright (C) 2020, Polar Group
%%% @doc
%%%  response record definition
%%% @end
%%% Created : 28. авг. 2020 14:28
%%%-------------------------------------------------------------------
-author("p01ar").

%% record response
%% request – data sent by client
%% code - RFC 2616 compliant HTTP response code
%% socket - Erlang socket to sent big chunks
%% body - RFC 2616 response body
%% upstream - controlling proccess(io_proxy)
%% is_ready - whether rule handling is finished
%% is_done - wether request ready to be sent immeadiatly
%% is_finished - whether request was sent by another module
%% headers - map of response headers to their values 
-record(response, {request, code, socket=bad, body = "", upstream = bad, 
                   is_ready=false, is_finished = false, is_done = false, headers = #{}}).
