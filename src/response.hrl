%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2020, Anna-Sofia Kasierocka
%%% @doc
%%%  Response record definition
%%% @end
%%%-------------------------------------------------------------------
-author("f104a").

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
                   is_sent = false, is_ready2send = false, headers = #{}}).
