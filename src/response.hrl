%%%-------------------------------------------------------------------
%%% @author p01ar
%%% @copyright (C) 2020, Polar Group
%%% @doc
%%%  response record definition
%%% @end
%%% Created : 28. авг. 2020 14:28
%%%-------------------------------------------------------------------
-author("p01ar").

-record(response, {request, code, socket=bad, body = "", upstream = bad, is_finished = false, is_done = false, headers = #{}}).
