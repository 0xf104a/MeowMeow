%%%-------------------------------------------------------------------
%%% @author p01ar
%%% @copyright (C) 2020, Polar Group
%%% @doc
%%%   Request record definitions
%%% @end
%%% Created : 28. авг. 2020 14:30
%%%-------------------------------------------------------------------
-author("p01ar").

-record(request, {src_addr, route, method, header, body = ""}).
