%%%-------------------------------------------------------------------
%%% @author p01ar
%%% @copyright (C) 2020, Polar Group
%%% @doc
%%%   Request record definitions
%%% @end
%%%-------------------------------------------------------------------
-author("p01ar").

-record(request, {src_addr, unfinished_line = "", 
                  route=nil, method=nil, header=#{}, 
                  http_ver = "HTTP/0.9", body = "", 
                  params = "", is_headers_accepted = false}).
