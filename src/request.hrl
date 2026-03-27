%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2020, Anna-Sofia Kasierocka
%%% @doc
%%%   Request record definitions
%%% @end
%%%-------------------------------------------------------------------
-author("f104a").

-record(request, {src_addr, unfinished_line = "", 
                  route=nil, method=nil, header=#{}, 
                  http_ver = "HTTP/0.9", body = "", 
                  params = "", is_headers_accepted = false}).
