-module(handle).
-export([handle_http11/1,abort/1,handle_headers/1, handler_start/1]).
-import(response,[response/3,get_desc/1]).
-import(parse_http,[http2map/1,mime_by_fname/1]).
-include_lib("kernel/include/file.hrl").
-define(version,"Ghost/1.0.1-alpha-180820").
-import(util, [get_time/0]).

abort(Code) ->
   Body=lists:flatten(io_lib:format("<html><head><title>~p ~s</title></head><body><h1><i>~p ~s</i></h1><hr><i> ~s </i></body></html>",[Code,get_desc(integer_to_list(Code)),Code,get_desc(integer_to_list(Code)),?version])),
   io:fwrite("~p ~s ~n",[Code,get_desc(integer_to_list(Code))]),
   StrTime=get_time(),
   response:response( #{"Date" => StrTime,
                        "Content-Type" => "text/html",
                        "Connection"=>"close",
                        "Server"=>?version},Code,Body).

get_filename(Route) ->
    FileName = "www"++Route,
    IndexName = "www"++Route++"/index.html",
    FileExists = filelib:is_regular(FileName),
    IndexExists = filelib:is_regular(IndexName),
    if FileExists -> FileName;
       IndexExists -> IndexName;
       true -> no_file
    end.

stat_file(no_file)->{0,no_file};
stat_file(FName)->
    {ok,FInfo}=file:read_file_info(FName),
    Access =FInfo#file_info.access,
    FSize = FInfo#file_info.size,
    if (Access /= read) and (Access /= read_write)-> {0,no_access};
       FSize == 0 -> {0,empty_file};
       true -> {FSize, ok}
    end.

handle_request(Upstream, Request) ->
    case maps:get(Request, method) of
         "GET" -> handle_get(Upstream, Request) 
handle_http11(Upstream, Request) -> 
    case parse_http:http2map(Request) of
         {aborted, Code} -> 
               Upstream ! abort(Code),
               Upstream ! close;
         {ok, Map} -> 
               handle_request(Upstream, Map);
         Any -> 
               logging:err("Parser returned bad response: ~p",[Any])
    end.
handler(Sock, Upstream, RequestLines)->
   receive 
	{data, Data}->
	     Lines = parse_http:update_lines(RequestLines, Data),
             logging:debug("Updated lines: ~p",[Lines]),
             Finished = parse_http:is_request_finished(Lines),
             if Finished ->
                logging:debug("Responding back"),
                
		exit(done)
             end
   end,
   handler(Sock, Upstream, Lines).
  
handler_start(Sock)->
   receive
	{upstream, Upstream} ->
			logging:debug("Recieved upstream PID. Starting handling."),
			Upstream ! recv,
			handler(Sock, Upstream, [""])
   end.
