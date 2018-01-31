-module(handle).
-export([handle_http11/1,abort/1,handle_headers/1]).
-import(response,[response/3,get_desc/1]).
-import(parse_http,[http2map/1,mime_by_fname/1]).
-define(version,"Ghost/1.0.0-prealpha-290118").


    
get_time() -> 
    Months=#{1=>"Jan",
             2=>"Feb",
             3=>"Mar",
             4=>"Apr",
             5=>"May",
             6=>"Jun",
             7=>"Jul",
             8=>"Aug",
             9=>"Sep",
             10=>"Oct",
             11=>"Nov",
             12=>"Dec"},
    Days=#{1=>"Mon",
           2=>"Tue",
           3=>"Wed",
           4=>"Thu",
           5=>"Fri",
           6=>"Sat",
           7=>"Sun"},
    {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:universaltime(),
    WeekDay=maps:get(calendar:day_of_the_week(Year,Month,Day),Days),
    lists:flatten(io_lib:format("~s, ~2..0w  ~s ~4..0w ~2..0w:~2..0w:~2..0w GMT",[WeekDay,Day,maps:get(Month,Months),Year,Hour,Minute,Second])).
abort(Code) ->
   Body=lists:flatten(io_lib:format("<html><head><title>~p ~s</title></head><body><h1><i>~p ~s</i></h1><hr><i> ~s </i></body></html>",[Code,get_desc(integer_to_list(Code)),Code,get_desc(integer_to_list(Code)),?version])),
   io:fwrite("~p ~s ~n",[Code,get_desc(integer_to_list(Code))]),
   StrTime=get_time(),
   response:response( #{"Date" => StrTime,
                        "Content-Type" => "text/html",
                        "Connection"=>"keep-alive",
                        "Server"=>?version},Code,Body).
get_filename(Route) ->
    FileName = "www"++Route,
    IndexName = "www"++Route++"/index.html",
    FileExists = filelib:is_regular(FileName),
    IndexExists = filelib:is_regular(IndexName),
    if FileExists -> FileName;
       IndexExists -> IndexName;
       true -> ""
    end.
 
handle_headers(Data)->
    [BinRoute|Params] = re:split(maps:get("route",Data),"\\?|\\#"),
    Route=unicode:characters_to_list(binary_to_list(BinRoute)),
    FileName=get_filename(Route),
    RouteInsecure=(string:rstr(Route,"..")/=0),
    io:fwrite("~s ~s -- ",[maps:get("method",Data),maps:get("route",Data)]),
    StrTime=get_time(),
    if  RouteInsecure->{aborted,400};
        FileName /= ""  ->
        ContentLength=filelib:file_size(FileName),
        if ContentLength>0 ->
                io:fwrite("200 OK ~n"),
                {ok,response:response_headers( #{"Date" => StrTime,
                                             "Connection" => "keep-alive",
                                             "Content-Length" => integer_to_list(ContentLength),
                                             "Content-Type" => mime_by_fname(FileName),
                                             "Server" => ?version},200),FileName};
                true->{ok,io:fwrite("204 No Content ~n"),
                        response:response( #{"Date" => StrTime,
                                            "Connection" => "keep-alive",
                                            "Content-Length" => integer_to_list(ContentLength),
                                            "Content-Type" => mime_by_fname(FileName),
                                            "Server" => ?version},204),FileName}
        end;
        true->{aborted,404}
    end.
    
handle(Data) ->  
    [BinRoute|Params] = re:split(maps:get("route",Data),"\\?|\\#"),
    Route=unicode:characters_to_list(binary_to_list(BinRoute)),
    FileName=get_filename(Route),
    RouteInsecure=(string:rstr(Route,"..")/=0),
    StrTime=get_time(),
    if RouteInsecure->abort(400);
       FileName /= ""  ->
        {ok, FContent}=file:read_file(FileName),
        Content=unicode:characters_to_list(binary_to_list(FContent)),
        ContentLength=length(Content),
        if ContentLength>0 ->
                io:fwrite("200 OK ~n"),
                response:response( #{"Date" => StrTime,
                                    "Connection" => "keep-alive",
                                    "Content-Length" => integer_to_list(ContentLength),
                                    "Content-Type" => mime_by_fname(FileName),
                                    "Server" => ?version},200,Content );
            true->io:fwrite("204 No Content ~n"),
                    response:response( #{"Date" => StrTime,
                                        "Connection" => "keep-alive",
                                        "Content-Length" => integer_to_list(ContentLength),
                                        "Content-Type" => mime_by_fname(FileName),
                                        "Server" => ?version},204,"")
        end;
       true -> abort(404)

       end.


                        
handle_http11(Data) ->
   case parse_http:http2map(Data) of
   {aborted,Code} -> abort(Code);
   {ok,Map} -> handle_headers(Map)
   end.
   

            
                    

              
