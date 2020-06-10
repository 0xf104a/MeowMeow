-module(handle).
-export([handle_http11/1,abort/1,handle_headers/1]).
-import(response,[response/3,get_desc/1]).
-import(parse_http,[http2map/1,mime_by_fname/1]).
-include_lib("kernel/include/file.hrl").
-define(version,"Ghost/1.0.0-prealpha-140220").



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

handle_file(_,_,no_file)->{aborted,404};
handle_file(_,_,no_access)->{aborted,403};
handle_file(FileName,_,empty_file)->io:fwrite("204 No Content~n"),
                                    {ok,response:response_headers( #{"Date" => get_time(),
                                             "Connection" => "keep-alive",
                                             "Content-Length" => integer_to_list(0),
                                             "Content-Type" => mime_by_fname(FileName),
                                             "Server" => ?version},204),FileName};
handle_file(FileName,FSize,ok)->io:fwrite("200 OK~n"),
                                {ok,response:response_headers( #{"Date" => get_time(),
                                            "Connection" => "keep-alive",
                                            "Content-Length" => integer_to_list(FSize),
                                            "Content-Type" => mime_by_fname(FileName),
                                            "Server" => ?version},200),FileName}.
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

handle_head(Data)->
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
                                            "Server" => ?version},200),head};
                true->io:fwrite("204 No Content ~n"),
                      {ok,response:response_headers( #{"Date" => StrTime,
                                            "Connection" => "keep-alive",
                                            "Content-Length" => integer_to_list(ContentLength),
                                            "Content-Type" => mime_by_fname(FileName),
                                            "Server" => ?version},204),head}
        end;
        true->{aborted,404}
      end.

handle_get(Data)->
    [BinRoute|Params] = re:split(maps:get("route",Data),"\\?|\\#"),
    Route=unicode:characters_to_list(binary_to_list(BinRoute)),
    FileName=get_filename(Route),
    RouteInsecure=(string:rstr(Route,"..")/=0),
    io:fwrite("~s ~s -- ",[maps:get("method",Data),maps:get("route",Data)]),
    {FSize,Stat}=stat_file(FileName),
    if  RouteInsecure->{aborted,400};
        true->handle_file(FileName,FSize,Stat)
    end.

handle_headers(Data)->
    case maps:get("method",Data) of
        "GET"->handle_get(Data);
        "HEAD"->handle_head(Data);
        _Else->{aborted,501}
    end.

handle_http11(Data) ->
   case parse_http:http2map(Data) of
   {aborted,Code} -> {aborted,Code};
   {ok,Map} -> handle_headers(Map);
   true->io:fwrite("500 Internal Server Error ~n"),
         io:fwrite("http2map() failure -- Unable to match ~n"),
         {aborted,500}
   end.
