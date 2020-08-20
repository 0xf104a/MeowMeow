-module(parse_http).
-export([http2map/1,mime_by_ext/1,mime_by_fname/1, update_lines/2, is_request_finished/1]).
-define(max_request_length,10000).
-define(mime_types_file,"mime.types").

param2map(List)->
      if length(List) >= 2 -> 
          #{lists:nth(1,List)=>lists:nth(2,List)};
      length(List) == 1 -> #{"body"=>lists:nth(1,List)};
      true -> #{}
end.

parse_params(Params,Parsed)->
    if length(Params)<1 -> Parsed;
       true->CurParsed=string:split(lists:nth(1,Params),":",all),
             parse_params(lists:delete(lists:nth(1,Params),Params),maps:merge(Parsed,param2map(CurParsed)))
end.

http2map(Lines) ->
     Header = string:split(lists:nth(1,Lines)," ",all),
     if length(Header) < 3 -> {aborted,400};
        true-> Params = lists:delete(lists:nth(1,Lines),Lines),
               Parsed = #{method=>lists:nth(1,Header),route=>lists:nth(2,Header),http_ver=>lists:nth(3,Header),body=>""},
               {ok,parse_params(Params,Parsed)}
    end.

parse_mimes(Data,Index,Parsed)->
        
        if Index>length(Data) -> Parsed;
           true -> Params = string:split(lists:nth(Index, Data), " ", all),
                   if length(Params) < 2 -> parse_mimes(Data,Index+1,Parsed);
                   true->
                    RawType = lists:nth(1, Params),
                    RawExts = lists:nth(2, Params),
                    Exts = string:split(lists:nth(2,string:split(RawExts,"=", all)), ",", all),
                    Type = lists:nth(2, string:split(RawType, "=", all)),
                    parse_mimes(Data, Index+1, maps:merge(Parsed, #{Type=>Exts}))
                   end
        end.

read_mimes(FName) ->
    {ok, File} = file:read_file(FName),
    Content = unicode:characters_to_list(File),
    Preprocessed=string:split(Content,"\n",all),
    parse_mimes(Preprocessed,1,#{}).    
    
find_mime(Ext,Data) -> 
    if length(Data)<1 -> "application/octet-stream";
       true->  [H|T]=Data,
               if H /= <<>> ->
                    Head=binary_to_list(H),
                    Params=string:split(Head," "),
                    RawType = lists:nth(1, Params),
                    RawExts = lists:nth(2, Params),
                    Exts = string:split(lists:nth(2,string:split(RawExts,"=", all)), ",", all),
                    Type = lists:nth(2, string:split(RawType, "=", all)),
                    Found=lists:member(Ext,Exts),
                    if  Found -> Type;
                        true->find_mime(Ext,T)
                    end;
                    true-> "application/octet-stream"
              end
    end.
    
mime_by_ext(Ext)->
    FName=?mime_types_file,
    {ok, File} = file:read_file(FName),
    Content = unicode:characters_to_list(File),
    Preprocessed=re:split(Content,"\n\n|\r\n|\n|\r|\r\n|\032",[]),
    find_mime(Ext,Preprocessed).

mime_by_fname(FName)->
    Splitted=string:split(FName,".",all),
    mime_by_ext(lists:nth(length(Splitted),Splitted)).

magic_merge(Cat1,"",Cat2)->
     Cat1++Cat2;
magic_merge(Cat1,Any,Cat2)->
     lists:sublist(Cat1,length(Cat1)-1)++[Any]++lists:sublist(Cat2,2,length(Cat2)-1).
magic_merge(List1, List2)-> 
%% Merges 2 lists with concatting last element from List1
%% and first element from List2
    Cat1 = lists:last(List1),
    Cat2 = lists:nth(1,List2),
    New = Cat1++Cat2,
    magic_merge(List1,New,List2).
    
update_lines(OldLines, Raw)->
    Lines=string:split(Raw,"\r\n",all),
    magic_merge(OldLines,Lines).

is_request_finished(Lines) when length(Lines)<2 -> false;
is_request_finished(Lines) ->
    T0 = lists:nth(length(Lines), Lines),
    T1 = lists:nth(length(Lines)-1, Lines),
    if (T0 == <<>>) and (T1 == <<>>) -> true;
       true -> false
    end. 
