-module(util).
-export([get_time/0]).

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
