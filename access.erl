-module(access).
-export([parse_access/1]).

get_cmd("")->{};
get_cmd(Cmd)->
	L=string:split(Cmd," "),
	if length(L) == 0 -> pass;
	   length(L) == 1 -> [{lists:nth(L,1),true}];
	   length(L) >  1 -> [K|V]=L,
			     [{K,V}]
	end.

parse_line(Dev, {ok,Line})->
	Cmd=get_cmd(lists:nth(1,string:split(string:trim(Line),"#"))),
	case Cmd of
		pass->{ok,[]};
		{"Section", true}->{ok,[]};
		{"Section", Name}->{ok,[{Name,parse_section(Dev, [], Name)}]};
		{"EndSection", _}->finish;
		{Key, Value}->{ok,[{Key,Value}]}
	end;
parse_line(_, eof)->finish.

parse_section(Dev, R, SectionName)->
	Line=file:read_line(Dev),
	case parse_line(Dev, Line) of
		{ok, Data}->parse_section(Dev,R++Data,SectionName);
		finish->R
	end.

parse_access(FName)->
	Dev=file:open(FName),
	R=parse_section(Dev,{}, global),
	file:close(Dev),
        R.

