-module(access).
-export([parse_access/1]).

get_cmd("")->pass;
get_cmd(Cmd)->
	L=string:split(Cmd," "),
	if length(L) == 0 -> pass;
	   length(L) == 1 -> {lists:nth(1,L),true};
	   length(L) >  1 -> [K|V]=L,
			     {K,V}
	end.

parse_line(Dev, {ok,Line})->
	Cmd=get_cmd(lists:nth(1,string:split(string:trim(Line),"#"))),
	case Cmd of
		pass->{ok,[]};
		{"Section", true}->{ok,[]};
		{"Route", [Name]}->{ok,[{Name,parse_section({ok,Dev}, [], Name)}]};
		{"End", _}->finish;
		{Key, Value}->{ok,[{Key,Value}]};
                Any -> logging:error("get_cmd/1 returned unexpected result ~p",[Any]) 
	end;

parse_line(_, eof)->finish.

parse_section({ok,Dev}, R, SectionName)->
	Line=file:read_line(Dev),
        logging:debug("Parsing ~p",[Line]),
	case parse_line(Dev, Line) of
		{ok, Data}->parse_section({ok,Dev},R++Data,SectionName);
		finish->R;
                Any->logging:error("parse_line/2 retuned unexpected result: ~p",[Any])
	end.

parse_access(FName)->
	Dev=file:open(FName,read),
	R=parse_section(Dev,[], global),
	file:close(Dev),
        R.

get_access(FName)->
	AccessFile=filaname:join([filename:dirname(FName),".access"]),
	parse_access(FName).
