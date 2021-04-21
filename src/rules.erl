%%%-------------------------------------------------------------------
%%% @author p01ar
%%% @copyright (C) 2020, Polar Group
%%% @doc
%%%  MeowMeow webserver rules handler
%%% @end
%%% Created : 26. авг. 2020 19:00
%%%-------------------------------------------------------------------
-module(rules).
-author("p01ar").
-include("config.hrl").
-include("response.hrl").
-include("request.hrl").
-import(response, [update_headers/2]).
-import(util, [sget2/2, pretty_addr/1]).
%% API
-export([init_rules/0, register_rule/2, execute_rule/3, register_basic/0]).

init_rules() ->
  %% Initializes rules table
  %% Returns nothing
  ets:new(rules, [set, named_table]),
  ok.

register_rule(Rule, RuleHandler) ->
  %% Register rule handler
  %% RuleHandler/2 -- function of Args, Response
  %% - returns processed response map
  ets:insert(rules, [{Rule, RuleHandler}]).

execute_rule(Rule, Args, Response) ->
  logging:debug("Executing rule ~s",[Rule]),
  case ets:lookup(rules, Rule) of
       [{Rule, Handler}] -> Handler(Args, Response);
       Any -> logging:err("Bad rule ~s, lookup responded: ~p",[Rule, Any]),
              {aborted, 500}
  end.

%% Basic rules
rule_abort(Args, _) ->
  {aborted, list_to_integer(lists:nth(1, Args))}.

rule_no_content(_, Response) ->
  StrTime = util:get_time(),
  Headers = #{
    "Server" => ?version,
    "Date" => StrTime
  },
  logging:debug("Old=~p",[Response#response.headers]),
  Response#response{is_done = true, code = 204, body = "", headers = update_headers(Response, Headers)}.

rule_disallow(_, _) ->
  {aborted, 403}.

rule_set_header(Arg, Response) ->
  [Header|Value] = string:split(Arg, " "),
  Response#response{headers = update_headers(Response, #{Header => Value})}.

rule_set_code(Arg, Response)->
  {Code, []} = string:to_integer(Arg),
  Response#response{code = Code}.

rule_fcgi_exec(Arg, Response) ->
  StrTime = util:get_time(),
  Headers = #{
    "Server" => ?version,
    "Date" => StrTime
  },
  NewResp = Response#response{headers = update_headers(Response, Headers)},
  try fcgi:fcgi_exec(Arg,NewResp) of
      Resp -> Resp
  catch
      Err -> logging:err("FastCGI seems to be unavailable!"),
             logging:debug("Error was ~p",[Err]),
             {aborted, 502}
  end.

rule_send_file(Arg, RawResponse) ->
   case handle:stat_file(file:read_file_info(Arg)) of
        {FSize,ok} -> Response = RawResponse#response{headers = update_headers(RawResponse, 
                                 #{"Content-Length" => erlang:integer_to_list(FSize)})},
                      io_proxy:tcp_send(Response#response.socket, 
                                 response:response_headers(Response#response.headers, 
                                                           Response#response.code)),
                       handle:send_file(Arg, Response#response.socket, ?chunk_size), 
                       Response#response{is_finished=true};
        Any ->         logging:err("Bad stat for ~s: ~p",[Arg, Any]),
                       {aborted, 500}
   end.

register_basic() ->
  logging:info("Registering basic rules"),
  register_rule("Abort", fun(Args, Resp) -> rule_abort(Args, Resp) end),
  register_rule("No-Content", fun(Args, Resp) -> rule_no_content(Args, Resp) end),
  register_rule("Disallow", fun(Args, Resp) -> rule_disallow(Args, Resp) end),
  register_rule("Set-Header", fun(Args, Resp) -> rule_set_header(Args, Resp) end), 
  register_rule("ExecFCGI", fun(Args, Resp) -> rule_fcgi_exec(Args, Resp) end),
  register_rule("Set-Code", fun(Args, Resp) -> rule_set_code(Args, Resp) end),
  register_rule("Send-File", fun(Args, Resp) -> rule_send_file(Args, Resp) end),
  ok.




