%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2020, 2026, Anna-Sofia Kasierocka
%%% @doc
%%%  MeowMeow webserver base rules.
%%%  Contains rule storing logic and base rules available
%%%  without modules
%%% @end
%%%-------------------------------------------------------------------
-module(rules).
-author("f104a").
-include("config.hrl").
-include("response.hrl").
-include("request.hrl").
-import(response, [update_headers/2]).
-import(util, [sget2/2, pretty_addr/1]).
-import(parse_http, [is_close/1]).
%% API
-export([init_rules/0, register_rule/2, execute_rule/3, register_basic/0, rulechain_exec/2]).

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
  logging:debug("Executing rule ~s ~p",[Rule, Args]),
  case ets:lookup(rules, Rule) of
       [{Rule, Handler}] ->
         try
           Handler(Args, Response)
         catch error:Error ->
           logging:err("Can not execute rule: ~p", [Error]),
           {aborted, 502}
         end;
       Any -> logging:err("Bad rule ~s, lookup responded: ~p",[Rule, Any]),
              {aborted, 500}
  end.

%% Basic rules
rule_abort(Args, _) ->
  {aborted, list_to_integer(lists:nth(1, Args))}.

rule_no_content(_, close) ->
  logging:err("Received closed response in No-Content. Probably other rule has finished processing request earlier");
rule_no_content(_, Response) ->
  StrTime = util:get_time(),
  Headers = #{
    "Server" => ?version,
    "Date" => StrTime
  },
  %%logging:debug("Old=~p",[Response#response.headers]),
  {ready2send, Response#response{is_ready2send = true, code = 204, body = "", headers = update_headers(Response, Headers)}}.

rule_disallow(_, _) ->
  {aborted, 403}.

rule_set_header(_, close) ->
  logging:err("Received closed response in Set-Header. Probably other rule has finished processing request earlier");
rule_set_header([Header, Value], Response) ->
  {ok, Response#response{headers = update_headers(Response, #{Header => Value})}}.

rule_set_code(_, close) ->
  logging:err("Received closed response in Set-Code. Probably other rule has finished processing request earlier");

rule_set_code(Arg, Response)->
  {Code, []} = string:to_integer(Arg),
  {ok, Response#response{code = Code}}.

rulechain_exec([], Response) -> {ok, Response};
rulechain_exec(Rules, Response) ->
  [{Rule, Args} | T] = Rules,
  NewResponseState = rules:execute_rule(Rule, Args, Response),
  case NewResponseState of
    {aborted, Code} -> {aborted, Code};
    {sent, NewResponse} -> {sent, NewResponse};
    {ok, NewResponse} -> rulechain_exec(T, NewResponse);
    {ready2send, NewResponse} -> {ok, NewResponse};
    Legacy ->
      logging:err("Got unexpected response! Maybe legacy? ~p @ rules:rulechain_exec/2", [Legacy]),
      logging:debug("Rule=~p", [Rule]),
      {aborted, 502}
  end.

register_basic() ->
  logging:info("Registering basic rules"),
  register_rule("Abort", fun(Args, Resp) -> rule_abort(Args, Resp) end),
  register_rule("No-Content", fun(Args, Resp) -> rule_no_content(Args, Resp) end),
  register_rule("Disallow", fun(Args, Resp) -> rule_disallow(Args, Resp) end),
  register_rule("Set-Header", fun(Args, Resp) -> rule_set_header(Args, Resp) end), 
  register_rule("Set-Code", fun(Args, Resp) -> rule_set_code(Args, Resp) end),
  ok.




