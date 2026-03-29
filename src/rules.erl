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

rule_no_content(_, close) ->
  logging:err("Received closed response in No-Content. Probably other rule has finished processing request earlier");
rule_no_content(_, Response) ->
  StrTime = util:get_time(),
  Headers = #{
    "Server" => ?version,
    "Date" => StrTime
  },
  %%logging:debug("Old=~p",[Response#response.headers]),
  Response#response{is_done = true, code = 204, body = "", headers = update_headers(Response, Headers)}.

rule_disallow(_, _) ->
  {aborted, 403}.

rule_set_header(_, close) ->
  logging:err("Received closed response in Set-Header. Probably other rule has finished processing request earlier");
rule_set_header([Header, Value], Response) ->
  Response#response{headers = update_headers(Response, #{Header => Value})}.

rule_set_code(_, close) ->
  logging:err("Received closed response in Set-Code. Probably other rule has finished processing request earlier");

rule_set_code(Arg, Response)->
  {Code, []} = string:to_integer(Arg),
  Response#response{code = Code}.

register_basic() ->
  logging:info("Registering basic rules"),
  register_rule("Abort", fun(Args, Resp) -> rule_abort(Args, Resp) end),
  register_rule("No-Content", fun(Args, Resp) -> rule_no_content(Args, Resp) end),
  register_rule("Disallow", fun(Args, Resp) -> rule_disallow(Args, Resp) end),
  register_rule("Set-Header", fun(Args, Resp) -> rule_set_header(Args, Resp) end), 
  register_rule("Set-Code", fun(Args, Resp) -> rule_set_code(Args, Resp) end),
  ok.




