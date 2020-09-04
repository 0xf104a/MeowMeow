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
  logging:debug("Execute rule: ~p ~p ~p", [Rule, Args, Response]),
  [{Rule, Handler}] = ets:lookup(rules, Rule),
  Handler(Args, Response).

%% Basic rules
rule_abort(Args, _) ->
  {aborted, list_to_integer(lists:nth(1, Args))}.

rule_no_content(_, Response) ->
  StrTime = util:get_time(),
  R = response:response_headers(
    #{"Date" => StrTime,
      "Connection" => "close",
      "Server" => ?version}, 204),
  Response#response.upstream ! {send, R},
  Response#response{is_finished = true}.
rule_disallow(_, _) ->
  {aborted, 403}.

register_basic() ->
  logging:info("Registering basic rules"),
  register_rule("Abort", fun(Args, Any) -> rule_abort(Args, Any) end),
  register_rule("No-Content", fun(Args, Any) -> rule_no_content(Args, Any) end),
  register_rule("Disallow", fun(Args, Any) -> rule_disallow(Args, Any) end),
  ok.




