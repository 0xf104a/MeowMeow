%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%  Provides environment-variable based context rules for access
%%%  configuration.
%%%
%%%  This module registers conditional rules that allow route sections to be
%%%  enabled or skipped depending on OS environment variables:
%%%
%%%  <ul>
%%%    <li>`If-Env Var Value' passes when `os:getenv(Var) == Value'.</li>
%%%    <li>`If-Not-Env Var Value' passes when `os:getenv(Var) /= Value'.</li>
%%%    <li>`If-Env-Def Var' passes when `Var' is defined in the environment.</li>
%%%  </ul>
%%%
%%%  Invalid argument lists make the corresponding context rule fail.
%%% @end
%%% -------------------------------------------------------------------
-module(env).
-author("f104a").

-behaviour(nya_module).

%% API
-export([init/0, terminate/1, get_custom_rules/0, get_context_rules/0]).

init() -> ok.

terminate(_State) -> ok.

get_custom_rules() -> [].

get_context_rules() ->
  [
    {"If-Env", fun(Args, Request) -> context_if_env(Args, Request) end},
    {"If-Not-Env", fun(Args, Request) -> context_if_not_env(Args, Request) end},
    {"If-Env-Def", fun(Args, Request) -> context_if_env_def(Args, Request) end}
  ].

context_if_env([Var, Value], _Request) ->
  os:getenv(Var) == Value;
context_if_env(_, _) -> false.

context_if_not_env([Var, Value], _Request) ->
  os:getenv(Var) /= Value;
context_if_not_env(_, _) -> false.

context_if_env_def([Var], _Request) ->
  os:getenv(Var) /= false;
context_if_env_def(_, _) -> false.
