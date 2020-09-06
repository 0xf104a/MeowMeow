%%%-------------------------------------------------------------------
%%% @author p01ar
%%% @copyright (C) 2020, Polar Group
%%% @doc
%%%   Application unit for MeowMeow webserver
%%% @end
%%%-------------------------------------------------------------------
-module(app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  app_sup:start_link().

stop(_State) ->
  ok.
