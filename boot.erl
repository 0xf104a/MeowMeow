#!/usr/bin/env escript
%%%-------------------------------------------------------------------
%%% @author p01ar
%%% @copyright (C) 2020, Polar Group
%%% @doc
%%%  Docker bootscript for MeowMeow webserver
%%% @end
%%% Created : 04. сент. 2020 14:04
%%%-------------------------------------------------------------------
-module(boot).
-author("p01ar").

main(_) ->
  server:run(8888),
  shell:start(false, true).
