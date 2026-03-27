%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2026, Anna-Sofia Kasierocka
%%% @doc
%%% This file contains module which sends static files.
%%% It exports following rules.
%%% # DocDir
%%% A directory from which static files are served.
%%% The routes also checked for presence of index.html if target path is directory.
%%% # SendFile
%%% Directly send given file for a route matched in configuration.
%%% @end
%%%-------------------------------------------------------------------
-module(static).
-author("f104a").
-behaviour(nya_module).
-include("../../request.hrl").
-include("../../response.hrl").
-include("../../config.hrl").

-export([init/0, terminate/1, get_custom_rules/0]).

rule_send_file(Arg, RawResponse) ->
  FInfo = file:read_file_info(Arg),
  %%logging:debug("FInfo: ~p", [FInfo]),
  case static_handler:stat_file(FInfo) of
    {FSize,ok} -> StrTime = util:get_time(),
      Response = handle:set_keepalive(RawResponse#response{headers = response:update_headers(RawResponse,
        #{"Content-Length" => erlang:integer_to_list(FSize),
          "Server" => ?version,
          "Date" => StrTime})}),
      Response#response.upstream ! cancel_tmr,
      static_handler:send_file(Response#response.socket, Arg, FSize),
      Response#response.upstream ! set_tmr,
      handle:close_connection(Response#response.request,Response#response.upstream),
      Response#response{is_finished=true};
    Any ->
      logging:err("Bad stat for ~s: ~p",[Arg, Any]),
      {aborted, 500}
  end.

rule_static_dir(Arg, Response) ->
  [DocDir] = Arg,
  static_handler:handle_file(DocDir, Response).

init() -> ok.

get_custom_rules() ->
  [
    {"SendFile", fun(Args, Response) -> rule_send_file(Args, Response) end},
    {"DocDir", fun(Args, Response) -> rule_static_dir(Args, Response) end}
  ].

terminate(_State) -> ok.