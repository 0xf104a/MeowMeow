%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2026, Anna-Sofia Kasierocka
%%% @doc
%%%   Behaviour definition for MeowMeow handler modules.
%%%
%%%   Any module that wants to be loaded dynamically by nya_loader
%%%   must implement this behaviour.
%%%
%%%   Callbacks:
%%%     init/1        – called once at startup with module config (proplist).
%%%                     Return {ok, State} or {error, Reason}.
%%%     get_custom_rules/0 - returns custom rules which may be used to invoke
%%%                          this module on some routes defined in configuration.
%%%
%%%     terminate/1   – called on clean shutdown.
%%% @end
%%%-------------------------------------------------------------------
-module(nya_module).
-author("f104a").
-include("request.hrl").
-include("response.hrl").

-type request()  :: #request{}.   %% from request.hrl
-type response() :: #response{}.  %% from response.hrl
-type config()   :: [{atom(), term()}].
-type state()    :: term().

-callback init() ->
  ok | {error, Reason :: term()}.

-callback get_custom_rules() ->
   Rules :: [{
     string(),
     fun(([string()], response()) ->
       HandledResp :: {aborted, integer()} | {ok, 204} | RespData :: response())
   }].

-callback terminate(State :: state()) -> ok.

-export_type([request/0, response/0, config/0, state/0]).
 