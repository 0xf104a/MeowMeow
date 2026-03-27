%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2026, Anna-Sophie Kasierocka
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------


-module(gs_mod).
-behaviour(gen_server).

%% API
-export([start_link/3, call/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-include("../../request.hrl"). %% Assuming your records are here

%% API
start_link(RegName, Module, Opts) ->
  % We register the process locally so we can call it by name
  gen_server:start_link({local, RegName}, ?MODULE, [Module, Opts], []).

call(RegName, Request) ->
  gen_server:call(RegName, {execute, Request}).

%% Callbacks
init([Module, Opts]) ->
  {ok, #{mod => Module, options => Opts}}.

handle_call({execute, Request}, _From, State) ->
  %% Dynamic Dispatch: Call the 'handle_mcp' function on the module
  %% that was passed in during start_link
  HandlerMod = maps:get(mod, State),
  Result = HandlerMod:handle_mcp(Request),
  {reply, Result, State}.

handle_cast(_Msg, State) -> {noreply, State}.