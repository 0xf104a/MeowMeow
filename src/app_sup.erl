%%%-------------------------------------------------------------------
%%% @author andrezay
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(app_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  AChild = #{id => 'AName',
    start => {'server', start, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => ['AModule']},

  {ok, {#{strategy => one_for_one,
    intensity => 5,
    period => 30},
    [AChild]}
  }.
