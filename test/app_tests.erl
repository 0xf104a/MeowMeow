%%%-------------------------------------------------------------------
%%% @doc EUnit suite for app.erl / app_sup.erl and the application spec
%%% app:start/2 boots the real server (compiled-in config paths) and is
%%% not started here; the supervisor child spec and metadata are checked.
%%% @end
%%%-------------------------------------------------------------------
-module(app_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/config.hrl").

setup() -> test_support:setup_config(#{"LogLevel" => "0"}).

app_test_() ->
  {setup, fun setup/0, fun test_support:teardown_all/1,
   fun(_) -> [sup_tests(), app_tests(), spec_tests()] end}.

sup_tests() ->
  [{"app_sup:init/1 returns a valid one_for_one supervisor spec",
    ?_test(begin
             {ok, {Flags, Children}} = app_sup:init([]),
             ?assertMatch(#{strategy := one_for_one}, Flags),
             ?assertEqual(ok, supervisor:check_childspecs(Children))
           end)},
   {"the single child starts the server via server:start/0 as a permanent worker",
    ?_test(begin
             {ok, {_, [Child]}} = app_sup:init([]),
             ?assertMatch(#{start := {server, start, []}, restart := permanent, type := worker}, Child)
           end)}].

app_tests() ->
  [{"app:stop/1 logs and returns ok", ?_assertEqual(ok, app:stop(state))}].

spec_tests() ->
  Load = fun() ->
           case application:load(app) of
             ok -> ok;
             {error, {already_loaded, app}} -> ok
           end
         end,
  [{"the application spec loads and names app as its callback module",
    ?_test(begin
             ok = Load(),
             ?assertEqual({ok, {app, []}}, application:get_key(app, mod))
           end)},
   {"the application depends on kernel, stdlib and crypto (mcp_port needs crypto)",
    ?_test(begin
             ok = Load(),
             {ok, Deps} = application:get_key(app, applications),
             ?assertEqual([], [kernel, stdlib, crypto] -- Deps)
           end)},
   {"the Server header version string matches the application vsn",
    ?_test(begin
             ok = Load(),
             {ok, Vsn} = application:get_key(app, vsn),
             ?assertEqual("MeowMeow/" ++ Vsn, lists:sublist(?version, length("MeowMeow/" ++ Vsn)))
           end)},
   {"e2e/vsn.txt (what the E2E suite compares the Server header against) matches the app vsn",
    ?_test(begin
             ok = Load(),
             {ok, Vsn} = application:get_key(app, vsn),
             {ok, Bin} = file:read_file("e2e/vsn.txt"),
             Txt = string:trim(binary_to_list(Bin)),
             ?assertEqual(Vsn, lists:sublist(Txt, length(Vsn)))
           end)}].
