%%%-------------------------------------------------------------------
%%% @doc EUnit suite for configuration.erl
%%%
%%% Covers the meow.conf line parser (parse_line/1, parse2map/2 -
%%% reachable thanks to export_all in the test profile), typed getters,
%%% and parsing of the real config files shipped in the repository.
%%% load/0 reads the compiled-in path /etc/nya/meow.conf and is only
%%% exercised when that file exists on the test machine.
%%% @end
%%%-------------------------------------------------------------------
-module(configuration_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/config.hrl").

setup() ->
  test_support:setup_config(#{"LogLevel" => "0",
                              "ListenPort" => "8080",
                              "AllowLegacyHttp" => "No",
                              "Flag" => " YES "}),
  test_support:tmp_dir().

cleanup(Dir) ->
  test_support:rm_rf(Dir),
  test_support:teardown_all(ok).

configuration_test_() ->
  {setup, fun setup/0, fun cleanup/1,
   fun(Dir) ->
     [getter_tests(), parse_line_tests(), parse_file_tests(Dir),
      shipped_config_tests(), load_tests()]
   end}.

%%--------------------------------------------------------------------
getter_tests() ->
  [{"get/1 returns the raw string value",
    ?_assertEqual("8080", configuration:get("ListenPort"))},
   {"get/2 string is the same as get/1",
    ?_assertEqual("8080", configuration:get("ListenPort", string))},
   {"get/2 int converts the value",
    ?_assertEqual(8080, configuration:get("ListenPort", int))},
   {"defaults from config.hrl are present when not overridden",
    ?_assertEqual("10000", configuration:get("KeepAlive"))},
   {"get/2 bool maps No to false",
    ?_assertEqual(false, configuration:get("AllowLegacyHttp", bool))},
   {"get/2 bool is case and whitespace insensitive (' YES ' -> true)",
    ?_assertEqual(true, configuration:get("Flag", bool))},
   {"get/1 of an unknown directive raises badkey",
    ?_assertError({badkey, "Nope"}, configuration:get("Nope"))},
   {"get/2 bool returns {error, parse_error} for a value other than Yes/No "
    "(that is what the function's own error branch tries to do). "
    "(regression: this used to fail because the branch calls logging:err with a format that has two "
    "placeholders but an empty argument list, so io_lib:format raises badarg)",
    ?_test(begin
             test_support:set_config("Weird", "maybe"),
             ?assertEqual({error, parse_error}, configuration:get("Weird", bool))
           end)}].

%%--------------------------------------------------------------------
parse_line_tests() ->
  [{"eof is passed through",
    ?_assertEqual(eof, configuration:parse_line(eof))},
   {"a directive line becomes a one-entry map",
    ?_assertEqual({ok, #{"ListenPort" => "8080"}},
                  configuration:parse_line({ok, "ListenPort 8080\n"}))},
   {"a comment-only line yields an empty map",
    ?_assertEqual({ok, #{}}, configuration:parse_line({ok, "# just a comment\n"}))},
   {"an empty line yields an empty map",
    ?_assertEqual({ok, #{}}, configuration:parse_line({ok, "\n"}))},
   {"an inline comment is stripped (meow.conf: 'KeepAlive 10000 #ms')",
    ?_assertEqual({ok, #{"KeepAlive" => "10000"}},
                  configuration:parse_line({ok, "KeepAlive 10000 #ms\n"}))},
   {"surrounding whitespace is trimmed",
    ?_assertEqual({ok, #{"LogLevel" => "4"}},
                  configuration:parse_line({ok, "   LogLevel 4   \n"}))},
   {"the value keeps everything after the first space (LoadModules list)",
    ?_assertEqual({ok, #{"LoadModules" => "static,header,route,env"}},
                  configuration:parse_line({ok, "LoadModules static,header,route,env\n"}))},
   {"the value is a flat string usable by list_to_integer",
    ?_test(begin
             {ok, #{"ListenPort" := V}} = configuration:parse_line({ok, "ListenPort 80\n"}),
             ?assertEqual(80, list_to_integer(V))
           end)}].

%%--------------------------------------------------------------------
parse_file_tests(Dir) ->
  File = test_support:write_file(filename:join(Dir, "meow.conf"),
    "# header comment\n"
    "ListenPort 80\n"
    "ListenHost 0.0.0.0\n"
    "\n"
    "LogLevel 4\n"
    "KeepAlive 100 # ms\n"
    "LoadModules static,header,route,env\n"),
  [{"parse2map merges the file over the defaults",
    ?_test(begin
             {ok, Dev} = file:open(File, read),
             {ok, Map} = configuration:parse2map(Dev, ?defconf),
             file:close(Dev),
             ?assertEqual("80", maps:get("ListenPort", Map)),
             ?assertEqual("0.0.0.0", maps:get("ListenHost", Map)),
             ?assertEqual("4", maps:get("LogLevel", Map)),
             ?assertEqual("100", maps:get("KeepAlive", Map)),
             ?assertEqual("static,header,route,env", maps:get("LoadModules", Map)),
             %% untouched defaults survive
             ?assertEqual("No", maps:get("AllowLegacyHttp", Map)),
             ?assertEqual("10240", maps:get("MaxPostSize", Map))
           end)},
   {"parse2map of an empty file returns the defaults untouched",
    ?_test(begin
             Empty = test_support:write_file(filename:join(Dir, "empty.conf"), ""),
             {ok, Dev} = file:open(Empty, read),
             ?assertEqual({ok, ?defconf}, configuration:parse2map(Dev, ?defconf)),
             file:close(Dev)
           end)},
   {"a later directive overrides an earlier one",
    ?_test(begin
             F = test_support:write_file(filename:join(Dir, "dup.conf"),
                                         "LogLevel 1\nLogLevel 3\n"),
             {ok, Dev} = file:open(F, read),
             {ok, Map} = configuration:parse2map(Dev, #{}),
             file:close(Dev),
             ?assertEqual(#{"LogLevel" => "3"}, Map)
           end)}].

%%--------------------------------------------------------------------
%% The config files committed to the repo must parse to what the README
%% and the E2E tests assume.
shipped_config_tests() ->
  Parse = fun(Rel) ->
            {ok, Dev} = file:open(Rel, read),
            {ok, Map} = configuration:parse2map(Dev, ?defconf),
            file:close(Dev),
            Map
          end,
  [{"config/meow.conf: port 80, host 127.0.0.1, keepalive 10000, the core modules, all loadable",
    ?_test(begin
             Map = Parse("config/meow.conf"),
             ?assertEqual("80", maps:get("ListenPort", Map)),
             ?assertEqual("127.0.0.1", maps:get("ListenHost", Map)),
             ?assertEqual("10000", maps:get("KeepAlive", Map)),
             ?assertEqual("No", maps:get("AllowLegacyHttp", Map)),
             Mods = util:parse_list(maps:get("LoadModules", Map)),
             ?assertEqual([], ["static", "header", "route", "env"] -- Mods),
             %% every listed module must exist in the build
             ?assertEqual([], [M || M <- Mods, code:ensure_loaded(list_to_atom(M)) =/= {module, list_to_atom(M)}])
           end)},
   {"tests/config/meow.conf: what the E2E Docker image runs with, all modules loadable",
    ?_test(begin
             Map = Parse("tests/config/meow.conf"),
             ?assertEqual("80", maps:get("ListenPort", Map)),
             ?assertEqual("0.0.0.0", maps:get("ListenHost", Map)),
             ?assertEqual("100", maps:get("KeepAlive", Map)),
             ?assertEqual(4, list_to_integer(maps:get("LogLevel", Map))),
             Mods = util:parse_list(maps:get("LoadModules", Map)),
             ?assertEqual([], [M || M <- Mods, code:ensure_loaded(list_to_atom(M)) =/= {module, list_to_atom(M)}])
           end)}].

%%--------------------------------------------------------------------
load_tests() ->
  case filelib:is_regular(?configfile) of
    false -> [];
    true ->
      [{"load/0 reads " ++ ?configfile ++ " into a fresh config_storage table",
        ?_test(begin
                 catch ets:delete(config_storage),
                 ?assertEqual(ok, configuration:load()),
                 ?assertMatch([{table, _}], ets:lookup(config_storage, table)),
                 ?assert(is_list(configuration:get("ListenPort")))
               end)}]
  end.
