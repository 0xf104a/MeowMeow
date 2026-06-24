%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2026, Anna-Sofia Kasierocka
%%% @doc
%%%  This module is responsible for loading nya_modules and handling
%%%  `LoadModules` directive in meow.conf
%%% @end
%%%-------------------------------------------------------------------
-module(nya_loader).
-author("f104a").

%% API
-export([nya_load/1, nya_load_all/0]).

nya_register_rules([]) -> ok;
nya_register_rules(Rules) ->
  [{RuleName, RuleFun} | Tail] = Rules,
  logging:debug("Registering ~p", [RuleName]),
  rules:register_rule(RuleName, RuleFun),
  nya_register_rules(Tail).

nya_register_context_rules([]) -> ok;
nya_register_context_rules(Rules) ->
  [{RuleName, RuleFun} | Tail] = Rules,
  logging:debug("Registering ~p", [RuleName]),
  rules:register_context_rule(RuleName, RuleFun),
  nya_register_context_rules(Tail).

nya_load(ModuleNameStr) ->
  logging:info("Loading module " ++ ModuleNameStr),
  ModuleName = list_to_atom(ModuleNameStr),
  %% 1. Try to load the module into the VM
  case code:ensure_loaded(ModuleName) of
    {module, ModuleName} ->
      %% 2. Verify it implements your behavior
      case is_nya_module(ModuleName) of
        true ->
          ok = ModuleName:init(),
          Rules = ModuleName:get_custom_rules(),
          ok = nya_register_rules(Rules),
          ContextRules = ModuleName:get_context_rules(),
          logging:debug("~p", [ContextRules]),
          ok = nya_register_context_rules(ContextRules),
          {ok, ModuleName};
        false ->
          logging:err("Can not load ~p: not a nya module", [ModuleNameStr]),
          {error, not_a_nya_module}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

nya_load_all() ->
  Modules = util:parse_list(configuration:get("LoadModules")),
  lists:map(fun(Mod) -> nya_load(Mod) end, Modules).

is_nya_module(Module) ->
  %% Check if the module exports the required behavior functions
  Attributes = Module:module_info(attributes),
  case lists:keyfind(behaviour, 1, Attributes) of
    {behaviour, Behaviours} ->
      lists:member(nya_module, Behaviours);
    _ -> false
  end.