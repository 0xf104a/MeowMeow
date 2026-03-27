%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(nya_loader).
-author("f104a").

%% API
-export([nya_load/1, nya_load_all/0]).

nya_register_rules([]) -> ok;
nya_register_rules(Rules) ->
  [{RuleName, RuleFun} | Tail] = Rules,
  rules:register_rule(RuleName, RuleFun),
  nya_register_rules(Tail).

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
          {ok, ModuleName};
        false -> {error, not_a_nya_module}
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