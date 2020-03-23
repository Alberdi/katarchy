-module(katarchy_forge).

-include("katarchy_blueprint.hrl").
-include("katarchy_mech.hrl").

-export([options/2]).

%% The only export will be called in the future from outside this module.
-ignore_xref([{?MODULE, options, 2}]).

%%--------------------------------------------------------------------
%% Exported functions
%%--------------------------------------------------------------------
options(Mech, BPs) ->
  options(Mech, BPs, []).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
apply_mod({attack_power, Fun, Value}, Mech) ->
  NewValue = apply_mod_fun(Fun, Value, Mech#mech.attack_power, attack_power),
  Mech#mech{attack_power = NewValue};
apply_mod({hit_points, Fun, Value}, Mech) ->
  NewValue = apply_mod_fun(Fun, Value, Mech#mech.hit_points, hit_points),
  Mech#mech{hit_points = NewValue};
apply_mod({speed, Fun, Value}, Mech) ->
  NewValue = apply_mod_fun(Fun, Value, Mech#mech.speed, speed),
  Mech#mech{speed = NewValue}.


apply_mod_fun(minus, Param, OldValue, hit_points) ->
  case Param >= OldValue of
    true -> throw({not_applicable, {hit_points, gte, Param+1}});
    false -> OldValue - Param
  end;
apply_mod_fun(minus, Param, OldValue, Field) ->
  case Param > OldValue of
    true -> throw({not_applicable, {Field, gte, Param}});
    false -> OldValue - Param
  end;
apply_mod_fun(plus, Param, OldValue, _) ->
  OldValue + Param;
apply_mod_fun(set, Param, _, _) ->
  Param.


options(_, [], Options) ->
  lists:reverse(Options);
options(Mech, [BP|BPs], Options) ->
  FoldFun = fun(Mod, {NewMech, Errors}) ->
                try {apply_mod(Mod, NewMech), Errors}
                catch throw:{not_applicable, Reason} ->
                        {NewMech, [Reason|Errors]}
                end end,
  case lists:foldl(FoldFun, {Mech, []}, BP#blueprint.mods) of
    {NewMech, []} -> options(Mech, BPs, [{NewMech,BP}|Options]);
    {_, Errors} -> options(Mech, BPs, [{not_applicable, Errors, BP}|Options])
  end.

