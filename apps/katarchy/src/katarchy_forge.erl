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
  NewValue = apply_mod_fun(Fun, Value, Mech#mech.attack_power),
  Mech#mech{attack_power = NewValue};
apply_mod({hit_points, Fun, Value}, Mech) ->
  NewValue = apply_mod_fun(Fun, Value, Mech#mech.hit_points),
  Mech#mech{hit_points = NewValue};
apply_mod({speed, Fun, Value}, Mech) ->
  NewValue = apply_mod_fun(Fun, Value, Mech#mech.speed),
  Mech#mech{speed = NewValue}.


apply_mod_fun(minus, Param, OldValue) ->
  OldValue - Param;
apply_mod_fun(plus, Param, OldValue) ->
  OldValue + Param;
apply_mod_fun(set, Param, _) ->
  Param.


options(_, [], Options) ->
  lists:reverse(Options);
options(Mech, [BP|BPs], Options) ->
  NewMech = lists:foldl(fun(Mod, MechAcc) -> apply_mod(Mod, MechAcc) end,
                        Mech, BP#blueprint.mods),
  options(Mech, BPs, [{NewMech,BP}|Options]).

