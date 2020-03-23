-module(katarchy_forge_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include("katarchy_blueprint.hrl").
-include("katarchy_mech.hrl").

%%--------------------------------------------------------------------
%% CT callbacks
%%--------------------------------------------------------------------
suite() ->
  [{timetrap, {seconds, 10}}].

all() ->
  [attack_power_decrease,
   attack_power_decrease_not_applicable,
   attack_power_decrease_to_zero,
   attack_power_increase,
   attack_power_reset,
   attack_power_set,
   empty_blueprints,
   hitpoints_decrease,
   hitpoints_decrease_not_applicable,
   hitpoints_decrease_to_zero,
   hitpoints_increase,
   hitpoints_set,
   multiple_blueprints,
   multiple_mods,
   multiple_mods_decrease_not_applicable,
   multiple_mods_decrease_one_not_applicable,
   same_mech,
   same_mech_modified,
   speed_decrease,
   speed_decrease_not_applicable,
   speed_decrease_to_zero,
   speed_increase,
   speed_set].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------
%% Test that a blueprint might decrease the attack power.
attack_power_decrease(_Config) ->
  BP = #blueprint{mods = [{attack_power, minus, 5}]},
  [{Mech, BP}] = katarchy_forge:options(#mech{attack_power = 8}, [BP]),
  3 = Mech#mech.attack_power.

%% Test that a blueprint that decreases the power below 0 isn't applicable.
attack_power_decrease_not_applicable(_Config) ->
  BP = #blueprint{mods = [{attack_power, minus, 2}]},
  [{not_applicable, [{attack_power, gte, 2}], BP}] =
    katarchy_forge:options(#mech{}, [BP]).

%% Test that a blueprint might decrease the attack power to zero.
attack_power_decrease_to_zero(_Config) ->
  BP = #blueprint{mods = [{attack_power, minus, 5}]},
  [{Mech, BP}] = katarchy_forge:options(#mech{attack_power = 5}, [BP]),
  0 = Mech#mech.attack_power.

%% Test that a blueprint might increase the attack power.
attack_power_increase(_Config) ->
  BP = #blueprint{mods = [{attack_power, plus, 5}]},
  [{Mech, BP}] = katarchy_forge:options(#mech{attack_power = 2}, [BP]),
  7 = Mech#mech.attack_power.

%% Test that a blueprint might set and lower the attack power.
attack_power_reset(_Config) ->
  MechInput = #mech{attack_power = 7},
  BP = #blueprint{mods = [{attack_power, set, 3}]},
  [{Mech, BP}] = katarchy_forge:options(MechInput, [BP]),
  3 = Mech#mech.attack_power.

%% Test that a blueprint can set attack power.
attack_power_set(_Config) ->
  BP = #blueprint{mods = [{attack_power, set, 5}]},
  [{Mech, BP}] = katarchy_forge:options(#mech{}, [BP]),
  5 = Mech#mech.attack_power.

%% Test that passing no blueprints doesn't return any options.
empty_blueprints(_Config) ->
  [] = katarchy_forge:options(#mech{}, []).

%% Test that a blueprint might increase the hitpoints.
hitpoints_decrease(_Config) ->
  BP = #blueprint{mods = [{hit_points, minus, 8}]},
  [{Mech, BP}] = katarchy_forge:options(#mech{}, [BP]),
  2 = Mech#mech.hit_points.

%% Test that a blueprint that decreases the hit poitns below 0 isn't applicable.
hitpoints_decrease_not_applicable(_Config) ->
  BP = #blueprint{mods = [{hit_points, minus, 14}]},
  [{not_applicable, [{hit_points, gte, 15}], BP}] =
    katarchy_forge:options(#mech{}, [BP]).

%% Test that a blueprint that decreases the hit poitns to 0 isn't applicable.
hitpoints_decrease_to_zero(_Config) ->
  BP = #blueprint{mods = [{hit_points, minus, 10}]},
  [{not_applicable, [{hit_points, gte, 11}], BP}] =
    katarchy_forge:options(#mech{}, [BP]).

%% Test that a blueprint might increase the hitpoints.
hitpoints_increase(_Config) ->
  BP = #blueprint{mods = [{hit_points, plus, 5}]},
  [{Mech, BP}] = katarchy_forge:options(#mech{}, [BP]),
  15 = Mech#mech.hit_points.

%% Test that a blueprint might set the hitpoints.
hitpoints_set(_Config) ->
  BP = #blueprint{mods = [{hit_points, set, 25}]},
  [{Mech, BP}] = katarchy_forge:options(#mech{}, [BP]),
  25 = Mech#mech.hit_points.

%% Test that multiple blueprints return multiple options.
multiple_blueprints(_Config) ->
  BP1 = #blueprint{mods = [{hit_points, plus, 1}]},
  BP2 =  #blueprint{mods = [{hit_points, plus, 2}]},
  [{Mech1, BP1}, {Mech2, BP2}] = katarchy_forge:options(#mech{}, [BP1, BP2]),
  11 = Mech1#mech.hit_points,
  12 = Mech2#mech.hit_points.

%% Test that a single blueprint might have multiple modifiers.
multiple_mods(_Config) ->
  BP = #blueprint{mods = [{hit_points, plus, 5}, {speed, set, 4}]},
  [{Mech, BP}] = katarchy_forge:options(#mech{}, [BP]),
  15 = Mech#mech.hit_points,
  4 = Mech#mech.speed.

%% Test that a single blueprint might have many not applicable decreases.
multiple_mods_decrease_not_applicable(_Config) ->
  BP = #blueprint{mods = [{attack_power, minus, 15}, {speed, minus, 4}]},
  [{not_applicable, [{speed, gte, 4}, {attack_power, gte, 15}], BP}] =
    katarchy_forge:options(#mech{}, [BP]).

%% Test that a single blueprint might have a not applicable decrease.
multiple_mods_decrease_one_not_applicable(_Config) ->
  BP = #blueprint{mods = [{hit_points, plus, 5}, {speed, minus, 4}]},
  [{not_applicable, [{speed, gte, 4}], BP}] =
    katarchy_forge:options(#mech{}, [BP]).

%% Test that the returned mech is the input if there aren't any mods.
same_mech(_Config) ->
  BP = #blueprint{},
  MechInput = #mech{id = <<"test_id">>},
  [{MechInput, BP}] = katarchy_forge:options(MechInput, [BP]).

%% Test that the returned mech doesn't reset the previous fields.
same_mech_modified(_Config) ->
  BP = #blueprint{mods = [{speed, set, 1}]},
  MechInput = #mech{id = <<"test_id">>, hit_points = 2},
  [{Mech, BP}] = katarchy_forge:options(MechInput, [BP]),
  Mech = MechInput#mech{speed = 1}.

%% Test that a blueprint might decrease the speed.
speed_decrease(_Config) ->
  BP = #blueprint{mods = [{speed, minus, 2}]},
  [{Mech, BP}] = katarchy_forge:options(#mech{speed = 9}, [BP]),
  7 = Mech#mech.speed.

%% Test that a blueprint that decreases the speed below 0 isn't applicable.
speed_decrease_not_applicable(_Config) ->
  BP = #blueprint{mods = [{speed, minus, 2}]},
  [{not_applicable, [{speed, gte, 2}], BP}] =
    katarchy_forge:options(#mech{speed = 1}, [BP]).

%% Test that a blueprint might decrease the speed to zero.
speed_decrease_to_zero(_Config) ->
  BP = #blueprint{mods = [{speed, minus, 2}]},
  [{Mech, BP}] = katarchy_forge:options(#mech{speed = 2}, [BP]),
  0 = Mech#mech.speed.

%% Test that a blueprint might increase the speed.
speed_increase(_Config) ->
  BP = #blueprint{mods = [{speed, plus, 2}]},
  [{Mech, BP}] = katarchy_forge:options(#mech{speed = 1}, [BP]),
  3 = Mech#mech.speed.

%% Test that a blueprint might set the speed.
speed_set(_Config) ->
  BP = #blueprint{mods = [{speed, set, 2}]},
  [{Mech, BP}] = katarchy_forge:options(#mech{speed = 3}, [BP]),
  2 = Mech#mech.speed.
