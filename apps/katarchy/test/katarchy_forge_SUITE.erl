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
   multiple_reqs,
   multiple_reqs_not_applicable,
   multiple_reqs_one_not_applicable,
   requirement,
   requirement_and_mod_not_applicable,
   requirement_eq_met,
   requirement_eq_not_met,
   requirement_gt_met,
   requirement_gt_not_met,
   requirement_gte_met,
   requirement_gte_met_eq,
   requirement_gte_not_met,
   requirement_lt_met,
   requirement_lt_not_met,
   requirement_lte_met,
   requirement_lte_met_eq,
   requirement_lte_not_met,
   requirement_skill_met,
   requirement_skill_not_met,
   requirement_skill_negative_met,
   requirement_skill_negative_not_met,
   same_mech,
   same_mech_modified,
   speed_decrease,
   speed_decrease_not_applicable,
   speed_decrease_to_zero,
   speed_increase,
   speed_set,
   skill_add,
   skill_remove].

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
  [{not_applicable, [{hit_points, gt, 14}], BP}] =
    katarchy_forge:options(#mech{}, [BP]).

%% Test that a blueprint that decreases the hit poitns to 0 isn't applicable.
hitpoints_decrease_to_zero(_Config) ->
  BP = #blueprint{mods = [{hit_points, minus, 10}]},
  [{not_applicable, [{hit_points, gt, 10}], BP}] =
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

%% Test that a single blueprint might have multiple requirements.
multiple_reqs(_Config) ->
  BP = #blueprint{reqs = [{hit_points, gte, 5}, {speed, eq, 0}]},
  Mech = #mech{},
  [{Mech, BP}] = katarchy_forge:options(Mech, [BP]).

%% Test that a single blueprint might have many not applicable requirements.
multiple_reqs_not_applicable(_Config) ->
  BP = #blueprint{reqs = [{attack_power, eq, 1}, {speed, gte, 9}]},
  [{not_applicable, [{attack_power, eq, 1}, {speed, gte, 9}], BP}] =
    katarchy_forge:options(#mech{}, [BP]).

%% Test that a single blueprint might have a not applicable requirement.
multiple_reqs_one_not_applicable(_Config) ->
  BP = #blueprint{reqs = [{speed, gte, 0}, {hit_points, eq, 2}]},
  [{not_applicable, [{hit_points, eq, 2}], BP}] =
    katarchy_forge:options(#mech{}, [BP]).

%% Test that a requirement can be met and the blueprint applied.
requirement(_Config) ->
  BP = #blueprint{mods = [{hit_points, set, 8}], reqs = [{hit_points, gte, 5}]},
  [{Mech, BP}] = katarchy_forge:options(#mech{}, [BP]),
  8 = Mech#mech.hit_points.

%% Test that a requirement can't be met and a mod is not applicable.
requirement_and_mod_not_applicable(_Config) ->
  BP = #blueprint{mods = [{speed, minus, 1}], reqs = [{attack_power, eq, 5}]},
  [{not_applicable, [{speed, gte, 1}, {attack_power, eq, 5}], BP}] =
    katarchy_forge:options(#mech{}, [BP]).

%% Test that an eq requirement can be met with an exact match.
requirement_eq_met(_Config) ->
  BP = #blueprint{reqs = [{hit_points, eq, 10}]},
  Mech = #mech{},
  [{Mech, BP}] = katarchy_forge:options(Mech, [BP]).

%% Test that an eq requirement can't be met.
requirement_eq_not_met(_Config) ->
  BP = #blueprint{reqs = [{attack_power, eq, 1}]},
  Mech = #mech{},
  [{not_applicable, [{attack_power, eq, 1}], BP}] =
    katarchy_forge:options(Mech, [BP]).

%% Test that a gt requirement can be met with a strictly greater parameter.
requirement_gt_met(_Config) ->
  BP = #blueprint{reqs = [{speed, gt, 0}]},
  Mech = #mech{speed = 1},
  [{Mech, BP}] = katarchy_forge:options(Mech, [BP]).

%% Test that a gt requirement can't be met.
requirement_gt_not_met(_Config) ->
  BP = #blueprint{reqs = [{attack_power, gt, 0}]},
  [{not_applicable, [{attack_power, gt, 0}], BP}] =
    katarchy_forge:options(#mech{}, [BP]).

%% Test that a gte requirement can be met with a strictly greater parameter.
requirement_gte_met(_Config) ->
  BP = #blueprint{reqs = [{hit_points, gte, 9}]},
  Mech = #mech{},
  [{Mech, BP}] = katarchy_forge:options(Mech, [BP]).

%% Test that a gte requirement can be met with an exact match.
requirement_gte_met_eq(_Config) ->
  BP = #blueprint{reqs = [{speed, gte, 2}]},
  Mech = #mech{speed = 2},
  [{Mech,  BP}] = katarchy_forge:options(Mech, [BP]).

%% Test that a gte requirement can't be met.
requirement_gte_not_met(_Config) ->
  BP = #blueprint{reqs = [{attack_power, gte, 2}]},
  [{not_applicable, [{attack_power, gte, 2}], BP}] =
    katarchy_forge:options(#mech{}, [BP]).

%% Test that a lt requirement can be met with a strictly lower parameter.
requirement_lt_met(_Config) ->
  BP = #blueprint{reqs = [{hit_points, lt, 11}]},
  Mech = #mech{},
  [{Mech, BP}] = katarchy_forge:options(Mech, [BP]).

%% Test that a lt requirement can't be met.
requirement_lt_not_met(_Config) ->
  BP = #blueprint{reqs = [{attack_power, lt, 1}]},
  [{not_applicable, [{attack_power, lt, 1}], BP}] =
    katarchy_forge:options(#mech{attack_power = 1}, [BP]).

%% Test that a lte requirement can be met with a strictly lower parameter.
requirement_lte_met(_Config) ->
  BP = #blueprint{reqs = [{attack_power, lte, 14}]},
  Mech = #mech{attack_power = 13},
  [{Mech, BP}] = katarchy_forge:options(Mech, [BP]).

%% Test that a lte requirement can be met with an exact match.
requirement_lte_met_eq(_Config) ->
  BP = #blueprint{reqs = [{speed, lte, 12}]},
  Mech = #mech{speed = 12},
  [{Mech,  BP}] = katarchy_forge:options(Mech, [BP]).

%% Test that a lte requirement can't be met.
requirement_lte_not_met(_Config) ->
  BP = #blueprint{reqs = [{hit_points, lte, 3}]},
  [{not_applicable, [{hit_points, lte, 3}], BP}] =
    katarchy_forge:options(#mech{}, [BP]).

%% Test that a blueprint might require a skill.
requirement_skill_met(_Config) ->
  BP = #blueprint{reqs = [{skills, has, jump}]},
  Mech = #mech{skills = [jump]},
  [{Mech, BP}] = katarchy_forge:options(Mech, [BP]).

%% Test that a blueprint might require a skill.
requirement_skill_not_met(_Config) ->
  BP = #blueprint{reqs = [{skills, has, slow}]},
  [{not_applicable, [{skills, has, slow}], BP}] =
    katarchy_forge:options(#mech{}, [BP]).

%% Test that a blueprint might require a skill.
requirement_skill_negative_met(_Config) ->
  BP = #blueprint{reqs = [{skills, has_not, triattack}]},
  Mech = #mech{},
  [{Mech, BP}] = katarchy_forge:options(Mech, [BP]).

%% Test that a blueprint might require a skill.
requirement_skill_negative_not_met(_Config) ->
  BP = #blueprint{reqs = [{skills, has_not, exploding}]},
  Mech = #mech{skills = [{exploding, 3}]},
  [{not_applicable, [{skills, has_not, exploding}], BP}] =
    katarchy_forge:options(Mech, [BP]).

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

%% Test that a blueprint might add a skill.
skill_add(_Config) ->
  BP = #blueprint{mods = [{skills, add, jump}]},
  [{Mech, BP}] = katarchy_forge:options(#mech{}, [BP]),
  true = lists:member(jump, Mech#mech.skills).

%% Test that a blueprint might remove a skill.
skill_remove(_Config) ->
  BP = #blueprint{mods = [{skills, del, hidden}]},
  [{Mech, BP}] = katarchy_forge:options(#mech{}, [BP]),
  false = lists:member(hidden, Mech#mech.skills).

