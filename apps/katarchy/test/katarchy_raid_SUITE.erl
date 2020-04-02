-module(katarchy_raid_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include("katarchy_mech.hrl").

%%--------------------------------------------------------------------
%% CT callbacks
%%--------------------------------------------------------------------
suite() ->
  [{timetrap, {seconds, 10}}].

all() ->
  [attack_double_ko,
   attack_double_ko_slow,
   attack_facing,
   attack_not_facing,
   attack_not_same_side,
   attack_outside,
   attack_perforating_melee,
   attack_perforating_melee_friend,
   attack_perforating_melee_limit,
   attack_perforating_ranged,
   attack_perforating_ranged_friend,
   attack_power,
   attack_power_overkill,
   attack_power_ranged,
   attack_ranged,
   attack_ranged_blocked,
   attack_ranged_double_ko,
   attack_ranged_no_target,
   attack_ranged_not_same_side,
   attack_ranged_once,
   attack_ranged_right,
   attack_slow,
   ballistic,
   ballistic_farther_first,
   ballistic_hidden,
   ballistic_hidden_other,
   ballistic_hidden_other_perforating,
   ballistic_perforating,
   critical_hit,
   critical_hit_dodged,
   critical_hit_explosive,
   critical_hit_explosive_many,
   critical_hit_explosive_prepared,
   critical_hit_explosive_used,
   critical_hit_half,
   critical_hit_half_twice,
   critical_hit_perforating,
   critical_hit_ranged,
   critical_hit_ranged_perforating,
   critical_hit_ranged_three_times,
   critical_hit_triattack,
   dodge,
   dodge_explosion,
   dodge_half,
   eshield,
   eshield_dodge,
   eshield_partial,
   eshield_regenerating,
   explosive,
   explosive_area,
   explosive_chain,
   explosive_hidden,
   explosive_ranged,
   hidden,
   hidden_ally_back,
   hidden_attacking_ranged,
   hidden_jump,
   hidden_perforating,
   hidden_ranged,
   hidden_ranged_ally_behind,
   hidden_ranged_perforating,
   hidden_ranged_transparent,
   hidden_ranged_transparent_adjacent,
   hidden_ranged_twice,
   hidden_ranged_twice_transparent,
   hidden_revealed_attacking,
   hidden_revealed_bumped,
   hidden_revealed_bumping,
   hidden_revealed_slower,
   mech_id_remains,
   movement_double,
   movement_faster_first,
   movement_jump,
   movement_jump_blocked,
   movement_jump_limit,
   movement_jump_pairs,
   movement_jump_speed,
   movement_limit_left,
   movement_limit_right,
   movement_not_blocking,
   movement_obstacle,
   movement_slow,
   movement_slow_last,
   movement_slower_last,
   movement_stop,
   movement_turns,
   movement_turns_track,
   not_same_position,
   single_mech,
   sitting_ducks,
   triattack,
   triattack_all_hidden,
   triattack_friendly_fire,
   triattack_friendly_fire_ranged,
   triattack_ranged,
   triattack_ranged_allies_cover].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------
%% Test that two mechs can ko each other.
attack_double_ko(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 1},
  MechR = #mech{position = {1,0}, attack_power = 1, side = right},
  {[MechL2, MechR2], _} = katarchy_raid:run([MechL, MechR]),
  destroyed = MechL2#mech.position,
  destroyed = MechR2#mech.position.

%% Test that two mechs can ko each other even if one is slower
attack_double_ko_slow(_Config) ->
  MechR = #mech{position = {1,0}, attack_power = 1, side = right},
  MechL = #mech{position = {0,0}, attack_power = 2, skills = [{slow, 2, 1}]},
  {[MechR2, MechL2], _} = katarchy_raid:run([MechR, MechL]),
  destroyed = MechL2#mech.position,
  destroyed = MechR2#mech.position.

%% Test that one mech can attack a rival that it's facing.
attack_facing(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 1},
  MechR = #mech{position = {1,0}, side = right},
  {[MechL, MechR2], _} = katarchy_raid:run([MechL, MechR]),
  destroyed = MechR2#mech.position.

%% Test that one mech can't attack a rival that isn't facing.
attack_not_facing(_Config) ->
  MechL = #mech{position = {1,0}, attack_power = 1},
  MechR = #mech{position = {0,0}, side = right},
  {[MechL, MechR], _} = katarchy_raid:run([MechL, MechR]).

%% Test that one mech can't attack an ally.
attack_not_same_side(_Config) ->
  Mech1 = #mech{position = {0,0}, attack_power = 1},
  Mech2 = #mech{position = {1,0}},
  {[Mech1, Mech2], _} = katarchy_raid:run([Mech1, Mech2]),
  {1,0} = Mech2#mech.position.

%% Test that one mech doesn't go out of bounds when attacking.
attack_outside(_Config) ->
  Mech = #mech{position = {0,0}, side = right, attack_power = 1},
  {[Mech], _} = katarchy_raid:run([Mech]).

%% Tests that a perforating attack can damage multiple adjacent enemies.
attack_perforating_melee(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 10, skills = [perforating]},
  MechR1 = #mech{position = {1,0}, side = right},
  MechR2 = #mech{position = {2,0}, side = right},
  {[MechL, MechR11, MechR22], Turns} =
    katarchy_raid:run([MechL, MechR1, MechR2]),
  destroyed = MechR11#mech.position,
  destroyed = MechR22#mech.position,
  1 = length(Turns).

%% Tests that a perforating attack can also damage allies.
attack_perforating_melee_friend(_Config) ->
  MechL1 = #mech{position = {0,0}, attack_power = 10, skills = [perforating]},
  MechR = #mech{position = {1,0}, side = right},
  MechL2 = #mech{position = {2,0}},
  {[MechL1, MechR2, MechL22], Turns} =
    katarchy_raid:run([MechL1, MechR, MechL2]),
  destroyed = MechR2#mech.position,
  destroyed = MechL22#mech.position,
  1 = length(Turns).

%% Tests that a perforating attack doesn't jump over empty spaces.
attack_perforating_melee_limit(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 10, skills = [perforating]},
  MechR1 = #mech{position = {1,0}, side = right},
  MechR2 = #mech{position = {2,0}, side = right},
  MechR4 = #mech{position = {4,0}, side = right},
  {[MechL, _, _, MechR4], _} =
    katarchy_raid:run([MechL, MechR1, MechR2, MechR4]).

%% Tests that a ranged attack can damage multiple enemies on a lane.
attack_perforating_ranged(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 10,
                skills = [ranged, perforating]},
  MechR1 = #mech{position = {2,0}, side = right},
  MechR2 = #mech{position = {4,0}, side = right},
  {[MechL, MechR11, MechR22], Turns} =
    katarchy_raid:run([MechL, MechR1, MechR2]),
  destroyed = MechR11#mech.position,
  destroyed = MechR22#mech.position,
  1 = length(Turns).

%% Tests that a ranged attack also damages an ally behind an enemy.
attack_perforating_ranged_friend(_Config) ->
  MechL1 = #mech{position = {0,0}, attack_power = 10,
                skills = [ranged, perforating]},
  MechR = #mech{position = {2,0}, side = right},
  MechL2 = #mech{position = {4,0}},
  {[MechL1, MechR2, MechL22], Turns} =
    katarchy_raid:run([MechL1, MechR, MechL2]),
  destroyed = MechR2#mech.position,
  destroyed = MechL22#mech.position,
  1 = length(Turns).

%% Test that the attack power allows to damage a rival faster.
attack_power(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 5},
  MechR = #mech{position = {1,0}, side = right},
  {[MechL, MechR2], Turns} = katarchy_raid:run([MechL, MechR]),
  destroyed = MechR2#mech.position,
  2 = length(Turns).

%% Test that the attack power allows to overdamage a rival.
attack_power_overkill(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 500},
  MechR = #mech{position = {1,0}, side = right},
  {[MechL, MechR2], Turns} = katarchy_raid:run([MechL, MechR]),
  destroyed = MechR2#mech.position,
  1 = length(Turns).

%% Test that the attack power allows to damage a rival faster when ranged.
attack_power_ranged(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 5, skills = [ranged]},
  MechR = #mech{position = {4,0}, side = right},
  {[MechL, MechR2], Turns} = katarchy_raid:run([MechL, MechR]),
  destroyed = MechR2#mech.position,
  2 = length(Turns).

%% Test that one mech can attack at range.
attack_ranged(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 1, skills = [ranged]},
  MechR = #mech{position = {4,0}, side = right},
  {[MechL, MechR2], _} = katarchy_raid:run([MechL, MechR]),
  destroyed = MechR2#mech.position.

%% Test that one mech can block the line of attack.
attack_ranged_blocked(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 1, skills = [ranged]},
  MechR = #mech{position = {4,0}, side = right},
  Obstacle = #mech{position = {2,0}},
  Mechs = [MechL, MechR, Obstacle],
  {Mechs, _} = katarchy_raid:run(Mechs).

%% Test that two ranged mechs can kill each other.
attack_ranged_double_ko(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 1, skills = [ranged]},
  MechR = #mech{position = {4,0}, attack_power = 1, skills = [ranged],
                side = right},
  {[MechL2, MechR2], _} = katarchy_raid:run([MechL, MechR]),
  destroyed = MechL2#mech.position,
  destroyed = MechR2#mech.position.

%% Test that one ranged mechs doesn't shoot if not needed.
attack_ranged_no_target(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 1, skills = [ranged]},
  MechR = #mech{position = {4,1}, side = right},
  {[MechL, MechR], _} = katarchy_raid:run([MechL, MechR]).

%% Test that one mech can't attack a friend.
attack_ranged_not_same_side(_Config) ->
  Mech1 = #mech{position = {0,0}, attack_power = 1, skills = [ranged]},
  Mech2 = #mech{position = {4,0}},
  {[Mech1, Mech2], _} = katarchy_raid:run([Mech1, Mech2]).

%% Test that ranged mechs don't attack twice.
attack_ranged_once(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 5, skills = [ranged]},
  MechR = #mech{position = {1,0}, side = right},
  {_, Turns} = katarchy_raid:run([MechL, MechR]),
  2 = length(Turns).

%% Test that one right mech can attack to the left.
attack_ranged_right(_Config) ->
  MechL = #mech{position = {0,0}},
  MechR = #mech{position = {4,0}, attack_power = 1, skills = [ranged],
                side = right},
  {[MechL2, MechR], _} = katarchy_raid:run([MechL, MechR]),
  destroyed = MechL2#mech.position.

%% Test that an attack is delayed for "slow" mechs.
attack_slow(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 1, skills = [{slow, 3, 1}]},
  MechR = #mech{position = {1,0}, hit_points = 1, side = right},
  {[_, MechR2], Turns} = katarchy_raid:run([MechL, MechR]),
  5 = length(Turns), % 2 more turns for hit, 3 turns for recharging
  destroyed = MechR2#mech.position.

%% Test that a ballistic mech can attack above obstacles.
ballistic(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 10,
                skills = [ranged, ballistic]},
  Obstacle = #mech{position = {2,0}},
  MechR = #mech{position = {5,0}, side = right},
  {[_, _, MechR2], _} = katarchy_raid:run([MechL, Obstacle, MechR]),
  destroyed = MechR2#mech.position.

%% Test that a ballistic mech attacks the farther enemy first.
ballistic_farther_first(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 10,
                skills = [ranged, ballistic]},
  MechR1 = #mech{position = {2,0}, side = right},
  MechR2 = #mech{position = {5,0}, side = right},
  {[_, _, _], [[MechL, MechR1, MechR21], [MechL, MechR12, MechR21]]}
    = katarchy_raid:run([MechL, MechR1, MechR2]),
  destroyed = MechR12#mech.position,
  destroyed = MechR21#mech.position.

%% Test that ballistic skips hidden mechs.
ballistic_hidden(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 10,
                skills = [ranged, ballistic]},
  MechR = #mech{position = {5,0}, side = right, skills = [hidden]},
  {[MechL, MechR], _} = katarchy_raid:run([MechL, MechR]).

%% Test that ballistic skips hidden mechs and attacks the next enemy.
ballistic_hidden_other(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 10,
                skills = [ranged, ballistic]},
  MechR1 = #mech{position = {2,0}, side = right},
  MechR2 = #mech{position = {5,0}, side = right, skills = [hidden]},
  {[MechL, MechR1F, MechR2], _} = katarchy_raid:run([MechL, MechR1, MechR2]),
  destroyed = MechR1F#mech.position.

%% Test that ballistic/perfoarting skips hidden mechs.
ballistic_hidden_other_perforating(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 10,
                skills = [ranged, ballistic]},
  MechR1 = #mech{position = {2,0}, side = right},
  MechR2 = #mech{position = {5,0}, side = right, skills = [hidden]},
  {[MechL, MechR1F, MechR2], _} = katarchy_raid:run([MechL, MechR1, MechR2]),
  destroyed = MechR1F#mech.position.

%% Test that ballistic/perfoarting damages all mechs in the line.
ballistic_perforating(_Config) ->
  MechL1 = #mech{position = {0,0}, attack_power = 10,
                skills = [ranged, ballistic, perforating]},
  MechL2 = #mech{position = {2,0}},
  MechR = #mech{position = {5,0}, side = right},
  {[MechL1, MechL2F, MechRF], _} = katarchy_raid:run([MechL1, MechL2, MechR]),
  destroyed = MechL2F#mech.position,
  destroyed = MechRF#mech.position.

%% Test that a mech can do a critical hit that deals double damage.
critical_hit(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 5,
                skills = [{critical, 1, 1}]},
  MechR = #mech{position = {1,0}, attack_power = 10, side = right},
  {[MechL2, MechR2], _} = katarchy_raid:run([MechL, MechR]),
  destroyed = MechL2#mech.position,
  destroyed = MechR2#mech.position.

%% Test that a dodged critical hit gets used.
critical_hit_dodged(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 2,
                skills = [{critical, 2, 2}]},
  MechR = #mech{position = {1,0}, side = right, skills = [{dodge, 2, 2}]},
  {_, Turns} = katarchy_raid:run([MechL, MechR]),
  10 = length(Turns).

%% Test that critical hit also affects explosions.
critical_hit_explosive(_Config) ->
  MechL = #mech{position = {0,0}, skills = [{critical, 1, 1}, {explosive, 5}]},
  MechR = #mech{position = {1,0}, attack_power = 10, side = right},
  {[MechL2, MechR2], _} = katarchy_raid:run([MechL, MechR]),
  destroyed = MechL2#mech.position,
  destroyed = MechR2#mech.position.

%% Test that critical hit also affects area explosions.
critical_hit_explosive_many(_Config) ->
  Mechs = [#mech{position = {0,0}, skills = [{critical, 1, 1}, {explosive, 7}]},
           #mech{position = {1,0}, attack_power = 10, side = right},
           #mech{position = {0,1}}],
  {Mechs2, _} = katarchy_raid:run(Mechs),
  true = lists:all(fun(X) -> X#mech.position =:= destroyed end, Mechs2).

%% Test that critical hit gets prepared by attacking before exploding.
critical_hit_explosive_prepared(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 1, speed = 2,
                skills = [{critical, 2, 1}, {explosive, 5}]},
  MechR = #mech{position = {1,0}, attack_power = 10, side = right},
  {[_, MechR2], _} = katarchy_raid:run([MechL, MechR]),
  destroyed = MechR2#mech.position.

%% Test that critical hit gets used if attacking before exploding.
critical_hit_explosive_used(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 1, speed = 2,
                skills = [{critical, 2, 2}, {explosive, 5}]},
  MechR = #mech{position = {1,0}, attack_power = 10, side = right},
  {[_, MechR2], _} = katarchy_raid:run([MechL, MechR]),
  3 = MechR2#mech.hit_points.

%% Test that a mech can do a critical hit that sometimes deals double damage.
critical_hit_half(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 5,
                skills = [{critical, 2, 1}]},
  MechR = #mech{position = {1,0}, hit_points = 15, side = right},
  {_, Turns} = katarchy_raid:run([MechL, MechR]),
  2 = length(Turns).

%% Test that a mech can do a critical hit twice with some normal hits between.
critical_hit_half_twice(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 4,
                skills = [{critical, 2, 2}]},
  MechR = #mech{position = {1,0}, hit_points = 20, side = right},
  {_, Turns} = katarchy_raid:run([MechL, MechR]),
  3 = length(Turns).

%% Test that a critical hit can perforate.
critical_hit_perforating(_Config) ->
  Mechs = [#mech{position = {0,0}, attack_power = 5,
                 skills = [{critical, 5, 5}, perforating]},
           #mech{position = {1,0}, side = right},
           #mech{position = {2,0}, side = right}],
  {[_, MechR1, MechR2], Turns} = katarchy_raid:run(Mechs),
  destroyed = MechR1#mech.position,
  destroyed = MechR2#mech.position,
  1 = length(Turns).

%% Test that a mech can do a critical hit while ranged.
critical_hit_ranged(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 6,
                skills = [{critical, 1, 1}, ranged]},
  MechR = #mech{position = {2,0}, side = right},
  {_, Turns} = katarchy_raid:run([MechL, MechR]),
  1 = length(Turns).

%% Test that a ranged critical hit can perforate.
critical_hit_ranged_perforating(_Config) ->
  Mechs = [#mech{position = {0,0}, attack_power = 5,
                 skills = [{critical, 4, 4}, perforating, ranged]},
           #mech{position = {2,0}, side = right},
           #mech{position = {5,0}, side = right}],
  {[_, MechR1, MechR2], Turns} = katarchy_raid:run(Mechs),
  destroyed = MechR1#mech.position,
  destroyed = MechR2#mech.position,
  1 = length(Turns).

%% Test that a mech can do a critical hit while ranged every three hits.
critical_hit_ranged_three_times(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 5,
                skills = [{critical, 3, 3}, ranged]},
  MechR = #mech{position = {5,0}, hit_points = 20, side = right},
  {_, Turns} = katarchy_raid:run([MechL, MechR]),
  3 = length(Turns).

%% Test that a mech can do a critical hit while triattacking.
critical_hit_triattack(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 5,
                skills = [{critical, 3, 3}, triattack]},
  MechR1 = #mech{position = {1,0}, side = right},
  MechR2 = #mech{position = {1,1}, side = right},
  {[_, MechR12, MechR22], Turns} = katarchy_raid:run([MechL, MechR1, MechR2]),
  destroyed = MechR12#mech.position,
  destroyed = MechR22#mech.position,
  1 = length(Turns).

%% Test that a dodging mech can avoid getting hit.
dodge(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 1, skills = [{dodge, 1, 1}]},
  MechR = #mech{position = {1,0}, attack_power = 10, side = right},
  {[MechL, MechR2], _} = katarchy_raid:run([MechL, MechR]),
  destroyed = MechR2#mech.position.

%% Test that a dodging mech can avoid getting hit by an explosion.
dodge_explosion(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 10, skills = [{dodge, 1, 1}]},
  MechR = #mech{position = {1,0}, side = right, skills = [{explosive, 10}]},
  {[MechL, MechR2], _} = katarchy_raid:run([MechL, MechR]),
  destroyed = MechR2#mech.position.

%% Test that a dodging mech can avoid getting hit half of the time.
dodge_half(_Config) ->
  MechL = #mech{position = {0,0}, skills = [{dodge, 2, 2}]},
  MechR = #mech{position = {1,0}, attack_power = 10, side = right},
  {[MechL2, MechR], Turns} = katarchy_raid:run([MechL, MechR]),
  destroyed = MechL2#mech.position,
  2 = length(Turns).

%% Test that a mech might have an energy shield that prevents damage.
eshield(_Config) ->
  MechL = #mech{position = {0,0}, skills = [{eshield, 1, 0}]},
  MechR = #mech{position = {1,0}, attack_power = 1, side = right},
  {[MechL, MechR], _} = katarchy_raid:run([MechL, MechR]).

%% Test that dodging doesn't use the eshield.
eshield_dodge(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 10,
                skills = [{dodge, 8, 8}, {eshield, 9, 9}]},
  MechR = #mech{position = {1,0}, attack_power = 40, side = right},
  {[MechL2, _], _} = katarchy_raid:run([MechL, MechR]),
  true = lists:member({eshield, 9, 9}, MechL2#mech.skills).

%% Test that a mech might have an eshield that prevents part of the damage.
eshield_partial(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 10,
                  skills = [{eshield, 1, 0}]},
  MechR = #mech{position = {1,0}, attack_power = 10, side = right},
  {[MechL2, _], _} = katarchy_raid:run([MechL, MechR]),
  1 = MechL2#mech.hit_points.

%% Test that a mech might have an eshield that regenerates a point per turn.
eshield_regenerating(_Config) ->
  MechL = #mech{position = {0,0}, skills = [{eshield, 4, 0}]},
  {[MechL2], Turns} = katarchy_raid:run([MechL]),
  true = lists:member({eshield, 4, 4}, MechL2#mech.skills),
  4 = length(Turns).

%% Test that killing an explosive also damages yourself.
explosive(_Config) ->
  Mech = #mech{position = {1,0}, attack_power = 10, speed = 2, side = right},
  Explosive = #mech{position = {0,0}, skills = [{explosive, 5}]},
  {[Mech2, _], _} = katarchy_raid:run([Mech, Explosive]),
  5 = Mech2#mech.hit_points.

%% Test that killing an explosive also does collateral damage.
explosive_area(_Config) ->
  Mech = #mech{position = {2,1}, attack_power = 10, hit_points = 5,
               speed = 2, side = right},
  Explosive = #mech{position = {1,1}, skills = [{explosive, 5}]},
  Collats = [#mech{position = {1,0}}, #mech{position = {0,1}},
             #mech{position = {1,2}}],
  {[_,_|Collats2], _} = katarchy_raid:run([Mech, Explosive | Collats]),
  true = lists:all(fun(X) -> X#mech.hit_points == 5 end, Collats2).

%% Test an explosive chain killing a everything on the map.
explosive_chain(_Config) ->
  Mechs = [#mech{position = {0,0}},
           #mech{position = {0,1}, skills = [{explosive, 10}]},
           #mech{position = {1,1}, skills = [{explosive, 10}]},
           #mech{position = {2,1}, skills = [{explosive, 10}]},
           #mech{position = {3,1}, side = right, attack_power = 10}],
  {NewMechs, _} = katarchy_raid:run(Mechs),
  true = lists:all(fun(X) -> destroyed == X#mech.position end, NewMechs).

%% Test that killing an explosive also does collateral damage to hidden units.
explosive_hidden(_Config) ->
  Mech = #mech{position = {2,0}, attack_power = 10, side = right},
  Explosive = #mech{position = {1,0}, skills = [{explosive, 10}]},
  Collat = #mech{position = {0,0}, skills = [hidden]},
  {[_,_,Collat2], _} = katarchy_raid:run([Mech, Explosive, Collat]),
  destroyed = Collat2#mech.position.

%% Tests killing an explosive from far away.
explosive_ranged(_Config) ->
  Explosive = #mech{position = {0,9}, skills = [{explosive, 2}]},
  Collat = #mech{position = {0,8}},
  MechR = #mech{position = {4,9}, attack_power = 1,
                side = right, skills = [ranged]},
  {[_, Collat2, MechR], _} = katarchy_raid:run([Explosive, Collat, MechR]),
  8 = Collat2#mech.hit_points.

%% Tests that a hidden unit can't be attacked.
hidden(_Config) ->
  MechL = #mech{position = {0,0}, skills = [hidden]},
  MechR = #mech{position = {1,0}, attack_power = 1, side = right},
  {[MechL, MechR], _} = katarchy_raid:run([MechL, MechR]).

%% Tests not attacking a hidden unit with an ally behind it.
hidden_ally_back(_Config) ->
  MechL = #mech{position = {0,0}},
  MechH = #mech{position = {1,0}, skills = [hidden]},
  MechR = #mech{position = {2,0}, attack_power = 1, side = right},
  {[MechL, MechH, MechR], _} = katarchy_raid:run([MechL, MechH, MechR]).

%% Tests that a hidden unit attacking at range doesn't get revealed.
hidden_attacking_ranged(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 2, skills = [ranged, hidden]},
  MechR = #mech{position = {3,0}, side = right},
  {[MechL, MechR2], _} = katarchy_raid:run([MechL, MechR]),
  destroyed = MechR2#mech.position.

%% Tests that a hidden jumping unit can sneak past a sentry.
hidden_jump(_Config) ->
  MechL = #mech{position = {0,0}, speed = 1, skills = [hidden, jump]},
  MechR = #mech{position = {4,0}, attack_power = 10, side = right},
  {[MechL2, MechR], _} = katarchy_raid:run([MechL, MechR]),
  raided = MechL2#mech.position,
  true = lists:member(hidden, MechL2#mech.skills).

%% Tests that a hidden unit also gets hit by perforating a unit in front of it.
hidden_perforating(_Config) ->
  Mechs = [#mech{position = {0,0}, skills = [hidden]},
           #mech{position = {1,0}},
           #mech{position = {2,0}, attack_power = 10,
                 side = right, skills = [perforating]}],
  {[MechH, _, _], _} = katarchy_raid:run(Mechs),
  destroyed = MechH#mech.position.

%% Tests that a hidden unit can't be ranged attacked.
hidden_ranged(_Config) ->
  MechL = #mech{position = {0,0}, skills = [hidden]},
  MechR = #mech{position = {4,0}, attack_power = 1,
                side = right, skills = [ranged]},
  {[MechL, MechR], _} = katarchy_raid:run([MechL, MechR]).

%% Tests that an ally behind a hidden mech doesn't get attacked.
hidden_ranged_ally_behind(_Config) ->
  Mechs = [#mech{position = {0,0}},
           #mech{position = {2,0}, skills = [hidden]},
           #mech{position = {1,0}, side = right},
           #mech{position = {4,0}, side = right,
                 attack_power = 1, skills = [ranged]}],
  {Mechs, _} = katarchy_raid:run(Mechs).

%% Tests that a hidden unit can't be ranged-perforating attacked.
hidden_ranged_perforating(_Config) ->
  Mechs = [#mech{position = {0,0}, skills = [hidden]},
           #mech{position = {4,0}, side = right,
                 attack_power = 10, skills = [ranged, perforating]}],
  {Mechs, _} = katarchy_raid:run(Mechs).

%% Tests that attacking a unit behind the hidden one actually hits the hidden.
hidden_ranged_transparent(_Config) ->
  MechL = #mech{position = {0,0}},
  MechH = #mech{position = {1,0}, skills = [hidden]},
  MechR = #mech{position = {4,0}, attack_power = 10,
                side = right, skills = [ranged]},
  {_, [[MechL, MechHT1, MechR]|_]} = katarchy_raid:run([MechL, MechH, MechR]),
  destroyed = MechHT1#mech.position.

%% Tests that a hidden unit just in front of a ranged one can get damaged.
hidden_ranged_transparent_adjacent(_Config) ->
  MechL = #mech{position = {0,0}},
  MechH = #mech{position = {3,0}, skills = [hidden]},
  MechR = #mech{position = {4,0}, attack_power = 10,
                side = right, skills = [ranged]},
  {_, [[MechL, MechHT1, MechR]|_]} = katarchy_raid:run([MechL, MechH, MechR]),
  destroyed = MechHT1#mech.position.

%% Tests that a lane with two mechs hidden don't have any changes.
hidden_ranged_twice(_Config) ->
  Mechs = [#mech{position = {1,0}, skills = [hidden]},
           #mech{position = {2,0}, skills = [hidden]},
           #mech{position = {4,0}, attack_power = 10,
                 side = right, skills = [ranged]}],
  {Mechs, _} = katarchy_raid:run(Mechs).

%% Tests that attacking a unit behind two hidden ones actually hits the hidden.
hidden_ranged_twice_transparent(_Config) ->
  MechL = #mech{position = {0,0}, skills = []},
  MechH1 = #mech{position = {1,0}, skills = [hidden]},
  MechH2 = #mech{position = {2,0}, skills = [hidden]},
  MechR = #mech{position = {4,0}, attack_power = 10,
                side = right, skills = [ranged]},
  {_, [[MechL, MechH1, MechH2T1, MechR]|_]} =
    katarchy_raid:run([MechL, MechH1, MechH2, MechR]),
  destroyed = MechH2T1#mech.position.

%% Tests that a hidden unit gets revealed after attacking.
hidden_revealed_attacking(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 10, skills = [hidden]},
  MechR = #mech{position = {1,0}, side = right},
  {[MechL2, _], _} = katarchy_raid:run([MechL, MechR]),
  false = lists:member(hidden, MechL2#mech.skills).

%% Tests that a hidden unit gets revealed after being bumped.
hidden_revealed_bumped(_Config) ->
  MechL = #mech{position = {0,0}, skills = [hidden]},
  MechR = #mech{position = {1,0}, speed = 1, side = right},
  {[MechL2, MechR], _} = katarchy_raid:run([MechL, MechR]),
  false = lists:member(hidden, MechL2#mech.skills).

%% Tests that a hidden unit gets revealed after bumping an enemy.
hidden_revealed_bumping(_Config) ->
  MechL = #mech{position = {0,0}, speed = 1, skills = [hidden]},
  MechR = #mech{position = {4,0}, side = right},
  {[MechL2, MechR], _} = katarchy_raid:run([MechL, MechR]),
  {3,0} = MechL2#mech.position,
  false = lists:member(hidden, MechL2#mech.skills).

%% Tests that a hidden unit gets revealed after being bumped when slower.
hidden_revealed_slower(_Config) ->
  MechL = #mech{position = {0,0}, speed = 1, skills = [hidden]},
  MechR = #mech{position = {2,0}, speed = 2, side = right},
  {[MechL2, _], _} = katarchy_raid:run([MechL, MechR]),
  false = lists:member(hidden, MechL2#mech.skills).

%% Test that mechs have one id that remains with them.
mech_id_remains(_Config) ->
  Mech = #mech{position = {0,0}, id = <<"alpha">>, speed = 1},
  {[Mech2], _} = katarchy_raid:run([Mech]),
  <<"alpha">> = Mech2#mech.id.

%% Test that one mech can move double.
movement_double(_Config) ->
  MechL = #mech{position = {0,0}, speed = 2},
  MechR = #mech{position = {7,0}, speed = 1, side = right},
  {[MechL2, MechR2], _} = katarchy_raid:run([MechL, MechR]),
  {4,0} = MechL2#mech.position,
  {5,0} = MechR2#mech.position.

%% Test that the faster mech reaches the target position first.
movement_faster_first(_Config) ->
  MechL = #mech{position = {0,0}, speed = 1},
  MechR = #mech{position = {2,0}, speed = 2, side = right},
  {[MechL, MechR2], _} = katarchy_raid:run([MechL, MechR]),
  {1,0} = MechR2#mech.position.

%% Test that a jumping mech can jump over another.
movement_jump(_Config) ->
  Mech = #mech{position = {0,0}, speed = 1, skills = [jump]},
  Obstacle = #mech{position = {1,0}},
  {_, [[MechT1,Obstacle]|_]} = katarchy_raid:run([Mech, Obstacle]),
  {2,0} = MechT1#mech.position.

%% Test that a jumping mech can't jump over two mechs.
movement_jump_blocked(_Config) ->
  Mech = #mech{position = {0,0}, speed = 1, skills = [jump]},
  Obstacle1 = #mech{position = {1,0}},
  Obstacle2 = #mech{position = {2,0}},
  Mechs = [Mech, Obstacle1, Obstacle2],
  {Mechs, _} = katarchy_raid:run(Mechs).

%% Test that a jumping mech can't jump over the last before the limit.
movement_jump_limit(_Config) ->
  Obstacle = #mech{position = {0,0}},
  Mech = #mech{position = {1,0}, speed = 1, skills = [jump], side = right},
  {[Mech, Obstacle], _} = katarchy_raid:run([Mech, Obstacle]).

%% Test two jumping mechs together reaching the end.
movement_jump_pairs(_Config) ->
  Mech1 = #mech{position = {0,0}, speed = 1, skills = [jump]},
  Mech2 = #mech{position = {1,0}, speed = 1, skills = [jump]},
  {[MechX, MechY], _} = katarchy_raid:run([Mech1, Mech2]),
  raided = MechX#mech.position,
  raided = MechY#mech.position.

%% Test that jumping only consumes one movement.
movement_jump_speed(_Config) ->
  Mech = #mech{position = {0,0}, speed = 2, skills = [jump]},
  Obstacle1 = #mech{position = {1,0}},
  Obstacle2 = #mech{position = {3,0}},
  {_, [[MechT1|_]|_]} = katarchy_raid:run([Mech, Obstacle1, Obstacle2]),
  {4,0} = MechT1#mech.position.

%% Test that one left minion eventually escapes the raid.
movement_limit_left(_Config) ->
  MechL = #mech{position = {0,0}, speed = 1},
  MechR = #mech{position = {0,1}, side = right},
  {[MechL2, MechR], _} = katarchy_raid:run([MechL, MechR]),
  raided = MechL2#mech.position.

%% Test that one right minion eventually escapes the raid.
movement_limit_right(_Config) ->
  MechL = #mech{position = {0,0}},
  MechR = #mech{position = {0,1}, speed = 1, side = right},
  {[MechL, MechR2], _} = katarchy_raid:run([MechL, MechR]),
  raided = MechR2#mech.position.

%% Test that a slower mech doesn't block a faster one.
movement_not_blocking(_Config) ->
  MechF = #mech{position = {0,0}, speed = 2},
  MechS = #mech{position = {1,0}, speed = 1},
  Obstacle = #mech{position = {3,0}},
  {[MechF2, MechS2, Obstacle], Turns} =
    katarchy_raid:run([MechF, MechS, Obstacle]),
  {1,0} = MechF2#mech.position,
  {2,0} = MechS2#mech.position,
  1 = length(Turns).

%% Test that one mech can move until one obstacle.
movement_obstacle(_Config) ->
  Mech = #mech{position = {0,0}, speed = 1},
  Obstacle = #mech{position = {5,0}},
  {[Mech2, Obstacle], _} = katarchy_raid:run([Mech, Obstacle]),
  {4,0} = Mech2#mech.position.

%% Test that one mech can move slowly.
movement_slow(_Config) ->
  Mech = #mech{position = {1,0}, speed = 1, side = right,
               skills = [{slow, 3, 1}]},
  {[MechF], Turns} = katarchy_raid:run([Mech]),
  8 = length(Turns), % 2 more turns in {1,0}, 3 in {0,0}, 3 in undefined.
  raided = MechF#mech.position.

%% Test that the slow mech moves last.
movement_slow_last(_Config) ->
  MechL = #mech{position = {0,0}, speed = 1, skills = [{slow, 4, 1}]},
  MechR = #mech{position = {2,0}, speed = 1, side = right},
  {[MechL2, MechR2], _} = katarchy_raid:run([MechL, MechR]),
  {0,0} = MechL2#mech.position,
  {1,0} = MechR2#mech.position.

%% Test that the slower mech moves last.
movement_slower_last(_Config) ->
  MechR = #mech{position = {2,0}, speed = 1, skills = [{slow, 2, 1}],
                side = right},
  MechL = #mech{position = {0,0}, speed = 1, skills = [{slow, 4, 1}]},
  {[MechR2, MechL2], _} = katarchy_raid:run([MechR, MechL]),
  {1,0} = MechR2#mech.position,
  {0,0} = MechL2#mech.position.

%% Test that two incoming mechs stop before running over each other.
movement_stop(_Config) ->
  MechL = #mech{position = {0,0}, speed = 1},
  MechR = #mech{position = {3,0}, speed = 1, side = right},
  {[MechL2, MechR2], _} = katarchy_raid:run([MechL, MechR]),
  {1,0} = MechL2#mech.position,
  {2,0} = MechR2#mech.position.

%% Test that each movement takes a turn.
movement_turns(_Config) ->
  Mech = #mech{position = {0,0}, speed = 2},
  Obstacle = #mech{position = {6,0}},
  {_, Turns} = katarchy_raid:run([Mech, Obstacle]),
  3 = length(Turns).

%% Test that each movement is properly tracked.
movement_turns_track(_Config) ->
  Mech = #mech{position = {0,0}, speed = 1},
  Obstacle = #mech{position = {3,0}},
  {_, [[Mech2|_],[Mech3|_]]} = katarchy_raid:run([Mech, Obstacle]),
  {1,0} = Mech2#mech.position,
  {2,0} = Mech3#mech.position.

%% Test that two mechs can't be in the same position.
not_same_position(_Config) ->
  Mech = #mech{position = {0,0}},
  Mech2 = #mech{position = {0,0}},
  invalid_setup = (catch katarchy_raid:run([Mech, Mech2])).

%% Test that a single mech always stays.
single_mech(_Config) ->
  Mech = #mech{},
  {[Mech], _} = katarchy_raid:run([Mech]).

%% Test that two sitting ducks don't do anything.
sitting_ducks(_Config) ->
  Mech = #mech{},
  {[Mech, Mech], _} = katarchy_raid:run([Mech, Mech]).

%% Test that a mech with triattack can damage the three front enemies.
triattack(_Config) ->
  MechL = #mech{position = {0,1}, attack_power = 10, skills = [triattack]},
  MechsR = [#mech{position = {1,0}, side = right},
            #mech{position = {1,1}, side = right},
            #mech{position = {1,2}, side = right}],
  {[MechL|MechsR2], _} = katarchy_raid:run([MechL|MechsR]),
  true = lists:all(fun(X) -> destroyed == X#mech.position end, MechsR2).

%% Test that a mech with triattack doesn't attack if all enemies are hidden.
triattack_all_hidden(_Config) ->
  MechL = #mech{position = {0,1}, attack_power = 10, skills = [triattack]},
  MechsR = [#mech{position = {1,0}, side = right, skills = [hidden]},
            #mech{position = {1,1}, side = right, skills = [hidden]},
            #mech{position = {1,2}, side = right, skills = [hidden]}],
  {[MechL|MechsR], _} = katarchy_raid:run([MechL|MechsR]).

%% Test that triattack can cause friendly fire.
triattack_friendly_fire(_Config) ->
  Attacker = #mech{position = {0,0}, attack_power = 10, skills = [triattack]},
  Ally = #mech{position = {1,0}},
  Enemy = #mech{position = {1,1}, side = right},
  {[Attacker, Ally2, Enemy2], _} = katarchy_raid:run([Attacker, Ally, Enemy]),
  destroyed = Ally2#mech.position,
  destroyed = Enemy2#mech.position.

%% Test that triattack ranged can cause friendly fire.
triattack_friendly_fire_ranged(_Config) ->
  Attacker = #mech{position = {0,0}, attack_power = 10,
                   skills = [triattack, ranged]},
  Ally = #mech{position = {3,0}},
  Enemy = #mech{position = {5,1}, side = right},
  {[Attacker, Ally2, Enemy2], _} = katarchy_raid:run([Attacker, Ally, Enemy]),
  destroyed = Ally2#mech.position,
  destroyed = Enemy2#mech.position.

%% Test that a mech with ranged triattack can damage up to three enemies.
triattack_ranged(_Config) ->
  MechL = #mech{position = {0,1}, attack_power = 10,
                skills = [triattack, ranged]},
  MechsR = [#mech{position = {3,0}, side = right},
            #mech{position = {4,1}, side = right},
            #mech{position = {5,2}, side = right}],
  {[MechL|MechsR2], _} = katarchy_raid:run([MechL|MechsR]),
  true = lists:all(fun(X) -> destroyed == X#mech.position end, MechsR2).

%% Test that a mech with triattack doesn't attack if it's covered with allies.
triattack_ranged_allies_cover(_Config) ->
  MechL = #mech{position = {0,1}, attack_power = 10, skills = [triattack]},
  MechR = #mech{position = {4,0}, side = right},
  Cover = [#mech{position = {1,0}},
           #mech{position = {1,1}},
           #mech{position = {1,2}}],
  {[MechL,MechR|Cover], _} = katarchy_raid:run([MechL,MechR|Cover]).

