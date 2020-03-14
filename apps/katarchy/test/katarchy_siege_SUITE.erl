-module(katarchy_siege_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("katarchy_mech.hrl").

%% Test server callbacks
-export([suite/0, all/0]).

%% Test cases
-export([attack_double_ko/1,
         attack_double_ko_slow/1,
         attack_facing/1,
         attack_not_facing/1,
         attack_not_same_side/1,
         attack_outside/1,
         attack_power/1,
         attack_power_overkill/1,
         attack_power_ranged/1,
         attack_ranged/1,
         attack_ranged_blocked/1,
         attack_ranged_double_ko/1,
         attack_ranged_no_target/1,
         attack_ranged_not_same_side/1,
         attack_ranged_right/1,
         attack_slow/1,
         mech_id_remains/1,
         movement_double/1,
         movement_faster_first/1,
         movement_jump/1,
         movement_jump_blocked/1,
         movement_jump_limit/1,
         movement_jump_pairs/1,
         movement_jump_speed/1,
         movement_limit_left/1,
         movement_limit_right/1,
         movement_not_blocking/1,
         movement_obstacle/1,
         movement_slow/1,
         movement_slow_last/1,
         movement_slower_last/1,
         movement_stop/1,
         movement_turns/1,
         movement_turns_track/1,
         not_same_position/1,
         single_mech/1,
         sitting_ducks/1]).

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
   attack_power,
   attack_power_overkill,
   attack_power_ranged,
   attack_ranged,
   attack_ranged_blocked,
   attack_ranged_double_ko,
   attack_ranged_no_target,
   attack_ranged_not_same_side,
   attack_ranged_right,
   attack_slow,
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
   sitting_ducks].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------
%% Test that two mechs can ko each other.
attack_double_ko(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 1},
  MechR = #mech{position = {1,0}, attack_power = 1, side = right},
  {[MechL2, MechR2], _} = katarchy_siege:run([MechL, MechR]),
  undefined = MechL2#mech.position,
  undefined = MechR2#mech.position.

%% Test that two mechs can ko each other even if one is slower
attack_double_ko_slow(_Config) ->
  MechR = #mech{position = {1,0}, attack_power = 1, side = right},
  MechL = #mech{position = {0,0}, attack_power = 2, skills = [{slow, 2, 2}]},
  {[MechR2, MechL2], _} = katarchy_siege:run([MechR, MechL]),
  undefined = MechL2#mech.position,
  undefined = MechR2#mech.position.

%% Test that one mech can attack a rival that it's facing.
attack_facing(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 1},
  MechR = #mech{position = {1,0}, side = right},
  {[MechL, MechR2], _} = katarchy_siege:run([MechL, MechR]),
  undefined = MechR2#mech.position.

%% Test that one mech can't attack a rival that isn't facing.
attack_not_facing(_Config) ->
  MechL = #mech{position = {1,0}, attack_power = 1},
  MechR = #mech{position = {0,0}, side = right},
  {[MechL, MechR], _} = katarchy_siege:run([MechL, MechR]).

%% Test that one mech can't attack an ally.
attack_not_same_side(_Config) ->
  Mech1 = #mech{position = {0,0}, attack_power = 1},
  Mech2 = #mech{position = {1,0}},
  {[Mech1, Mech2], _} = katarchy_siege:run([Mech1, Mech2]),
  {1,0} = Mech2#mech.position.

%% Test that one mech doesn't go out of bounds when attacking.
attack_outside(_Config) ->
  Mech = #mech{position = {0,0}, side = right, attack_power = 1},
  {[Mech], _} = katarchy_siege:run([Mech]).

%% Test that the attack power allows to damage a rival faster.
attack_power(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 5},
  MechR = #mech{position = {1,0}, side = right},
  {[MechL, MechR2], Turns} = katarchy_siege:run([MechL, MechR]),
  undefined = MechR2#mech.position,
  2 = length(Turns).

%% Test that the attack power allows to overdamage a rival.
attack_power_overkill(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 500},
  MechR = #mech{position = {1,0}, side = right},
  {[MechL, MechR2], Turns} = katarchy_siege:run([MechL, MechR]),
  undefined = MechR2#mech.position,
  1 = length(Turns).

%% Test that the attack power allows to damage a rival faster when ranged.
attack_power_ranged(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 5, skills = [ranged]},
  MechR = #mech{position = {4,0}, side = right},
  {[MechL, MechR2], Turns} = katarchy_siege:run([MechL, MechR]),
  undefined = MechR2#mech.position,
  2 = length(Turns).

%% Test that one mech can attack at range.
attack_ranged(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 1, skills = [ranged]},
  MechR = #mech{position = {4,0}, side = right},
  {[MechL, MechR2], _} = katarchy_siege:run([MechL, MechR]),
  undefined = MechR2#mech.position.

%% Test that one mech can block the line of attack.
attack_ranged_blocked(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 1, skills = [ranged]},
  MechR = #mech{position = {4,0}, side = right},
  Obstacle = #mech{position = {2,0}},
  Mechs = [MechL, MechR, Obstacle],
  {Mechs, _} = katarchy_siege:run(Mechs).

%% Test that two ranged mechs can kill each other.
attack_ranged_double_ko(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 1, skills = [ranged]},
  MechR = #mech{position = {4,0}, attack_power = 1, skills = [ranged],
                side = right},
  {[MechL2, MechR2], _} = katarchy_siege:run([MechL, MechR]),
  undefined = MechL2#mech.position,
  undefined = MechR2#mech.position.

%% Test that one ranged mechs doesn't shoot if not needed.
attack_ranged_no_target(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 1, skills = [ranged]},
  MechR = #mech{position = {4,1}, side = right},
  {[MechL, MechR], _} = katarchy_siege:run([MechL, MechR]).

%% Test that one mech can't attack a friend.
attack_ranged_not_same_side(_Config) ->
  Mech1 = #mech{position = {0,0}, attack_power = 1, skills = [ranged]},
  Mech2 = #mech{position = {4,0}},
  {[Mech1, Mech2], _} = katarchy_siege:run([Mech1, Mech2]).

%% Test that one right mech can attack to the left.
attack_ranged_right(_Config) ->
  MechL = #mech{position = {0,0}},
  MechR = #mech{position = {4,0}, attack_power = 1, skills = [ranged],
                side = right},
  {[MechL2, MechR], _} = katarchy_siege:run([MechL, MechR]),
  undefined = MechL2#mech.position.

%% Test that an attack is delayed for "slow" mechs.
attack_slow(_Config) ->
  MechL = #mech{position = {0,0}, attack_power = 1, skills = [{slow, 3, 3}]},
  MechR = #mech{position = {1,0}, hit_points = 1, side = right},
  {[_, MechR2], Turns} = katarchy_siege:run([MechL, MechR]),
  5 = length(Turns), % 2 more turns for hit, 3 turns for recharging
  undefined = MechR2#mech.position.

%% Test that mechs have one id that remains with them.
mech_id_remains(_Config) ->
  Mech = #mech{position = {0,0}, id = <<"alpha">>, speed = 1},
  {[Mech2], _} = katarchy_siege:run([Mech]),
  <<"alpha">> = Mech2#mech.id.

%% Test that one mech can move double.
movement_double(_Config) ->
  MechL = #mech{position = {0,0}, speed = 2},
  MechR = #mech{position = {7,0}, speed = 1, side = right},
  {[MechL2, MechR2], _} = katarchy_siege:run([MechL, MechR]),
  {4,0} = MechL2#mech.position,
  {5,0} = MechR2#mech.position.

%% Test that the faster mech reaches the target position first.
movement_faster_first(_Config) ->
  MechL = #mech{position = {0,0}, speed = 1},
  MechR = #mech{position = {2,0}, speed = 2, side = right},
  {[MechL, MechR2], _} = katarchy_siege:run([MechL, MechR]),
  {1,0} = MechR2#mech.position.

%% Test that a jumping mech can jump over another.
movement_jump(_Config) ->
  Mech = #mech{position = {0,0}, speed = 1, skills = [jump]},
  Obstacle = #mech{position = {1,0}},
  {_, [[MechT1,Obstacle]|_]} = katarchy_siege:run([Mech, Obstacle]),
  {2,0} = MechT1#mech.position.

%% Test that a jumping mech can't jump over two mechs.
movement_jump_blocked(_Config) ->
  Mech = #mech{position = {0,0}, speed = 1, skills = [jump]},
  Obstacle1 = #mech{position = {1,0}},
  Obstacle2 = #mech{position = {2,0}},
  Mechs = [Mech, Obstacle1, Obstacle2],
  {Mechs, _} = katarchy_siege:run(Mechs).

%% Test that a jumping mech can't jump over the last before the limit.
movement_jump_limit(_Config) ->
  Obstacle = #mech{position = {0,0}},
  Mech = #mech{position = {1,0}, speed = 1, skills = [jump], side = right},
  {[Mech, Obstacle], _} = katarchy_siege:run([Mech, Obstacle]).

%% Test two jumping mechs together reaching the end.
movement_jump_pairs(_Config) ->
  Mech1 = #mech{position = {0,0}, speed = 1, skills = [jump]},
  Mech2 = #mech{position = {1,0}, speed = 1, skills = [jump]},
  {[MechX, MechY], _} = katarchy_siege:run([Mech1, Mech2]),
  undefined = MechX#mech.position,
  undefined = MechY#mech.position.

%% Test that jumping only consumes one movement.
movement_jump_speed(_Config) ->
  Mech = #mech{position = {0,0}, speed = 2, skills = [jump]},
  Obstacle1 = #mech{position = {1,0}},
  Obstacle2 = #mech{position = {3,0}},
  {_, [[MechT1|_]|_]} = katarchy_siege:run([Mech, Obstacle1, Obstacle2]),
  {4,0} = MechT1#mech.position.

%% Test that one left minion eventually escapes the siege.
movement_limit_left(_Config) ->
  MechL = #mech{position = {0,0}, speed = 1},
  MechR = #mech{position = {0,1}, side = right},
  {[MechL2, MechR], _} = katarchy_siege:run([MechL, MechR]),
  undefined = MechL2#mech.position.

%% Test that one right minion eventually escapes the siege.
movement_limit_right(_Config) ->
  MechL = #mech{position = {0,0}},
  MechR = #mech{position = {0,1}, speed = 1, side = right},
  {[MechL, MechR2], _} = katarchy_siege:run([MechL, MechR]),
  undefined = MechR2#mech.position.

%% Test that a slower mech doesn't block a faster one.
movement_not_blocking(_Config) ->
  MechF = #mech{position = {0,0}, speed = 2},
  MechS = #mech{position = {1,0}, speed = 1},
  Obstacle = #mech{position = {3,0}},
  {[MechF2, MechS2, Obstacle], Turns} =
    katarchy_siege:run([MechF, MechS, Obstacle]),
  {1,0} = MechF2#mech.position,
  {2,0} = MechS2#mech.position,
  1 = length(Turns).

%% Test that one mech can move until one obstacle.
movement_obstacle(_Config) ->
  Mech = #mech{position = {0,0}, speed = 1},
  Obstacle = #mech{position = {5,0}},
  {[Mech2, Obstacle], _} = katarchy_siege:run([Mech, Obstacle]),
  {4,0} = Mech2#mech.position.

%% Test that one mech can move slowly.
movement_slow(_Config) ->
  Mech = #mech{position = {1,0}, speed = 1, side = right,
               skills = [{slow, 3,3}]},
  {[MechF], Turns} = katarchy_siege:run([Mech]),
  8 = length(Turns), % 2 more turns in {1,0}, 3 in {0,0}, 3 in undefined.
  undefined = MechF#mech.position.

%% Test that the slow mech moves last.
movement_slow_last(_Config) ->
  MechL = #mech{position = {0,0}, speed = 1, skills = [{slow, 4, 1}]},
  MechR = #mech{position = {2,0}, speed = 1, side = right},
  {[MechL2, MechR2], _} = katarchy_siege:run([MechL, MechR]),
  {0,0} = MechL2#mech.position,
  {1,0} = MechR2#mech.position.

%% Test that the slower mech moves last.
movement_slower_last(_Config) ->
  MechR = #mech{position = {2,0}, speed = 1, skills = [{slow, 2, 1}],
                side = right},
  MechL = #mech{position = {0,0}, speed = 1, skills = [{slow, 4, 1}]},
  {[MechR2, MechL2], _} = katarchy_siege:run([MechR, MechL]),
  {1,0} = MechR2#mech.position,
  {0,0} = MechL2#mech.position.

%% Test that two incoming mechs stop before running over each other.
movement_stop(_Config) ->
  MechL = #mech{position = {0,0}, speed = 1},
  MechR = #mech{position = {3,0}, speed = 1, side = right},
  {[MechL2, MechR2], _} = katarchy_siege:run([MechL, MechR]),
  {1,0} = MechL2#mech.position,
  {2,0} = MechR2#mech.position.

%% Test that each movement takes a turn.
movement_turns(_Config) ->
  Mech = #mech{position = {0,0}, speed = 2},
  Obstacle = #mech{position = {6,0}},
  {_, Turns} = katarchy_siege:run([Mech, Obstacle]),
  3 = length(Turns).

%% Test that each movement is properly tracked.
movement_turns_track(_Config) ->
  Mech = #mech{position = {0,0}, speed = 1},
  Obstacle = #mech{position = {3,0}},
  {_, [[Mech2|_],[Mech3|_]]} = katarchy_siege:run([Mech, Obstacle]),
  {1,0} = Mech2#mech.position,
  {2,0} = Mech3#mech.position.

%% Test that two mechs can't be in the same position.
not_same_position(_Config) ->
  Mech = #mech{position = {0,0}},
  Mech2 = #mech{position = {0,0}},
  invalid_setup = (catch katarchy_siege:run([Mech, Mech2])).

%% Test that a single mech always stays.
single_mech(_Config) ->
  Mech = #mech{},
  {[Mech], _} = katarchy_siege:run([Mech]).

%% Test that two sitting ducks don't do anything.
sitting_ducks(_Config) ->
  Mech = #mech{},
  {[Mech, Mech], _} = katarchy_siege:run([Mech, Mech]).

