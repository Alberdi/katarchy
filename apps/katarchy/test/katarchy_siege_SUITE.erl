-module(katarchy_siege_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("katarchy_mech.hrl").

%% Test server callbacks
-export([suite/0, all/0]).

%% Test cases
-export([movement_double/1,
         movement_faster_first/1,
         movement_jump/1,
         movement_jump_blocked/1,
         movement_jump_limit/1,
         movement_jump_speed/1,
         movement_limit_left/1,
         movement_limit_right/1,
         movement_not_blocking/1,
         movement_obstacle/1,
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
  [movement_double,
   movement_faster_first,
   movement_jump,
   movement_jump_blocked,
   movement_jump_limit,
   movement_jump_speed,
   movement_limit_left,
   movement_limit_right,
   movement_not_blocking,
   movement_obstacle,
   movement_stop,
   movement_turns,
   movement_turns_track,
   not_same_position,
   single_mech,
   sitting_ducks].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------
%% Test that one minion can move double.
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

