-module(katarchy_siege_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("katarchy_mech.hrl").

%% Test server callbacks
-export([suite/0, all/0]).

%% Test cases
-export([movement_double/1,
         movement_faster_first/1,
         movement_limit_left/1,
         movement_limit_right/1,
         movement_not_blocking/1,
         movement_obstacle/1,
         movement_stop/1,
         movement_turns/1,
         movement_turns_track/1,
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
   movement_limit_left,
   movement_limit_right,
   movement_not_blocking,
   movement_obstacle,
   movement_stop,
   movement_turns,
   movement_turns_track,
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

%% Test that a single mech always stays.
single_mech(_Config) ->
  Mech = #mech{},
  {[Mech], _} = katarchy_siege:run([Mech]).

%% Test that two sitting ducks don't do anything.
sitting_ducks(_Config) ->
  Mech = #mech{},
  {[Mech, Mech], _} = katarchy_siege:run([Mech, Mech]).

