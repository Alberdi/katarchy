-module(katarchy_siege_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("katarchy_mech.hrl").

%% Test server callbacks
-export([suite/0, all/0]).

%% Test cases
-export([movement_limit_left/1,
         movement_limit_right/1,
         movement_obstacle/1,
         movement_stop/1,
         single_mech/1,
         sitting_ducks/1]).

%%--------------------------------------------------------------------
%% CT callbacks
%%--------------------------------------------------------------------
suite() ->
  [{timetrap, {seconds, 10}}].

all() ->
  [movement_limit_left,
   movement_limit_right,
   movement_obstacle,
   movement_stop,
   single_mech,
   sitting_ducks].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------
%% Test that one left minion eventually escapes the siege.
movement_limit_left(_Config) ->
  MechL = #mech{position = {0,0}, speed = 1},
  MechR = #mech{position = {0,1}, side = right},
  [MechL2, MechR] = katarchy_siege:run([MechL, MechR]),
  undefined = MechL2#mech.position.

%% Test that one right minion eventually escapes the siege.
movement_limit_right(_Config) ->
  MechL = #mech{position = {0,0}},
  MechR = #mech{position = {0,1}, speed = 1, side = right},
  [MechL, MechR2] = katarchy_siege:run([MechL, MechR]),
  undefined = MechR2#mech.position.

%% Test that one mech can move until one obstacle.
movement_obstacle(_Config) ->
  Mech = #mech{position = {0,0}, speed = 1},
  Obstacle = #mech{position = {5,0}},
  [Mech2, Obstacle] = katarchy_siege:run([Mech, Obstacle]),
  {4,0} = Mech2#mech.position.

%% Test that two incoming mechs stop before running over each other.
movement_stop(_Config) ->
  MechL = #mech{position = {0,0}, speed = 1},
  MechR = #mech{position = {3,0}, speed = 1, side = right},
  [MechL2, MechR2] = katarchy_siege:run([MechL, MechR]),
  {1,0} = MechL2#mech.position,
  {2,0} = MechR2#mech.position.

%% Test that a single mech always stays.
single_mech(_Config) ->
  Mech = #mech{},
  [Mech] = katarchy_siege:run([Mech]).

%% Test that two sitting ducks don't do anything.
sitting_ducks(_Config) ->
  Mech = #mech{},
  [Mech, Mech] = katarchy_siege:run([Mech, Mech]).

