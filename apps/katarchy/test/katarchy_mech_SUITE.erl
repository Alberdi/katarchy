-module(katarchy_mech_SUITE).
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
  [skill_add,
   skill_add_second,
   skill_add_slow,
   skill_delete,
   skill_delete_exploding,
   skill_delete_one_remains,
   skill_delete_non_existent,
   skill_delete_slow,
   skill_inc_exploding,
   skill_inc_slow].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------
%% Test that a skill can be added to a mech.
skill_add(_Config) ->
  Mech = katarchy_mech:skill_add(jump, #mech{}),
  true = lists:member(jump, Mech#mech.skills).

%% Test that a skill can be added on top of another.
skill_add_second(_Config) ->
  Mech = katarchy_mech:skill_add(hidden, #mech{skills = [jump]}),
  true = lists:member(jump, Mech#mech.skills),
  true = lists:member(hidden, Mech#mech.skills).

%% Test that the slow skill can be added.
skill_add_slow(_Config) ->
  Skill = {slow, 7, 3},
  Mech = katarchy_mech:skill_add(Skill, #mech{}),
  [Skill] = Mech#mech.skills.

%% Test that a skill can be removed from a mech.
skill_delete(_Config) ->
  Mech = katarchy_mech:skill_delete(perforating, #mech{skills = [perforating]}),
  [] = Mech#mech.skills.

%% Test that a exploding skill can be deleted with an atom.
skill_delete_exploding(_Config) ->
  Mech = #mech{skills = [{exploding, 3}]},
  Mech2 = katarchy_mech:skill_delete(exploding, Mech),
  [] = Mech2#mech.skills.

%% Test that removing a non-existent skill is a no-op.
skill_delete_non_existent(_Config) ->
  Mech = #mech{},
  Mech = katarchy_mech:skill_delete(hidden, Mech).

%% Test removing just one skill from a mech with more than one.
skill_delete_one_remains(_Config) ->
  SlowSkill = {slow, 3, 3},
  Mech = #mech{skills = [SlowSkill, triattack]},
  Mech2 = katarchy_mech:skill_delete(triattack, Mech),
  [SlowSkill] = Mech2#mech.skills.

%% Test that a slow skill can be deleted with an atom.
skill_delete_slow(_Config) ->
  Mech = #mech{skills = [{slow, 4, 4}]},
  Mech2 = katarchy_mech:skill_delete(slow, Mech),
  [] = Mech2#mech.skills.

%% Test that the exploding skill can be incremented if added twice.
skill_inc_exploding(_Config) ->
  Skill = {exploding, 7},
  Mech = katarchy_mech:skill_add(Skill, #mech{skills = [{exploding, 10}]}),
  [{exploding, 17}] = Mech#mech.skills.

%% Test that the slow skill can be incremented if added twice.
skill_inc_slow(_Config) ->
  Skill = {slow, 3, 1},
  Mech = katarchy_mech:skill_add(Skill, #mech{skills = [{slow, 4, 2}]}),
  [{slow, 7, 3}] = Mech#mech.skills.

