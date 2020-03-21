-module(katarchy_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% CT callbacks
%%--------------------------------------------------------------------
suite() ->
  [{timetrap, {seconds, 10}}].


all() ->
  [raid_mech_attack_power,
   raid_mech_explosive,
   raid_mech_hidden,
   raid_mech_fake_fields,
   raid_mech_position_yx,
   raid_mech_side_right,
   raid_mech_simple,
   raid_mech_skills,
   raid_mech_slow].


init_per_suite(Config) ->
  {ok, AppStartList} = application:ensure_all_started(katarchy),
  [{app_start_list, AppStartList}|Config].


end_per_suite(Config) ->
  [application:stop(App) || App <- ?config(app_start_list, Config)],
  Config.

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------
%% Test that the attack_power in a mech can be sent and returned.
raid_mech_attack_power(_Config) ->
  {[MechJson], _} = post_raid(ct:get_config(json_mech_full)),
  {Mech} = jiffy:decode(MechJson),
  12 = proplists:get_value(<<"attack_power">>, Mech).

%% Test that an explosive skill in a mech can be sent and returned.
raid_mech_explosive(_Config) ->
  {[MechJson], _} = post_raid(ct:get_config(json_mech_explosive)),
  {Mech} = jiffy:decode(MechJson),
  [{Skill}] = proplists:get_value(<<"skills">>, Mech),
  <<"explosive">> = proplists:get_value(<<"type">>, Skill),
  5 = proplists:get_value(<<"value">>, Skill).

%% Test that a hidden skill in a mech can be sent and returned.
raid_mech_hidden(_Config) ->
  {[MechJson], _} = post_raid(ct:get_config(json_mech_hidden)),
  {Mech} = jiffy:decode(MechJson),
  [{Skill}] = proplists:get_value(<<"skills">>, Mech),
  <<"hidden">> = proplists:get_value(<<"type">>, Skill).

%% Test that a mech with fake fields can be sent and returned.
raid_mech_fake_fields(_Config) ->
  {[MechJson], _} = post_raid(ct:get_config(json_mech_fake_fields)),
  {Mech} = jiffy:decode(MechJson),
  <<"mech_fake_fields">> = proplists:get_value(<<"id">>, Mech).

%% Test that a mech with a position with y before x can be sent and returned.
raid_mech_position_yx(_Config) ->
  {[MechJson], _} = post_raid(ct:get_config(json_mech_position_yx)),
  {Mech} = jiffy:decode(MechJson),
  {Position} = proplists:get_value(<<"position">>, Mech),
  0 = proplists:get_value(<<"x">>, Position),
  8 = proplists:get_value(<<"y">>, Position).

%% Test that a mech with a right side can be sent and returned.
raid_mech_side_right(_Config) ->
  {[MechJson], _} = post_raid(ct:get_config(json_mech_full)),
  {Mech} = jiffy:decode(MechJson),
  <<"right">> = proplists:get_value(<<"side">>, Mech).

%% Test that multiple skills can be sent and returned.
raid_mech_skills(_Config) ->
  {[MechJson], _} = post_raid(ct:get_config(json_mech_full)),
  {Mech} = jiffy:decode(MechJson),
  Skills = proplists:get_value(<<"skills">>, Mech),
  SkillTypes = [proplists:get_value(<<"type">>, S) || {S} <- Skills],
  true = lists:member(<<"jump">>, SkillTypes),
  true = lists:member(<<"perforating">>, SkillTypes),
  true = lists:member(<<"ranged">>, SkillTypes).

%% Test that a simple mech can be sent and returned.
raid_mech_simple(_Config) ->
  {[MechJson], _} = post_raid(ct:get_config(json_mech_simple)),
  {Mech} = jiffy:decode(MechJson),
  <<"mech_simple">> = proplists:get_value(<<"id">>, Mech).

%% Test that a slow skill in a mech can be sent and returned.
raid_mech_slow(_Config) ->
  {[MechJson], _} = post_raid(ct:get_config(json_mech_slow)),
  {Mech} = jiffy:decode(MechJson),
  [{Skill}] = proplists:get_value(<<"skills">>, Mech),
  <<"slow">> = proplists:get_value(<<"type">>, Skill),
  3 = proplists:get_value(<<"value">>, Skill).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
post_raid(Data) ->
  {ok, {{_, 200, _}, _, Body}} =
  httpc:request(post, {"http://localhost:8080/raid", [],
                       "application/json", Data}, [], []),
  {State} = jiffy:decode(Body),
  {proplists:get_value(<<"finalState">>, State),
   proplists:get_value(<<"turns">>, State)}.

