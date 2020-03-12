-module(katarchy_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Test server callbacks
-export([suite/0, all/0,
        init_per_suite/1, end_per_suite/1]).

%% Test cases
-export([siege_mech_fake_fields/1,
         siege_mech_full/1,
         siege_mech_position_yx/1,
         siege_mech_simple/1]).

%%--------------------------------------------------------------------
%% CT callbacks
%%--------------------------------------------------------------------
suite() ->
  [{timetrap, {seconds, 10}}].


all() ->
  [siege_mech_fake_fields,
   siege_mech_full,
   siege_mech_position_yx,
   siege_mech_simple].


init_per_suite(Config) ->
  {ok, AppStartList} = application:ensure_all_started(katarchy),
  [{app_start_list, AppStartList}|Config].


end_per_suite(Config) ->
  [application:stop(App) || App <- ?config(app_start_list, Config)],
  Config.

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------
%% Test that a mech with fake fields can be sent and returned.
siege_mech_fake_fields(_Config) ->
  {[MechJson], _} = post_siege(ct:get_config(json_mech_fake_fields)),
  {Mech} = jiffy:decode(MechJson),
  <<"mech_fake_fields">> = proplists:get_value(<<"id">>, Mech).

%% Test that a mech with all fields can be sent and returned.
siege_mech_full(_Config) ->
  {[MechJson], _} = post_siege(ct:get_config(json_mech_full)),
  {Mech} = jiffy:decode(MechJson),
  <<"mech_full">> = proplists:get_value(<<"id">>, Mech).

%% Test that a mech with a position with y before x can be sent and returned.
siege_mech_position_yx(_Config) ->
  {[MechJson], _} = post_siege(ct:get_config(json_mech_position_yx)),
  {Mech} = jiffy:decode(MechJson),
  <<"mech_position_yx">> = proplists:get_value(<<"id">>, Mech).

%% Test that a simple mech can be sent and returned.
siege_mech_simple(_Config) ->
  {[MechJson], _} = post_siege(ct:get_config(json_mech_simple)),
  {Mech} = jiffy:decode(MechJson),
  <<"mech_simple">> = proplists:get_value(<<"id">>, Mech).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
post_siege(Data) ->
  {ok, {{_, 200, _}, _, Body}} =
  httpc:request(post, {"http://localhost:8080/siege", [],
                       "application/json", Data}, [], []),
  {State} = jiffy:decode(Body),
  {proplists:get_value(<<"finalState">>, State),
   proplists:get_value(<<"turns">>, State)}.

