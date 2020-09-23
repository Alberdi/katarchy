-module(katarchy_game_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% CT callbacks
%%--------------------------------------------------------------------
suite() ->
  [{timetrap, {seconds, 10}}].

all() ->
  [forge,
   forge_disallowed,
   forge_skip,
   raid,
   start,
   start_action,
   store_disallowed,
   store_skip].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------
%% Test that the forge comes after the store.
forge(_Config) ->
  {ok, Pid} = katarchy_game:start(),
  ok = katarchy_game:buy(Pid, []),
  {forge, _} = katarchy_game:next_action(Pid).

%% Test that the forge doesn't work on a non-forge state.
forge_disallowed(_Config) ->
  {ok, Pid} = katarchy_game:start(),
  false = ok =:= (catch katarchy_game:forge(Pid, [])).

%% Test that you can skip the forge without forging anything.
forge_skip(_Config) ->
  {ok, Pid} = katarchy_game:start(),
  ok = katarchy_game:buy(Pid, []),
  ok = katarchy_game:forge(Pid, []).

%% Test that a game can be started.
start(_Config) ->
  {ok, Pid} = katarchy_game:start(),
  true = is_pid(Pid).

%% Test that a game starts in the store mode.
start_action(_Config) ->
  {ok, Pid} = katarchy_game:start(),
  {store, _} = katarchy_game:next_action(Pid).

%% Test that the forge doesn't work on a non-forge state.
store_disallowed(_Config) ->
  {ok, Pid} = katarchy_game:start(),
  ok = katarchy_game:buy(Pid, []),
  false = ok =:= (catch katarchy_game:buy(Pid, [])).

%% Test that you can skip the store without buying anything.
store_skip(_Config) ->
  {ok, Pid} = katarchy_game:start(),
  ok = katarchy_game:buy(Pid, []).

%% Test that the raid comes after the forge.
raid(_Config) ->
  {ok, Pid} = katarchy_game:start(),
  ok = katarchy_game:buy(Pid, []),
  ok = katarchy_game:forge(Pid, []),
  {raid, _} = katarchy_game:next_action(Pid).

