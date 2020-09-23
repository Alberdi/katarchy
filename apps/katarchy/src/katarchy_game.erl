-module(katarchy_game).

-export([buy/2, forge/2, next_action/1, start/0]).

-behaviour(gen_statem).
-export([callback_mode/0, init/1]).
-export([forge/3, raid/3, store/3]).

%%--------------------------------------------------------------------
%% Exported functions
%%--------------------------------------------------------------------
buy(Pid, Items) ->
  gen_statem:call(Pid, {buy, Items}).


forge(Pid, Items) ->
  gen_statem:call(Pid, {forge, Items}).


next_action(Pid) ->
  gen_statem:call(Pid, next_action).


start() ->
  gen_statem:start(?MODULE, [], []).

%%--------------------------------------------------------------------
%% gen_statem callbacks
%%--------------------------------------------------------------------
callback_mode() ->
  state_functions.


forge({call, From}, {forge, _Items}, Data) ->
  {next_state, raid, Data, [{reply, From, ok}]};
forge({call, From}, next_action, Data) ->
  {keep_state_and_data, [{reply, From, {forge, Data}}]}.


raid({call, From}, next_action, Data) ->
  {keep_state_and_data, [{reply, From, {raid, Data}}]}.


store({call, From}, {buy, _Items}, Data) ->
  {next_state, forge, Data, [{reply, From, ok}]};
store({call, From}, next_action, Data) ->
  {keep_state_and_data, [{reply, From, {store, Data}}]}.


init(_) ->
  {ok, store, {}}.
