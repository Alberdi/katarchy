-module(katarchy_siege).

-include("katarchy_mech.hrl").

-export([run/1]).

%%--------------------------------------------------------------------
%% Exported functions
%%--------------------------------------------------------------------
run(Mechs) ->
  case do_movement(Mechs) of
    Mechs ->
      Mechs;
    NewMechs ->
      ct:log("NewMechs: ~p~n", [NewMechs]),
      run(NewMechs)
  end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
do_movement(Mechs) ->
  do_movement(Mechs, Mechs).

do_movement([Mech|LeftToMove], AllMechs) ->
  NewMechs = move(Mech, Mech#mech.speed, AllMechs),
  do_movement(LeftToMove, NewMechs);
do_movement([], MovedMechs) ->
  MovedMechs.

move(_Mech, 0, Mechs) ->
  Mechs;
move(Mech, Speed, Mechs) ->
  {PosX, PosY} = Mech#mech.position,
  TargetPos = case Mech#mech.side of
            left -> {PosX + 1, PosY};
            right -> {PosX -1, PosY}
          end,
  case lists:keymember(TargetPos, 2, Mechs) of
    true ->
      Mechs;
    false ->
      NewMech = Mech#mech{position = TargetPos},
      NewMechs = lists:keyreplace(Mech#mech.position, 2, Mechs, NewMech),
      move(NewMech, Speed-1, NewMechs)
  end.

