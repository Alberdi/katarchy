-module(katarchy_siege).

-include("katarchy_mech.hrl").

-export([run/1]).

-define(GRID_LIMIT, 10).

%%--------------------------------------------------------------------
%% Exported functions
%%--------------------------------------------------------------------
run(Mechs) ->
  validate_setup(Mechs),
  {NewMechs, Turns} = run_turns(Mechs, []),
  Return = {NewMechs, lists:reverse(Turns)},
  ct:log("katarchy_siege:run/1 returned ~p~n", [Return]),
  Return.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
do_movement(Mechs) ->
  FasterFirst = fun(X,Y) -> X#mech.speed > Y#mech.speed end,
  FasterMechs = lists:sort(FasterFirst, Mechs),
  do_movement(FasterMechs, Mechs, {[],[]}).

do_movement([#mech{position = undefined}|LeftToMove], AllMechs, Passes) ->
  do_movement(LeftToMove, AllMechs, Passes);
do_movement([Mech|LeftToMove], AllMechs, {CurrentPass, PrevPass} = Passes) ->
  case move(Mech, Mech#mech.speed, AllMechs) of
    {complete, NewMechs} ->
      do_movement(LeftToMove, NewMechs, Passes);
    {incomplete, NewMech, NewMechs} ->
      do_movement(LeftToMove, NewMechs, {[NewMech|CurrentPass], PrevPass})
  end;
do_movement([], NewMechs, {SamePass, SamePass}) ->
  NewMechs;
do_movement([], NewMechs, {CurrentPass, _}) ->
  do_movement(lists:reverse(CurrentPass), NewMechs, {[], CurrentPass}).


jump(Mech, Mechs, Speed, BlockedPos) ->
  case next_position(Mech#mech{position = BlockedPos}) of
    undefined ->
      {complete, Mechs};
    NextPosition ->
      case lists:keymember(NextPosition, 2, Mechs) of
        true ->
          {incomplete, Mech, Mechs};
        false ->
          move_step(Mech, NextPosition, Speed, Mechs)
      end
  end.


move(_Mech, 0, Mechs) ->
  {complete, Mechs};
move(Mech, Speed, Mechs) ->
  TargetPos = next_position(Mech),
  case lists:keymember(TargetPos, 2, Mechs) of
    true ->
      case lists:member(jump, Mech#mech.skills) of
        true ->
          jump(Mech, Mechs, Speed, TargetPos);
        false ->
          {incomplete, Mech, Mechs}
      end;
    false ->
      move_step(Mech, TargetPos, Speed, Mechs)
  end.


move_step(Mech, TargetPos, Speed, Mechs) ->
  NewMech = Mech#mech{position = TargetPos},
  NewMechs = lists:keyreplace(Mech#mech.position, 2, Mechs, NewMech),
  move(NewMech, Speed-1, NewMechs).


next_position(Mech) ->
  {PosX, PosY} = Mech#mech.position,
  case Mech#mech.side of
    left when PosX > ?GRID_LIMIT ->
      undefined;
    left ->
      {PosX + 1, PosY};
    right when PosX =:= 0 ->
      undefined;
    right ->
      {PosX - 1, PosY}
  end.


run_turns(Mechs, Turns) ->
  case do_movement(Mechs) of
    Mechs ->
      {Mechs, Turns};
    NewMechs ->
      run_turns(NewMechs, [NewMechs|Turns])
  end.


validate_setup(Mechs) ->
  Positions = [M#mech.position || M <- Mechs, M#mech.position =/= undefined],
  UniquePositions = length(lists:usort(Positions)),
  try
    UniquePositions = length(Positions)
  catch
    error:{badmatch, _} ->
      throw(invalid_setup)
  end.

