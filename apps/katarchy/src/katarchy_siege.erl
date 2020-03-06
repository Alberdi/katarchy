-module(katarchy_siege).

-include("katarchy_mech.hrl").

-export([run/1]).

-define(GRID_LIMIT, 10).

%%--------------------------------------------------------------------
%% Exported functions
%%--------------------------------------------------------------------
run(Mechs) ->
  {NewMechs, Turns} = run_turns(Mechs, []),
  {NewMechs, lists:reverse(Turns)}.

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


move(_Mech, 0, Mechs) ->
  {complete, Mechs};
move(Mech, Speed, Mechs) ->
  {PosX, PosY} = Mech#mech.position,
  TargetPos = case Mech#mech.side of
                left when PosX > ?GRID_LIMIT ->
                  undefined;
                left ->
                  {PosX + 1, PosY};
                right when PosX =:= 0 ->
                  undefined;
                right ->
                  {PosX - 1, PosY}
              end,
  case lists:keymember(TargetPos, 2, Mechs) of
    true ->
      {incomplete, Mech, Mechs};
    false ->
      NewMech = Mech#mech{position = TargetPos},
      NewMechs = lists:keyreplace(Mech#mech.position, 2, Mechs, NewMech),
      move(NewMech, Speed-1, NewMechs)
  end.

run_turns(Mechs, Turns) ->
  case do_movement(Mechs) of
    Mechs ->
      {Mechs, Turns};
    NewMechs ->
      run_turns(NewMechs, [NewMechs|Turns])
  end.

