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
do_attack(Mechs) ->
  AttackingFun = fun(X) -> X#mech.position =/= undefined andalso
                           X#mech.attack_power > 0 end,
  AttackingMechs = lists:filter(AttackingFun, Mechs),
  FasterFirst = fun(X,Y) -> X#mech.speed > Y#mech.speed end,
  FasterMechs = lists:sort(FasterFirst, AttackingMechs),
  NewMechs = do_attack(FasterMechs, Mechs),
  NewMechs2 = do_attack_ranged(NewMechs),
  lists:map(fun incapacitate_if_needed/1, NewMechs2).


do_attack([], Mechs) ->
  Mechs;
do_attack([Mech|LeftToAttack], Mechs) ->
  case next_position(Mech) of
    undefined ->
      do_attack(LeftToAttack, Mechs);
    NextPosition ->
      case lists:keyfind(NextPosition, 2, Mechs) of
        TargetMech when TargetMech#mech.side =/= Mech#mech.side ->
          NewHitPoints = TargetMech#mech.hit_points - Mech#mech.attack_power,
          NewMech = TargetMech#mech{hit_points = NewHitPoints},
          NewMechs = lists:keyreplace(TargetMech#mech.position, 2,
                                      Mechs, NewMech),
          do_attack(LeftToAttack, NewMechs);
        _ ->
          do_attack(LeftToAttack, Mechs)
      end
  end.


do_attack_ranged(Mechs) ->
  AttackingFun = fun(X) -> X#mech.position =/= undefined andalso
                           lists:member(ranged, X#mech.skills) end,
  AttackingMechs = lists:filter(AttackingFun, Mechs),
  FasterFirst = fun(X,Y) -> X#mech.speed > Y#mech.speed end,
  FasterMechs = lists:sort(FasterFirst, AttackingMechs),
  NewMechs = do_attack_ranged(FasterMechs, Mechs),
  lists:map(fun incapacitate_if_needed/1, NewMechs).

do_attack_ranged([], Mechs) ->
  Mechs;
do_attack_ranged([Mech|LeftToAttack], Mechs) ->
  NextPosition = next_position(Mech),
  NewMechs = range_attack(Mech, NextPosition, Mechs),
  do_attack_ranged(LeftToAttack, NewMechs).


range_attack(_Mech, undefined, Mechs) ->
  Mechs;
range_attack(Mech, TargetPos, Mechs) ->
  case lists:keyfind(TargetPos, 2, Mechs) of
    false ->
      NextPosition = next_position(TargetPos, Mech#mech.side),
      range_attack(Mech, NextPosition, Mechs);
    TargetMech when TargetMech#mech.side =/= Mech#mech.side ->
      NewHitPoints = TargetMech#mech.hit_points - Mech#mech.attack_power,
      NewMech = TargetMech#mech{hit_points = NewHitPoints},
      lists:keyreplace(TargetPos, 2, Mechs, NewMech);
    _ ->
      Mechs
  end.


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


incapacitate_if_needed(Mech) when Mech#mech.hit_points =< 0 ->
  Mech#mech{position = undefined};
incapacitate_if_needed(Mech) ->
  Mech.


jump(Mech, Mechs, Speed, BlockedPos) ->
  case next_position(BlockedPos, Mech#mech.side) of
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
  next_position(Mech#mech.position, Mech#mech.side).

next_position({PosX, PosY}, Side) ->
  case Side of
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
  MovedMechs = do_movement(Mechs),
  case do_attack(MovedMechs) of
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

