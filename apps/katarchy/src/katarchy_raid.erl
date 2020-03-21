-module(katarchy_raid).

-include("katarchy_mech.hrl").

-export([run/1]).

-define(GRID_LIMIT, 10).

%%--------------------------------------------------------------------
%% Exported functions
%%--------------------------------------------------------------------
run(Mechs) ->
  validate_setup(Mechs),
  {NewMechs, Turns} = run_turns(Mechs, []),
  {NewMechs, lists:reverse(Turns)}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
adjacent_mechs(Position, Mechs) ->
  lists:filtermap(fun(Side) ->
                      case next_position(Position, Side) of
                        undefined ->
                          false;
                        Pos ->
                          case lists:keyfind(Pos, 2, Mechs) of
                            false ->
                              false;
                            Mech ->
                              {true, Mech}
                          end
                      end
                  end, [left, right, up, down]).


can_attack(Mech) ->
  not is_slowed(Mech) andalso
  Mech#mech.position =/= undefined andalso
  Mech#mech.attack_power > 0.


can_move(Mech) ->
  not is_slowed(Mech) andalso
  Mech#mech.position =/= undefined andalso
  Mech#mech.speed > 0.


damage(Damage, Mech, Mechs) ->
  NewHitPoints = Mech#mech.hit_points - Damage,
  NewMech = Mech#mech{hit_points = NewHitPoints},
  NewMechs = lists:keyreplace(Mech#mech.position, 2, Mechs, NewMech),
  explode_if_killed(NewMech, NewMechs).


explode_if_killed(Mech, Mechs) ->
  case {lists:keyfind(explosive, 1, Mech#mech.skills), Mech#mech.hit_points} of
    {{explosive, Value}, HP} when Value > 0 andalso HP =< 0 ->
      NewSkills = lists:keyreplace(explosive, 1, Mech#mech.skills,
                                   {explosive, 0}),
      NewMech = Mech#mech{skills = NewSkills},
      NewMechs = lists:keyreplace(Mech#mech.position, 2, Mechs, NewMech),
      lists:foldl(fun(X, MechsAcc) -> damage(Value, X, MechsAcc) end,
                  NewMechs, adjacent_mechs(NewMech#mech.position, NewMechs));
    _ ->
      Mechs
  end.


is_faster(M1, M2) ->
  case [lists:keyfind(slow, 1, M#mech.skills) || M <- [M1,M2]] of
    [false, {slow, _, _}] ->
      true;
    [{slow, _, _}, false] ->
      false;
    [{slow, MaxX, _}, {slow, MaxY, _}] ->
      MaxX =< MaxY;
    [false, false] ->
      M1#mech.speed >= M2#mech.speed
  end.


is_slowed(Mech) ->
  case lists:keyfind(slow, 1, Mech#mech.skills) of
    {slow, _Max, I} when I =/= 0 ->
      true;
    _ ->
      false
  end.


do_attack(Mechs) ->
  AttackingMechs = lists:filter(fun can_attack/1, Mechs),
  FasterMechs = lists:sort(fun is_faster/2, AttackingMechs),
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
        Target when Target#mech.side =/= Mech#mech.side ->
          NewMechs = case lists:member(hidden, Target#mech.skills) of
            true -> Mechs;
            false -> do_attack(Mech, Target, Mechs)
          end,
          do_attack(LeftToAttack, NewMechs);
        _ ->
          do_attack(LeftToAttack, Mechs)
      end
  end.


do_attack(Attacker, Target, Mechs) ->
  NewMechs = damage(Attacker#mech.attack_power, Target, Mechs),
  {Attacker2, NewMechs2} = reveal_hidden(Attacker, NewMechs),
  case lists:member(perforating, Attacker#mech.skills) of
    true ->
      NextPosition = next_position(Target#mech.position, Attacker2#mech.side),
      case lists:keyfind(NextPosition, 2, NewMechs2) of
        false ->
          NewMechs2;
        NewTarget ->
          do_attack(Attacker2, NewTarget, NewMechs2)
      end;
    false ->
      NewMechs2
  end.


do_attack_ranged(Mechs) ->
  AttackingFun = fun(X) -> lists:member(ranged, X#mech.skills)
                           andalso can_attack(X) end,
  AttackingMechs = lists:filter(AttackingFun, Mechs),
  FasterMechs = lists:sort(fun is_faster/2, AttackingMechs),
  NewMechs = do_attack_ranged(FasterMechs, Mechs),
  lists:map(fun incapacitate_if_needed/1, NewMechs).

do_attack_ranged([], Mechs) ->
  Mechs;
do_attack_ranged([Mech|LeftToAttack], Mechs) ->
  NewMechs = range_attack(Mech, Mechs),
  do_attack_ranged(LeftToAttack, NewMechs).


range_attack(Mech, Mechs) ->
  MechsInFront = mechs_in_front(Mech, Mechs),
  TargeteableEnemy = targeteable_enemy(Mech, MechsInFront),
  case {lists:member(perforating, Mech#mech.skills), TargeteableEnemy} of
    {true, true} ->
      lists:foldl(fun(X, MechsAcc) ->
                      damage(Mech#mech.attack_power, X, MechsAcc) end,
                  Mechs, MechsInFront);
    _ ->
      range_attack(Mech, MechsInFront, TargeteableEnemy, Mechs)
  end.

range_attack(_, [], _, Mechs) ->
  Mechs;
range_attack(Mech, [Target|_], TargeteableEnemy, Mechs) ->
  IsHidden = lists:member(hidden, Target#mech.skills),
  case {Target#mech.side == Mech#mech.side, IsHidden, TargeteableEnemy} of
    {false, false, _} ->
      damage(Mech#mech.attack_power, Target, Mechs);
    {false, true, true} ->
      damage(Mech#mech.attack_power, Target, Mechs);
    _ ->
      Mechs
  end.


targeteable_enemy(_Mech, []) ->
  false;
targeteable_enemy(Mech, ListMechs) ->
  [Target|OtherMechs] = ListMechs,
  IsEnemy = Target#mech.side =/= Mech#mech.side,
  IsHidden = lists:member(hidden, Target#mech.skills),
  case {IsEnemy, IsHidden} of
    {false, _} -> false;
    {true, false} -> true;
    {true, true} -> targeteable_enemy(Mech, OtherMechs)
  end.


do_movement(Mechs) ->
  MovingMechs = lists:filter(fun can_move/1, Mechs),
  FasterMechs = lists:sort(fun is_faster/2, MovingMechs),
  do_movement(FasterMechs, Mechs, {[],[]}).

do_movement([Mech|LeftToMove], AllMechs, {CurrentPass, PrevPass} = Passes) ->
  case move(Mech, Mech#mech.speed, AllMechs) of
    {complete, NewMechs} ->
      do_movement(LeftToMove, NewMechs, Passes);
    {incomplete, NewMech, NewMechs} ->
      {NewMech2, NewMechs2} = reveal_hidden(NewMech, NewMechs),
      NextPosition = next_position(Mech),
      BumpedMech = lists:keyfind(NextPosition, 2, NewMechs2),
      {_, NewMechs3} = reveal_hidden(BumpedMech, NewMechs2),
      do_movement(LeftToMove, NewMechs3, {[NewMech2|CurrentPass], PrevPass})
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


mechs_in_front(Mech, Mechs) ->
  NextPosition = next_position(Mech#mech.position, Mech#mech.side),
  lists:reverse(mechs_in_front(NextPosition, Mech#mech.side, Mechs, [])).

mechs_in_front(undefined, _, _, LinedMechs) ->
  LinedMechs;
mechs_in_front(Position, Side, Mechs, LinedMechs) ->
  NewLinedMechs = case lists:keyfind(Position, 2, Mechs) of
    false -> LinedMechs;
    Mech -> [Mech|LinedMechs]
  end,
  NextPosition = next_position(Position, Side),
  mechs_in_front(NextPosition, Side, Mechs, NewLinedMechs).


move(_Mech, 0, Mechs) ->
  {complete, Mechs};
move(Mech, Speed, Mechs) ->
  TargetPos = next_position(Mech),
  case TargetPos =/= undefined andalso lists:keymember(TargetPos, 2, Mechs) of
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
    left when PosX >= ?GRID_LIMIT-1 ->
      undefined;
    left ->
      {PosX + 1, PosY};
    right when PosX =:= 0 ->
      undefined;
    right ->
      {PosX - 1, PosY};
    up when PosY =:= 0 ->
      undefined;
    up ->
      {PosX, PosY - 1};
    down when PosY >= ?GRID_LIMIT-1 ->
      undefined;
    down ->
      {PosX, PosY + 1}
  end.


reveal_hidden(Mech, Mechs) ->
  case lists:member(hidden, Mech#mech.skills) of
    false ->
      {Mech, Mechs};
    true ->
      NewSkills = lists:delete(hidden, Mech#mech.skills),
      NewMech = Mech#mech{skills = NewSkills},
      {NewMech, lists:keyreplace(Mech#mech.position, 2, Mechs, NewMech)}
  end.


run_turns(InitialMechs, Turns) ->
  Mechs = [slow_tick(Mech) || Mech <- InitialMechs],
  MovedMechs = do_movement(Mechs),
  FinalMechs = do_attack(MovedMechs),
  case lists:member(FinalMechs, Turns) orelse FinalMechs =:= InitialMechs of
    true ->
      {FinalMechs, Turns};
    false ->
      run_turns(FinalMechs, [FinalMechs|Turns])
  end.


slow_tick(Mech) ->
  case lists:keyfind(slow, 1, Mech#mech.skills) of
    {slow, Max, I} when I > 0 ->
      NewSkills = lists:keyreplace(slow, 1, Mech#mech.skills,
                                   {slow, Max, I-1}),
      Mech#mech{skills = NewSkills};
    {slow, Max, I} when I =:= 0 ->
      NewSkills = lists:keyreplace(slow, 1, Mech#mech.skills,
                                   {slow, Max, Max-1}),
      Mech#mech{skills = NewSkills};
    _ ->
      Mech
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

