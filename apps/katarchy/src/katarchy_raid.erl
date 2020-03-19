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
        TargetMech when TargetMech#mech.side =/= Mech#mech.side ->
          NewMechs = do_attack(Mech, TargetMech, Mechs),
          do_attack(LeftToAttack, NewMechs);
        _ ->
          do_attack(LeftToAttack, Mechs)
      end
  end.


do_attack(Attacker, Target, Mechs) ->
  NewMechs = damage(Attacker#mech.attack_power, Target, Mechs),
  case lists:member(perforating, Attacker#mech.skills) of
    true ->
      NextPosition = next_position(Target#mech.position, Attacker#mech.side),
      case lists:keyfind(NextPosition, 2, NewMechs) of
        false ->
          NewMechs;
        NewTarget ->
          do_attack(Attacker, NewTarget, NewMechs)
      end;
    false ->
      NewMechs
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
  NextPosition = next_position(Mech),
  NewMechs = range_attack(Mech, NextPosition, Mechs, false),
  do_attack_ranged(LeftToAttack, NewMechs).


range_attack(_Mech, undefined, Mechs, _) ->
  Mechs;
range_attack(Mech, TargetPos, Mechs, DidPerforate) ->
  case lists:keyfind(TargetPos, 2, Mechs) of
    false ->
      NextPosition = next_position(TargetPos, Mech#mech.side),
      range_attack(Mech, NextPosition, Mechs, DidPerforate);
    Target when Target#mech.side =/= Mech#mech.side orelse DidPerforate ->
      NewMechs = damage(Mech#mech.attack_power, Target, Mechs),
      case lists:member(perforating, Mech#mech.skills) of
        true ->
          NextPosition = next_position(TargetPos, Mech#mech.side),
          range_attack(Mech, NextPosition, NewMechs, true);
        false ->
          NewMechs
      end;
    _ ->
      Mechs
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

