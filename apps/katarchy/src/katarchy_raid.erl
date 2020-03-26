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


do_attack(Mechs) ->
  AttackingMechs = lists:filter(fun can_attack/1, Mechs),
  FasterMechs = lists:sort(fun is_faster/2, AttackingMechs),
  do_attack(FasterMechs, Mechs).

do_attack([], Mechs) ->
  Mechs;
do_attack([Mech|LeftToAttack], Mechs) ->
  Positions = positions_to_attack(Mech),
  IsRanged = lists:member(ranged, Mech#mech.skills),
  TargetMechs = [begin case IsRanged of 
                         true -> mechs_in_front(P, Mech#mech.side, Mechs);
                         false -> case lists:keyfind(P, 2, Mechs) of
                                    false -> [];
                                    M -> [M]
                                  end
                       end end || P <- Positions],
  NewMechs =
  case lists:any(fun(X) -> visible_lane_enemy(Mech, X) end, TargetMechs) of
    true ->
      lists:foldl(fun(MechsInFront, Ms) when IsRanged ->
                      do_attack_ranged(Mech, MechsInFront, Ms);
                     ([Target], Ms) ->
                      do_attack_melee(Mech, Target, Ms) end,
                  Mechs, TargetMechs);
    false ->
      Mechs
  end,
  do_attack(LeftToAttack, NewMechs).


do_attack_melee(Attacker, Target, Mechs) ->
  NewMechs = damage(Attacker#mech.attack_power, Target, Mechs),
  {Attacker2, NewMechs2} = reveal_hidden(Attacker, NewMechs),
  case lists:member(perforating, Attacker#mech.skills) of
    true ->
      NextPosition = next_position(Target#mech.position, Attacker2#mech.side),
      case lists:keyfind(NextPosition, 2, NewMechs2) of
        false ->
          NewMechs2;
        NewTarget ->
          do_attack_melee(Attacker2, NewTarget, NewMechs2)
      end;
    false ->
      NewMechs2
  end.


do_attack_ranged(Mech, MechsInFront, Mechs) ->
  case lists:member(perforating, Mech#mech.skills) of
    true ->
      lists:foldl(fun(X, MechsAcc) ->
                      damage(Mech#mech.attack_power, X, MechsAcc) end,
                  Mechs, MechsInFront);
    false ->
      damage(Mech#mech.attack_power, hd(MechsInFront), Mechs)
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


incapacitate_if_needed(Mech) when Mech#mech.hit_points =< 0 ->
  Mech#mech{position = undefined};
incapacitate_if_needed(Mech) ->
  Mech.


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
    {slow, _Max, I} when I =/= 0 -> true;
    _ -> false
  end.


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


mechs_in_front(Position, Side, Mechs) ->
  lists:reverse(mechs_in_front(Position, Side, Mechs, [])).

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


positions_to_attack(Mech) ->
  NextPosition = next_position(Mech),
  Positions = case lists:member(triattack, Mech#mech.skills) of
                true ->
                  [next_position(NextPosition, up), NextPosition,
                   next_position(NextPosition, down)];
                false ->
                  [NextPosition]
              end,
  [Pos || Pos <- Positions, Pos =/= undefined].


reveal_hidden(Mech, Mechs) ->
  case lists:member(hidden, Mech#mech.skills) of
    false ->
      {Mech, Mechs};
    true ->
      NewMech = katarchy_mech:skill_delete(hidden, Mech),
      {NewMech, lists:keyreplace(Mech#mech.position, 2, Mechs, NewMech)}
  end.


run_turns(InitialMechs, Turns) ->
  Mechs = [slow_tick(Mech) || Mech <- InitialMechs],
  MovedMechs = do_movement(Mechs),
  AttackedMechs = do_attack(MovedMechs),
  FinalMechs = lists:map(fun incapacitate_if_needed/1, AttackedMechs),
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


visible_lane_enemy(_Mech, []) ->
  false;
visible_lane_enemy(Mech, ListMechs) ->
  [Target|OtherMechs] = ListMechs,
  IsEnemy = Target#mech.side =/= Mech#mech.side,
  IsHidden = lists:member(hidden, Target#mech.skills),
  case {IsEnemy, IsHidden} of
    {false, _} -> false;
    {true, false} -> true;
    {true, true} -> visible_lane_enemy(Mech, OtherMechs)
  end.

