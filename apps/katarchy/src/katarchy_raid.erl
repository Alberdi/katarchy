-module(katarchy_raid).

-include("katarchy_mech.hrl").

-export([run/1]).

-define(CRITICAL(V), V * 2).
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
                        Pos when is_tuple(Pos) ->
                          case mech_at(Pos, Mechs) of
                            false ->
                              false;
                            Mech ->
                              {true, Mech}
                          end;
                        _ ->
                          false
                      end
                  end, [left, right, up, down]).


can_attack(Mech) ->
  not is_slowed(Mech) andalso
  is_tuple(Mech#mech.position) andalso
  Mech#mech.attack_power > 0.


can_move(Mech) ->
  not is_slowed(Mech) andalso
  is_tuple(Mech#mech.position) andalso
  Mech#mech.speed > 0.


damage(Attacker, Target, Mechs) when is_record(Attacker, mech) ->
  {Damage, NAttacker} =
  case tick(critical, Attacker) of
    {inactive, CMech} -> {CMech#mech.attack_power, CMech};
    {active, CMech} -> {?CRITICAL(CMech#mech.attack_power), CMech}
  end,
  damage(Damage, Target, mech_update(NAttacker, Mechs));
damage(Damage, Mech, Mechs) ->
  NewMech =
  case tick(dodge, Mech) of
    {inactive, DodgeMech} ->
      case lists:keyfind(eshield, 1, DodgeMech#mech.skills) of
        false ->
          NewHitPoints = DodgeMech#mech.hit_points - Damage,
          DodgeMech#mech{hit_points = NewHitPoints};
        {eshield, Max, V} when Damage =< V ->
          Skills = DodgeMech#mech.skills,
          DodgeMech#mech{skills = lists:keyreplace(eshield, 1, Skills,
                                                   {eshield, Max, V-Damage})};
        {eshield, Max, V} ->
          Skills = DodgeMech#mech.skills,
          NSkills = lists:keyreplace(eshield, 1, Skills, {eshield, Max, 0}),
          NewHitPoints = DodgeMech#mech.hit_points - Damage + V,
          DodgeMech#mech{hit_points = NewHitPoints, skills = NSkills}
      end;
    {active, DodgeMech} ->
      DodgeMech
  end,
  explode_if_killed(NewMech, mech_update(NewMech, Mechs)).


do_attack(Mechs) ->
  AttackingMechs = lists:filter(fun can_attack/1, Mechs),
  FasterMechs = lists:sort(fun is_faster/2, AttackingMechs),
  Map = lists:zip(Mechs, lists:seq(1, length(Mechs))),
  FasterIndexes =  [element(2, lists:keyfind(V, 1, Map)) || V <- FasterMechs],
  do_attack(FasterIndexes, Mechs).


do_attack([], Mechs) ->
  Mechs;
do_attack([MechIndex|LeftToAttack], Mechs) ->
  Mech = lists:nth(MechIndex, Mechs),
  Positions = positions_to_attack(Mech),
  IsBallistic = lists:member(ballistic, Mech#mech.skills),
  IsRanged = lists:member(ranged, Mech#mech.skills),
  Targets = [case {IsRanged, IsBallistic} of
               {true, true} ->
                 Ms = lists:reverse(mechs_in_front(P, Mech#mech.side, Mechs)),
                 NotTall = fun(M) -> not lists:member(tall, M#mech.skills) end,
                 case lists:dropwhile(NotTall, Ms) of
                   [] -> Ms;
                   L -> L
                 end;
               {true, false} -> mechs_in_front(P, Mech#mech.side, Mechs);
               {false, _} -> case mech_at(P, Mechs) of
                               false -> [];
                               M -> [M]
                             end
             end || P <- Positions],
  NewMechs =
  case lists:any(fun(X) -> visible_lane_enemy(Mech, X) end, Targets) of
    true ->
      lists:foldl(fun(MechsInFront, Ms) when IsRanged ->
                      do_attack_ranged(Mech, MechsInFront, Ms, IsBallistic);
                     ([Target], Ms) ->
                      do_attack_melee(Mech, Target, Ms) end,
                  Mechs, Targets);
    false ->
      Mechs
  end,
  do_attack(LeftToAttack, NewMechs).


do_attack_melee(Attacker, Target, Mechs) ->
  NewMechs = damage(Attacker, Target, Mechs),
  {Attacker2, NewMechs2} = reveal_hidden(Attacker, NewMechs),
  case lists:member(perforating, Attacker#mech.skills) of
    true ->
      NextPosition = next_position(Target#mech.position, Attacker2#mech.side),
      case mech_at(NextPosition, NewMechs2) of
        false -> NewMechs2;
        NewTarget -> do_attack_melee(Attacker2, NewTarget, NewMechs2)
      end;
    false ->
      NewMechs2
  end.


do_attack_ranged(Mech, MechsInFront, Mechs, IsBallistic) ->
  case lists:member(perforating, Mech#mech.skills) of
    true ->
      lists:foldl(fun(X, MechsAcc) -> damage(Mech, X, MechsAcc) end,
                  Mechs, MechsInFront);
    false ->
      [Target|Others] = MechsInFront,
      case {lists:member(hidden, Target#mech.skills), IsBallistic} of
        {true, true} -> do_attack_ranged(Mech, Others, Mechs, IsBallistic);
        _ -> damage(Mech, hd(MechsInFront), Mechs)
      end
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
      BumpedMech = mech_at(NextPosition, NewMechs2),
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
      DisabledMech = katarchy_mech:skill_delete(explosive, Mech),
      {Damage, NewMech} = case tick(critical, DisabledMech) of
                            {active, CMech} -> {?CRITICAL(Value), CMech};
                            {inactive, CMech} -> {Value, CMech}
                          end,
      NewMechs = mech_update(NewMech, Mechs),
      lists:foldl(fun(X, MechsAcc) -> damage(Damage, X, MechsAcc) end,
                  NewMechs, adjacent_mechs(NewMech#mech.position, NewMechs));
    _ ->
      Mechs
  end.


incapacitate_if_needed(Mech) when Mech#mech.hit_points =< 0 ->
  Mech#mech{position = destroyed};
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
    {slow, _Max, I} when I =/= 1 -> true;
    _ -> false
  end.


jump(Mech, Mechs, Speed, BlockedPos) ->
  case next_position(BlockedPos, Mech#mech.side) of
    raided ->
      {complete, Mechs};
    NextPosition ->
      case lists:keymember(NextPosition, 2, Mechs) of
        true ->
          {incomplete, Mech, Mechs};
        false ->
          move_step(Mech, NextPosition, Speed, Mechs)
      end
  end.


mech_at(Position, Mechs) ->
  lists:keyfind(Position, #mech.position, Mechs).


mech_update(Mech, Mechs) ->
  lists:keyreplace(Mech#mech.position, #mech.position, Mechs, Mech).


mechs_in_front(Position, Side, Mechs) ->
  lists:reverse(mechs_in_front(Position, Side, Mechs, [])).

mechs_in_front(Position, Side, Mechs, LinedMechs) when is_tuple(Position) ->
  NewLinedMechs = case mech_at(Position, Mechs) of
    false -> LinedMechs;
    Mech -> [Mech|LinedMechs]
  end,
  NextPosition = next_position(Position, Side),
  mechs_in_front(NextPosition, Side, Mechs, NewLinedMechs);
mechs_in_front(_, _, _, LinedMechs) ->
  LinedMechs.


move(_Mech, 0, Mechs) ->
  {complete, Mechs};
move(Mech, Speed, Mechs) ->
  TargetPos = next_position(Mech),
  case is_tuple(TargetPos) andalso lists:keyfind(TargetPos, 2, Mechs) of
    false ->
      move_step(Mech, TargetPos, Speed, Mechs);
    TargetMech ->
      case lists:member(jump, Mech#mech.skills) andalso
           not lists:member(tall, TargetMech#mech.skills) of
        true ->
          jump(Mech, Mechs, Speed, TargetPos);
        false ->
          {incomplete, Mech, Mechs}
      end
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
      raided;
    left ->
      {PosX + 1, PosY};
    right when PosX =:= 0 ->
      raided;
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
  [Pos || Pos <- Positions, is_tuple(Pos)].


reveal_hidden(Mech, Mechs) ->
  case lists:member(hidden, Mech#mech.skills) of
    false ->
      {Mech, Mechs};
    true ->
      NewMech = katarchy_mech:skill_delete(hidden, Mech),
      {NewMech, mech_update(NewMech, Mechs)}
  end.


run_turns(InitialMechs, Turns) ->
  Mechs = [tick_turn(Mech) || Mech <- InitialMechs],
  MovedMechs = do_movement(Mechs),
  AttackedMechs = do_attack(MovedMechs),
  FinalMechs = lists:map(fun incapacitate_if_needed/1, AttackedMechs),
  case lists:member(FinalMechs, Turns) orelse FinalMechs =:= InitialMechs of
    true ->
      {FinalMechs, Turns};
    false ->
      run_turns(FinalMechs, [FinalMechs|Turns])
  end.


tick(Skill, Mech) ->
  case lists:keyfind(Skill, 1, Mech#mech.skills) of
    {Skill, Max, I} when I < Max ->
      NewSkills = lists:keyreplace(Skill, 1, Mech#mech.skills,
                                   {Skill, Max, I+1}),
      {inactive, Mech#mech{skills = NewSkills}};
    {Skill, Max, I} when I >= Max ->
      NewSkills = lists:keyreplace(Skill, 1, Mech#mech.skills,
                                   {Skill, Max, 1}),
      {active, Mech#mech{skills = NewSkills}};
    _ ->
      {inactive, Mech}
  end.


tick_turn(Mech) ->
  {_, SlowMech} = tick(slow, Mech),
  case lists:keyfind(eshield, 1, SlowMech#mech.skills) of
    {eshield, Max, V} when V < Max ->
      NewSkills = lists:keyreplace(eshield, 1, SlowMech#mech.skills,
                                   {eshield, Max, V+1}),
      SlowMech#mech{skills = NewSkills};
    _ ->
      SlowMech
  end.


validate_setup(Mechs) ->
  Positions = [M#mech.position || M <- Mechs, is_tuple(M#mech.position)],
  UniquePositions = length(lists:usort(Positions)),
  try
    UniquePositions = length(Positions)
  catch
    error:{badmatch, _} ->
      throw(invalid_setup)
  end.


visible_lane_enemy(_Mech, []) ->
  false;
visible_lane_enemy(Mech, [Target|OtherMechs]) ->
  IsEnemy = Target#mech.side =/= Mech#mech.side,
  IsHidden = lists:member(hidden, Target#mech.skills),
  case {IsEnemy, IsHidden} of
    {false, _} -> false;
    {true, false} -> true;
    {true, true} -> visible_lane_enemy(Mech, OtherMechs)
  end.

