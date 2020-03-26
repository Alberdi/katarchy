-module(katarchy_mech).

-include("katarchy_mech.hrl").

-export([skill_add/2, skill_delete/2]).

%%--------------------------------------------------------------------
%% Exported functions
%%--------------------------------------------------------------------
skill_add(Skill, Mech) when is_tuple(Skill) ->
  case {Skill, lists:keyfind(element(1, Skill), 1, Mech#mech.skills)} of
    {{Atom, V1, V2}, {Atom, OldV1, OldV2}} ->
      NewSkill = {Atom, OldV1 + V1, OldV2 + V2},
      NewMech = skill_delete(Atom, Mech),
      NewMech#mech{skills = [NewSkill|NewMech#mech.skills]};
    {{Atom, V1}, {Atom, OldV1}} ->
      NewSkill = {Atom, OldV1 + V1},
      NewMech = skill_delete(Atom, Mech),
      NewMech#mech{skills = [NewSkill|NewMech#mech.skills]};
    {_, false} ->
      Mech#mech{skills = [Skill|Mech#mech.skills]}
  end;
skill_add(Skill, Mech) ->
  Mech#mech{skills = [Skill|Mech#mech.skills]}.


skill_delete(Atom, Mech) ->
  NewSkills = case lists:keyfind(Atom, 1, Mech#mech.skills) of
    false ->
      lists:delete(Atom, Mech#mech.skills);
    Tuple ->
      lists:delete(Tuple, Mech#mech.skills)
  end,
  Mech#mech{skills = NewSkills}.

