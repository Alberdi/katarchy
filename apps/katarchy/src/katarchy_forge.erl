-module(katarchy_forge).

-include("katarchy_blueprint.hrl").
-include("katarchy_mech.hrl").

-export([options/2]).

%% The only export will be called in the future from outside this module.
-ignore_xref([{?MODULE, options, 2}]).

%%--------------------------------------------------------------------
%% Exported functions
%%--------------------------------------------------------------------
options(Mech, BPs) ->
  options(Mech, BPs, []).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
apply_mod({skills, add, Value}, Mech) ->
  katarchy_mech:skill_add(Value, Mech);
apply_mod({skills, del, Value}, Mech) ->
  katarchy_mech:skill_delete(Value, Mech);
apply_mod({Field, Fun, Value}, Mech) ->
  Index = mech_index(Field),
  NewValue = apply_mod_fun(Fun, Value, element(Index, Mech), Field),
  setelement(Index, Mech, NewValue).


apply_mod_fun(minus, Param, OldValue, hit_points) when Param >= OldValue ->
  throw({not_applicable, {hit_points, gt, Param}});
apply_mod_fun(minus, Param, OldValue, Field) when Param > OldValue ->
  throw({not_applicable, {Field, gte, Param}});
apply_mod_fun(minus, Param, OldValue, _Field) ->
  OldValue - Param;
apply_mod_fun(plus, Param, OldValue, _) ->
  OldValue + Param;
apply_mod_fun(set, Param, _, _) ->
  Param.


invalid_req({Field, Fun, V} = Req, Mech) ->
  Index = mech_index(Field),
  case satisfies_req_fun(element(Index, Mech), Fun, V) of
    true -> false;
    false -> {true, Req}
  end.


mech_index(attack_power) ->
  #mech.attack_power;
mech_index(hit_points) ->
  #mech.hit_points;
mech_index(skills) ->
  #mech.skills;
mech_index(speed) ->
  #mech.speed.


options(_, [], Options) ->
  lists:reverse(Options);
options(Mech, [BP|BPs], Options) ->
  ReqErrors = lists:filtermap(fun(Req) -> invalid_req(Req, Mech) end,
                              BP#blueprint.reqs),
  FoldFun = fun(Mod, {NewMech, Errors}) ->
                try {apply_mod(Mod, NewMech), Errors}
                catch throw:{not_applicable, Reason} ->
                        {NewMech, [Reason|Errors]}
                end end,
  case lists:foldl(FoldFun, {Mech, ReqErrors}, BP#blueprint.mods) of
    {NewMech, []} -> options(Mech, BPs, [{NewMech,BP}|Options]);
    {_, Errors} -> options(Mech, BPs, [{not_applicable, Errors, BP}|Options])
  end.


satisfies_req_fun(Param1, eq, Param2) ->
  Param1 == Param2;
satisfies_req_fun(Param1, gt, Param2) ->
  Param1 > Param2;
satisfies_req_fun(Param1, gte, Param2) ->
  Param1 >= Param2;
satisfies_req_fun(Param1, lt, Param2) ->
  Param1 < Param2;
satisfies_req_fun(Param1, lte, Param2) ->
  Param1 =< Param2;
satisfies_req_fun(Param1, has, Param2) ->
  case lists:keyfind(Param2, 1, Param1) of
    false ->
      lists:member(Param2, Param1);
    _ ->
      true
  end;
satisfies_req_fun(Param1, has_not, Param2) ->
  not satisfies_req_fun(Param1, has, Param2).

