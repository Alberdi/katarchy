-module(katarchy_siege_hlr).
-behavior(cowboy_handler).

-include("katarchy_mech.hrl").

-export([init/2]).

%%--------------------------------------------------------------------
%% cowboy_handler functions
%%--------------------------------------------------------------------
init(#{method := <<"POST">>} = Req0, State) ->
  <<"POST">> = cowboy_req:method(Req0),
  {ok, Data, Req} = cowboy_req:read_body(Req0),
  JsonMechs = jiffy:decode(Data),
  Mechs = json_to_mechs(JsonMechs),
  {FinalState, Turns} = katarchy_siege:run(Mechs),
  Output = #{<<"finalState">> => [mech_json(Mech) || Mech <- FinalState],
             <<"turns">> => turns_json(Turns)},
  Req2 = cowboy_req:reply(200,
                          #{<<"content-type">> => <<"application/json">>,
                            <<"Access-Control-Allow-Origin">> => <<"*">>},
                          jiffy:encode(Output),
                          Req),
  {ok, Req2, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
mech_json(Mech) ->
  jiffy:encode({zip(record_info(fields, mech), tl(tuple_to_list(Mech)))}).


turns_json(Turns) ->
  [[mech_json(Mech) || Mech <- Mechs] || Mechs <- Turns].


zip([], []) ->
  [];
zip([_|T], [undefined|T2]) ->
  zip(T, T2);
zip([skills|T], [V|T2]) ->
  Skills = lists:map(fun({Atom, X, _}) -> {[{type, Atom}, {value, X}]};
                        ({Atom, X}) -> {[{type, Atom}, {value, X}]};
                        (Atom) -> {[{type, Atom}]} end, V),
  [{skills, Skills} | zip(T, T2)];
zip([position|T], [{X,Y}|T2]) ->
  [{position, {[{x,X}, {y,Y}]}} | zip(T, T2)];
zip([H|T], [H2|T2]) ->
  [{atom_to_binary(H, utf8), H2} | zip(T, T2)].


json_to_mechs(JsonMechs) ->
  [json_to_mech(JsonMech, #mech{}) || {JsonMech} <- JsonMechs]. 

json_to_mech([], Mech) ->
  Mech;
json_to_mech([{<<"position">>,{[{<<"y">>, Y},{<<"x">>, X}]}}|Fields], Mech)
    when is_integer(X) andalso is_integer(Y) ->
  json_to_mech(Fields, Mech#mech{position = {X,Y}});
json_to_mech([{<<"position">>,{[{<<"x">>, X},{<<"y">>, Y}]}}|Fields], Mech)
    when is_integer(X) andalso is_integer(Y) ->
  json_to_mech(Fields, Mech#mech{position = {X,Y}});
json_to_mech([{<<"id">>, V}|Fields], Mech) when is_binary(V) ->
  json_to_mech(Fields, Mech#mech{id = V});
json_to_mech([{<<"attack_power">>, V}|Fields], Mech) when is_integer(V) ->
  json_to_mech(Fields, Mech#mech{attack_power = V});
json_to_mech([{<<"hit_points">>, V}|Fields], Mech) when is_integer(V) ->
  json_to_mech(Fields, Mech#mech{hit_points = V});
json_to_mech([{<<"side">>, <<"right">>}|Fields], Mech) ->
  json_to_mech(Fields, Mech#mech{side = right});
json_to_mech([{<<"speed">>, V}|Fields], Mech) when is_integer(V) ->
  json_to_mech(Fields, Mech#mech{speed = V});
json_to_mech([{<<"skills">>, V}|Fields], Mech) when is_list(V) ->
  RawSkills = [{proplists:get_value(<<"type">>, Skill),
                proplists:get_value(<<"value">>, Skill)} || {Skill} <- V],
  Skills = lists:filtermap(fun({<<"jump">>, _}) -> {true, jump};
                              ({<<"perforating">>, _}) -> {true, perforating};
                              ({<<"ranged">>, _}) -> {true, ranged};
                              ({<<"explosive">>, I}) -> {true, {explosive, I}};
                              ({<<"slow">>, I}) -> {true, {slow, I, I}};
                              (_) -> false end, RawSkills),
  json_to_mech(Fields, Mech#mech{skills = Skills});
json_to_mech([_|Fields], Mech) ->
  json_to_mech(Fields, Mech).

