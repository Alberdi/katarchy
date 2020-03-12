%%%-------------------------------------------------------------------
%% @doc katarchy public API
%% @end
%%%-------------------------------------------------------------------

-module(katarchy_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile(
               [{'_', [{"/siege", katarchy_siege_hlr, []}]}]),
  {ok, _} = cowboy:start_clear(my_http_listener, [{port, 8080}],
                               #{env => #{dispatch => Dispatch}}),
  katarchy_sup:start_link().

stop(_State) ->
  ok.

%% internal functions
