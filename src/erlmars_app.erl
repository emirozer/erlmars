-module(erlmars_app).

-behaviour(application).
-export([
    start/2,
    stop/1
]).

start(_Type, _StartArgs) ->
    erlmars_worker:start_link(),
    erlmars_sup:start_link().

stop(_State) ->
    ok.
