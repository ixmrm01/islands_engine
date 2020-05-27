%%%-------------------------------------------------------------------
%% @doc islands_engine public API
%% @end
%%%-------------------------------------------------------------------

-module(islands_engine_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ets:new(game_state, [public, named_table]),
    islands_engine_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
