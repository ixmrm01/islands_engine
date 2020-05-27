-module(guesses).

-include("guesses.hrl").

-export([new/0, add/3]).

new() ->
    #guesses{}.

add(hit, Guesses, Coordinate) ->
    Guesses#guesses{hits = lists:reverse([Coordinate | Guesses#guesses.hits])};
add(miss, Guesses, Coordinate) ->
    Guesses#guesses{misses = lists:reverse([Coordinate | Guesses#guesses.misses])}.
