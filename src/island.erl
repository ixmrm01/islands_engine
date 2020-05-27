-module(island).

-include("coordinate.hrl").
-include("island.hrl").

-export([new/2, types/0, guess/2, forested/1, overlaps/2]).

new(Type, UpperLeft) ->
    island(offsets(Type), UpperLeft).

types() ->
    [atoll, dot, l_shape, s_shape, square].

guess(Island, Coordinate) ->
    guessed(lists:member(Coordinate, Island#island.coordinates),
            Island,
            Coordinate).

forested(Island) ->
    lists:sort(Island#island.coordinates) =:= lists:sort(Island#island.hit_coordinates).

overlaps(ExistingIsland, NewIsland) ->
    case sets:is_disjoint(sets:from_list(ExistingIsland#island.coordinates),
                          sets:from_list(NewIsland#island.coordinates)) of
      true ->
          false;
      false ->
          true
    end.

%% internal functions

offsets(atoll) ->
    {ok, [{0, 0}, {0, 1}, {1, 1}, {2, 0}, {2, 1}]};
offsets(dot) ->
    {ok, [{0, 0}]};
offsets(l_shape) ->
    {ok, [{0, 0}, {1, 0}, {2, 0}, {2, 1}]};
offsets(s_shape) ->
    {ok, [{0, 1}, {0, 2}, {1, 0}, {1, 1}]};
offsets(square) ->
    {ok, [{0, 0}, {0, 1}, {1, 0}, {1, 1}]};
offsets(_) ->
    {error, invalid_island_type}.

island({ok, Offsets}, UpperLeft) ->
    add_coordinates(add_coordinate(Offsets, UpperLeft, []));
island({error, Message}, _UpperLeft) ->
    {error, Message}.

add_coordinate([], _UpperLeft, L) ->
    {ok, lists:reverse(L)};
add_coordinate([{OffsetRow, OffsetCol} | T], UpperLeft, L) ->
    case coordinate:new(OffsetRow + UpperLeft#coordinate.row,
                        OffsetCol + UpperLeft#coordinate.col) of
      {ok, C} ->
          add_coordinate(T, UpperLeft, [C | L]);
      {error, Message} ->
          {error, Message}
    end.

add_coordinates({ok, L}) ->
    {ok, #island{coordinates = L}};
add_coordinates({error, Message}) ->
    {error, Message}.

guessed(true, Island, Coordinate) ->
    {hit,
     Island#island{hit_coordinates =
                       lists:reverse([Coordinate | Island#island.hit_coordinates])}};
guessed(false, _Island, _Coordinate) ->
    miss.
