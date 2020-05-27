-module(board).

-export([new/0, position_island/3, guess/2, all_islands_positioned/1]).

new() ->
    #{}.

position_island(Board, Key, Island) ->
    case maps:find(Key, Board) of
      error ->
          positioned_island(overlaps_existing_island(maps:next(maps:iterator(Board)),
                                                     Island),
                            Board,
                            Key,
                            Island);
      {ok, _Island} ->
          {error, overlapping_island}
    end.

guess(Board, Coordinate) ->
    guess_response(check_all_islands(maps:next(maps:iterator(Board)),
                                     Coordinate),
                   Board).

all_islands_positioned(Board) ->
    lists:sort(maps:keys(Board)) =:= lists:sort(island:types()).

%% internal functions

overlaps_existing_island(none, _NewIsland) ->
    false;
overlaps_existing_island({_Key, Island, I}, NewIsland) ->
    case island:overlaps(Island, NewIsland) of
      false ->
          overlaps_existing_island(maps:next(I), NewIsland);
      true ->
          true
    end.

positioned_island(false, Board, Key, Island) ->
    maps:put(Key, Island, Board);
positioned_island(true, _Board, _Key, _Island) ->
    {error, overlapping_island}.

check_all_islands(none, _Coordinate) ->
    miss;
check_all_islands({Key, Island, I}, Coordinate) ->
    case island:guess(Island, Coordinate) of
      {hit, HittedIsland} ->
          {Key, HittedIsland};
      miss ->
          check_all_islands(maps:next(I), Coordinate)
    end.

guess_response({Key, Island}, Board) ->
    NewBoard = Board#{Key := Island},
    {hit, forest_check(Key, Island), win_check(NewBoard), NewBoard};
guess_response(miss, Board) ->
    {miss, none, no_win, Board}.

forest_check(Key, Island) ->
    case island:forested(Island) of
      true ->
          Key;
      false ->
          none
    end.

win_check(Board) ->
    case all_forested(maps:next(maps:iterator(Board))) of
      true ->
          win;
      false ->
          no_win
    end.

all_forested(none) ->
    true;
all_forested({_Key, Island, I}) ->
    case island:forested(Island) of
      true ->
          all_forested(maps:next(I));
      false ->
          false
    end.
