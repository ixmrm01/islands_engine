-module(rules).

-export([new/0, check/2]).

new() ->
    #{state => initialized, player1 => islands_not_set, player2 => islands_not_set}.

check(#{state := initialized} = Rules, add_player) ->
    {ok, Rules#{state := players_set}};
check(#{state := players_set} = Rules, {position_islands, Player}) ->
    case maps:get(Player, Rules) of
      islands_set ->
          error;
      islands_not_set ->
          {ok, Rules}
    end;
check(#{state := players_set} = Rules, {set_islands, Player}) ->
    % {ok, Rules#{Player := islands_set}};
    NewRules = Rules#{Player := islands_set},
    case both_players_islands_set(NewRules) of
      true ->
          {ok, NewRules#{state := player1_turn}};
      false ->
          {ok, NewRules}
    end;
check(#{state := player1_turn} = Rules, {guess_coordinate, player1}) ->
    {ok, Rules#{state := player2_turn}};
check(#{state := player1_turn} = Rules, {win_check, WinOrNot}) ->
    case WinOrNot of
      no_win ->
          {ok, Rules};
      win ->
          {ok, Rules#{state := game_over}}
    end;
check(#{state := player2_turn} = Rules, {guess_coordinate, player2}) ->
    {ok, Rules#{state := player1_turn}};
check(#{state := player2_turn} = Rules, {win_check, WinOrNot}) ->
    case WinOrNot of
      no_win ->
          {ok, Rules};
      win ->
          {ok, Rules#{state := game_over}}
    end;
check(_State, _Action) ->
    error.

%% internal functions

both_players_islands_set(Rules) ->
    maps:get(player1, Rules) == islands_set andalso maps:get(player2, Rules) == islands_set.
