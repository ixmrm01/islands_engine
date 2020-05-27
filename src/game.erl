%%%-------------------------------------------------------------------
%%% @doc islands_engine game gen_server.
%%% @end
%%%-------------------------------------------------------------------

-module(game).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
% -define(TIMEOUT, 15000).
-define(TIMEOUT, 60 * 60 * 1000).

-record(state, {valor}).

%%--------------------------------------------------------------------
%% API Function Exports
%%--------------------------------------------------------------------

-export([start_link/1,
         via_tuple/1,
         pid_from_name/1,
         stop/0,
         % demo_call/1,
         % demo_cast/2,
         add_player/2,
         position_island/5,
         set_islands/2,
         guess_coordinate/4]).

%%--------------------------------------------------------------------
%% gen_server Function Exports
%%--------------------------------------------------------------------

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%--------------------------------------------------------------------
%% API Function Definitions
%%--------------------------------------------------------------------

start_link(Name) when is_list(Name) ->
    % gen_server:start_link({local, ?SERVER}, ?MODULE, Name, []).
    gen_server:start_link(via_tuple(Name), ?MODULE, Name, []).

via_tuple(Name) ->
    {via, syn, term_to_binary(Name)}.

pid_from_name(Name) ->
    syn:whereis(term_to_binary(Name)).

stop() ->
    gen_server:cast(?SERVER, stop).

% demo_call(Game) ->
    % gen_server:call(Game, demo_call).

% demo_cast(Pid, NewValue) ->
    % gen_server:cast(Pid, {demo_cast, NewValue}).

add_player(Game, Name) when is_list(Name) ->
    gen_server:call(Game, {add_player, Name}).

position_island(Game, Player, Key, Row, Col)
    when Player == player1 orelse Player == player2 ->
    gen_server:call(Game, {position_island, Player, Key, Row, Col}).

set_islands(Game, Player) when Player == player1 orelse Player == player2 ->
    gen_server:call(Game, {set_islands, Player}).

guess_coordinate(Game, Player, Row, Col)
    when Player == player1 orelse Player == player2 ->
    gen_server:call(Game, {guess_coordinate, Player, Row, Col}).

%%--------------------------------------------------------------------
%% gen_server Function Definitions
%%--------------------------------------------------------------------

% init(Args) ->
    % {ok, #state{valor = Args}}.

init(Name) ->
    % Player1 = #{name => Name, board => board:new(), guesses => guesses:new()},
    % Player2 = #{name => none, board => board:new(), guesses => guesses:new()},
    % {ok, #state{valor = #{player1 => Player1, player2 => Player2, rules => rules:new()}}, ?TIMEOUT}.

    % case ets:lookup(game_state, Name) of
        % [] -> State = fresh_state(Name);
        % [{_key, State}] -> State
    % end,
    % ets:insert(game_state, {Name, State}),
    % {ok, #state{valor = State}, ?TIMEOUT}.

    self() ! {set_state, Name},
    {ok, #state{valor = fresh_state(Name)}}.

% handle_call(demo_call, _From, State) ->
    % {reply, State, State};
handle_call({add_player, Name}, _From, State) ->
    case rules:check(maps:get(rules, State#state.valor), add_player) of
      {ok, Rules} ->
          reply_success(update_rules(update_player2_name(State, Name),
                                     Rules),
                        ok);
      error ->
          {reply, error, State}
    end;
handle_call({position_island, Player, Key, Row, Col}, _From, State) ->
    case rules:check(maps:get(rules, State#state.valor), {position_islands, Player}) of
      {ok, Rules} ->
          validate_coordinate(State, Rules, Player, Key, Row, Col);
      error ->
          {reply, error, State}
    end;
handle_call({set_islands, Player}, _From, State) ->
    case rules:check(maps:get(rules, State#state.valor), {set_islands, Player}) of
      {ok, Rules} ->
          validate_islands(State, Rules, Player);
      error ->
          {reply, error, State}
    end;
handle_call({guess_coordinate, Player, Row, Col}, _From, State) ->
    case rules:check(maps:get(rules, State#state.valor), {guess_coordinate, Player}) of
      {ok, Rules} ->
          validate_guess(State, Rules, Player, Row, Col);
      error ->
          {reply, error, State}
    end.

% handle_cast({demo_cast, NewValue}, State) ->
    % {noreply, State#state{valor = #{test => NewValue}}};
handle_cast(stop, State) ->
    {stop, normal, State}.

% handle_info(first, State) ->
    % io:format("This message has been handled by handle_info/2, matching on first.~n"),
    % {noreply, State};
handle_info(timeout, State) ->
    {stop, {shutdown, timeout}, State};
handle_info({set_state, Name}, _State) ->
    case ets:lookup(game_state, Name) of
      [] ->
          FromState = fresh_state(Name);
      [{_key, FromState}] ->
          FromState
    end,
    ets:insert(game_state, {Name, FromState}),
    {noreply, #state{valor = FromState}, ?TIMEOUT};
handle_info(brutal_replace_state, State) ->
    TempRules = maps:get(rules, State#state.valor),
    NewState = State#state{valor = (State#state.valor)#{rules := TempRules#{state := player1_turn}}},
    {noreply, NewState}.

terminate({shutdown, timeout}, State) ->
    Player1 = maps:get(player1, State#state.valor),
    ets:delete(game_state, maps:get(name, Player1)),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal Function Definitions
%%--------------------------------------------------------------------

fresh_state(Name) ->
    Player1 = #{name => Name, board => board:new(), guesses => guesses:new()},
    Player2 = #{name => none, board => board:new(), guesses => guesses:new()},
    #{player1 => Player1, player2 => Player2, rules => rules:new()}.

update_player2_name(State, Name) ->
    TempPlayer = maps:get(player2, State#state.valor),
    State#state{valor = (State#state.valor)#{player2 := TempPlayer#{name := Name}}}.

validate_coordinate(State, Rules, Player, Key, Row, Col) ->
    case coordinate:new(Row, Col) of
      {ok, Coordinate} ->
          validate_island(State, Rules, Player, Key, Coordinate);
      {error, Message} ->
          {reply, {error, Message}, State}
    end.

validate_island(State, Rules, Player, Key, Coordinate) ->
    case island:new(Key, Coordinate) of
      {ok, Island} ->
          validate_board(State, Rules, Player, Key, Island);
      {error, Message} ->
          {reply, {error, Message}, State}
    end.

validate_board(State, Rules, Player, Key, Island) ->
    case board:position_island(player_board(State, Player), Key, Island) of
      {error, Message} ->
          {reply, {error, Message}, State};
      Board ->
          reply_success(update_rules(update_board(State, Player, Board),
                                     Rules),
                        ok)
    end.

validate_islands(State, Rules, Player) ->
    Board = player_board(State, Player),
    case board:all_islands_positioned(Board) of
      true ->
          reply_success(update_rules(State, Rules),
                        {ok, Board});
      false ->
          {reply, {error, not_all_islands_positioned}, State}
    end.

validate_guess(State, Rules, Player, Row, Col) ->
    case coordinate:new(Row, Col) of
      {ok, Coordinate} ->
          OpponentPlayer = opponent(Player),
          OpponentBoard = player_board(State, OpponentPlayer),
          {HitOrMiss, ForestedIsland, WinStatus, NewOpponentBoard} = board:guess(OpponentBoard,
                                                                                 Coordinate),
          {ok, NewRules} = rules:check(Rules, {win_check, WinStatus}),
          reply_success(update_rules(update_guesses(update_board(State,
                                                                 OpponentPlayer,
                                                                 NewOpponentBoard),
                                                    Player,
                                                    HitOrMiss,
                                                    Coordinate),
                                     NewRules),
                        {HitOrMiss, ForestedIsland, WinStatus});
      {error, Message} ->
          {reply, {error, Message}, State}
    end.

opponent(player1) ->
    player2;
opponent(player2) ->
    player1.

player_board(State, Player) ->
    maps:get(board, maps:get(Player, State#state.valor)).

update_board(State, Player, Board) ->
    TempPlayer = maps:get(Player, State#state.valor),
    State#state{valor = (State#state.valor)#{Player := TempPlayer#{board := Board}}}.

update_guesses(State, Player, HitOrMiss, Coordinate) ->
    TempPlayer = maps:get(Player, State#state.valor),
    TempGuesses = maps:get(guesses, TempPlayer),
    NewGuesses = guesses:add(HitOrMiss, TempGuesses, Coordinate),
    State#state{valor = (State#state.valor)#{Player := TempPlayer#{guesses := NewGuesses}}}.

update_rules(State, Rules) ->
    State#state{valor = (State#state.valor)#{rules := Rules}}.

reply_success(State, Reply) ->
    Player1 = maps:get(player1, State#state.valor),
    ets:insert(game_state, {maps:get(name, Player1), State}),
    {reply, Reply, State, ?TIMEOUT}.
