%%%-------------------------------------------------------------------
%% @doc islands_engine top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(islands_engine_sup).

-behaviour(supervisor).

-export([start_link/0, start_game/1, stop_game/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

% start_link(_Options) ->
    % supervisor:start_link({local, ?SERVER}, ?MODULE, ok).

start_game(Name) ->
    supervisor:start_child(?MODULE, [Name]).

stop_game(Name) ->
    ets:delete(game_state, Name),
    supervisor:terminate_child(?MODULE, game:pid_from_name(Name)).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => simple_one_for_one, % one_for_all,
                 intensity => 1, % 0,
                 period => 1},
    ChildSpecs = [#{id => game,
                    start => {game, start_link, []},
                    restart => transient,
                    shutdown => 5000,
                    type => worker,
                    modules => [game]}],
    {ok, {SupFlags, ChildSpecs}}.

% init(ok) ->

%% internal functions

% pid_from_name(Name) ->
    % syn:whereis(term_to_binary(Name)).
