-ifndef(GUESSES_HRL).
-define(GUESSES_HRL, true).

-record(guesses, {hits   = [] :: [] | list(),
                  misses = [] :: [] | list()}).

-endif.
