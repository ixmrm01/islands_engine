-ifndef(COORDINATE_HRL).
-define(COORDINATE_HRL, true).

-record(coordinate, {row = [] :: [] | binary(),
                     col = [] :: [] | binary()}).

-endif.
