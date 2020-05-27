-ifndef(ISLAND_HRL).
-define(ISLAND_HRL, true).

-record(island, {coordinates     = [] :: [] | list(),
                 hit_coordinates = [] :: [] | list()}).

-endif.
