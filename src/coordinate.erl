-module(coordinate).

-include("coordinate.hrl").

-export([new/2]).

new(Row, Col) when Row > 0, Row < 11, Col > 0, Col < 11 ->
    {ok, #coordinate{row = Row, col = Col}};
new(_Row, _Col) ->
    {error, invalid_coordinate}.
