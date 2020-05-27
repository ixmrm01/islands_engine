-module(demo_proc).

-export([loop/0]).

loop() ->
    receive
      Message ->
          io:format("I got a message: ~p~n", [Message])
    end,
    loop().

