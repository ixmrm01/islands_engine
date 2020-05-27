# islands_engine
Back end application for the islands game from book [Functional Web Development with Elixir, OTP, and Phoenix](https://pragprog.com/book/lhelph/functional-web-development-with-elixir-otp-and-phoenix)

* **[Erlang](https://www.erlang.org) instead of Elixir**
* **[Syn](http://www.ostinelli.net/an-evaluation-of-erlang-global-process-registries-meet-syn/) instead of Registry**

## Build

Download Islands Engine

```
$ cd islands_engine
$ rebar3 shell
```

## Test

```
> {ok, Game} = gen_server:start_link(game, [], []).
> Game ! first.
> {ok, Game} = gen_server:start_link(game, #{test => "test value"}, []).
> gen_server:call(Game, demo_call).
> game:demo_call(Game).
> gen_server:cast(Game, {demo_cast, "another value"}).
> game:demo_cast(Game, "another another value").  
```

## Web interface

[Islands Interface](https://github.com/ixmrm01/islands_interface)

## Learn more

* [Erlang Coding Standards & Guidelines](https://github.com/inaka/erlang_guidelines)
* [Writing Beautiful Code](http://www.gar1t.com/blog/writing-beautiful-code-erlang-factory.html)
* [Adopting Erlang](https://adoptingerlang.org/)
* [Erlang build tool](https://github.com/erlang/rebar3)
* [Erlang Formatter for Rebar3](https://github.com/AdRoll/rebar3_format)
