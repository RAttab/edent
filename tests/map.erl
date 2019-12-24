-module(map).

-export([
        basic_test/0
    ]).

basic_test() ->
    #{
        a
        =>
            b,
        b
        =>
            c
    }.
