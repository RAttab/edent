-module(map).

-export([
        basic_test/0,
        nested_test/0
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

nested_test() ->
    #{
        a
        =>
            #{
                x
                =>
                    y,
                y
                =>
                    z
            },
        b
        =>
            c
    }.
