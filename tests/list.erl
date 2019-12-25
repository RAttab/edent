-module(list).


-export([
        basic_test/0,
        nested_test/0,
        comprehension_test/0
    ]).

basic_test() ->
    [
        a,
        b
    |
        [
            x,
            y
        ]
    ].

nested_test() ->
    [
        a,
        [
            b,
            c
        ],
        d
    |
        [
            x,
            y
        ]
    ].

comprehension_test() ->
    [
        X
    ||
        X
        <-
            [
                true,
                false
            ],
        X
    ].
