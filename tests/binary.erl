-module(binary).

-export([
        basic_test/1
    ]).

basic_test(X) ->
    <<
        $x,
        X:16,
        $z:8/little-signed-integer,
        (
            X+1
        ):8
    >>.
