-module(block_misc).

-export([
        begin_test/0,
        if_test/1
]).

begin_test() ->
    begin
        ok
    end,
    ok.

if_test(X) ->
    if
        X ->
            ok;
        true ->
            ok
    end,
    ok.
