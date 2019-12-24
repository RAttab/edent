-module(block_misc).

-export([
        begin_test/0,
        begin_nested_test/0,
        if_test/1,
        if_nested_test/1
    ]).

begin_test() ->
    begin
        ok
    end,
    ok.

begin_nested_test() ->
    begin
        begin
            ok
        end
    end,
    ok.

if_test(X) ->
    if
        X
        ->
            ok
            ;
        true
        ->
            ok
    end,
    ok.

if_nested_test(X) ->
    if
        X
        ->
            if
                X
                ->
                    ok
                    ;
                true
                ->
                    ok
            end
            ;
        true
        ->
            ok
    end,
    ok.
