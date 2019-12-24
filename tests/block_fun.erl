-module(block_fun).

-export([
        basic_test/0,
        multi_test/1,
        when_test/1,
        param_test/2
    ]).

basic_test() ->
    ok.

multi_test(ok) ->
    ok;
multi_test(_) ->
    ok.

when_test(X)
when
    is_atom(X)
->
    ok.

param_test(
    ok,
    {
        _Y,
        {
            _Z
        }
    }
) ->
    ok;
param_test(
    _X,
    {
        _Y,
        {
            _Z
        }}) ->
    ok.
