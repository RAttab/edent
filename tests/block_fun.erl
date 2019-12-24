-module(block_fun).

-export([
        basic_test/0,
        param_test/2,
        when_test/1,
        multi_test/1
    ]).

basic_test() ->
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

when_test(X)
when
    is_atom(X)
->
    ok.

multi_test(ok) ->
    ok;
multi_test(_) ->
    ok.
