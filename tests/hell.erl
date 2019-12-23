-module(hell).

-export([
        nested_catch_expr_test/1
]).


%% TODO: Right now the inner catch matches against the outer try which
%% it shouldn't but this requires contextual information (ie. use
%% erl_parse instead of erl_scan) which we don't want to do. On the
%% bright side, if you write shit like this you deserve everything
%% that you get.
nested_catch_expr_test(X) ->
    try
        ok,
        catch X,
        ok
    catch
        _ ->
            ok
    end,
    ok.
