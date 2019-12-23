-module(hell).
-module("include.hrl").

-export([
        catch_test/1
]).


catch_test(X) ->
    try
        catch X
    catch
        _ ->
            ok
    end,

    ok.
