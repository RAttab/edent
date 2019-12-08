
main([Input, _ | _ ]) when
Input == []
->
    case
        a + b
    of
        1 + 2 ->
            blah;
        _ ->
            ok
    end,


    {ok, Data} = file:read_file(Input),
    process(Data),
    halt(0);
main(_) ->
    io:fwrite(standard_error, "usage: nedent <input> <output>~n", []),
    halt(1).
