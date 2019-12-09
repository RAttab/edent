
% comment
%% double comment
main([Input, _ | _ ]) when
Input == []
->
  case
     a_b@d + 'Bob'
  of
      1 + 2 ->
          $a;
      _ ->
           ok
  end,
    %8mid comment

 {ok, Data} = file:read_file(Input),
  process(Data),
  halt(0);
    main(_) ->
   io:fwrite(standard_error, "usage: nedent <input> <output>~n", []),
  halt(1).
