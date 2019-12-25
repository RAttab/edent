# edent

Code indenter for Erlang.


## Design Considerations

- Uniformize indentation across multiple code bases.
- Maintains user decisions related to formatting.
- Dirt simple (someone else is working on a proper formatter).
- Should clean up trailing whitespaces


## Known bugs

- `erl_scan` eats the `\n` character after a `.` character BUT NOT if there's
  any form of whitespace in between the two characters.

  ```erlang
  1> erl_scan:string("a.\nb", 0, [return]).
  {ok,[{atom,0,a},{dot,0},{atom,1,b}],1}

  2> erl_scan:string("a. \nb", 0, [return]).
  {ok,[{atom,0,a},{dot,0},{white_space,0,"\n"},{atom,1,b}],1}
  ```

  Because of this behaviour, edent inserts an `\n` after each `.` so if you have
  trailing whitespaces then it's possible that edent will insert extra
  end-of-lines in your file. This might be fixable upstream, and I'll open an
  issue at some point.

- `erl_scan` converts all escape sequences into their actual character.

  ```erlang
  1> erl_scan:string("$a,$\n,$^C,$\hFFFF", 0, [return]).
  {ok,[{char,0,97},
     {',',0},
     {char,0,10},
     {',',1},
     {char,1,94},
     {var,1,'C'},
     {',',1},
     {char,1,104},
     {var,1,'FFFF'}],
    1}
  ```

  Because of this, edent just sort of has to guess which characters to escape
  and which ones to keep as is. The chosen behaviour is documented in this test:
  [escape.erl](tests/escape.erl). I have a feeling that this is strictly not
  fixable upstream as too much code depends on this behaviour so... \shrug
