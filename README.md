# edent

Code indenter for Erlang.


## Design Considerations

- Uniformize indentation across multiple code bases.
- Maintains user decisions related to formatting.
- Dirt simple (someone else is working on a proper formatter).
- Should clean up trailing whitespaces
- Should be self-contained. Dependency on OTP is ok.

An important side effect of keeping things simple is that edent is not
customizable. **It has one indentation style and that's it**.

If you see problems, then please create a PR containing a test with what the
indentation should look like. Note that if the change is subjective in nature
then get your hopes up. Note that you can always fork and modify to suite your
personal predilections.


## Usage

The only dependency required to run this is OTP; any relatively recent version
should be fine.

```sh
$ ./edent
usage: edent <input>...
       edent -o <output> <input>
```

The first form will overwrite each input file with their indented versions. This
will be commonly used like this:

```sh
$ find . -name '*.erl' | xargs ./edent
```

The second form is mostly useful for testing and debugging where you want to not
overwrite the orignal file. It's currently used for the test suite of edent.


## Development

edent is a pretty simple escript so nothing too fancy is required to get up and
running.

To run tests:

```sh
$ ./tests.sh
$ ./tests.sh <test>
```

The general idea of the tests, is that the files in the `tests` folder describe
the expected behaviour of edent. The test harness will run edent on a file and
diff the result against the original. A difference indicates a bug.

Otherwise, any changes should ideally be covered by a test because it's already
pretty hard to keep track of how everything should line up so tests are the path
to sanity.


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

- There's a few known pieces of syntax that `edent` can't handle. These are
  documented in the [tests/hell.erl](hell.erl) test cases. Hopefully these
  should be relatively rare in the wild.
