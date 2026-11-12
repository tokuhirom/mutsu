# A running child's `.out.get`, and a `my %*ENV` after a process write

Two fixes from Red's migration tests (t/134, t/136, t/154, t/161):

- `.get` on the `:out` (or `:err`) pipe of a `run(..., :in, :out)` child read
  the pipe to its end and returned its first line. A child that prints a line
  and then waits on its stdin for the parent (Red's lock-holder helper:
  `say "locked"; $*IN.get`) deadlocked both sides. `.get` now reads one line at
  a time while the child runs; whatever it read ahead is returned first by a
  later `.slurp-rest`, and `.get` after the end of the pipe is `Nil`.
- Once anything had written the process `%*ENV`, a dynamic `my %*ENV = ...`
  was ignored: reads, callees and child processes all saw the process hash.
  The process-stash lookup collapsed its "this is a lexical binding" answer to
  "no answer". Red provisions its temporary test database with
  `my %*ENV = %environment` and hands it to child commands that way.

A third, from t/161: with user `infix:<ne>` or `infix:<!=>` candidates in scope
(Red exports both), `$x ne any(@r)` threaded the Junction through `ne`, which
is true as soon as `@r` holds a second value. Rakudo's core `ne` is
`not a eq b` over the whole operands, so the comparison means "equal to none
of them". Red's table-removal loop, `@pending.grep({ $_ ne any(@ready) })`,
never shrank and the test hung.

And a fourth (#17500, t/145): a statement that calls the topic (`.(args);`,
`.(Type) for @blocks`) FETCHed the Proxy the call handed back, where rakudo
leaves it alone. Red's legacy `^migrate` runs `.(Type) for @blocks` over
`{ .first = .original * 2 }`, whose `is rw` accessor answers a write-only Proxy
whose FETCH dies. Such a statement now sinks through `SinkPopKeepProxy`, which
still runs a user `sink` method; a call through a variable or an element is
FETCHed as before.
