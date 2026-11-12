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
