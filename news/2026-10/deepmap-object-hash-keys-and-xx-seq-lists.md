# deepmap keeps object-hash keys; `xx` repeats a Seq as a List

Two gaps found by running the `ML::TriesWithFrequencies` suite (locked on #11640):

- `deepmap` / `duckmap` / `nodemap` over an object hash (what `categorize` returns, or
  `my %h{Any}`) built a plain hash from the internal `.WHICH`-encoded keys, so the keys
  surfaced as `Str|a`. The result now carries the source's key type and original keys.
- `LHS xx N` with a re-evaluated left side (`$list.tail(*-1) xx 3`) repeated a `Seq`,
  where Rakudo repeats a `List`; a `@`-parameter `.all ~~ Positional` check on the result
  therefore failed. Unrolled, thunked and runtime-count paths all listify the Seq now.

`t/11-classifications` and `t/12-random-choice` of the distribution now pass by direct run.
