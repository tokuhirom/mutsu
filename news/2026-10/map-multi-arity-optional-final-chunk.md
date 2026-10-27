# A short final chunk still runs a multi-arity `map` callback when its required positionals are covered

`seq.map(-> $a, $b? { ... })` over a `Seq` made from an iterator (what Red's `ResultSeq.Seq`
is) died with "Not enough elements for map block arity" on an odd element count, where the
eager list and rakudo call the callback with the one element left (`$b` unset). The lazy
`MultiMap` stage now records how many positionals are required and runs the final short chunk
when it covers them; a callback that still needs more keeps the error. Red `t/41-fallback`
subtest 1 (`.map: -> $a, $b? {...}` through the `$*RED-FALLBACK` path) no longer dies.
