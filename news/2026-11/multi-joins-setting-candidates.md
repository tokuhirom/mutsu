# A `multi` that joins a setting routine lists the setting's candidates

A scope can add a `multi` to a setting multi routine, as in
`{ multi infix:<+>(Str $l, Int $r) { 1 } }`. Rakudo then derives a proto
holding the setting's candidates plus the scope's own, so inside that
block `&infix:<+>.candidates` counts 32. mutsu's run-time family is a
dispatcher over the user candidates that falls back to the built-in, and
its `.candidates` listed only the one user candidate. Dispatch itself was
already right.

`.candidates` and `.cando` of such a family now list the setting's
candidates first, then the program's (#17923). The family stays the
program's own when it declares a `proto`, or when the name is a routine
of its own rather than a setting multi.

The setting candidates still cannot be called directly (#18009).
