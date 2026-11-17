# `OUTER::` from a unit's mainline sees none of its lexicals

A unit's mainline is its outermost lexical scope, so `OUTER::` from it is
outside the unit: `my $x = 1; say OUTER::<$x>` and `say $OUTER::x` are `Nil` on
rakudo. mutsu let the read cross the frame boundary to the runtime walk, which
found the mainline's own `$x`, so `say OUTER::<$x> given 1` printed 1. The
compiler now answers such a read with Nil when the code being compiled is a
unit's own mainline (not a routine, a closure or an `EVAL`, whose outer scopes
live outside the compilation) and the depth reaches past its outermost scope.
`OUTER::` from a block or a routine still reads the mainline. Fixes #17898.
