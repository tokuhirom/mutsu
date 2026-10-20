# A block that declares a `sub` no longer copies its enclosing scope on exit

`for ^N { { sub g { 1 }; $t += g() } }` cost O(E) per iteration, E = entries
of the enclosing scope (#9170): with 4000 top-level subs, 271K instructions an
iteration instead of 78K at 500.

A sub declared in a block also stores a copy of itself under a reserved env
key, so a closure that escapes the block can still call it after the block's
routine table is rolled back. That copy held `env().clone()`, a second handle
on the live env chain. On exit the enclosing tier was therefore shared, and the
block's first write-back (`$t`) deep-copied all of it.

The copy now takes the capture any closure created at that point would take:
the sub's free variables and the memoized layers of the enclosing tiers. The
enclosing tier stays unshared, and the write-back is O(1) again.
