# A nested `use lib` takes effect at BEGIN time

A `use lib` written inside a routine or block used to extend the repository
chain only when that scope ran, so `sub u { use lib "/x" }` left `$*REPO`
untouched unless `u` was called, and a top-level `use` later in the file could
not resolve through it. In rakudo it is applied at BEGIN time, for the whole
process, whether or not the scope ever runs.

The nested walker of the BEGIN prologue (ADR-0134,
`src/runtime/begin_prologue/nested.rs`) now lifts a nested `use lib` the way it
lifts a nested `BEGIN`: the statement moves from its position into the unit's
prologue, in source order. Its argument may read anything a lifted BEGIN body
can. Running the scope later no longer touches the chain, so its path is not
re-promoted over a later one. Because the path is in effect before any later
lifted effect runs, a `BEGIN` behind a nested `use lib` is no longer blocked
from lifting (#10472's residue). A `use lib` that cannot be lifted keeps the
old in-position behavior. (#10481)
