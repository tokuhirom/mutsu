# A routine with a CATCH returns its tail block's value

A routine body that holds a `CATCH` phaser is compiled as an implicit try
region. The tail of that region kept its value for an expression, a call, an
`if`, an assignment and a `given`, but a tail bare block `{ ... }` was compiled
in statement position, so the routine returned `Nil`. With this change,
`sub f { CATCH { ... }; { 42 } }` returns 42, as on rakudo. The tail block
still fails its arity check when it uses a placeholder, and still runs its own
ENTER/LEAVE phasers.

This made Red's grouped projection (`.classify(...).map(...)`, whose method
holds a `CATCH` before a block that builds the SQL projection) return `Any`.
Red's `t/188-group-aggregate` now passes 28/28, as it does on rakudo.

`RakuAST::Name.canonicalize` now also spells a stash name such as
`EXPORT::DEFAULT::` (the bare `::` is the empty string). Before, a CHECK-time
walk over a unit's calls died on it.
