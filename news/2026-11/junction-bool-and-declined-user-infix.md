# Junctions collapse through user `Bool`; declined user `==` reads Bool elements

Two wrong answers that Red's migration planner and query compiler ran into:

- A Junction boolified (`!`, `?`, `so`, `not`, `if`, `.Bool`, `.so`, `.not`)
  with the pure truthiness of each eigenstate, so a user `Bool` method never
  ran. Red explores the branches of a query block from its AST's `Bool`, so
  `.grep({ !($_ eq any(<a b>)) })` compiled to `WHERE 0` and Red's migration
  verifier saw an empty table set. A Junction now collapses through each
  eigenstate's own `Bool`, stopping where rakudo stops asking (`any` at the
  first true, `all` at the first false, `one` at the second true, `none` at the
  first true).
- With a user `infix:<==>` candidate in scope, `1 == %h<k>` hands the element's
  container to the generic infix fallback. A Bool in a Hash element carries a
  `Scalar` rendering marker, which the native numeric comparison table read as
  `0`, so equal Red migration columns compared different. The table now reads
  through that wrapper as it already did through a mixin.

Together they let Red's t/137, t/139 and t/141 migration tests pass (#17513).

A third fix from the same run: a `when`/`default` body with `ENTER`/`LEAVE`
phasers was compiled inline, so its `LEAVE` never ran, neither on a normal exit
nor when the body died. Red finishes its SQLite `BEGIN IMMEDIATE` statement in
such a `LEAVE`; the unfinished statement then made the later `COMMIT` fail with
"SQL statements in progress" (t/132). The body is now a phaser block scope, as
an `if` branch with phasers already was.
