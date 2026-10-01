# The remaining rewriting passes move onto `VisitMut`

The second slice of ADR-10499 ports the rewriting passes that remained after the first slice
(148 → 140 hand-rolled walkers). Three of them used to rebuild every node by hand and are now a
clone plus an in-place `VisitMut`: the WhateverCode placeholder replacement, the proto `{*}`
rewrite and the loop-exit phaser wrapping. The WhateverCode replacement also had two copies,
one for numbered placeholders and one for `$_`, which are now a single pass. The supply-body
`emit`/`done` rewrite is now one visitor instead of a statement walker plus an expression walker.

Every position a pass newly reaches was checked against rakudo:

- **Proto `{*}`.** A `{*}` is now a dispatch point in a `say` argument, any operand, a
  `given`/`when` body, an interpolated block and a sub-call argument. For example,
  `proto f($) { say {*} }` printed a Block and now prints the candidate's result.
- **Loop exits.** A `next`/`last` in a `given`/`when` body, a `try`, a `do` block, or in
  expression form (`$_ == 2 and next`) now runs the loop's NEXT/UNDO/LEAVE phasers.
- **Supply bodies.** The `emit`/`done` rewrite now covers everything in the supply block's own
  frame.
- **WhateverCode classification.** The classifier now also reaches `for`/`whenever` parameter
  defaults, sub-signatures, traits and `handles` expressions.

Each pass stops where code runs in another frame, and each stop is an explicit, commented hook
override. Two of these stops remain differences from rakudo and are filed: a `{*}` in a
method-call callback (#10555), and a `next` raised from a closure the loop body calls (#10566).
