# The remaining rewriting passes move onto `VisitMut`

The second slice of ADR-10499 ports the rewriting passes that remained after the first slice
(137 → 130 hand-rolled walkers). Two of them used to rebuild every node by hand and are now a
clone plus an in-place `VisitMut`: the WhateverCode placeholder replacement and the proto `{*}`
rewrite. The ENTER-expression hoist's probe and extraction now share one frame boundary. The WhateverCode replacement also had two copies,
one for numbered placeholders and one for `$_`, which are now a single pass. The supply-body
`emit`/`done` rewrite is now one visitor instead of a statement walker plus an expression walker.

Every position a pass newly reaches was checked against rakudo:

- **Proto `{*}`.** A `{*}` is now a dispatch point in a `say` argument, any operand, a
  `given`/`when` body, an interpolated block and a sub-call argument. For example,
  `proto f($) { say {*} }` printed a Block and now prints the candidate's result.
- **Supply bodies.** The `emit`/`done` rewrite now covers everything in the supply block's own
  frame.
- **WhateverCode classification.** The classifier now also reaches `for`/`whenever` parameter
  defaults, sub-signatures, traits and `handles` expressions.

Each pass stops where code runs in another frame, and each stop is an explicit, commented hook
override. One of these stops remains a difference from rakudo and is filed: a `{*}` in a
method-call callback (#10555).
