# `.&{ ... }` keeps its `&` in `.AST` and `.DEPARSE`

`@a.&{ .elems }` (a block called as a method on the invocant) was a `Call::TermAsMethod` whose
callee was the bare block, so the deparsed text lost the `&` (`@a.{ .elems }`, which is a different
call). Rakudo itemizes the block: the callee is `Contextualizer::Item(Block)`. The view now builds
that, and lowering takes the block back out of the item, so `EVAL($ast)` is unchanged. The hash
angle subscript under a hyper operator (`%h>>.<k>`, printed as an `AT-KEY` call) is still a
separate gap.
