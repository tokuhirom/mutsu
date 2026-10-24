# RakuAST: `map`, `grep`, `first` and `find-nodes` walk the tree

Every `RakuAST::Node` now has the tree helpers rakudo 2026.09 gives it.

- `map(&mapper, :depth-first)`, `grep($test, :depth-first, :k, :kv, :p)` and
  `first($test, :end, :depth-first)` walk the node's descendants through
  `visit-children`: pre-order by default, post-order with `:depth-first`
  (rakudo's `src/core.c/RakuAST/Fixups.rakumod`).
- `find-nodes($type, :condition, :stopper)` walks breadth first. A type
  `:stopper` keeps a match but does not look below it, and a code `:stopper`
  skips the subtree (`src/Raku/ast/base.rakumod`).

mutsu used to answer `map`/`grep`/`first` with the generic list methods over
the node itself, so `$ast.grep(RakuAST::IntLiteral)` was empty. It had no
`find-nodes` at all. Found by the RakuAST class/method survey (#7564);
part of #14916.
