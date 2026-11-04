# Structural expressions are carried through the executing finalization

The census of `finalize_unit`'s remaining conversions listed expression kinds
that fell back only because nobody had listed them as carried: a zen slice,
`$<name>`, a pseudo-stash, a multi-dimensional index, an
expression assignment, a dynamic hyper method call, a symbolic dereference, a
chained comparison, a feed, a loop control and a heredoc interpolation. Their
conversion rebuilds the tree the parser made, which
`finalization_is_the_ordinary_tree` now pins for each form; they are carried
and their operands finalized where they stand (ADR-15760 slice 2). `.AST` and
`EVAL($ast)` are unchanged.

`IndexAssign` stays converted: its `IndexBindValue` marker child only has a
RakuAST rendering inside the assignment.
