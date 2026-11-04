# Pod inside a block is a statement of that block's list

Pod written at column 0 inside the block of a class, sub or `if` was left out
of `.AST`. It is now a `RakuAST::Doc::Block` statement of that block's
`StatementList` (#14880), and the list's range is the union of its items, as
rakudo 2026.09 shapes them (checked for a block before, after, between and
alone among the statements). The block goes into the innermost block around it
unless a statement of that block contains it (a heredoc line that looks like
Pod stays text). An indented block, which rakudo gives a `margin`, and Pod in a
list literal stay out of the tree.
