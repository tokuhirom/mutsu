# RakuAST: `@a[|| @i]` keeps its dimension spread

The 6.e multislice subscript `@a[|| @i]` (the list `@i` spread over the
subscript's dimensions) lost its `||` in the RakuAST round trip and indexed
with `@i` as one plain slice. It now converts to rakudo 2026.09's
`ApplyPrefix(Prefix::Multislice, @i)` in the subscript's one-statement
`SemiList` (a new `RakuAST::Prefix::Multislice` node class), an assignment
folds into the subscript's `assignee` as rakudo does, and the lowering spreads
the list again — for arrays, hashes and subscript adverbs such as `:delete`.
Two more `t/` files pass under `MUTSU_RAKUAST=1`.
