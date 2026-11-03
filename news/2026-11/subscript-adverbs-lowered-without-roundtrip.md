# Subscript adverbs are lowered where the parser built them

`@a[0]:exists`, `%h<k>:delete`, `@a[1, 2]:kv` and the other colonpairs after a
subscript were kept by the parser as a typed `ParsedExpr::SubscriptAdverbs`
node, but the executing finalization still converted the whole node to RakuAST
and lowered it back (about 1 in 10 of the expressions it converted in a census
of `t/`). The expansion into the shapes the compiler executes is now one
function over the adverbs (`subscript_adverb::apply_adverbs`), shared by the
RakuAST lowering and by the finalization, which lowers the subscript and the
adverb values where they stand and applies it directly (ADR-15760 slice 2).
