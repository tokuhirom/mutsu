# Substitutions and transliterations are carried through the executing finalization

`s///`, `S///` and `tr///` (`Expr::Subst`, `NonDestructiveSubst`, `Transliterate`)
were converted to RakuAST and lowered back by `finalize_unit` although the round
trip rebuilds the identical tree: `finalization_is_the_ordinary_tree` now pins
that for the quote, adverb, assignment and metaassignment forms. They are
carried as the parser built them and their assignment-form thunks are finalized
where they stand (ADR-15760 slice 2). `.AST` and `EVAL($ast)` are unchanged.
