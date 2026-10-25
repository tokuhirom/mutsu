RakuAST Stage 2: a grouped declaration becomes a typed parser node

`SourceForm::SignatureDecl` is gone. `my ($a, @b) = RHS` (and `:=`, `state`, `our`, typed and nested groups) is held
as `ParsedStmt::SignatureDecl` while the statement modifiers and loose tails that split or wrap it are parsed, then
lowered once to the declarations-reading-a-staging-temporary expansion. The modifier splitter, the declaration-tail
head check and the bareword term table read the node instead of a record at the front of the expansion; the
`convert.rs` reverse arms, the record statement and `walk_source`'s arm are deleted. `SourceForm` goes from 6 to 5
variants. Tracked in #14966.
