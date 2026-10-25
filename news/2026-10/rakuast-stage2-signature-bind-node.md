RakuAST Stage 2: a signature literal bind becomes a typed parser node

`SourceForm::SignatureBind` is gone. `:(...) := RHS` is built by the parser as `ParsedStmt::SignatureBind` and
lowered once to the temporary declarations and assignments `signature_bind::expand` builds; a `.AST` parse
keeps the node and `parsed_view` renders it. The `convert.rs` reverse arms, the record statement that opened
the expansion and its `walk_source` arm are deleted. `SourceForm` goes from 7 to 6 variants. Tracked in #14966.
