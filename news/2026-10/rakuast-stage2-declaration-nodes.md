RakuAST Stage 2: grouped `has`, attribute colon lists and `.=` declarations become typed parser nodes

`SourceForm::AttributeGroup`, `AttributeColonArglist` and `MethodAssignDecl` are gone. The parser builds
`ParsedStmt::{AttributeGroup, AttributeColonArglist, MethodAssignDecl}` and lowers each once
(`conditional_lower`), keeping the node for a `.AST` parse. The `convert.rs` arms that read the records back,
`signature_decl::attribute_{group,colon}_expansion` and the record statement that opened the `.=` expansion are
deleted; the RakuAST lowering of a `.=` declaration returns the written node, and the expression-position form
derives its single declaration from it. `SourceForm` goes from 10 to 7 variants. Tracked in #14966.
