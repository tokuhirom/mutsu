# RakuAST Stage 2: four `Stmt::SourceForm` marker records become parser nodes

`SourceForm::BarePhaser`, `ImmediateCall`, `DeclarationModifier` and `CommaDeclarations` are
gone. The parser keeps the construct as written (`ParsedStmt::{BarePhaser, ImmediateCall,
AttributeModifier, CommaDecls}`); `rakuast::lower` produces the expansion the compiler runs, and
`.AST` renders the node directly. The `convert.rs` arms that read the marker back, the
`ast::immediate_call::parts` recognizer and `convert_conditional.rs` are deleted.

A side effect: `my $a = 1, my $b = 2, my $c = 3` now renders as one flat `ApplyListInfix` of three
declarations, as rakudo does (before, the third declaration was a nested list). Tracked in #14966.
