RakuAST Stage 2: four expression-level `SourceForm` records become typed parser nodes

`SourceForm::SupplyBlock`, `StartBlock`, `HyperAssign` and `InlineSubsetTerm` are gone. The parser keeps the
construct as written (`ParsedExpr::{Supply, StartBody, HyperAssign, InlineSubset}`, carried by `Expr::Ast`)
and lowers it once; `.AST` renders the node directly. The `convert.rs` arms that read the records back,
`react::supply_record`, `inline_subset_term::inline_subset_declaration` and the `prefix_call` start-record
arm are deleted, and the hyper-assignment and inline-subset expansions no longer open with a compiler-inert
record statement. `SourceForm` goes from 14 to 10 variants. Tracked in #14966.
