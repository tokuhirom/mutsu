# RakuAST: origins of a `package` or `module` block

A `class` declaration already had the ranges of its name and block; a
`package` or `module` did not, because the parser recorded none for it. The
parser now notes them (`Stmt::Package.spans`, the same `PackageSpans` a class
carries), so the `Name`, the `Block`, its `Blockoid` and `StatementList` have
rakudo's ranges (#15780). `unit` declarations and `grammar` bodies are
unchanged.
