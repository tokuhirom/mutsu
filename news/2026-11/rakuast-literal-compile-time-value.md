# `Literal.compile-time-value` and `has-compile-time-value`

A `RakuAST::Literal` (`IntLiteral`, `NumLiteral`, `RatLiteral`, `StrLiteral`, `ComplexLiteral`,
`VersionLiteral`) answers `has-compile-time-value` (`True`) and `compile-time-value` (the value it
holds), as on rakudo 2026.09 (#14916). The other node classes' answers (a role default, plus
computed ones for `QuotedString`, `StatementList`, `Statement::Expression`, `Term::Name`, ...) are
not covered yet.
