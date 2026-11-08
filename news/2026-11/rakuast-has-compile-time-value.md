# `has-compile-time-value` on every RakuAST node

Any `RakuAST::Node` answers `has-compile-time-value` as rakudo 2026.09 does (#14916): `True` for the
66 classes that do the `CompileTimeValue` role, computed for `QuotedString`/`Heredoc` (all segments
plain strings), `StatementList`/`SemiList`/`StatementSequence`, `Statement::Expression`,
`Circumfix::Parentheses` and `ColonPair::Value` (a list that is not exactly one statement, or a
statement with a modifier, answers the `Int` 0, as rakudo does), and `False` for every other node.
