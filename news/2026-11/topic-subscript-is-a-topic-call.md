# `.<k>`, `.[i]` and `.{k}` are a `Term::TopicCall` in the RakuAST tree

A subscript on the topic (`.<amount>`, `.[0]`, `.{'a'}`) used to show up as an `ApplyPostfix` on
`$_` with a `HashIndex` even for `<...>`. Rakudo answers `Term::TopicCall` over the bare subscript
(`LiteralHashIndex` for `<...>`), so a tree walker that rewrites `ApplyPostfix` nodes (Qwiratry's
aggregate body rewrite) met a `$_` operand it never sees on rakudo and died calling
`compile-time-value` on it. The parser now records the topic spelling on the `Index`, and the
`.AST` view and the lowering both round-trip it.
