# `.DEPARSE` of `$::(EXPR)::name`

A symbolic lookup with a static tail, `$::($unit)::x110`, kept only the string
the parser builds for it (`$::($unit ~ "::x110")`), so `.AST.DEPARSE` printed
that concatenation. The parser now marks how many trailing segments were
written as static names (`Spelling::IndirectTail`), and the view renders
`Var::Package(Name(EmptyEdge, Part::Expression($unit), Part::Simple("x110")))`
as rakudo 2026.11 does; `EVAL` of such a tree lowers the static tail back into
the lookup string (#18292).

`.DEPARSE("XX")` with a localization — the path the L10N::XX distributions use
through `RakuAST::Deparse.slang` — is pinned by a test with a fixture in their
generated shape (`t/lib/RakuAST/Deparse/L10N/XT.rakumod`); the 18 real
distributions were checked against rakudo 2026.11 by hand for #14845.
