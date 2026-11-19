# RakuAST: the empty name edge is `Name::Part::EmptyEdge`

Rakudo 2026.11 renamed `RakuAST::Name::Part::Empty` to `RakuAST::Name::Part::EmptyEdge`
(rakudo/rakudo#6771), and a trailing `::` (`Foo::`) is now an `EmptyEdge` instance like the
leading one, not the bare type object. `.AST`, `.raku` and the type hierarchy follow it; the
old `Empty` spelling is still accepted as input. `src/rakuast/hierarchy.tsv` is regenerated
from rakudo 2026.11 (it also gains `MetaInfix::CrossZip`, `StatementModifier::IfUnless`,
`Var::PackageHeaderLookup`, `WhereThunk` and loses `Expression::QAST`). The four statements
dropped from the `.AST` text ratchet by #18240 are back, plus 18 more. Fixes #18241.

`vendor/rakudo`'s `RakuAST/Deparse.rakumod` is re-vendored at the 2026.11 tag (it checks
`Name::Part::EmptyEdge`, and asks a `Heredoc` for `IMPL-AWAITS-BODY`, which mutsu now
answers: a parsed heredoc never awaits its body).
