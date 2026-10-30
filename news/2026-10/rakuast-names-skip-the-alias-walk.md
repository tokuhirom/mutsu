# A measured RakuAST name nobody declared skips the alias walk in type-name resolution

`try_resolved_type_capture_name` resolves a constraint spelling before every type check. For a
name that is not a directly registered type it walks `resolve_type_alias_chain`, then
`resolve_relative_compound_type` and `published_head_type`. A measured RakuAST name
(`RakuAST::IntLiteral`, from rakudo's hierarchy table) is not a registered type either, so each of
the 267 `multi method deparse` candidates of `RakuAST::Deparse` paid that walk, ~10k instructions,
and it always answered "not an alias".

It cannot answer anything else: Raku does not let a program bind a qualified name with `constant`
(`my constant RakuAST::Foo = ...` is `X::Syntax::Missing`), and a declaration of the same name is a
directly registered type, which the check keeps (`has_type_direct`). A measured, undeclared
`RakuAST::` name now returns at once.

Measured: `t/rakuast/rakuast-deparse.t` on the debug binary 121 s -> 42 s (the 121 s includes the
`debug_assert` recomputation of #15680; the goal in #15641 is under 60 s).
