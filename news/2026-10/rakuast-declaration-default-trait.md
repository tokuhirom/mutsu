# RakuAST: `is default` declarations and typed declarations cross the boundary

`my $x is default(3)` was refused by `.AST` as a "declaration with traits"
-- `is default` is by far the most common trait on a `my` -- and a typed
declaration (`my Int $n = 4`) rendered but could not be lowered back, so
`EVAL` of any program containing one failed under `MUTSU_RAKUAST=1`.

Measured on rakudo 2026.09, the trait is
`Trait::Is(name => Name.from-identifier("default"), argument =>
Circumfix::Parentheses(SemiList(…)))` in the declaration's `traits`, ahead of
its `initializer`. The parser already keeps a declaration's traits in source
order (`VarDecl.custom_traits`), so the new `rakuast::decl_traits` renders
them in that order and lowers them back; any other trait stays refused. A
`Type::Simple` declaration type now lowers to the declaration's
`type_constraint`.

Lowering showed one more asymmetry: `Nil` renders as `Type::Simple(Nil)` (as in
rakudo) but lowered to a bareword type object rather than the parser's `Nil`
literal, so a round-tripped `$x = Nil` did not restore an `is default` value.
It lowers to the literal now.

The round-trip ratchet grows from 1226 to 1288 of 5792 `t/` files. Pinned by
`t/rakuast/rakuast-decl-default-trait.t`, which passes under both
mutsu and raku.
