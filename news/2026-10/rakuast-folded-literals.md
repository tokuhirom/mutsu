# RakuAST: terms the parser folds to a value cross the boundary

The parser resolves a few terms to the value they denote while it parses: the
type object for `Any`, a `Num` for `1e0`, `Inf` and `NaN`, and the empty `Slip`
for `Empty`. `.AST` saw only the value and refused it, which stopped about 500
`t/` files in the `MUTSU_RAKUAST=1` round-trip mode.

They now render as the nodes rakudo 2026.09 keeps for them, and lower back:
`Any` is `Type::Simple(Name.from-identifier("Any"))`, the same node an
unresolved type name renders as; `1e0` / `Inf` / `NaN` are a new
`RakuAST::NumLiteral` class (constructible, with `.value`), whose leaf renders
as the Num literal (`NumLiteral.new(2500e0)`, `NumLiteral.new(Inf)`); and
`Empty` is `Term::Name(Name.from-identifier("Empty"))`.

Rendering `Inf` showed that the shared `Num.raku` formatter assumed a finite
value and spelled `Inf` as `Infe0`; it now returns `NaN`, `Inf` and `-Inf`
bare, for both of its callers.

The round-trip ratchet grows from 1041 to 1113 of 5721 `t/` files. Pinned by
`t/rakuast/rakuast-folded-literals.t`, which passes under both mutsu and raku.
