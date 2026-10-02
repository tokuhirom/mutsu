# RakuAST: optional and trait-carrying parameters cross the boundary

A signature parameter written `$x?` or with a built-in trait (`$x is copy`,
`is rw`, `is raw`, `is readonly`) was a "non-trivial signature parameter" that
`.AST` refused -- the most frequent refusal of a routine under
`MUTSU_RAKUAST=1` once list declarations converted.

Measured on rakudo 2026.09, `$x?` is the same `Parameter` with
`optional => True`, and a trait is a `traits` list after every other field,
`Trait::Is(name => Name.from-identifier("copy"))`. Both now render and lower back
to the parser's `ParamDef` (`optional_marker`, `traits`); `Parameter.traits`
answers `()` when there are none. A trait with an argument (`is encoded(...)`)
and an optional named parameter or one with a default stay refused.

Lowering one also showed that a single-parameter pointy block always became the
parser's lightweight `Lambda`, which has nowhere to keep `?` or a trait: a
round-tripped `-> $p? { … }` died with "Too few positionals" when called without
an argument. Only a plain parameter takes that form now, as in the parser.

The round-trip ratchet grows from 1135 to 1156 of 5743 `t/` files. Pinned by
`t/rakuast/rakuast-parameter-optional-traits.t`, which passes under
both mutsu and raku.
