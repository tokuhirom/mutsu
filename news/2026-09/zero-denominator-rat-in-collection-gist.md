# A zero-denominator Rat inside a collection dies when gisted

`say {a => 1/0}`, `say [1/0]` and `[1/0].Str` printed `{a => Inf}` / `[Inf]`,
while `say 1/0` already died. Rakudo defers the `1/0` error until the Rational
is coerced to a string, and a collection's `.gist`/`.Str` coerces every element,
so all of these die with `X::Numeric::DivideByZero`.

The pure renderers are infallible, so a new walk,
`runtime::utils::zero_denominator_rational_error`, looks through the plain
aggregates a renderer expands (Array/List/Seq/Slip, Hash values and object-hash
keys, Pair key and value, Junction eigenstates, item containers) and hands back
the exception. `say`/`put`/`print`/`note`, the native collection `.gist` and the
native collection `.Str` ask it before rendering. `.raku` is unaffected (it
renders `<1/0>`). Test: `t/types/numeric/divide-by-zero-rat-in-collection-gist.t`
(GH #9608).
