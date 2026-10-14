# `combinations` and `batch` are `Any`'s rows for every receiver

`(1..3).combinations(2)`, `$seq.batch(2)`, `"abc".combinations(2)` and `(1..10).batch(:elems(4))` used to reach a
name-matching arm of the one-argument cascade, because the method table only knew `List.combinations` and the
positional `Any.batch`, and a `Range` or a `Seq` did not reach `Any`'s rows for them. `Any.combinations` and the named
`batch(:elems)` are now rows that every receiver reaches, so the arms are gone. On the way, `(1..*).combinations(2)`
throws `X::Cannot::Lazy` instead of hanging, and `.batch()` with no size is `X::Multi::NoMatch` as in Rakudo
(ADR-11276 §9.100).
