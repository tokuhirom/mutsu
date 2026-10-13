# `parse-base` and `in-range` lose their cascade arms

`Str.parse-base` and `Range.in-range` are now answered only by their method-table rows: the
one-argument dispatch cascade no longer re-routes them. `"ff".parse-base(<16>)` accepts an
`IntStr` radix (it is an `Int`), and `Str.parse-base(16)` on the type object reports the same
bind failure as Rakudo (ADR-11276 §9.103).
