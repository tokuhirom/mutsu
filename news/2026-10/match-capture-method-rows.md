# A `Match` answers its `Capture` methods from `Capture`'s rows

A `Match` is a `Capture` in Rakudo, and now in mutsu's method table too (ADR-11276 §9.95). `.keys`, `.values`, `.kv`,
`.pairs`, `.antipairs`, `.hash` and `.elems` of a match come from `Capture`'s rows, which read its captures. The `Match` block
of `methods_0arg` that answered them by name is down to the methods `Match` keeps for itself.

Two answers moved to Rakudo's. `.antipairs` of a match used to be answered on the matched string (`"ab".antipairs`); it now
pairs each capture with its index or name. And `.values`/`.kv` of a plain `Capture` flatten an array argument, as Rakudo's
do: `\([1, 2], 3).values` is `(1, 2, 3)`, while an itemized `$[1, 2]` stays whole.
