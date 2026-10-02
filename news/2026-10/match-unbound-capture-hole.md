# An unbound interior positional capture iterates as `Mu`; Match key/value views are Seqs

An unmatched optional capture before a later one (`"1b" ~~ / (a)? (\d) /`) is a
*hole* in rakudo's capture list, and the access modes disagree on it: `$m[0]`,
`$0` and `$m.list[0]` read `Nil` and `:exists` is False, while iterating the
list — `.list.raku`, `.values`, `.pairs`, `.kv`, a list assignment, the
`:list(...)` part of `Match.raku` — yields `Mu`. mutsu rendered the slot as
`Nil` everywhere.

The stored capture list keeps `Nil` (what the subscript reads answer), and
the `.list`/`.List` view now holds the `Mu` type object in that slot with the
`ArrayData::initialized` set leaving it out, so `hole_at` reports it as a hole
and a `List` element read turns it back into `Nil`. Iteration, `.values`,
`.pairs`, `.kv` and `.raku` present it as `Mu`.

`Match.keys`, `.values`, `.pairs`, `.kv` and `.chunks` now return a `Seq`, as
in rakudo (`.caps` stays a `List`). (#10690)
