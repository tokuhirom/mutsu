# `IO::Path::Parts` iterates its pairs

`IO::Path::Parts` now has its own `iterator` row: the `volume`, `dirname` and
`basename` pairs, in that order. As in Rakudo, `Any`'s methods written on
`self.iterator` (`map`, `grep`, `first`, `head`, `tail`, `sort`, `skip`,
`batch`, `Seq`, ...) read those pairs, while the ones written on `self.list`
(`elems`, `list`, `reverse`, `join`) still see the object as one item. A
`for` loop over `$parts<>` and `my @a = $parts<>` take the pairs too, so the
zen slice no longer has to hand out a List of pairs in place of the object:
`$parts<>` and `$parts[]` are the `IO::Path::Parts` itself (#14034).
