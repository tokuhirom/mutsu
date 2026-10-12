# `head` and `tail` are method-table rows

The argless `.head` and `.tail` bodies left the name-matching cascade for
the method table (ADR-11276 §9.84): `List.head`, `Map.head`, `Seq.head`,
`List.tail` and `Array.tail` answer on the owners Rakudo declares them on,
and `Any.head`/`Any.tail` answer every other receiver at both arities,
type objects included (`Int.head` is `Int`). `Mu.head` now raises
`X::Method::NotFound`, and `IO::Path.head` and `Date.head` answer the
receiver itself, as in Rakudo.
