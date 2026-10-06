# The collections' methods move into built-in method rows

ADR-11276 slice 3C. About 175 of the 354 methods Rakudo declares on the collection owners (`Any`,
`List`, `Array`, `Hash`, `Map`, `Range`, `Pair`, `Capture`, `Set`, `Bag`, `Mix` and their mutable
forms) are now rows in the built-in method table, registered on the type Rakudo declares them
on, with the cascade arms that implemented them deleted or reduced to a call to the row's
handler.

What moved: `Range`'s own methods and element reads (`bounds`, `is-int`, `infinite`, `int-bounds`,
`in-range`, `rand`, `elems`, `min`, `max`, `minmax`, `list`, `Numeric`, `sum`, `reverse`); the quant
hashes' views and sizes (`keys`, `values`, `kv`, `pairs`, `antipairs`, `total`, `elems`,
`default`, `of`, `hash`, `list`, `kxxv`, `invert`, `Baggy.Numeric`); `AT-KEY`, `EXISTS-KEY` and
`ACCEPTS` of the associatives; `Capture`'s views, positional subscript and the `.Capture` of every
collection; `Pair`'s views; `hyper`, `race`, `lazy` and `item`; `Range`'s `AT-POS` and
`List`'s and `Range`'s `EXISTS-POS`; and the small coercions (`Slip`, `List`, `list`, `hash`, `default`).

Moving the arm bodies fixed a few answers on the way. `Capture.AT-KEY`, `EXISTS-KEY`, `AT-POS` and
`EXISTS-POS` work (they died or answered "does not support associative indexing"),
`Pair.antipairs` is the one swapped pair, and `Range.is-int` agrees with `int-bounds` and
`minmax`: `1..*`, `1..Inf` and `*..5` are not Int ranges, a big-Int end and a Bool end are
(`(True..5).minmax` is `(1 5)`).

The families that share one implementation across groups (`gist`, `raku`, `Str`, `WHICH`, `fmt`,
`clone`), the sampling methods, the `Seq` shape, the mutating methods and the interpreter rows
stay for the slices and changes that own them; ADR-11276 §9.16 says why, and
`scripts/method-rows-report.py --inventory` lists what is left.
