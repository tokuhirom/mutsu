# The quant hashes' `Str` and `clone` are method-table rows

`Str` on `Set`, `SetHash`, `Bag`, `BagHash`, `Mix` and `MixHash`, and `clone` on `SetHash`,
`BagHash` and `MixHash`, are now method-table rows that share one implementation with the
dispatch cascade (ADR-11276 §9.73). That empties the `quant hashes` group of the migration
inventory.

A `Bag` or `Mix` now renders each element by its `gist`, as Rakudo's `Baggy.Str` does, while a
`Set` keeps using the element's `Str`: `bag(a => 1).Str` is `a => 1` (it was `a	1`) and
`bag($[1, 2]).Str` is `[1 2]`.
