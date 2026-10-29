# `Set`/`Bag`/`Mix` `.min`/`.max`/`.minmax` order their pairs

`(bag <a b b c c c>).max` returned the whole Bag. A `Set`/`Bag`/`Mix` (and the
mutable twins) is a collection of `key => weight` pairs in rakudo, so its
extremum is the largest / smallest `Pair` (`:c(3)`, `:a(1)`), compared as Pairs
(key first). `min`/`max`/`minmax` now order `.pairs` for a QuantHash. This takes
Native::FindVersion's `t/01-basic.t`, `t/02-constant.t` and `t/05-with-arg.t`
(`Version.new: $set.max.key`) from 0 to 2 of 2 each.
