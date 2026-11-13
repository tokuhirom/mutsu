# HyperSeq and RaceSeq are no longer Positional

`(1, 2).hyper ~~ Positional` and the `.race` counterpart were True in mutsu
but False in rakudo: like `Seq`, the parallel sequences do
`PositionalBindFailover`, not `Positional`. Because of that, binding one to
an `@x` or `Positional:D` parameter kept the HyperSeq/RaceSeq instead of
caching it into a List. The smartmatch now agrees with rakudo, and the binding
caches as it does for a `Seq`.
