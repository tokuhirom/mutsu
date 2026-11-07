# `sort` with several list arguments keeps each as an item

`sort` is declared `+values`, so only a lone positional is flattened. The routine
form flattened every Array/Seq argument, so `sort([10], [9,1], [8,2])` returned
`(1, 2, 8, 9, 10)` instead of ordering the three arrays. Math::Sequences'
`strict-partitions` test (`t/support.rakutest`) now passes.
