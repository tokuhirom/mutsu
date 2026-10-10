# `.Bool` / `.so` / `.not` on an empty gather Seq

`my $e = gather { if 0 { take 1 } }; say $e.so` answered `True` where Rakudo answers `False`: the
boolean-context forms (`so $e`, `?$e`) already pulled one element, but the method-call forms went
through the pure `Value::truthy`, which reports every LazyList true. `CallMethod` and
`CallMethodMut` now route the three method forms on a gather Seq through the same one-element pull
(`eval_truthy`), so only the first element is forced and the rest stays lazy. Pinned by
`t/collections/lazy-seq/gather-seq-bool-methods.t` (#12582).
