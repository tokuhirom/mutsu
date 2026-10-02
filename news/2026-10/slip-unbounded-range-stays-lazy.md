# A slipped unbounded Range keeps a list lazy

`|(1..*)` used to flatten the Range eagerly, up to the 1M-element range
expansion cap. So `my @a = 0, |(1..*)` built a finite, non-lazy Array of
1000002 elements. In Rakudo it is lazy, and `.elems` throws `X::Cannot::Lazy`
(#10862).

Now the Slip carries the Range as its `.succ`-stepping lazy list
(`runtime::unbounded_range::lazy_list`), the same way it already carried an
infinite `...` sequence or a lazy `.map` pipe. The list constructor turns that
into a lazy concatenation. `(0, |(1..*))`, `[0, |(1..*)]`, `0, |("a"..*)` and
`0, |(^Inf)` are all lazy now and reify only what is read.
