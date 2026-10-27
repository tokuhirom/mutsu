# QuantHash and Hash coercions fold a finite `.map` pipe over `Seq.new($iterator)`

`Seq.new(MyIterator.new).map({ .[0] => .[1] }).Bag` (Red's
`ResultAssociative.Bag` over a result-set iterator) took the whole lazy pipe for
one element and answered `(=>1).Bag`; `.Mix` refused it as lazy, and `.Set`,
`.hash`, `.Map`, `.SetHash` and `.BagHash` returned empty or odd-element results.
The lazy step now reifies the receiver for `Set`/`Bag`/`Mix` (and their `*Hash`
forms), `hash`/`Hash`/`Map` and `Slip`, so a pipe that ends is folded and an
unbounded one still throws `X::Cannot::Lazy`. Red's `t/26-classify` and
`t/39-xmas` Bag assertions use this.
