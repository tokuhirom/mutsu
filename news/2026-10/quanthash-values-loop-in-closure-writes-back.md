# A `.values` loop over a captured BagHash writes it from inside a closure

```raku
sub run(&c) { c() }
my $b = <a>.BagHash;
run { $_ = 7 for $b.values };
say $b<a>;   # 7
```

Looping over `$b.values`, `.kv` or `.pairs` with a writable topic stores the
updated BagHash, SetHash or MixHash back into `$b`. A closure that did this to
a captured `$b` now shares the variable through a cell, so the write reaches it
directly rather than through the return of the routine that ran the closure
(ADR-12529 §3, slice 30).
