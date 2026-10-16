# A closure's `:=` of a captured Set or Bag reaches its creator

```raku
sub run(&c) { c() }
my %qh := Set.new(1, 2);
my &peek = { %qh.elems };
run { %qh := Set.new(3, 4, 5) };
say peek();   # 3
```

A `%` or `@` variable bound to an immutable value (a Set, a Bag, a Map, a
List) and rebound inside a closure now shares the new binding with the block
that declared it and every closure that captured it, as an Array or Hash
variable already did (ADR-12529 §3, slice 29).
