# `Pair.value` as an lvalue stops searching the environment (ADR-0036 slice 4)

`$p.value = X` used to work by *looking for* the container the pair came from. Because a mutsu Pair
held a snapshot of its value rather than the element's `Scalar`, the assignment path scanned
`self.env` for a `Hash` whose entry at the pair's key — or an `Array` whose element at the pair's
integer index — happened to compare equal, required the match to be unique, rebuilt the whole
container and wrote it back by identity. Adjacent arms extended the same trick to shaped arrays by
index tuple and to standalone pairs by scanning every binding that held "a Pair with the same key and
the same old value".

That search is deleted. [ADR-0036](../../docs/adr/0036-element-container-pairs-from-subscripts-and-pairs.md)'s
earlier slices had already made every element producer hand out the element's own container, so by
the time a pair reaches the setter it is carrying the thing to write into. What is left is a short
list of container kinds: a live hash-node ref, a shared cell, a mutable QuantHash weight, a
reference value — and otherwise the assignment dies, the way raku's `rw` `Pair.value` accessor dies
when there is no container behind the value.

## Measuring before deleting

Each of the compensator's ten exits was instrumented and every `t/` file and every whitelisted roast
file was run under it. It fired in five `t/` files and two roast files; five of the ten exits — the
whole `__mutsu_hash_ref` branch, both `target_var`-keyed lookups, the hash scan and the hash
writeback — never fired at all, because slice 3's `.pairs` routing had already taken every
hash-backed shape. That left three real gaps, each a piece of the same model rather than a special
case.

**A shaped array's `.pairs` did not hand out element containers.** The producer declined on
`data.shape.is_some()`. A one-dimensional shape stores its leaves flat and only needed to be let in;
a multi-dimensional shape keeps them in nested inner arrays, so `.pairs` over it now walks down to
each leaf and promotes it there, keyed by the index tuple raku uses (`(0 1) => …`).

**A reference-valued Pair now assigns into its value.** Rakudo's `Pair` binds its value
(`$!value := value`), so the pair and the variable it was built from are the same container:

```raku
my @a = 1, 2;
my $p = (a => @a);
$p.value = (3, 4);
say @a;             # [3 4]
```

mutsu used to print `[1 2]` here and give the pair a bare `List`. Three more rows moved to raku's
answer at the same time: `%h` likewise gets the new contents, and `(a => C.new).value = 5` and
`(a => (1,2)).value = 5` now die instead of silently rebinding. An empty `List` value is the one
shape where raku neither writes nor dies, and that is preserved.

**`$p.value<k> = v` cloned the container and rebound it by name.** With the search gone that forked
the pair away from the variable it aliases, so only the first write was visible through both. For a
Pair accessor the element write now happens in place, in the container the pair holds.

## Element type constraints reach the deferred slots

The enforcement half of the slice seeds a promoted element cell with its container's `of`-type, but a
`:=` bind to a slot that does not exist yet never reaches the promotion primitives — it materializes
a fresh cell at the first write, and that cell carried no constraint:

```raku
my Str @a;
my $r := @a[2];
$r = 42;            # silently stored an Int in a Str array
```

All three materialization sites now go through one checked helper that reads the constraint off the
deferred path's terminal and refuses the write before installing anything. The missing-key hash bind
(`my Int %h; my $r := %h<k>; $r = "s"`) is fixed by the same change.

`t/pair-value-assign-binds-container.t` pins the whole surface — 21 assertions, every one of them
checked against real raku first.
