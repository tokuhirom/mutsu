# Element assign/delete on a lazy array no longer reifies 100,000 elements

Writing a single element of a lazy `@`-array used to materialize a hard-coded
100,000-element prefix and convert the value to a plain `real_array`, so the
array stopped being lazy afterward:

```
$ mutsu -e 'my @d = 1, 2, 3 ... Inf; @d[2] = 99; say @d.elems'
100000
$ raku  -e 'my @d = 1, 2, 3 ... Inf; @d[2] = 99; say @d.elems'
Cannot .elems a lazy list
```

With a geometric sequence every one of those 100,000 elements is a `2**n`
bignum, so a three-element write allocated ~666 MB and took over a second —
found while timing the whole `t/` suite: `t/lazy-array-assign-preserve.t` was
the single memory outlier at 670 MB peak, against a ~50 MB baseline for every
other file. After this fix the same write is O(1) in the touched index: ~50 KB
and 0.05 s.

## Bounded reify, and the array survives the mutation

`Interpreter::reify_lazy_array_slot` (the chokepoint both `@a[i] = v`,
`vm_var_assign_element.rs`, and `@a[i]:delete`, `vm_var_delete_ops.rs`, share)
now takes an optional `touched_index`: both call sites peek the (not-yet-
popped) subscript off the VM stack, and for the common plain non-negative
`Int` shape only reify `index + 1` elements instead of the historical
100,000-element cap. A subscript this peek can't cheaply resolve (a slice
assign, a `WhateverCode` delete index) still falls back to the old cap,
unchanged.

The reified prefix is installed as a temporary `real_array` so the existing
(LazyList-unaware) element-assign/delete machinery — typed-array holes, shape
metadata, `ContainerRef` rewrap — runs completely unchanged. A new
`restore_lazy_array_slot` then rebuilds a `LazyList` around the mutated prefix
and the ORIGINAL `LazyList`'s live source (same sequence spec / closure-seq
state / gather coroutine), and writes it back into both env and the caller's
local slot — skipping the slot left it holding the stale temporary Array, and
a later per-statement `locals`→`env` reconcile silently clobbered the restored
`LazyList` the next time the array was touched (e.g. a `.is-lazy`/`.gist`
between the mutation and a later read). Only the mutated prefix overwrites the
underlying cache; a longer tail already pulled by an earlier out-of-range read
survives untouched, and so does an earlier override further out. A trailing
`:delete` hole is also protected from the usual shrink-on-trailing-hole
trimming, which is correct for a genuinely finite array but wrong here — the
live tail continues past the hole.

## Keeping self-referential generation uncorrupted

`SequenceSpec` (arithmetic/geometric `...`) and closure-seq (`1, 1, * + *
... *`) extension both compute their next element(s) from their own *prior*
elements. Naively writing a mutation straight into `cache` and letting
extension keep reading from `cache.last()` corrupts every later term:
`@a[2] = 99` on `1,2,4...Inf` would make `@a[3]` compute from `99` instead of
the true `4`, giving `198` instead of raku's `8`. `LazyList` gained a second
field, `generation_state`, mirroring `cache`'s length at every extension step
but holding the sequence's TRUE trailing history, immune to any override.
`extend_sequence_cache` and `extend_closure_sequence` now read/extend
`generation_state` to decide what comes next, then append only the
newly-generated tail to `cache`, leaving any already-overridden position
alone. Every other `LazyList` kind (gather coroutine, map/grep pipe, cat-pull,
WALK-pending, scan) already keeps its own generator state independent of
`cache` and needed no change.

## Verified against raku

```
$ raku -e 'my @a = 1, 2, 4 ... Inf; @a[2] = 99; say @a.is-lazy; say @a[^4]; say @a[10]; say @a.is-lazy'
True
(1 2 99 8)
1024
True
$ raku -e 'my @a = 1, 2, 4 ... Inf; @a[2]:delete; say @a[^4]'
(1 2 (Any) 8)
```

mutsu now matches both exactly. `t/lazy-array-assign-preserve.t` was extended
to pin all of this: `.is-lazy` stays `True` after both `@a[i] = v` and
`@a[i]:delete`, `.elems` still throws `X::Cannot::Lazy`, a later out-of-range
read still reifies further from the live source, and a `:delete`d slot renders
as an `Any` hole without shrinking the array. `docs/lazy-arrays.md` gained an
L2c section documenting the design.
