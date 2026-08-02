# A combinator over a live Supply stays connected to its source

`Supply.merge`, `Supply.reduce` and `Supply.classify`/`categorize` snapshotted
their source's `values` array at combinator time. For a *live*
(Supplier-backed) source that array is empty — nothing has been emitted yet,
because the combinator's Supply has not even been tapped — so the result was a
finished Supply of nothing: it fired `done` immediately and dropped every
subsequent emission.

```raku
my $s1 = Supplier.new;
my $s2 = Supplier.new;
my @res;
$s1.Supply.merge($s2.Supply).tap({ @res.push($_) });
$s1.emit(1); $s2.emit('a'); $s1.emit(2);
say @res;        # raku: [1 a 2]        mutsu: []
```

Each combinator now keeps the connection:

- **`merge`** registers a forward tap on every live source into one output
  supplier, so emissions interleave as they arrive. `done` needed real
  bookkeeping — a merged Supply is done only once *every* source is done, not
  when the first finishes — which is what the new
  `native_methods/state_supplier_merge.rs` tracks, in the same shape as the zip
  state beside it.
- **`reduce`** folds as the source emits and delivers its single result at
  `done`. It shares `produce`'s accumulator (a `reduce_downstream` on
  `ProduceState`) but emits nothing until the source finishes. The old code
  stored a `reduce_source` attribute that nothing ever read.
- **`classify`/`categorize`** back each group with a *preserving* supplier.
  The group's `Pair` reaches the outer tap before any value lands in the group,
  so the usual consumer — one that collects the pairs first and taps the groups
  afterwards — was listening too late and saw only `done`. Values delivered to
  an already-listening tap are marked consumed, so a group is never replayed
  twice.

Two smaller faults surfaced alongside:

- **`Supply.live` was not a method.** It answered only as a loose attribute
  accessor, so it worked exactly for the supplies whose constructor happened to
  store a `live` attribute and died with `No such method 'live'` on the ones a
  combinator built. It is a real method in the spec (`method live(Supply:D:
  --> Bool:D)`), and is now one here: a Supplier-backed supply is live,
  everything else is on demand.
- **`Supply.rotor` emitted `List`s where rakudo emits `Array`s.** rakudo's
  Supply combinator collects each group into an `@batched` array before
  emitting, so `.rotor(3 => -2)` gives `[1, 2, 3]`, not `(1, 2, 3)`.

Pinned by `t/supply-live-combinators.t`.
