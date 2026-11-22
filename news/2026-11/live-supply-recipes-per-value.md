# Live `skip`, `min`, `max`, `minmax`, `rotor`, `repeated`, `encode` and `decode` are per-tap recipes

Over a live (Supplier-backed) source, these combinators used to snapshot the
source's values when the method ran. A live source has none yet, so they
emitted nothing at all:

```raku
my $s = Supplier.new;
$s.Supply.skip(1).tap(&say);
$s.Supply.map(* + 0).rotor(2).tap(&say);
$s.emit($_) for 3, 1, 2;   # now: 1, [3 1], 2 (was: nothing)
```

Each is now a per-tap recipe (ADR-18443). Every tap keeps its own state:
the count still to skip, the running extreme, the pending `rotor` batch, the
partial grapheme of `decode`. A stage with no tap does nothing, and closing
the tap releases the chain below it. `repeated` rides the `unique` stage and
emits the values `unique` drops.

A finite supply folds its values through the same per-tap state, so
`Supply.from-list(...).min` and a live `.min` cannot drift apart (#18579).
