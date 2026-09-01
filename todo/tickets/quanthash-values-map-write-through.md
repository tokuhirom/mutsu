# Implement QuantHash metamodel parameterization and writable `.values`

`roast/S02-types/quanthash.t` is the only current unfudged-raku PASS that is
not in `roast-whitelist.txt`. Rakudo v2026.07 passes all 129 tests. On current
main, mutsu stops after test 4: after `Set.^parameterize(Str)`, its next
`Set.^parameterize(Int())` attempt tries to dispatch `.new` on the invalid
type name `Set[Str][Int(Any)]`.

## Reproduction

```raku
my $type := Set;
$type := Set.^parameterize(Str);
$type := Set.^parameterize(Int());
say $type.new(<1 2>).keys.sort; # raku: (1 2)
```

The test repeats that contract for `Set`, `Bag`, `Mix`, `SetHash`, `BagHash`,
and `MixHash`, using nominal `Str` plus coercive `Int()` and `Date()` key
types. Once parameterization works, the last mutable section also requires:

```raku
my %qh is SetHash[Int()];
%qh<42 666> = 1, 1;
%qh.values.map({ $_ = 0 });
say %qh.elems; # raku: 0; mutsu must also produce 0
```

## Root cause and boundary

The implementation already has trait-level handling for spellings such as
`SetHash[Int]`, but it does not implement the metamodel `.^parameterize`
contract used by roast. It must return a usable parameterized type object,
preserve the exact parameter for `.keyof`, and avoid accumulating a previous
parameter into a nested type name when parameterization is applied again.

After that entry point is working, `.values` must preserve an assignable path
to each mutable QuantHash weight when its result is consumed by `map`. This is
not ordinary Hash-element storage: setting a mutable QuantHash weight to zero
removes the key, so the implementation must retain the dedicated QuantHash
weight update/removal semantics rather than treating weights as generic
`ContainerRef` elements.

The relevant existing write-through behavior is the mutable QuantHash
`.pairs`/`.value` path and its regression pin
`t/for-pairs-value-quanthash-writeback.t`. Reuse or extend that operation where
possible; do not add a VM fallback evaluator.

## Acceptance criteria

- `.^parameterize(Str)`, `.^parameterize(Int())`, and
  `.^parameterize(Date())` work for all six immutable and mutable QuantHash
  types; `.keyof` reports the requested type and `.new` applies coercion or
  raises the matching exception.
- Re-parameterizing a base QuantHash type does not manufacture a nested name
  such as `Set[Str][Int(Any)]`.
- `%qh.values.map({ $_ = 0 })` removes all keys for parameterized `SetHash`,
  `BagHash`, and `MixHash`, matching `raku`.
- The same write-through behavior is covered for a plain mutable `Hash`, so
  the producer/callback mechanism is tested independently of QuantHash
  zero-removal semantics.
- Add focused regression coverage under `t/` for metamodel parameterization,
  repeated parameterization, the three mutable QuantHash variants, a nonzero
  write, and zero-removal.
- `MUTSU_FUDGE=1 prove -e target/debug/mutsu roast/S02-types/quanthash.t`
  passes all 129 tests, then add it to `roast-whitelist.txt` in sorted order.
- Existing `.pairs`/`.value` QuantHash write-through tests remain green.

## Validation

Run the focused regression test and the roast file while iterating. Before
publishing an implementation PR, run `cargo fmt --all`,
`cargo clippy -- -D warnings`, `make test`, and `make roast`.
