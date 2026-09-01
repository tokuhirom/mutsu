# Make mutable QuantHash `.values.map` assignments write through

`roast/S02-types/quanthash.t` is the only current unfudged-raku PASS that is
not in `roast-whitelist.txt`. Rakudo v2026.07 passes all 129 tests. mutsu has
the parameterized QuantHash implementation, but the three mutable variants
still fail the same final assertion: assigning zero through `.values.map` does
not remove their keys.

## Reproduction

```raku
my %qh is SetHash[Int()];
%qh<42 666> = 1, 1;
%qh.values.map({ $_ = 0 });
say %qh.elems; # raku: 0; mutsu must also produce 0
```

The equivalent section of `roast/S02-types/quanthash.t` runs once for each of
`SetHash`, `BagHash`, and `MixHash`; its three `did all keys get removed`
assertions are tests 97, 112, and 127.

## Root cause and boundary

`.values` must preserve an assignable path to each mutable QuantHash weight
when its result is consumed by `map`. The current path hands the callback a
value that assignment does not write back to the originating QuantHash. This
is not ordinary Hash-element storage: setting a mutable QuantHash weight to
zero removes the key, so the implementation must retain the dedicated
QuantHash weight update/removal semantics rather than treating weights as
generic `ContainerRef` elements.

The relevant existing write-through behavior is the mutable QuantHash
`.pairs`/`.value` path and its regression pin
`t/for-pairs-value-quanthash-writeback.t`. Reuse or extend that operation where
possible; do not add a VM fallback evaluator.

## Acceptance criteria

- `%qh.values.map({ $_ = 0 })` removes all keys for parameterized `SetHash`,
  `BagHash`, and `MixHash`, matching `raku`.
- The same write-through behavior is covered for a plain mutable `Hash`, so
  the producer/callback mechanism is tested independently of QuantHash
  zero-removal semantics.
- Add a focused regression test under `t/` that covers the three QuantHash
  variants, a nonzero write, and zero-removal.
- `MUTSU_FUDGE=1 prove -e target/debug/mutsu roast/S02-types/quanthash.t`
  passes all 129 tests, then add it to `roast-whitelist.txt` in sorted order.
- Existing `.pairs`/`.value` QuantHash write-through tests remain green.

## Validation

Run the focused regression test and the roast file while iterating. Before
publishing an implementation PR, run `cargo fmt --all`,
`cargo clippy -- -D warnings`, `make test`, and `make roast`.
