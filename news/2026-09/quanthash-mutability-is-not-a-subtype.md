# `Set ~~ SetHash` is False again (and both spellings do the same roles)

`Set.new("a") ~~ SetHash` answered `True`, as did `Bag ~~ BagHash` and
`Mix ~~ MixHash`. `Set` and `SetHash` are *siblings* under `Any` in both
implementations — neither is a superclass of the other — so the accept had no
basis. It was one-directional: `SetHash.new("a") ~~ Set` already answered
`False`, and `.isa`, `.^mro`, `.^name` and `.WHAT` were all correct.

## Root cause

mutsu spells the mutable/immutable distinction as a `bool` inside one `Value`
variant (`ValueView::Set(_, mutable)`), and `value_type_name` already reads it —
so the exact-name comparison in `type_matches` is the whole rule. A stale
bridge sat below it:

```rust
// SetHash/BagHash/MixHash are mutable variants sharing the same Value variants
if constraint == "SetHash" && value_type == "Set" { return true; }
```

which predates `value_type_name` learning to report the mutable names. It is
gone.

## The other half of the same mistake

Removing it exposed the mirror-image bug: the lists that say which *roles* a
QuantHash satisfies named only the three immutable spellings, so the mutable
ones fell through to the (now correct) exact-name comparison and failed.

| Program | before | rakudo |
|---|---|---|
| `Set.new("a") ~~ QuantHash` | False | True |
| `SetHash.new("a") ~~ QuantHash` | False | True |
| `SetHash.new("a") ~~ Associative` | False | True |
| `multi f(Associative)` given a `SetHash` | picked `Any` | picks `Associative` |
| `sub f(%h)` given a `SetHash` | refused | binds |

`QuantHash` had no row at all; `Associative`'s row and the built-in MRO table
used for multi-candidate ranking each listed `Set`/`Bag`/`Mix` and stopped.
All three now name the six spellings, with the mutable ones as siblings of the
immutable ones rather than as bridges to them.

## Scope

Pinned by `t/quanthash-mutability-smartmatch.t` (35 assertions measured against
rakudo 2026.07): both directions for all three pairs, each spelling matching
itself, an `is SetHash` container, `Setty`/`Baggy`/`Mixy`/`QuantHash`/
`Associative` for both spellings, a `SetHash` parameter refusing a `Set`, a
`when` chain picking `Set`, `Associative` multi-candidate ranking, and a `%h`
parameter accepting a `SetHash`. `roast/S02-types/{set,sethash,bag,baghash,
mix,mixhash}.t` (1676 subtests) stay green.
