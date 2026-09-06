# `Set ~~ SetHash` is True (and `Bag ~~ BagHash`, `Mix ~~ MixHash`)

Found 2026-09-06 while writing `t/set-reduction-one-arg.t` for the set-operator
one-arg rule. The reduction fix is unrelated — this is a smartmatch/type-object
defect that the new test tripped over when it tried to assert "the result is a
`Set`, *not* a `SetHash`".

## Repro

```raku
say Set.new("a") ~~ SetHash;   # raku: False   mutsu: True
say Set          ~~ SetHash;   # raku: False   mutsu: True
say Bag.new("a") ~~ BagHash;   # raku: False   mutsu: True
```

## Narrowed — it is one-directional, and `.isa` / `.^mro` are already right

| Program | raku | mutsu |
|---|---|---|
| `Set.new("a").^mro` | `((Set) (Any) (Mu))` | same — correct |
| `Set.new("a").isa(SetHash)` | `False` | `False` — correct |
| `Set.new("a").^name` | `Set` | `Set` — correct |
| `SetHash.new("a") ~~ Set` | `False` | `False` — correct |
| **`Set.new("a") ~~ SetHash`** | `False` | **`True`** |

So the type identity of the value is right everywhere it is *asked for*; only
the smartmatch against the mutable type object accepts, and only in the
immutable → mutable direction. `SetHash` is not a superclass of `Set` in either
implementation (they are siblings under `Any`, both doing `Setty`), so the
accept has no basis.

## Where to look

mutsu stores mutability as a `bool` in the value view itself
(`ValueView::Set(_, mutable)` / `Bag` / `Mix`) rather than as a distinct type,
so any type-object `ACCEPTS` that classifies by the *view variant* and forgets
to compare the flag will accept both spellings. The `SetHash ~~ Set`
direction answering `False` suggests one side of that comparison already checks
the flag and the other does not — find the type-object smartmatch/`ACCEPTS`
path for QuantHash values and make it compare mutability in both directions.

## Neighbourhood to check when fixing

`Bag`/`BagHash` and `Mix`/`MixHash` (both show the same asymmetry); the
`Setty`/`Baggy`/`Mixy`/`QuantHash` role smartmatches, which must keep matching
*both* spellings; `when` clauses and `given` over a set; a signature parameter
typed `SetHash` receiving a `Set` (which must fail to bind); and `.^isa` /
`does` / `WHAT` staying as correct as they already are.
