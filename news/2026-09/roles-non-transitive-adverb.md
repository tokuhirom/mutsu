# `.^roles(:!transitive)` on a built-in type stops at the direct roles

```raku
say 1.^roles(:!transitive).map(*.^name).join(",");
# raku:  Real          mutsu: Real,Numeric
```

`Int` does `Real`, and `Real` does `Numeric`; only `Real` is a **direct** role,
so `:!transitive` must stop there. The user-declared side already honoured the
adverb — `collect_roles_for_class` derives directness by dropping every entry
reachable from another entry's own `does`, which works when those `does` lists
are registered. A built-in type's role list is a flat, pre-flattened seed with
no such edges to walk, so the filter had nothing to filter on.

## Direct roles are now recorded, not re-derived

The built-ins whose closure is wider than their direct compositions seed
`class_direct_composed_roles` alongside the flattened `class_composed_roles`,
and `Interpreter::direct_composed_roles` prefers a recorded list. Deriving was
not merely incomplete here, it was unfixable by adding edges: `Rat`'s direct
role is the *parametric* `Rational[Int,Int]`, whose un-parameterized `Rational`
has no registered parents at all unless the prelude that declares it happened
to be injected — so `Rat`'s "direct" list kept `Real` no matter what.

The walk also goes up the MRO now, stopping at the **nearest** contributor: a
class that composes nothing of its own reports its ancestor's direct roles
(`class L is Int {}; L.^roles(:!transitive)` is `Real`), while one that composes
something reports only its own (`Buf`, whose MRO carries `Blob`, is `Blob[T]` —
not `Blob[T], Positional[T], Stringy`).

## The Positional/Associative built-ins had no roles at all

Measuring the neighbourhood turned up a bigger gap than the ticket recorded:
`Array`, `Hash`, `Map`, `List`, `Slip`, `Range`, `Pair`, `Seq`, `Buf`, `Blob`,
`Set`, `Bag` and `Mix` answered an **empty** `.^roles`, with or without the
adverb, because none of them had a `class_composed_roles` entry. All thirteen
are seeded now, each list measured against raku v2026.07:

| type | `.^roles` | `.^roles(:!transitive)` |
|---|---|---|
| `Array` / `List` / `Slip` / `Range` | `Positional, Iterable` | both |
| `Hash` / `Map` | `Associative, Iterable` | both |
| `Pair` | `Associative` | `Associative` |
| `Seq` | `Sequence, PositionalBindFailover, Iterable` | `Sequence, Iterable` |
| `Buf` | `Blob[T], Positional[T], Stringy` | `Blob[T]` |
| `Blob` | `Positional[T], Stringy` | both |
| `Set` / `Bag` / `Mix` | `Setty`/`Baggy`/`Mixy` + `QuantHash, Associative` | the first only |

`:local` is deliberately untouched: mutsu already agrees with raku on it for a
built-in and for a user class that composes roles directly, and the one shape
where they differ (`class M is K {}`, where rakudo answers `Y,X` for a class
that composes nothing) is a separate question from this ticket's.

Pinned by `t/roles-non-transitive-adverb.t`, whose 24 assertions pass unchanged
under rakudo.
