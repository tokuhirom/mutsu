# The hash constructor *is* the store, and both ADR-0040 compensators are gone

ADR-0040 slice 4 fixed the chained-subscript store and then had to stop: the two
compensators it was scoped to delete — `itemize_hash_value` on the hash-subscript
read and `raku_hash_value` in the `.raku` renderer — were still load bearing for
one nameable class. A `Hash` that a native Rust builtin builds directly stored
its values **bare**, because none of ADR-0040's store hooks were on that path.

This slice routes that class through the store and deletes both compensators.

## The store is the constructor, not the ~160 call sites

The ticket proposed enumerating the native hash-construction sites. There are
about 160 `Value::hash(...)` call sites, so the enumeration went one level down
instead: **`Value::hash` itself** is the single funnel every one of them passes
through, and it is exactly where ADR-0040 says a stored aggregate is itemized.
The hook is the same scan-then-rebuild-only-if-needed shape
`itemize_real_array_elements` already uses for the array half — a hash of plain
scalars is never touched, so the assignment paths (which itemize on their own way
in) pay only the scan.

## The scan had to stop going through `view()`

The first build broke `value::match_lazy::tests::lazy_match_children_stay_lazy_one_level`.
`Value::needs_element_itemization` answered through `Value::view()`, and a
`view()` of a lazy `Match` **forces** it (ADR-0016 P5) — so scanning a hash of
capture nodes materialized every one of them just to conclude that a `Match`
never needs itemizing. It is now a pure representation-tag probe
(`NanBox::needs_element_itemization`), which is both non-forcing and cheaper, and
removes the same latent forcing from the array-half scan.

## Four kinds of "hash" are not a Hash

Running the full `t/` suite against the central hook failed exactly four files,
and every one named a real distinction rather than an accident. mutsu represents
several associative things with the `Value::Hash` repr whose values raku says are
**not** element containers:

| what | raku | mutsu before |
| --- | --- | --- |
| a `Map` (`Map.new(…)`, `.Map`) | `Map.new((a => (1,2)))<a>.raku` is `(1, 2)`, arity 2 | `$(1, 2)`, arity 1 |
| a `Match`'s capture map | `$/.hash<x>.VAR.^name` is `Array` | itemized, so `for $<hunk>` saw one item |
| a slurpy `*%h` parameter | `f(a => ("x","y"))` sees `("x", "y")`, arity 2 | `$("x", "y")`, arity 1 |
| `%_` / leftover-named | the same hash under another name | idem |

These now build through a second constructor, `Value::hash_bare_values`, which is
the old `Value::hash` body. A plain `%`-param that receives a real `Hash` still
sees that hash's own itemization, so the split is by *what kind of associative
thing this is*, not by call site.

## Both compensators deleted

With the store fixed, the instrumented sweep — both sites behind
`MUTSU_COMP_PROBE`, the whole `t/` suite (3601 files) plus the full roast
whitelist (1435 files) — showed that what still reached them was no longer a gap
to paper over:

| compensator | before slice 4b | after | what still reaches it |
| --- | --- | --- | --- |
| read-side (`itemize_hash_value`) | 3 in `t/`, 17 in roast | 7 in `t/`, **0 in roast** | only bare-valued hashes: 6 are this slice's own new counter-current pins, the 7th is a `Capture`'s `.hash` |
| render-side (`raku_hash_value`) | 0 in `t/`, 1 in roast | unchanged | a self-referential hash, where the rendered value is the cycle sentinel and the test only asserts `ok $foo.raku` |

The 17 roast firings — the whole native-hash class — are gone, and every one of
the seven survivors is the compensator producing a **wrong** answer for a
bare-valued hash. So the deletion is a fix, not merely a cleanup:
`Map.new((a => (1,2)))<a>`, `%h.Map<a>`, a slurpy `*%h`'s values and a
`Capture`'s `.hash` values all answered `$(1, 2)`-shaped before and answer raku's
bare shape now.

## `.Map` had to grow the decont it only half had

`to_map` deconted its values only when the receiver was already a `Hash`. A
list-of-`Pair`s receiver folded through `to_hash` — which now itemizes, because
that is what a `Hash` store does — and kept the itemization, so
`C.new(|(a => (1,2,3), b => (4,5,6)).Map)` bound `Int @.a` to one `List` instead
of three `Int`s (`roast/S32-hash/map.t`). Before this slice that path stored bare
*by accident* while the render compensator made `.raku` look itemized anyway —
the same "one value, three answers" shape, in the other direction. Both `.Map`
arms decont now.

## What did not follow

`.VAR` on a bare-valued hash still answers `Scalar` where raku says `List`: the
discriminator reads `ArrayKind` for arrays and has nowhere to read the same bit
from for a hash. Pre-existing (the compensator made `.raku` and arity wrong in
the same place before), and filed as
`todo/tickets/var-on-a-bare-valued-hash-answers-scalar.md`.

## Verification

`t/element-store-itemization.t` grew a slice-4b section, dual-oracled against
`raku`: a natively built `Hash` (`.classify`, **bound** so no Raku assignment
store can paper over the construction) itemizing consistently across the
subscript read *and* `.values`, plus the four counter-current shapes. Full local
`t/` suite (3601 files, 36255 tests), the full roast whitelist (1435 files,
218833 tests) and the bundled-battery gate all pass.
