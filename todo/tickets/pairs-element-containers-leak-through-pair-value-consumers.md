# `.pairs` cannot hand out element containers yet: a cell-valued Pair leaks through its consumers

## Status

**ADR-0036 slice 3 is deferred for `.pairs` specifically.** The container-aware producer layer it
called for **did** land (`src/vm/vm_element_producers.rs`, 2026-08-27) and carries
`.values`/`.reverse`/`.sort` for ADR-0045 slice 4. `.pairs` was implemented, measured, and backed out
of the routing list. Rows 3, 4 and 9 of ADR-0036 §1.3 stay `todo`-marked.

## What happens

Routing `.pairs` makes it hand out Pairs whose value is the element's own `Scalar` container. Every
consumer that reads a Pair's value **as data** then sees a `ContainerRef` where it expects a value.
Measured leaks, each a real test failure:

| consumer | symptom |
| --- | --- |
| `"…".trans(%matcher.pairs)` | type-tests the value (`is_closure`, then Regex/Array/Range shape); a cell answers "no" to all, so a closure replacement silently became a stringified one. `roast/S05-transliteration/with-closure.t` 5, 7 |
| `%a = %reset.pairs` into a **Hash** | stored the *cells*, aliasing two hashes together, so mutating one rewrote the other |
| `%a = %reset.pairs` into a **BagHash** | every weight became `1` — the weight extraction fell through its `Int`/`Num`/`Bool` arms to the truthy `_` arm. `roast/S03-metaops/infix.t`, 396 subtests |
| `%src.pairs.map({ .key => .value })` | `.value` returns the cell, so the rebuilt pair carries it too and the same weight collapse follows |
| `.antipairs` after `.pairs` | the key was no longer de-itemized, because the de-itemization ran on a cell. `t/element-store-itemization.t` 80 |

The first three were each fixed at their own site; the pattern did not stop, which is the finding.

## Root cause, and why it is not a short list of sites

Two things compound:

1. **`.pairs` promotes the source's elements IN PLACE.** After `%h.pairs`, `%h` holds cells from then
   on — so the exposure is not "consumers of the `.pairs` result" but "consumers of any container a
   producer has ever run over".
2. **Bulk iteration bypasses the element read chokepoint.** ADR-0036 §5 Q4 asked whether
   `resolve_array_entry` is genuinely the only read chokepoint and expected yes. It is the only
   chokepoint for *element* reads, but `h.iter()` / `items.iter()` in the coercion layer walk the
   storage directly, and `ValueView::Pair(k, v)` / `ValueView::ValuePair(k, v)` destructuring reads
   the pair's value without any accessor at all. `src/runtime/utils/set_coerce.rs` and
   `src/runtime/utils/coerce_containers.rs` alone hold **15** such destructuring sites.

`.values`/`.reverse`/`.sort` do **not** have this problem, which is why they shipped: they hand out a
flat list of cells, and list consumers go through the decontainerizing paths. It is specifically the
*Pair wrapper* that carries a cell into code which reads it structurally.

## What the fix needs

A **read chokepoint for a Pair's value**, so that "give me this pair's value as data" and "give me
this pair's value as an lvalue" are different operations. Today they are the same field read.

The natural candidate — decontainerizing in the `"value"` accessor
(`src/builtins/methods_0arg/coercion.rs`, the `ValueView::Pair(_, v) | ValueView::ValuePair(_, v)`
arm) — is **not sufficient and conflicts with a shipped row**: it does not touch the 15 structural
destructuring sites, and ADR-0036 row 6 requires `(@a[0]:p).value.VAR.^name` to be `Scalar`, which
needs `.value` to return the container. So the design question is real and wants its own decision:
either a distinct "pair value view" that `.VAR` consumes, or an audited conversion of every
structural pair-value read to go through an accessor.

Note that the `:p`/`:kv` subscript adverbs (ADR-0036 slice 2) already put cells in Pair values and
have shipped since 2026-08-20 without this trouble — because they promote **one** element on demand,
where `.pairs` promotes the whole container and is far more likely to be fed straight into a
coercion.

## Reproduce

```raku
my %src = a => 1, b => 2, c => 3;
my %z is BagHash;
%z = %src.pairs;
say %z.sort;      # raku (a => 1 b => 2 c => 3);  with .pairs routed: (a => 1 b => 1 c => 1)
```

Re-add `"pairs"` to `ELEMENT_PRODUCERS` in `src/vm/vm_element_producers.rs` (both the array and hash
arms) to reproduce; `roast/S03-metaops/infix.t` fails 396/5076 immediately.
