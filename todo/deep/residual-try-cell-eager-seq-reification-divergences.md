# Residual try-cell divergences: `.map` runs its callback eagerly

**Re-diagnosed 2026-08-22.** All twelve cells below were re-measured against a
current `main` build and all twelve still reproduce — none was fixed by an
intervening change. The design for the main root cause now lives in
[ADR-0058](../../docs/adr/0058-map-grep-produce-a-deferred-seq.md) (`Proposed`),
and the whole cell matrix is pinned as a rakudo-verified oracle in
`t/map-callback-runs-at-consumption.t` (23 rows; raku passes 23/23, mutsu 12,
with the 11 divergent rows marked `todo`). This file stays open only to track
that the work is not done.

## The root cause is NOT what this ticket originally said

The original text (and [ADR-0034](../../docs/adr/0034-seq-reification-is-in-place-and-distinct-from-consumption.md)
§6 quoting it) blamed "mutsu forces a `map`-produced `LazyList` at the
assignment/call boundary" and pointed at `force_lazy_list_vm`'s callers. That
does not fit the cells: `(1..3)` is **finite**, so `is_lazy_pipe_source`
(`src/runtime/methods_collection.rs`) is false and **no `LazyList` is ever
built**. `dispatch_map_method` (`src/runtime/methods_dispatch_match2.rs`)
materializes the source and calls `eval_map_over_items` immediately, inside the
`try`. There is no deferred value whose force could be moved — the callback has
already run by the time the `try` block's tail value exists.

mutsu's `try`/sink *placement* is also not at fault and must not be touched:
`compile_try_region` leaves the tail value on the stack and lets the enclosing
statement's `SinkPop` force it outside the trap, which is rakudo's own rule
(`news/2026-08/try-statement-sink-semantics-pinned.md`).

**The real cause is that `.map`/`.grep` are eager over a finite source in mutsu
and lazy in rakudo.** This is observable with no `try` and no exception at all:

```raku
my $s = (1..3).map({ say "side $_"; $_ }); say "before"; say $s.List;
# raku:  before / side 1 / side 2 / side 3 / (1 2 3)
# mutsu: side 1 / side 2 / side 3 / before / (1 2 3)
```

`.grep` behaves the same way and does not even have the `return`/stub deferral
`.map` has.

## What is left to do

- **Nine of the eleven divergent rows** (P4, P5, P12, P13, P18, Q9, Q11, Q14 and
  the side-effect-ordering row) are ADR-0058's target. Implementing that ADR
  un-`todo`s them; it also makes mutsu **stricter**, so a full `make roast` is
  mandatory, not `make test` alone.
- **The other two rows** (Q5/Q6/R6/R7's exit status) are a **separate, narrower
  bug**, recorded in ADR-0058 §1.4: the stub-map path already defers correctly
  and already matches raku once the enclosing `try` is removed. What diverges is
  how a `fail` raised *during the force* resolves when a `try` sits lexically
  between it and the routine — mutsu returns it from the routine as a `Failure`,
  rakudo throws it. Nobody has looked into that yet.

`t/try-sink-semantics.t` pins the sink-placement half and must keep passing
through both fixes.
