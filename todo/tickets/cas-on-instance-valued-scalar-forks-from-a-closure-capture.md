# `cas` on an Instance-valued scalar runs on the legacy name lane, so a closure that captured the same name never sees the swap

Found on 2026-08-28 while writing the ADR-0055 slice-1 pins. Verified
**pre-existing on `main`** (`239c3b818`, unmodified build) — it is not caused by
the slice-1 cell relaxation, and it reproduces identically for a typed and an
untyped scalar.

## Symptom

```raku
class Node { has $.k }
my $head = Node.new(k => 1);
my $obs  = { $head.k };
cas $head, $head, Node.new(k => 2);
say $obs();     # raku: 2    mutsu: 1
say $head.k;    # raku: 2    mutsu: 2   (the direct read is fine)
```

The direct read of `$head` after the `cas` is correct, so the swap itself
happened. What is lost is the *coherence between the two lanes*: the closure's
captured binding and the storage `cas` actually wrote to are different objects.
Adding a type constraint (`my Node $head`) changes nothing.

## Root cause (established by reading, not yet gdb-confirmed)

`Interpreter::atomic_scalar_cell` (`src/runtime/builtins_atomic_shared.rs`)
refuses to promote a binding to a shared `ContainerRef` cell when the value is
an `Instance` (its value-kind skip list still carries `ValueView::Instance`,
unlike `box_captured_lexicals`, which dropped that entry in ADR-0025 slice 1).
So `cas` on an Instance-valued scalar falls through to the legacy name-keyed
lane (`__mutsu_atomic_name::` → `__mutsu_atomic_value::N`).

Meanwhile the closure capture takes the *other* mechanism: `$head` is an
escaping capture, so `box_captured_lexicals` promotes it to a cell. The two
lanes then never see each other's writes — the `cas` publishes into the shared
store, the closure reads its cell.

This is the same lane-forking hazard as
`news/2026-08/atomic-cell-shape-refusal-asymmetry-resolved.md`, but in the
opposite direction: there the *capture* promoted mid-sequence while the lane was
live (fixed on 2026-08-28 by `legacy_atomic_lane_owns`, which makes the capture
decline); here the *atomic op* refuses to promote a shape the capture side is
happy to promote, so the atomic op ends up on the legacy lane while the capture
sits on a cell.

## Why it is not a one-line fix

Dropping `ValueView::Instance` from `atomic_scalar_cell`'s skip list is the
obvious move and mirrors what ADR-0025 slice 1 already did on the capture side —
but `atomic_scalar_cell`'s doc comment records a specific reason its
seed-and-retire protocol is confined to that function (it runs synchronously in
the thread owning the atomic op, so it may seed from and retire the legacy lane
where the capture sites may not). Widening what it promotes therefore has to be
weighed against the `roast/S17-lowlevel/cas.t` regression that protocol was
written for. The right first step is a gdb breakpoint on `atomic_scalar_cell`'s
skip return for the repro above, then a targeted relaxation with
`roast/S17-lowlevel/cas.t` and `t/atomic-cell-shape-refusal-symmetry.t` as the
acceptance surface.

A related question worth answering in the same pass: the two skip lists
(`atomic_scalar_cell`'s and `box_captured_lexicals`') were meant to mirror each
other — the comment in `atomic_scalar_cell` still says "mirrors
`box_captured_lexicals`" — but they have now diverged on `Instance`,
`Package`, `Array` and `Hash`. Either they should be one shared predicate with
documented, deliberate exceptions, or the comment should stop claiming they
mirror.

## Repro

The snippet above; `raku` is the oracle. A slightly larger version covering the
typed / untyped / no-capture cases is easy to reconstruct: only the two
capture-carrying rows diverge.
