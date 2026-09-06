# Residual try-cell divergences: a force-time `fail` under an enclosing `try`

**Scope narrowed 2026-09-07.** [ADR-0058](../../docs/adr/0058-map-grep-produce-a-deferred-seq.md)
step 2 shipped: `.map` (method form) now returns a `Seq` whose
`SeqSource::MapGrep` body runs the callback at first consumption, so the nine
rows this ticket owned — P4, P5, P12, P13, P18, Q9, Q11, Q14 and the
side-effect-ordering row — now match rakudo and are un-`todo`d in
`t/map-callback-runs-at-consumption.t`. ADR-0058 S8 records what step 0
measured and which of the ADR's premises did not survive contact with the code.

## What is left

**Two rows, and they are a different bug from the one this file was opened for.**
Q5/Q6/R6/R7 use a `...` **stub** callback, which mutsu already deferred before
ADR-0058 (`create_lazy_map_list`, gated on `is_stub_routine_body`), so
eagerness was never their problem. Measured against a current build, mutsu
matches raku exactly for the stub map as soon as the enclosing `try` is removed:

```raku
sub ee { map -> $x, $y { ... }, 1..6; say "reached-tail"; "done" }
say ee(); say "alive";       # both: "Stub code executed", exit 1
say ee().^name; say "alive"; # both: Failure / alive, exit 0
```

Add the `try` back and the divergence appears:

```raku
sub ee { try { map -> $x, $y { ... }, 1..6 }; say "reached-tail"; $! }
say ee().^name; say "alive"
# raku:  throws, "reached-tail" never printed, exit 1
# mutsu: Failure / reached-tail / alive, exit 0
```

So the force lands in the right place (the enclosing statement's `SinkPop`,
outside the trap — `t/try-sink-semantics.t` pins that half and must keep
passing). What differs is **how a `fail` raised during that force resolves when
a `try` sits lexically between it and the routine**: mutsu lets it return from
the routine as a `Failure`, rakudo throws it. Nobody has looked into that yet.

The two rows are pinned as the only remaining `todo`s in
`t/map-callback-runs-at-consumption.t` (rows 1 and 3, "a force-time `fail` under
an enclosing `try` returns a Failure instead of throwing"). Un-`todo`ing them is
this ticket's completion signal.

## Not part of this ticket

ADR-0058 steps 3 (extend the deferral to `builtin_map` and both `grep` entry
points) and 4 (retire the `body_contains_return`/`is_stub_routine_body`
deferral predicate and `create_lazy_map_list`) are tracked in the ADR, not here.
