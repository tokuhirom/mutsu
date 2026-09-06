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

## Measured 2026-09-07: the discriminator is `try` specifically, and only the exit status differs

Re-run on a fresh build. **Only ONE assertion of the two `todo` rows actually
fails**: the exit status. `unlike $out, /'reached-tail'/` already passes in
mutsu -- the statement after the `try` does not run there either. So the
divergence is narrower than "returns a Failure instead of throwing": both
implementations abandon the rest of `ee`; rakudo lets the exception reach the
top and exits 1, mutsu returns a `Failure` from `ee` and the caller's
`.^name` (which does not force it) leaves the program alive at exit 0.

The discriminator is `try` **specifically**, not "a block between the `fail` and
the routine". Measured, with `sub ee { BLOCK; say "T"; 99 }` and a stub-map
inside:

| the block between | rakudo |
|---|---|
| `try { ... }` | **throws**, `T` unreached |
| `{ ... }` (bare block) | `fail` returns a Failure from `ee`, alive |
| `do { ... }` | same as the bare block |
| no block at all | same as the bare block |

and when the Seq is forced INSIDE the `try` (`try { my @z = map ... }`, or
`try { eager map ... }`) rakudo and mutsu already agree: the `try` catches it,
`$!` is `X::StubCode`, execution continues. So this is not about where the force
lands -- `t/try-sink-semantics.t` pins that correctly -- it is about what a
`fail` raised at a statement sink does when a `try` sits lexically between it
and the routine. Nobody has explained rakudo's rule here yet, and a fix keyed on
"the sink of a `try` statement's value" would be an ad-hoc special case; the
rule has to be understood first.

## Not part of this ticket

ADR-0058 step 3 (the listop `map` form, then grep) was attempted on 2026-09-07
and parked behind a step-2 hole -- see ADR-0058 §9 and
`todo/deep/deferred-map-callback-runs-in-the-consuming-frames-env.md`. It would
not move these two rows either way: they use a pure `...` stub body, which the
older `create_lazy_map_list` deferral already handles. Step 4 (retire the
`body_contains_return`/`is_stub_routine_body` predicate and
`create_lazy_map_list`) is tracked in the ADR, not here.
