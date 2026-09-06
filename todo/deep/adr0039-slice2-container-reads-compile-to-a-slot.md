# ADR-0039 slice 2: `@`/`%` reads should compile to a slot, and the blocker is gone

**Design: [ADR-0039](../../docs/adr/0039-container-lexicals-resolve-lexically.md)
§4.2 first bullet; the measurements are §9, §10 and §11.**

A container read (`Expr::ArrayVar` / `Expr::HashVar`) still compiles to a by-name
`env` lookup where a scalar read compiles to `GetLocal(slot)`. Slice 2 flips it.
The flip has been implemented and measured twice and withdrawn twice — but the
reason it was withdrawn the second time **no longer reproduces**, which is why
this is now its own file instead of a paragraph inside a symptom report that has
been closed.

## Why it is worth doing

The by-name read is what *hides* store-lane bugs: two paths that end up naming
different containers under one name look fine as long as every read re-resolves
the name. Slice 1 (`unit_lexicals`) already made the declaration side lexical;
until the read side follows, a whole class of "the slot and `env` disagree"
defects stays invisible rather than fixed, and every write site has to keep
paying the by-name lookup.

## The blocker ADR-0039 §10.3 named is measured closed

§10.3 withdrew the flip because `make roast` then failed two whitelisted files —
`roast/S15-nfg/concat-stable.t` and `roast/integration/advent2014-day05.t` —
which were one root cause: *a container mutated from a nested frame propagated to
its owner by NAME only*, so a replacing write left the owner's slot stale.

That root cause was closed by `da8e94252` (ADR-0055 slice 1b, "an escaping
container capture the frame cannot vouch for gets a cell"), which puts the
container in a shared `ContainerRef` cell at its declaration so a closure holds
the *binding* rather than the *name*. Verified 2026-09-06 by building at
`da8e94252^`: six probe shapes (in-place growth with a live shadow, whole-container
replace, shrinking `.shift`, hash key add, hash replace, `.=` rebuild) all diverge
before it and all match `raku` after. Pinned as
`t/nested-frame-container-mutation-reaches-owner.t`. Closed out in
`news/2026-09/nested-frame-container-mutation-reaches-its-owner.md`.

## What to expect when you re-attempt it

**Re-measure first.** Everything below was measured on `2a9e06f91`, i.e. *before*
`da8e94252`. Some of it may have gone the same way the blocker did; the project's
standing rule about stale `todo/` diagnoses applies to this file too, and it is
the third pass on this flip.

The flip itself: `Expr::ArrayVar` / `Expr::HashVar` emit `GetLocal(slot)` when
`local_map` holds the sigiled name. Fallout on `prove t/` was 8 files / 14
assertions, from four store-side defects (ADR-0039 §10.2 has the detail):

1. An expression-position container declaration allocates no slot —
   `compile_expr_stmt`'s `Stmt::VarDecl` arm (`compiler/expr_block.rs`) takes a
   `decl_slot` only when the declaration shadows an outer binding, and
   `local_map` retains a popped sibling block's slot for a first declaration.
2. The element-assign fast path is never handed its baked `target_slot`, so it
   nil'd and re-seeded the wrong binding. Fix: thread `target_slot` in and use
   `resolve_local_slot`.
3. Whole-container rebuilds replaced the node — **already fixed and shipped** as
   `Interpreter::store_container_preserving_identity` (§10.4).
4. A QuantHash re-tag rebuilds its data node: `register_var_container_type_metadata`
   (`runtime/runtime_container.rs`) tags the env value and re-inserts it, so
   `my %h is MixHash` left the declaring slot on the untagged original.

`make roast` then failed four files. Two were the blocker above (now closed). The
other two are ordinary and were diagnosed:

- `S32-list/classify.t` 25-27 — a bug in fix 3's own probe (it promoted a parent-tier
  env entry into the frame overlay); corrected by probing read-only.
- `S03-metaops/hyper.t` 18-19, 92-93 — `@r»++` with an outer same-named `my @r`:
  `write_back_hyper_target_var` resolves by `find_local_slot` / `locals_set_by_name`
  (`position` → the OUTER slot). Same class as defect 2; needs a baked slot on
  `HyperMethodCall`.

## What NOT to do

Do not reach for decl-site cell-boxing of every `@`/`%` as the repair for a
store-lane gap. `docs/captured-outer-cell-sharing.md` §7.1d records that broader
`@`/`%` decl-site boxing regressed ~12 files through decont leaks, which is why
`register_container_ref_capture_if_free` (`compiler/expr_call.rs`) is restricted
to plain `$` names. ADR-0055 slice 1b's cell is narrow on purpose — it applies
only to an *escaping capture the frame cannot vouch for* — and widening it is a
different decision, not a slice-2 detail.

## Acceptance

- `prove t/` green with the flip on, including `t/closure-topic-readonly.t` and
  `t/push-inline-array-decl.t`.
- **A full local `make roast`** (not delegated to CI): the flip changes a
  universal property of container reads, which is the shape ADR-0040 slice 2's
  17 counter-current fixes came from, 9 of them found only by roast.
- `t/nested-frame-container-mutation-reaches-owner.t` and
  `t/container-lexical-declarator-matrix.t` keep passing — they are the two pins
  that say the write/capture lane is still sound underneath the flip.
