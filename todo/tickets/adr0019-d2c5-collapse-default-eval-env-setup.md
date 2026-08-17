# ADR-0019 D2c-5: collapse the three near-duplicated default-evaluation env-setup shapes

Filed as a standalone ticket when ADR-0019 was marked Accepted/Implemented (G4) so this leftover
does not get lost once the ADR stops being actively tracked.

## Background

ADR-0019's D2c box (compiling attribute defaults/constraints as child chunks via
`CompiledAttrDecl`/`DeclTraitArg` and `eval_decl_trait_arg`) landed in full (D2c-1..4, 2026-08-07/08
-- see `docs/adr/0019-compiled-declarations-and-unified-method-dispatch.md`'s D2c entry). One
optional sub-box was scoped but never started:

> **D2c-5 (optional)** — collapse the three near-duplicated default-evaluation env-setup shapes
> (class walker, default ctor, `dispatch_new`) into one; gated on raku-verifying shape B's
> `has_class_scoped_subs` special case first. Not started, low priority.

## Scope

Three call sites independently set up the env before evaluating an attribute's default-value
expression:

1. The class-body attribute walker (used by e.g. `CREATE`'s slot initialization).
2. The native default constructor fast path (`build_native_default_instance`,
   `src/runtime/methods_object_default_ctor.rs`).
3. `dispatch_bless`/`dispatch_new`-style construction (`src/runtime/methods_dispatch_new.rs`).

Each independently builds (a variant of) the same env-setup shape around
`eval_decl_trait_arg`/`run_decl_code` before evaluating a `has $.x = EXPR` default. Collapsing them
into one shared helper removes drift risk between the three, but needs the `has_class_scoped_subs`
special case (a class-scoped `sub` referenced from an attribute default) raku-verified first, to
confirm all three sites actually need identical behavior there before merging them.

## Why this is a separate ticket, not urgent

Low priority, explicitly optional in the ADR-0019 checklist -- correctness is not at risk (D2c-1..4
already migrated all three sites off raw `Expr`/`eval_block_value` onto the typed
`CompiledAttrDecl`/`DeclTraitArg` representation; this ticket is pure de-duplication of the
surrounding env-setup code, not a missing feature). Pick up opportunistically.
