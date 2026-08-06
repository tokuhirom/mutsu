# `CompiledFns::default()` call sites silently drop nested-sub bytecode outside methods

Found 2026-08-06 while attempting ADR-0019 C6e-3c (dropping
`CompiledSubDeclPlan::legacy_body`); see that ticket
(`todo/deep/c6e-legacy-body-drop-blocked-by-gate-rejected-shapes.md`) for the
full narrative. This file is the actionable, generalized follow-up.

## The defect

A compiled routine that declares a nested `sub` (or a block-lexical `our sub`)
only runs that nested sub correctly when the *executing call's*
`compiled_fns: &CompiledFns` table happens to contain the nested sub's
compiled key. Several call sites do not thread the real table through and
instead pass `CompiledFns::default()` (an empty map) — the routine's own body
still resolves fine (it runs from its own `CompiledFunction`/`compiled_code`),
but any `RegisterSub` op inside that body for a nested declaration then finds
`compiled_fns.get(&key)` returns `None`. Before ADR-0019 C6e-3b/3c, this was
silently papered over: the nested sub's registration fell back to its AST
body (`legacy_body` on `CompiledSubDeclPlan`) and ran through the tree-walking
interpreter, producing the right answer at the cost of not being compiled.
With `legacy_body` gone (or even just with the C6e-3b default-empty-body
policy pushed further), a def in this state has neither a working AST body
nor a resolvable compiled routine — it is silently a no-op / returns Nil.

This is the *same defect class* already fixed once for method bodies: PR
#5982 (`project-adr0019-c6e3c-nested-sub-lifted` memory) found that all 7
`call_compiled_method` call sites (`class_dispatch.rs`,
`builtins_dispatch_next.rs`, four `vm/vm_call_method_compiled_*.rs` files)
substituted a hardcoded `CompiledFns::default()` for the executing method's
own functions table, and fixed it by giving `MethodDef` its own
`compiled_fns: Option<Arc<CompiledFns>>`, populated in
`compile_method_def_in_place_with_dist` from the per-method `Compiler`'s own
`compiled_functions` (previously discarded — only `compiled_code` was kept),
then threading `method_def.compiled_fns` through all 7 sites instead of the
hardcoded empty table.

## Why it is not just a methods problem

`grep -rln 'CompiledFns::default()' src/` currently finds ~17 files:

```
src/runtime/methods_distribution.rs
src/runtime/decl_types.rs
src/runtime/resolution_eval.rs
src/vm/vm_call_method_compiled_interpret.rs   (already fixed for methods)
src/runtime/runtime_module_export_sub.rs      (checked: EXPORT-only, deliberate)
src/vm/vm_hyper_method_ops.rs
src/vm/vm_misc_coerce.rs
src/vm/vm_dispatch_helpers.rs
src/runtime/resolution_call_sub.rs            (confirmed reproducer, see below)
src/compiler/mod.rs
src/runtime/class_dispatch.rs                 (already fixed for methods)
src/vm/vm_var_assign_post_incdec.rs
src/vm/vm_call_dispatch.rs
src/runtime/builtins_dispatch_next.rs         (already fixed for methods)
src/vm/vm_call_method_compiled_mut.rs         (already fixed for methods)
src/vm/vm_set_arith_ops.rs
src/vm/vm_arith_ops.rs
src/vm/vm_call_method_compiled_cache.rs       (already fixed for methods)
```

Not all of these matter (some are genuinely fine to use an empty table — e.g.
`runtime_module_export_sub.rs`'s `apply_module_export` calls the special
`EXPORT` sub, which by construction should not itself declare callable nested
subs that outlive it). But at least one — `resolution_call_sub.rs:385`, in
the "code object built from a registry routine" carrier branch of
`call_sub_value` — is a confirmed live reproducer:

```raku
proto sub is-eqv2(|) {*}
multi sub is-eqv2(Mu $got, Mu $expected, Str:D $desc) {
    sub test-eqv (Mu $got, Mu $expected) { $got eqv $expected }
    my $test = test-eqv $got, $expected;
    say "test=$test desc=$desc";
}
is-eqv2(42, 42, 'ints');
```

This is (almost verbatim) `Test::Util`'s real `_is-eqv` helper
(`roast/packages/Test-Helpers/lib/Test/Util.rakumod`), which is why
`t/is-eqv.t` was one of the tests that broke during the C6e-3c attempt. A
plain (non-`multi`) top-level `sub outer { sub inner {...}; inner() }` does
NOT reproduce it — something about how a `multi` candidate resolved to a Sub
*value* and invoked via `call_sub_value` lands on the
`data.compiled_routine.is_some()` branch (`resolution_call_sub.rs:382-391`)
that hardcodes `let empty_fns = CompiledFns::default();` instead of the
routine's own sibling-functions table.

## What's missing structurally

Unlike `MethodDef`, neither `FunctionDef` (the registry entry for an ordinary
named sub) nor `CompiledFunction` (the compiled-routine descriptor,
`Arc<CompiledFunction>` — this is what `SubData::compiled_routine` and
`data.compiled_routine` above hold) nor `SubData` itself carries a reference
to "this routine's own compiled sibling-functions table". `CompiledFunction`
only has `code: CompiledCode` (its own bytecode), not the map of nested-sub
keys → `CompiledFunction`s that its `RegisterSub` ops need to resolve.

## Suggested next steps

1. Decide where the per-routine `compiled_fns` table should live: probably a
   new field on `CompiledFunction` itself (`Arc<CompiledFns>` or similar,
   populated once at compile time from the compiling unit's
   `compiled_functions`, mirroring `MethodDef.compiled_fns`), since that is
   the thing every one of these call sites already has in hand
   (`data.compiled_routine` / `cf`).
2. Audit each of the ~17 `CompiledFns::default()` sites individually: does
   this call path ever execute a routine that could contain nested `sub`
   declarations? Fix the ones that can (thread the real table); leave a
   `// TODO:` or a comment justifying the ones that provably cannot (like
   `runtime_module_export_sub.rs`'s `EXPORT`-only use), following this
   project's "measure, don't assume" convention — env-gated A/B against the
   full `t/` + roast whitelist per site, the same way #5982 and the
   `hoist_nested_our_subs` fix (see the C6e-3c ticket) were validated.
3. Only after that lands should ADR-0019 C6e-3c (dropping
   `CompiledSubDeclPlan::legacy_body`) be re-attempted — re-run `make test`
   after the field removal as the acceptance gate; the removal itself is
   mechanical once every reader's real dependency is gone.

## What NOT to do

Do not re-attempt dropping `legacy_body` before this lands — the field is the
only thing currently keeping every one of these mis-wired call sites correct
by accident (falling back to interpreting the real AST body). Removing it
first would silently turn "runs slowly, via the interpreter" into "runs
nothing, silently returns Nil" for every affected call path, which is worse
and much harder to notice than a `make test` failure caught immediately after
the field-removal PR.
