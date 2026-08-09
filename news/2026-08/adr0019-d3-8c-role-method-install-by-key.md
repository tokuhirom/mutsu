# ADR-0019 D3-8c: install main-pass-compiled role method bodies by key

`role_body_method_decl` now installs a role method's bytecode straight from the main-pass compile
ADR-0019 D3-8a produces, the same install-by-key guard D3-8b already applied to the class walker
(`class_body_method_decl`): when `CompiledMethodDecl::compiled_routine_key` resolves in the
ambient `CompiledFns` pool *and* the resolved `CompiledFunction`'s `params`/`param_defs` match
exactly what this registration walk just computed for the same declaration, the compiled bytecode
installs directly instead of leaving `MethodDef::compiled_code`/`compiled_fns` `None` for the
registration-time throwaway compile (`compile_method_def_in_place_with_dist`) to fill in later.

The role side is simpler than the class side: `role_body_method_decl` never performs a
`::?CLASS`-style parameter-type substitution, so the `effective_param_defs` this walk computes IS
exactly what `compile_method_body` computed at plan-lowering time (`is_hidden: false`, no
auto-positional-`@_` detection — `add_role_decl_plan` already documented this divergence from the
class side). No separate pre-substitution snapshot is needed, unlike D3-8b's
`raw_param_defs_for_key_check`.

The ambient `CompiledFns` table reaches the role walker the same way D3-8b wired the class walker:
`exec_register_role_op` gained a `compiled_fns: &CompiledFns` parameter (threaded from
`exec_register_decl_op`, which already had it), and `register_role_decl`/`RoleDeclCx` gained a
`compiled_fns` field. `register_role_decl` has only the one call site (the VM op), so no
`CompiledFns::default()` plumbing was needed elsewhere.

Because the install happens inside `register_role_decl` itself — before the `role_candidates`
snapshot composition reads is cloned — the per-composing-class recompile disappears for free (the
design doc's decision 6): a `MUTSU_VM_STATS=1` repro (a role with two methods composed into 3
classes directly, plus 5 more inside a loop) confirmed `method_body_runtime_compiles` dropped from
18 to 0. The D3-8a byte-parity unit tests (including its two role-specific fixtures), the full
`t/` suite (2974 files, 28019 tests), and all 121 whitelisted `roast/S12-*`/`S14-*` files stayed
green on a release build.

D3-8d (the fallback-narrowing survey confirming every remaining
`method_body_runtime_compiles` hit is one of the enumerated dynamic shapes — `augment class`,
`.^add_method`, computed names) remains open.
