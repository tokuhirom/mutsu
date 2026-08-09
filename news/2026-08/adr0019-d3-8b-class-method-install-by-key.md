# ADR-0019 D3-8b: install main-pass-compiled method bodies by key

`class_body_method_decl` now installs a class method's bytecode straight from the main-pass
compile ADR-0019 D3-8a produces, instead of leaving `MethodDef::compiled_code`/`compiled_fns`
`None` for the registration-time throwaway compile (`compile_method_def_in_place_with_dist`) to
fill in on first call. When `CompiledMethodDecl::compiled_routine_key` resolves in the ambient
`CompiledFns` pool *and* the resolved `CompiledFunction`'s `params`/`param_defs` match exactly
what this registration walk just computed for the same declaration, the compiled bytecode installs
directly; any mismatch (or a missing key, e.g. a computed method/class name) falls back unchanged
to today's behavior.

The ambient `CompiledFns` table reaches the class walker through a small plumbing extension:
`exec_register_class_op` gained a `compiled_fns: &CompiledFns` parameter (mirroring
`exec_register_sub_op`, which already had one), threaded through `ClassDeclModifiers`/
`ClassBodyCx` down to `class_body_method_decl`. The two non-VM-op callers of `register_class_decl`
(role-pun synthesis, mixin-type synthesis) pass an empty table — harmless, since both register
with an empty body.

A `MUTSU_VM_STATS=1` stress repro (`class C { method m($x) { $x + 1 } }` redeclared and
instantiated 50 times in a loop) confirmed the effect directly: `method_body_runtime_compiles`
dropped from 50 to 0.

The full `make roast` run for this slice caught one real regression before it could land:
`roast/S12-introspection/walk.t`'s `$?PACKAGE.^name` returned a mangled name for a class declared
inside a closure body (a `subtest "..." => { my class C2 { ... } }` shape, a common test-file
pattern). The root cause was in D3-8a's compile-time package-name predictor
(`qualified_class_decl_name`): it didn't account for the compiler's synthetic STATE-SCOPE
pseudo-package (a `current_package` value containing `"::&"`, used purely for `state`-variable key
uniqueness inside a sub/closure body) — a case the same file's `qualify_package_name`/
`qualify_variable_name` already special-cased, just not the newer D3-8a helper. Inside a state
scope, `current_package` doesn't track the runtime's real `current_package()` the way ordinary
package-scope bracketing does, so the "compile-time mirrors registration-time" assumption silently
breaks — and since a wrong package name gets baked directly into the body's bytecode (not carried
as a parameter), the params-equality install guard can't catch it. Fixed by extending the bail-out
itself: a declaration nested inside a state scope now skips main-pass method-body compilation
entirely (`compiled_routine_key` stays `None`), falling back to the unaffected registration-time
compile — the same treatment already given to computed names and hoisted shells. The role-side
twin (`qualified_role_decl_name`) got the identical fix even though it isn't observable yet (D3-8c,
the role-walker cutover, hasn't landed), to avoid rediscovering the same bug later.

`role_body_method_decl` is untouched — D3-8c, a separate future slice needing its own roast S14 +
battery-gate verification, since parametric-role method dispatch is load-bearing for the bundled
Cro/OO::Monitors batteries.
