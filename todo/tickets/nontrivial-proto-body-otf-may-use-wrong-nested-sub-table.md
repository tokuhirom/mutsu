# `vm_try_run_nontrivial_proto_body` may pass the wrong nested-sub table to a freshly OTF-compiled proto body

While investigating the ADR-0019 C6e-3c blocker (see
`todo/deep/c6e-legacy-body-drop-blocked-by-gate-rejected-shapes.md`), the fix
landed for `call_shared_state_body` in `src/vm/vm_call_func_ops.rs` — it was
passing the *caller's* `compiled_fns` table to
`call_compiled_function_named` instead of the callee's own
(`cf.compiled_fns`), which resolves nested `RegisterDecl` opcodes against the
wrong table (ADR-0019 C6e-3c's "own nested-sub table" carrier,
`CompiledFunction::compiled_fns`).

`vm_try_run_nontrivial_proto_body` (same file, ~line 1671) has the identical
shape and was NOT audited or fixed in this pass:

```rust
let cf = self.otf_compile_function_def(&proto_def);
let pkg = proto_def.package.resolve();
self.push_proto_dispatch_frame(name.to_string(), args.clone());
let result = self.call_compiled_function_named(&cf, args, compiled_fns, &pkg, name);
```

This handles a *non-trivial* proto body (`proto foo($x) { say "x"; {*} }`,
as opposed to a bodyless/`{*}`-only proto). `cf` comes from a fresh
`otf_compile_function_def` call, so `cf.compiled_fns` should be populated by
the standard `helpers_sub_body.rs` construction site if the proto body
itself declares any nested subs/protos/multis. Passing `compiled_fns` (the
caller's table) instead of `cf.compiled_fns.as_deref().unwrap_or(compiled_fns)`
means such a nested declaration could hit the same "wrong empty table" bug
`call_shared_state_body` had.

Not confirmed with a repro — this is a scoping note from code inspection,
not a verified bug. To check: does a proto with a non-trivial body AND a
nested named sub declaration (e.g. `proto foo($x) { my sub helper() {...};
say helper(); {*} }`) reach this function, and does forcing that nested
sub's registration body-less (the same `MUTSU_FORCE_BODYLESS` env-gated
instrument used for the C6e-3c investigation) break it the same way
`oc-proto` did?

If confirmed, the fix is the same one-line pattern:
`let fns = cf.compiled_fns.as_deref().unwrap_or(compiled_fns);` before the
`call_compiled_function_named` call.
