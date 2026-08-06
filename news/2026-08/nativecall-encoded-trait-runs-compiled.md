# NativeCall's `is encoded(...)` param trait no longer excludes a def from compiled dispatch

ADR-0019 C6e-3c listed the NativeCall marshalling trait (`is encoded(...)`)
as the last parameter-shape/trait exclusion in
`def_module_single_sig_body_ok_ignoring_state` — the shared gate that
decides whether a routine runs its plan-attached/OTF-compiled bytecode or
falls back to the interpreter tree-walk.

Measurement (full `t/` + roast under the existing C6e-2 instrumentation)
found zero live readers of the trait for dispatch: actual string encoding
for a native call happens explicitly via `.encode(...)` in the prelude
(`nativecall_manage.rs`), not through this parameter trait, and the shared
compiled binder (`bind_function_args_values`) only branches on
`rw`/`raw`/`copy`/`invocant`. A genuine `is native(...)` sub never reaches
this gate at all — `native_call_specs` is checked by name before body
dispatch runs.

Widened the gate's trait allowlist to include `encoded`. Sigilless scalars,
sub-signature destructuring, `start`-containing bodies (C6e-2a/2b/2c), and
now NativeCall marshalling traits (C6e-3c) all run compiled — no parameter
shape or trait excludes a def from the compiled dispatch path anymore.

Pinned by `t/encoded-param-compiled.t` (module-single OTF dispatch,
repeated calls, recursion, multi-candidate dispatch, and an EVAL-boundary
call); `t/nativecall-module-compat.t` already covered parse/marshal
correctness for the trait itself.

This does not by itself unblock dropping `CompiledSubDeclPlan::legacy_body`
— a separate structural reader remains
(`vm_call_named_inner.rs`'s computed-name/out-of-scope Sub-value fallback,
tracked in
`todo/deep/c6e-legacy-body-drop-blocked-by-gate-rejected-shapes.md`).
