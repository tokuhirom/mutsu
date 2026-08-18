# The class-dispatch `compiled_holder` on-demand recompile is confirmed rare, not a live hot path

`todo/tickets/adr0019-method-body-compile-dedup-remnants.md` item 2 asked whether
`run_resolved_method_celled`'s on-demand-compile fallback in `src/runtime/class_dispatch.rs`
(the `compiled_holder: Option<MethodDef>` local clone-and-compile, for a method resolved before
its owner's registration compile pass ran, or added purely at runtime) was still reachable
post the Phase E dispatch-resolver unification, or had become a dead/rare corner the resolver
bypasses.

It turned out to be very much reachable — and, before this fix, unboundedly hot. Investigating
with the `method_body_runtime_compiles` `MUTSU_VM_STATS` counter (wired for exactly this
question in ADR-0019 D3-8), a sweep of every `t/*.t` file found 526 files with a nonzero count,
several in the hundreds (`t/where-named-param-sibling-ref.t`: 265, `t/mustache-battery.t`: 191).
`rust-gdb` breakpoints on `compile_method_def_in_place_with_dist` traced the dominant shape to
`Interpreter::run_proto_method` (`src/runtime/dispatch_proto.rs`): every `proto method` /
`proto submethod` call built a brand-new synthetic `MethodDef` with `compiled_code: None`
hardcoded, so the `compiled_holder` fallback recompiled the same proto body from AST on every
single dispatch — not once per registration, forever.

Fixed with `Registry::proto_compiled_cache` (keyed by `(owner, method_name)`, cleared whenever
`set_proto_method` installs a new body so a class/EVAL redeclaration can never see a stale
compile): `run_proto_method` now checks the cache before building the synthetic `MethodDef`,
compiles once on a genuine miss, and reuses the cached result on every later call.
`where-named-param-sibling-ref.t` went 265 → 1, `mustache-battery.t` 191 → 1, with the full `t/`
suite (3242 files, 30036 tests) still green. Pinned by `tests/proto_method_body_compiled_once.rs`.

A same-day follow-up reverified the `compiled_holder` site itself directly (rather than only its
dominant caller): with the proto-method hot path eliminated, the fallback now serves only its
intended rare purpose — a method reached before its owner's registration compile pass, or one
added at runtime (a role method punned via `does`, a custom-HOW method) — and is no longer a
live per-call bug for ordinary dispatch. No further change was needed at that call site.

That reverification also surfaced a distinct, more general gap in the same neighborhood — every
hoisted class/role forward-reference shell pays a throwaway per-method compile that is
discarded unread when the real declaration supersedes it moments later, which affects
`compile_class_methods` as much as `compile_role_methods`. That is a separate, higher-blast-radius
finding, recorded as its own investigation in
`todo/deep/adr0019-hoisted-type-shell-throwaway-method-compile.md` rather than folded into this
fix.
