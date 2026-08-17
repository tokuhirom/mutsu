# ADR-0019 E5 step 3: measurement counters for the hyper non-mut paths

Instrumented `exec_hyper_method_call_op` (`HyperMethodCall`) and
`exec_hyper_method_call_dynamic_op` (`HyperMethodCallDynamic`), both in
`src/vm/vm_hyper_method_ops.rs`, with the same `MUTSU_VM_STATS`-gated
dispatch-entry counters E5 step 1 introduced for `CallMethod`. This is the
third of the four E5 measurement slices for ADR-0019 Phase E (routing VM
method-call opcodes through the resolver): pure insertions, zero behavior
change, reusing the same two generic counter functions with
`entry = "hypermethodcall"` / `"hypermethodcalldynamic"`.

Unlike `CallMethod`/`CallMethodDynamic`, a hyper opcode loops over every
element of its target and dispatches once per element, so the outcome sum
here counts element-level dispatches rather than opcode executions — a hyper
over a multi-element array legitimately records more outcomes than the
opcode ran. This was confirmed directly and is documented as the expected
verification identity for this pair of entries, distinct from step 1/2's
`sum(outcomes) == opcode-histogram count` check.

A full `t/` sweep (3018 files, 50 hyper-active) shows `hypermethodcall`
dominated by `native`/`user` (575/191, ~75%/25% of its disjoint element
dispatches, same conclusion as step 1). `hypermethodcalldynamic` recorded
intercept traffic only (`>>.&sub` forms) and zero `native`/`user` locally —
not dead code, just missing local coverage: two whitelisted roast files
(`S03-metaops/hyper.t`, `S12-methods/parallel-dispatch.t`) exercise the
`».method`/`».$name(...)` string-dispatch branch directly and confirm real
`native`/`user` traffic there. Also re-confirmed a real behavior gap named
in the design doc: `exec_hyper_method_call_dynamic_op` has no
`skip_native`/`has_user_method` gate anywhere, unlike its static twin — an
open verification item for the eventual E5c/E6 cutover, not something this
slice changes.

`make test` (full `t/`, 3018 files, 28265 subtests) passes unchanged. This is
one slice of an ongoing campaign: the last E5 measurement entry
(`call_method_all_with_fallback`) and all cutover sub-slices
(E5b/E5c/E5d) are still to do. Full taxonomy tables and sweep detail:
`news/2026-08/adr0019-e5-e7-entry-routing.md` (§"Measurement slice results —
hyper non-mut paths (E5 step 3)") and
`docs/adr/0019-compiled-declarations-and-unified-method-dispatch.md` (E5
bullet).
