# ADR-0019 E5 step 2: measurement counters for CallMethodDynamic

Instrumented `exec_call_method_dynamic_op` (the `CallMethodDynamic` opcode
handler, `src/vm/vm_call_method_mut_ops.rs`) with the `MUTSU_VM_STATS`-gated
dispatch-entry counters introduced by E5 step 1 for `CallMethod`
(`record_dispatch_entry_outcome`/`record_dispatch_entry_intercept` in
`src/vm/vm_stats.rs`). This is the second of the E5 measurement slices for
ADR-0019 Phase E (routing VM method-call opcodes through the resolver):
pure insertions, zero behavior change, reusing the same two generic counter
functions with `entry = "callmethoddynamic"` rather than adding new ones.

15 intercept arm names were classified and instrumented (the `.+`/`.*`
all-methods modifiers, `$obj.$coderef(...)` direct-Sub dispatch, `.return`,
`.hyper`/`.race` config validation, and nine HyperSeq/RaceSeq delegate-method
arms), plus `native`/`user` outcomes at the plain dispatch probe and a
`notfound` overlay matching step 1's convention. Verified disjoint-and-complete
(`sum(outcomes) == CallMethodDynamic` opcode-histogram count) against five
targeted `t/` files with zero mismatches — this entry sees far less traffic
than `CallMethod`, so a full `t/`-wide sweep was not run, per the task's own
guidance not to over-invest in sweep tooling for a smaller entry.

This is one slice of an ongoing campaign, not a standalone feature: the
remaining E5 measurement entries (hyper non-mut paths,
`call_method_all_with_fallback`) and all cutover sub-slices (E5b/E5c/E5d) that
actually route dispatch through the resolver are still to do. Full taxonomy
table and verification detail:
`todo/deep/adr0019-e5-e7-entry-routing.md` (§"Measurement slice results —
CallMethodDynamic (E5 step 2)") and
`docs/adr/0019-compiled-declarations-and-unified-method-dispatch.md` (E5
bullet).
