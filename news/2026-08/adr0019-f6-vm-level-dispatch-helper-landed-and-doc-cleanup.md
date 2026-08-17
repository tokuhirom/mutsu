# ADR-0019 F6: VM-level direct-dispatch helper landed at every blocked family; stale doc comments fixed

`todo/deep/adr0019-f6-vm-level-dispatch-helper-needed.md` scoped a real problem: five of the seven
`run_instance_method` caller families named in ADR-0019's F6 box could not migrate onto
`call_method_with_values` the way the coercion and mut-lvalue families did, because each lives
inside `call_method_with_values`'s (or `call_method_mut_with_values`'s) own call graph — a naive
swap recurses into itself and crashes with a stack overflow, as a reverted instance-ops attempt
confirmed directly.

The fix the doc called for — a VM-level `resolve_method_cached` + `dispatch_compiled_method` pair
callable without re-entering the outer dispatch functions — was built as
`Interpreter::try_dispatch_compiled_method_direct` (and its `_as`/`_with_attrs_cell` siblings) in
`src/vm/vm_call_method_compiled_direct.rs`, and applied to every remaining named site across
instance-ops, new-dispatch, mut-dispatch, and general-call-dispatch. Each site now tries the direct
resolver first and falls back to `run_instance_method_at` only for cases the direct path cannot
serve (a residual `COERCE` no-match redirect, value-dependent multi-method resolution, an
augmented-native-type `.new` shape) — verified per-site with the full local `t/` suite, targeted
roast subsets, and `scripts/battery-testsuite.sh`.

The qualified-dispatch family's shared helper (`run_resolved_method_compiled_or_treewalk` /
`run_resolved_method_celled`) was separately investigated and found to be sound, load-bearing
orchestration — not a duplicate of the modern resolver — so it is retained rather than migrated;
see ADR-0019's F6 box for the full design conclusion. With that, F6 has no further open
code-migration slices.

As a closing sweep, the eight stale doc comments the F6 box text called out (referencing a
`run_instance_method_resolved` function name that no longer exists — it was renamed to
`forward_resolved_delegation` in #3683, with a separate `run_resolved_instance_method` and
`run_resolved_method_celled`/`run_resolved_method_compiled_or_treewalk` introduced later by
ADR-0019 E9c-2) were corrected to name whichever current function each comment actually describes,
rather than deleted outright — each carries real information (env_dirty writeback caveats,
delegation-forwarder-only reachability) still worth keeping accurate.

This retires `todo/deep/adr0019-f6-vm-level-dispatch-helper-needed.md`.
