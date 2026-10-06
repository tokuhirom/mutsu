# The debug function-key audit scans once per functions-map state

A debug build paid O(registered functions) on every function call: the
`#[cfg(debug_assertions)]` staleness audit in `fn_base_name_registered_sym`
compared the `fn_keys_by_base` entry with a fresh scan of the whole functions
map each time `CallFunc` asked the gate. With `use Test` loaded that is
hundreds of routines per call, so the `$i⚛++` stress loops in roast
`S17-lowlevel/atomic-ops.t` and `atomic.t` ran 4-5x slower than without it, and
the quick `scripts/dev gate` timed them out (#12119).

The audit is now memoized per base name. A functions-map version names one
content and is never reused for another (`runtime::function_table`), and an
index entry is an immutable `Arc<[Symbol]>`, so the same entry under the same
version compares equal to the same scan. The comparison re-runs only when the
map moved to a state that base name has not been audited under, or the entry
was replaced. The audit is exactly as strict as before: a map write that never
announced itself changes the version, so the next call panics. Two unit tests
pin both halves, "scans once per map state" and "a missed invalidation still
fails".

The residual slowness of `cas-int.t` is a different cost, tracked in #12120.
