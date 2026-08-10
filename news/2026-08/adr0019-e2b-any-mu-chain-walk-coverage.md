# ADR-0019 E2b: fixing the native-row coverage check's own blind spot

ADR-0019 Phase E box E2a landed a `native_call_unmodeled` counter to measure
the gap between the new `NativeMethodRow` catalog and what the real
`native_method_{0,1,2}arg` cascades actually recognize -- the box E2b is
supposed to drive to zero. Before adding rows, a fresh `MUTSU_VM_STATS=1`
sweep over the full `t/` suite (2996 files) found the counter itself was
over-counting: `Str x so` alone accounted for 20392 of 37904 total
unmodeled hits.

The root cause was in the check, not the cascades. `so`, `not`, and
`defined` are declared on `Any` and recognized by the shared, receiver-
type-agnostic arity-0 cascade arms (`dispatch_core_str`/`dispatch_core_coerce`,
tried unconditionally for every value in `methods_0arg/mod.rs`'s
`try_dispatch!` chain) -- but `Interpreter::record_native_row_coverage`
looked up a row only at the receiver's own concrete owner
(`dispatch_owner_name`, e.g. `Str`), never at `Any` where the row actually
lives. Every inherited-and-correctly-served call was flagged unmodeled.

The fix mirrors E4a's own resolver design: `record_native_row_coverage` now
walks the full `Interpreter::dispatch_owner_chain` and looks for a covering
row at *any* level, not just the first. Four rows were hand-added for the
`Any`/`Mu` owners that E2a's original probe never reached (it only covered
11 concrete-type owners with a `builtin_sample_value`, and there is no single
representative sample for an abstract owner like `Any`): `Any::so`,
`Any::not`, `Any::defined`, and `Mu::DEFINITE` (a quoted pseudo-method,
marked `SPECIAL` since it is a compiler-level construct rather than an
ordinary `.^methods`-visible method).

A fresh sweep confirmed the fix: `native_call_unmodeled` dropped from 37904
to 12154, a 68% reduction, with `Str x so` and all the `defined`/`so`
variants disappearing entirely from the breakdown. The remaining ~12k hits
(`Match`, `Pair`, `Seq`, `Array::list`/`item`, `FatRat`, exception types,
`RakuAST::*`) are genuinely missing per-owner rows -- the next E2b
sub-slices, following the design's expectation that this box subdivides
into several mechanical row-addition PRs.
