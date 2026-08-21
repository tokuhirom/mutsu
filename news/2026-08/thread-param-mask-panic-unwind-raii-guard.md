# `thread_redeclared_vars`/`thread_param_shadow_vars` masking is now RAII-guarded against panic unwinds

This closes out the last remaining item from the panic-unwind side-channel
state audit (`news/2026-08/panic-unwind-call-dispatch-state-raii-guards.md`):
`mask_thread_redeclared_params`/`unmask_thread_redeclared_params`
(`src/runtime/runtime_shared_vars.rs`) mask a named-sub or method parameter's
bare name out of the cross-thread shared-var lane
(`thread_redeclared_vars`/`thread_param_shadow_vars`) for the duration of its
call, so two unrelated routines that merely happen to share a parameter name
don't alias each other once a `start{}` thread has run (see
`t/thread-callee-param-does-not-clobber-caller.t`). The unmask used to be a
plain statement near the end of `call_compiled_function_named_inner`
(`src/vm/vm_call_named_inner.rs`) and the equivalent method-dispatch call site
(`src/vm/vm_method_dispatch.rs`) — a Rust panic (integer overflow, index OOB,
...) raised inside the call body unwinds straight past such a statement, only
`Drop` runs on unwind, leaving both masks in place for the rest of the
panicking thread's lineage.

## Fix: `ThreadParamMaskGuard`

Added to `src/vm/vm_call_state_guard.rs` alongside the sibling guards
(`StateScopeGuard`, `WhenMatchedGuard`, `MarkContextGuard`), following the
same "v3" disjoint-heap-allocation pattern that module documents (a raw
pointer taken directly into a field embedded in `Interpreter`'s own struct is
unsound — verified by `cargo miri test --lib gc::soundness_smoke` for the
earlier guards — so each guarded field must live in its own separate heap
allocation). The difference from the existing `Cell`-based guards: unmasking
removes a *specific set of names* (recorded in `ThreadParamMask`) from two
`HashSet<String>` fields, not a single scalar or a depth to truncate back to,
and a `HashSet` isn't `Copy`, so `Cell`'s get/set API doesn't fit it well.
`thread_redeclared_vars`/`thread_param_shadow_vars` are now
`Box<RefCell<HashSet<String>>>` instead of a plain `HashSet<String>` — same
disjoint-allocation property as `Box<Cell<T>>`, but `RefCell` keeps ordinary
`insert`/`remove`/`contains` available through `borrow`/`borrow_mut`. All
~20 direct read/write call sites across `src/runtime/` and `src/vm/` were
updated to the `.borrow()`/`.borrow_mut()` form; `ThreadParamMask` gained an
`unmask()` method that removes exactly its own recorded names from the two
sets given only the `RefCell`s (no `&Interpreter` needed), which is what lets
the guard's `Drop` call it through the two raw pointers it captured at
construction.

Both call sites (`call_compiled_function_named_inner`,
`call_compiled_method` in `vm_method_dispatch.rs`) now construct the guard right after masking
and explicitly `drop()` it at exactly the point the old manual
`unmask_thread_redeclared_params` call ran — preserving the normal-path
timing byte for byte, while also firing during a panic unwind that never
reaches that line, matching the pattern already established for
`state_scope_guard`/`when_matched_guard`/`pkg_guard` in the same functions.

## Verification

- Three new Rust unit tests in `src/runtime/runtime_shared_vars_tests.rs`
  construct the guard directly and inspect `Interpreter` state around a
  `std::panic::catch_unwind`:
  `thread_param_mask_guard_restores_on_panic_unwind` (the core fix — the mask
  is undone even when the guarded closure panics),
  `thread_param_mask_guard_restores_on_normal_drop` (non-panicking path still
  works), and `thread_param_mask_guard_does_not_disturb_an_ancestor_mask` (an
  ancestor frame's own mask on the same name survives an inner guard's drop,
  per `ThreadParamMask`'s "only remove what THIS mask added" contract).
  Confirmed all three fail without the fix (temporarily neutering the guard's
  `Drop` and re-running reproduced exactly the leak the ticket described,
  then the fix was restored and reverified).
- New end-to-end test `t/thread-param-mask-panic-unwind.t`: a panicking
  callee whose parameter shares a bare name with its caller's own parameter,
  called through a `start{}`/`await` boundary and caught by `try`, does not
  corrupt the caller's value; a later unrelated call sharing the same
  parameter name after such a panic still binds and returns correctly.
- `cargo test --lib` (862 tests) and `make test` (30681 tests) are clean
  except the pre-existing `t/compunit-can-install.t` environment artifact
  (assumes `/` is not writable, does not hold in this dev container —
  unrelated to this change).

## Investigation note: an end-to-end *corrupted-value* repro proved elusive

The ticket's own sketch anticipated needing a session to build and confirm a
repro; several were tried and none demonstrated an observable difference
before/after the fix at the Raku level, even with the guard's `Drop`
deliberately neutered for comparison:

- A direct closure capture of the shared bare name (`my $x; await start { $x
  = ... }`) is unaffected by the mask either way — `clone_for_thread` seeds
  the spawned thread's own env from the parent's *current* live value
  directly, bypassing the bare-name `shared_vars` store entirely for a
  genuinely lexically-captured write.
- A fresh, uninitialized `my $x;` inside the reader — meant to trigger the
  Nil-value shared-store fallback read in `vm_var_assign_local_get.rs` — masks
  itself via the ordinary "`my`-redeclaration" tracking
  (`vm_var_assign_set_local.rs`) before the read ever runs, regardless of any
  leaked parameter mask, so the fallback path never activates either way.
- The `outer`/`inner` shared-parameter-name pattern from
  `t/thread-callee-param-does-not-clobber-caller.t`, adapted so `inner`
  panics: `outer`'s own value survives because it is protected by `outer`'s
  *own* (non-panicking) mask, not by whatever `inner`'s panic did to state on
  the child thread it ran on — a leak there is invisible once that throwaway
  thread's `Interpreter` clone is discarded.

The guard is still applied defensively regardless — the underlying structural
gap (a manually-paired mask/unmask that a Rust panic can skip) is real and
worth closing on its own terms, per the same reasoning the earlier
side-channel-state audit used for its other seven guards. The Rust unit tests
above pin the exact mechanism precisely and deterministically, which turned
out to be a more reliable oracle than chasing a Raku-level symptom through
several layers of interacting masking/capture machinery.

## A separate finding along the way: parameter readonly-marks leak the same way

While hunting for a Raku-level repro, a **different**, single-threaded bug
surfaced by accident: a routine parameter's readonly mark (unrelated to
`thread_redeclared_vars`) also appears to leak past a panic-unwound call —
`sub f($x) { die-via-panic() }; try { f(1) }; $x = 2;` (a *different*,
unrelated `$x` merely sharing the bare name) fails with "Cannot assign to a
readonly variable" afterward, with no threading involved at all. This is a
distinct mechanism and root cause from the fix above, not yet traced to its
exact call sites, so it's filed separately as
`todo/tickets/readonly-param-mark-leaks-on-panic-unwind.md` for a future
session.
