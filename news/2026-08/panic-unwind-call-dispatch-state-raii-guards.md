# Panic-unwind recovery: call-dispatch side-channel state is now RAII-guarded

A prior session fixed the main panic-recovery gap: when a Rust panic (integer
overflow, index OOB, capacity overflow, ...) raised deep inside a nested
closure/sub call is caught at a `try`/`EVAL` boundary and converted to a
catchable `X::AdHoc`, `Interpreter::recover_call_frames_after_panic` pops
every `call_frames` entry pushed since the boundary so `locals`/`upvalues`/
`env`/the value stack all correctly unwind back to it. That fix is pinned by
`t/panic-recovery-call-frames.t`.

What remained open: several pieces of call-dispatch state are mutated and
restored **outside** `call_frames`, as a plain Rust local in the dispatch
function itself (`call_compiled_closure`, `call_compiled_closure_with_topic`,
`call_compiled_function_named_inner`) -- most visibly `current_package`,
switched to the callee's declaring package for the call's duration and
restored with a `self.set_current_package(saved)` statement near the
function's end. A Rust unwind skips straight past such a statement (only
`Drop` runs on unwind), so `current_package` was left as whatever package the
panicking callee happened to run in. Concretely: `t/vm-panic-boundary.t` under
`MUTSU_REAL_TEST=1` (the vendored real `Test.rakumod`) ran only 6/9 subtests --
after the `call_frames` fix, execution correctly continued past the panic, but
the very next unqualified call inside `dies-ok` (`proclaim(...)`, a `unit
module Test;`-scoped sub) failed with `Unknown function: proclaim`, because
`current_package` had been left as the panicking user block's own package
(`GLOBAL`) instead of restored to `Test`.

## Fix: RAII guards instead of manual restore statements

Every manually-saved-and-restored piece of state found by auditing the three
dispatch functions was converted to an RAII guard whose `Drop` restores it --
so the restore runs on both normal return AND a Rust panic unwind, with no
interaction needed with `recover_call_frames_after_panic`'s pop-loop:

- **`current_package`** (`call_compiled_closure_with_topic`,
  `call_compiled_function_named_inner`): `CurrentPackageGuard`
  (`src/runtime/accessors_stack.rs`). `current_package`/`current_package_sym`
  are already interior-mutable (`Arc<RwLock<String>>` / `Arc<AtomicU32>`, the
  same handles `set_current_package_shared` uses for the `&self` regex
  matcher), so the guard just holds cloned `Arc` handles and writes through
  them directly on drop -- no `&mut Interpreter` borrow needed, fully safe.
- **Pragma state** (`use fatal`/`use strict`/`use newline`/`use
  MONKEY-TYPING`) in `call_compiled_closure`: `PragmaGuard`
  (`src/vm/vm_call_state_guard.rs`). This function is short enough that every
  subsequent `self.*` access was rewritten to go through the guard's
  `Deref`/`DerefMut` instead, so it holds a plain safe `&'a mut Interpreter`
  borrow -- no raw pointer needed here either.
- **`state_scope_id`** (both `call_compiled_closure_with_topic` and
  `call_compiled_function_named_inner`) and **`when_matched`** (the latter
  only): `StateScopeGuard`/`WhenMatchedGuard`
  (`src/vm/vm_call_state_guard.rs`). Unlike `current_package`, these are
  plain fields with no interior-mutable backing, and the two call sites are
  too large to route every subsequent `self.*` call through a safe
  `DerefMut`-based guard without rewriting hundreds of unrelated lines. Each
  guard instead holds a raw `*mut Interpreter` captured at construction time,
  with a documented safety invariant (the `Interpreter` outlives the guard,
  does not move, and is not re-entered through a different `&mut` alias while
  the guard is alive -- true for every call site, each an ordinary `&mut
  self` method holding one exclusive borrow for its whole body).

Each guard is constructed immediately after mutating the field (so it
captures the pre-mutation value to restore) and explicitly `drop()`-ed at
exactly the point the old manual restore statement ran -- preserving the
normal-path timing/semantics byte-for-byte, while ALSO firing during a panic
unwind that never reaches that line.

## Two further side-channel stacks, found by re-auditing the pop-loop itself

Auditing `recover_call_frames_after_panic`'s own scope turned up three more
stacks pushed/popped **separately** from `call_frames`, so a caught panic left
them holding every entry pushed since the boundary too, for the same reason
`call_frames` needed the original fix:

- `caller_env_stack`/`callframe_stack` (the `CALLER::`/backtrace machinery;
  always pushed/popped together, so one depth recovers both)
- `let_saves` (the `let`/`temp` restore-on-exit log)
- `test_assertion_line_stack` (Test-module failure-line bookkeeping)

`recover_call_frames_after_panic` now also truncates all three back to their
boundary-entry depths. Each is a bare truncate (discard), not a
pop-with-writeback: the frame whose state those entries were meant to act on
no longer exists by the time recovery runs, so replaying their normal
pop-time effect (e.g. `pop_caller_env_with_writeback`'s dynamic-variable
writeback, or `restore_let_saves`'s env write) would write stale values into
the now-restored caller state instead of cleaning it up.

## Verification

- New regression test `t/panic-recovery-package-state.t` (2 subtests, no
  `MUTSU_REAL_TEST` needed): one exercises `call_compiled_function_named_inner`'s
  guard directly (a package-qualified sub panics, `try` catches it, an
  unqualified sibling call afterward must still resolve in the ORIGINAL
  package); the other exercises `call_compiled_closure_with_topic`'s guard (a
  closure created in one package panics when invoked from a named sub in
  another package). Both reproduced the "Unknown function" failure on the
  pre-fix code and pass after the fix.
- `t/vm-panic-boundary.t` under `MUTSU_REAL_TEST=1` (the vendored real
  `Test.rakumod`, which is what originally surfaced this bug) now passes
  **9/9**, up from 6/9.
- `t/panic-recovery-call-frames.t` and the default (non-`MUTSU_REAL_TEST`)
  `t/vm-panic-boundary.t` still pass, confirming no regression to the
  already-shipped `call_frames` recovery.
- `cargo test` (854+ unit/integration tests) and a full `prove t/` run (30560
  tests) are clean except one pre-existing environment artifact unrelated to
  this change (`t/compunit-can-install.t` assumes `/` is not writable by the
  test user, which does not hold in this dev container).

## Known remaining residue (not fixed here)

`mask_thread_redeclared_params`/`unmask_thread_redeclared_params`
(`src/runtime/runtime_shared_vars.rs`) mask/unmask specific variable names in
two process-wide `HashSet<String>`s (`thread_redeclared_vars`,
`thread_param_shadow_vars`) rather than pushing/popping a simple stack
depth, so they need a different fix shape (recording exactly which names a
call's mask inserted, for precise removal on panic recovery) than the
depth-truncation approach used here. Only relevant when `shared_vars_active`
(cross-thread `start`/`Promise`/shared-var programs). Filed as
`todo/tickets/thread-param-mask-leaks-on-panic-unwind.md`.
