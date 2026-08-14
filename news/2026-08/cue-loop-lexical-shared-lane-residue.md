# A loop-redeclared lexical mutated by a `cue` callback carried the previous iteration's value forward

`for 1..6 -> $round { my $a = 0; my $c = $*SCHEDULER.cue({ cas $a, {.succ} }, :every(0.1)); sleep 1; $c.cancel; say $a }`
printed accumulating totals (`10, 20, 30, 41, 52, 62`) instead of resetting to
roughly 10 every round, as real Raku does. The `todo/tickets` entry (filed
2026-08-05) traced this to the bare-name atomic `cas` lane and speculated the
fix needed a binding-generation component on the atomic key — ADR-0010/Track-B
adjacent design work.

Re-investigating with `rust-gdb` breakpoints and a temporary env-gated
instrumented build (`MUTSU_DEBUG_CAS`) found a simpler, more general root
cause: `env` only mirrors a local's current value when something in the SAME
frame reads it BY NAME (`compute_needs_env_sync`'s per-store env-write gate).
A loop-body local touched only via a NAME-KEYED `cas` call inside a NESTED
closure never earns that mirror — `cas` is deliberately excluded from the
free-var-write fold that would otherwise force it (`compiler/expr_call.rs`'s
`n != "__mutsu_cas_var"` guard, there so `cas`'s cross-thread behavior can ride
the name-keyed lane instead of a per-binding cell). So `env["a"]` kept
whatever a PREVIOUS round's finished cue synced back into it, and the next
round's `my $a = 0` only reset the LOCAL SLOT, not that stale `env` mirror.
When the next `.cue()` call cloned the interpreter for its worker thread
(`clone_for_thread_for_block`), the clone's `env` snapshot carried the stale
value forward as the starting point for the fresh `cas` counter.

The reason `start`/`await start { ... }` was NOT affected turned out to be an
accident, not evidence of a sound cell-based mechanism: `vm/vm_call_func_ops.rs`
had a narrow, function-name-hardcoded `if name == "start" { self.sync_env_from_locals(code) }`
that flushed every local into `env` right before a **bareword** `start` call
spawned its thread. Any OTHER thread-spawning construct that reaches a spawn
via `clone_for_thread`/`clone_for_thread_for_block` — a scheduler `.cue()`
method call, `Promise.start`, `Thread.start`, a `supply`/`react` `whenever`
worker — is a METHOD call or native dispatch, never took that path, and so
inherited the same staleness `cue` exhibited.

Fixed by moving the "sync locals to env before a spawn" step out of the
per-callee special case and into `clone_for_thread_excluding` itself (the one
function every one of those constructs funnels through), using the live
frame's `current_code` pointer — the same lifetime invariant
`atomic_scalar_cell` already relies on — to reach the calling frame's
`CompiledCode` for the sync. This fixes `.cue()`, `Promise.start`,
`Thread.start`, and any other spawner uniformly, and the two now-redundant
`if name == "start"` special cases in `vm_call_func_ops.rs` were removed.

Pinned by `t/cue-loop-lexical-reset.t`, which reproduces the exact `for`-loop
shape and asserts each round's `cas` count stays within a small multiple of
the first round's instead of growing round over round.
