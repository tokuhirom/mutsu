# `Lock::Async` gained `protect-or-queue-on-recursion` and `with-lock-hidden-from-recursion-check`

`Lock::Async` is deliberately **not** re-entrant, so `.protect` deadlocks when the caller chain
already holds the lock. Rakudo's answer is a pair of recursion-aware methods
(`Type/Lock/Async.rakudoc`); mutsu had neither, and both died with
`No such method '...' for invocant of type 'Lock::Async'`.

## Semantics, as measured (raku v2026.06) rather than as the doc's examples read

The documentation's two worked examples are racy — their stated `# OUTPUT: 5` / `# OUTPUT: 2` depend
on a `.then` callback winning a race against the mainline, and real raku prints `4` and nothing
respectively. The actual, reproducible contract is:

- `protect-or-queue-on-recursion(&code)`
  - **not** recursing (lock free, or held by something outside this caller chain): behaves exactly
    like `.protect` and returns an undefined `Any` — not the block's value, and not `Nil`
    (`$r<> =:= Any` is `True`, `=:= Nil` is `False`).
  - recursing (this caller chain already entered the same lock through this method): **queues**
    `&code` and returns a `Promise`, kept with the queued block's own return value. The queued block
    runs only after the outer call released the lock — a nested call observably logs
    `outer-start, outer-end, inner`.
  - the outer call returns `Any` either way; it does not propagate the inner `Promise`.
  - the recursion list follows the **caller chain**, not the lock's owner: a nested plain `sub` call
    still counts as recursion.
- `with-lock-hidden-from-recursion-check(&code)` runs `&code` immediately with this lock removed
  from the recursion list and returns the block's own value. It **never acquires the lock** —
  pinned by running it from a pool thread while the mainline held the lock: it completed instead of
  blocking.

## Implementation

A new `runtime/lock_async_recursion.rs` holds both methods, intercepted in the VM next to the
existing `.protect` fast path (`vm_call_method_ops.rs` / `vm_call_method_mut_ops.rs`) rather than
being added to the `runtime/methods.rs` slow path.

Two new `Interpreter` fields model the caller chain:

- `lock_async_recursion: Vec<u64>` — lock ids entered through `protect-or-queue-on-recursion`. A
  spawned thread's `clone_for_thread` starts it empty, which *is* the documented "the lock was
  locked by something outside the caller chain" case.
- `lock_async_deferred: Vec<(u64, Value, SharedPromise)>` — blocks queued by a recursive call,
  drained FIFO by the outermost frame once it has released the lock (a queued block may itself
  queue more; the same loop picks those up). It lives on the interpreter rather than in a
  thread-local specifically so `visit_roots` enumerates the queued `Value`s — a `Gc`-managed closure
  sitting in an unscanned thread-local between queueing and draining would be collectible.

Both new methods take and release the lock through the same `acquire_lock` / `release_lock` pair
`.protect` uses, and bracket the call with `enter_critical_section` / `leave_critical_section` so
shared-scalar publication is unchanged.

Pinned by `t/thread-and-lock-async.t` (subtests 16–27), which passes unchanged under both `raku` and
`mutsu`.
