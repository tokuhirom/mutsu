# `Lock::Async` is missing `protect-or-queue-on-recursion` and `with-lock-hidden-from-recursion-check`

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Lock/Async.rakudoc:162,202`).

## Repro

```raku
my Lock::Async $lock .= new;
my Int         $count = 0;

$lock.protect-or-queue-on-recursion({
    $count++
});
say $count;
```

- `raku`: runs (result depends on the rest of the doc's example; the point is the
  method exists and dispatches).
- `mutsu` (`target/debug/mutsu`): dies immediately with
  `No such method 'protect-or-queue-on-recursion' for invocant of type 'Lock::Async'`.

The doc's second example (`Type/Lock/Async.rakudoc:202`) hits the same missing method,
plus also calls `.with-lock-hidden-from-recursion-check`, which is equally
unimplemented.

## Root cause

Per `raku-doc/doc/Type/Lock/Async.rakudoc`, `Lock::Async` documents two methods beyond
the basic `.lock`/`.protect`:

- `.protect-or-queue-on-recursion(&code)` — like `.protect`, but detects when the
  calling thread is already inside a `.protect-or-queue-on-recursion` call on the same
  lock (recursion) and, in that case, queues `&code` to run after the outer call
  finishes (returning a `Promise`) rather than deadlocking.
- `.with-lock-hidden-from-recursion-check(&code)` — runs `&code` under the lock without
  it counting toward the above recursion detection.

Neither is implemented on mutsu's `Lock::Async`.

## Affected files (starting point)

- Wherever `Lock::Async`'s existing methods (`.lock`, `.protect`) are implemented (grep
  for `"Lock::Async"` in `src/runtime/`) — add these two alongside them. This is a
  genuinely nontrivial concurrency feature (per-thread recursion tracking + queued
  continuation), not a one-line stub.
