# `Lock`/`condition`/`Thread.start` signal-wait deadlocks when the lock/condition/topic variables are declared inside a loop body

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Lock/ConditionVariable.rakudoc:69`).

## Root cause hypothesis

A `Lock`, its `.condition`, and a shared counter variable, all declared with `my` and used from
a spawned `Thread.start` closure plus a `.protect`/`.wait`/`.signal` rendezvous, works correctly
at the **top level** / inside a bare block:

```raku
{
    my $lock = Lock.new;
    my $cond = $lock.condition;
    my $done = 0;
    Thread.start({ sleep 0.1; $lock.protect({ $done = 1; $cond.signal; }); });
    $lock.protect({ $cond.wait({ $done == 1 }); });
    say "done";
}
```

This prints `done` and exits (verified with `target/debug/mutsu`). But wrapping the **exact
same code** inside a `for`-loop body (even a single-iteration `for 1..1 -> $iter { ... }`) or a
`while`-loop body hangs forever — `$cond.wait` never sees `$done` become `1`, even though the
`Thread.start` closure still runs and calls `$cond.signal`:

```raku
for 1..1 -> $iter {
    my $lock = Lock.new;
    my $cond = $lock.condition;
    my $done = 0;
    Thread.start({ sleep 0.1; $lock.protect({ $done = 1; $cond.signal; }); });
    $lock.protect({ $cond.wait({ $done == 1 }); });
    say "done";
}
```

- `raku`: prints `done` and exits normally.
- `mutsu` (`target/debug/mutsu`): hangs indefinitely (verified `timeout 10` → exit 124, both for
  `for` and `while` loop bodies).

This strongly suggests a loop-body-scoped local variable (`$lock`/`$cond`/`$done`) captured by
the `Thread.start` closure is not sharing the same underlying cell/state as the loop-body-local
copy that `$cond.wait`'s predicate closure reads — i.e. the spawned thread's closure and the
waiting thread's closure end up looking at two different `$done` cells (or the `$lock` used by
`.protect` in the spawned closure isn't the same `Lock` object instance), so the signal never
reaches / is never observed by the waiter. This matches the general class of dual-store
(locals-vs-env) desync bugs this codebase has hit before with closures over loop-scoped
variables (see `docs/doc-diff-backlog.md`'s "Sigilless-parameter scoping" deferred cluster for
a related loop-scope leak, and the project memory note on "env-writeback campaign: the
state-sync bug shape" for the general bug shape), but this is a fresh, undiagnosed instance —
not yet confirmed to share the exact same root cause.

## Minimal repro

```raku
for 1..1 -> $iter {
    my $lock = Lock.new;
    my $cond = $lock.condition;
    my $done = 0;
    Thread.start({ sleep 0.1; $lock.protect({ $done = 1; $cond.signal; }); });
    $lock.protect({ $cond.wait({ $done == 1 }); });
    say "done";
}
```

- `raku`: prints `done`.
- `mutsu` (`target/debug/mutsu`): hangs (deterministic, reproduces every run; also reproduces
  with a `while` loop instead of `for`).

## Affected files (starting point)

- Loop-body compilation (for/while loop-scoped local allocation) and how loop-body closures
  capture `my`-declared locals — `src/compiler/stmt.rs` (loop compilation),
  `src/vm/vm_control_ops.rs` (loop execution), and the `Lock`/`condition`/`Thread` runtime
  implementation (concurrency-related `runtime/` submodules) for how `.protect`/`.wait`/
  `.signal` read/write their captured variables across threads.
