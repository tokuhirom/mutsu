# `Thread.start({...})` never produces output — the main program exits before the thread runs

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/concurrency.rakudoc:709`).

## Repro

```raku
my $thread = Thread.start({ for  1 .. 10  -> $v { say $v }});
```

(no explicit `.join`/wait — the script ends immediately after starting the thread)

- raku: reliably prints `1` through `10` before the process exits (verified 3/3 runs) — raku's
  main thread waits for spawned non-daemon `Thread`s to finish before the process terminates.
- mutsu (`target/debug/mutsu`): produces **no output at all** (verified 3/3 runs, deterministic —
  not a race/flake) — the main program exits immediately, and the spawned thread's work is never
  observed to run (or runs after the process has already begun tearing down and its output is
  lost).

Adding an explicit `.join` after `.start` makes mutsu produce correct output (`1`..`10`), so the
underlying thread-spawn-and-run mechanism itself works — the bug is specifically that mutsu's
process-exit path does not wait for outstanding non-daemon threads the way raku's does.

## Analysis

Per Raku's threading model, `Thread` objects spawned via `.start` are (by default) not daemon
threads, so the runtime should keep the process alive until they complete, even if the main
program's own statements have all finished. mutsu appears to exit the process as soon as the main
thread's statements complete, without joining/waiting on any still-running `Thread`s it spawned.

## Affected files (starting point)

- Wherever `Thread.start` spawns the underlying OS thread (concurrency runtime module)
- `src/main.rs` / process-exit path — needs to join outstanding non-daemon threads before the
  process actually terminates
