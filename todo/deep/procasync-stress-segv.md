# `roast/S17-procasync/stress.t` segfaults the interpreter (rare, CI-only so far)

`roast/S17-procasync/stress.t` is whitelisted and normally green, but on CI run
[30590633128](https://github.com/tokuhirom/mutsu/actions/runs/30590633128) (job 91031987260, PR
#5582, 2026-07-30) it killed the interpreter with a signal:

```
roast/S17-procasync/stress.t   (Wstat: 11 (Signal: SEGV) Tests: 23 Failed: 0)
  Non-zero wait status: 11
  Parse errors: Bad plan.  You planned 24 tests but ran 23.
```

This is **not** a failed assertion. `Failed: 0` with `Wstat: 11` means the top-level `mutsu` process
running the `.t` file itself died of SIGSEGV — the 23 tests that did run all passed, and test 24
never emitted a result. A crash-class failure in the interpreter is worth a real root-cause, so it
is recorded here rather than quarantined in `flaky-tests.txt`.

## Where it dies

Twenty-three tests had completed, so the crash lands in the file's last block — the
rakudo#3299 regression test, which is itself titled *"No memory corruption when starting many
Proc::Async instances"*:

```raku
is_run ｢
  my $prog   = $*DISTRO.is-win ?? 'cmd'   !! 'cat';
  my @target = $*DISTRO.is-win ?? «/c ""» !! '/dev/null';

  for ^1200 {
    my $proc = Proc::Async.new($prog, |@target);
    react {
        whenever $proc.start { done }
        whenever signal(SIGTERM) {}
        whenever Promise.in(5) {}
    }
  }

  print 'pass'
｣, {:out<pass>, :err(''), :0status}, 'No memory corruption when starting many Proc::Async instances';
```

Note the shape: 1200 iterations, each building a fresh `react` block with a `signal(SIGTERM)` tap
and a `Promise.in(5)` timer alongside the process. That is a lot of churn through the signal
handling, timer, and process-reaping paths, with taps being created and torn down 1200 times.

Two caveats on locating the fault, both of which need checking before assuming the crash is in the
child:

- `is_run` runs its code in a **subprocess**. A SIGSEGV in that child would normally surface as a
  failed `status`/`err` assertion, not as `Wstat: 11` on the `.t` file. So the wait status points at
  the *parent* — the mutsu process running `stress.t` — crashing while it drove `is_run`. Confirm
  which process actually faulted before chasing the child's code.
- The preceding block (`doesn't-hang`, 100 `Proc::Async` in a `react`) also touches the same
  machinery, and a delayed fault from it could land here.

## Why it is filed as deep

The root cause is unknown and the failure class is memory unsafety, which puts it near two open
areas rather than in a self-contained slice:

- PLAN.md §2's **GC soundness tail** (the remaining unsafe raw-pointer writes). A rare SIGSEGV under
  heavy allocate/tear-down churn is exactly the shape that tail predicts.
- The concurrency scheduling work — `signal()` taps, `Promise.in` timers, and process reaping all
  run off the main thread.

Attributing it correctly probably needs a core dump or an ASan/valgrind run rather than a code read,
and the fix may well be an ADR-level decision about the raw-pointer sites.

## Reproduction status

Rare. Not reproduced yet outside the one CI run:

- The failing job was re-run and needs its result recorded here once known.
- The last six `main` CI runs before it (30590224445, 30589790880, 30588793948, 30556546586,
  30556056462, 30551748477) show no SIGSEGV for this file.
- Local repro attempts should use the **release** build, since that is what `make roast` runs:
  `cargo build --release && MUTSU_FUDGE=1 prove -e target/release/mutsu roast/S17-procasync/stress.t`,
  looped. Under CPU contention (`-j4` or heavier) is the likelier repro condition, matching CI.

The PR it appeared on (#5582) deleted an unreferenced Raku test-helper module and edited PLAN.md —
no Rust changes at all — so the crash cannot have been introduced by that diff.

## What to do when picking this up

1. Get a fault address and a backtrace: run the inner 1200-iteration program directly (not through
   `is_run`) in a loop under `rust-gdb -batch`, or enable core dumps and inspect the core.
2. If it will not reproduce standalone, run the whole file under load and check whether the parent or
   the `is_run` child faulted.
3. Only quarantine it in `flaky-tests.txt` if the evidence standard in `docs/flaky-test-policy.md` is
   actually met — and note that a *crash* is a poor quarantine candidate, since retrying hides a real
   memory-safety bug rather than tolerating benign non-determinism.
