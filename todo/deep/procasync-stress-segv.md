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

**Not reproduced outside that single CI job.** Measured 2026-07-30:

- **Re-running the very same failing job passed** (12m03s, whole `test` job green). So the crash did
  not survive a retry on an identical commit.
- **Local: 22/22 clean.** Release build (`cargo build --release`), `MUTSU_FUDGE=1 prove -e
  target/release/mutsu roast/S17-procasync/stress.t` — 6 serial runs, then 16 more as 4 concurrent
  instances × 4 rounds to imitate CI's contention. Every run exited 0.
- The six `main` CI runs preceding it (30590224445, 30589790880, 30588793948, 30556546586,
  30556056462, 30551748477) show no SIGSEGV for this file.

So the observed rate is roughly 1 in several dozen runs *on CI hardware*, and 0 in 22 locally — low
enough that a repro loop is not the way in. Anyone picking this up should expect to need a core dump
from CI, or a sanitizer build, rather than a local reproduction. Use the **release** build when
trying anyway, since that is what `make roast` runs.

The PR it appeared on (#5582) deleted an unreferenced Raku test-helper module and edited PLAN.md —
no Rust changes at all — so the crash cannot have been introduced by that diff.

## What to do when picking this up

> **Superseded in part by the 2026-08-19 investigation below.** Step 1 (a local repro loop under
> `rust-gdb`) has now been tried across four configurations and ~96 runs with no fault — see §3.
> Step 2's advice ("prefer a CI-side approach") is right, but the net it points at turns out to be
> muffled in three separate places — see §2 and §5, which are the actual work to do.

1. Get a fault address and a backtrace: run the inner 1200-iteration program directly (not through
   `is_run`) in a loop under `rust-gdb -batch`, or enable core dumps and inspect the core.
2. It very likely will *not* reproduce standalone (see above) — so prefer a CI-side approach rather
   than burning local cycles on a loop that has already come up empty 22 times.
   **The cheapest version of that has landed**
   ([news](../../news/2026-07/crash-report-on-fatal-signal.md), `src/crash_report/`): a fatal-signal
   handler now writes `tmp/crash/<pid>.txt` with the signal, fault address, pid, **argv** and a
   backtrace, and CI prints it and uploads it as the `crash-reports` artifact. So the next occurrence
   answers the parent-vs-`is_run`-child question below by itself — check the artifact first. Anything
   heavier (core dumps, a sanitizer job) is only worth building once that report says it is needed.
3. Only quarantine it in `flaky-tests.txt` if the evidence standard in `docs/flaky-test-policy.md` is
   actually met — and note that a *crash* is a poor quarantine candidate, since retrying hides a real
   memory-safety bug rather than tolerating benign non-determinism.

## Investigation, 2026-08-19 — evidence, exclusions, and the design that follows

This session did **not** root-cause the crash. It is written up honestly as such: no fault address,
no backtrace, no local reproduction. What it did produce is (a) a much narrower suspect surface, so
the next person does not repeat the audit, (b) a genuine, previously unknown crash of the same class
found in the CI artifact history, and (c) the reason the crash-report net that was supposed to catch
the recurrence has caught nothing — which is where the actionable work is.

No ADR is proposed. Nothing below is a costly-to-reverse architectural decision; every remedy is a
well-scoped diagnostics slice, so the design lives here rather than in `docs/adr/`.

### 1. The crash-report net has been live for 19 days and has not caught a recurrence

`src/crash_report/` landed 2026-07-31 (`f783aec99`), **one day after** the only observed occurrence
(2026-07-30), so the original crash necessarily predates it.

Every `ci.yml` run with `conclusion: failure` whose artifacts are still retained was swept
(artifacts expire after 7 days, so the window is 2026-08-12..18: 61 runs × up to three crash-report
artifacts each — `crash-reports`, `gc-stress-crash-reports`, `jit-stress-crash-reports`).
That yielded **106 crash reports**:

- **105** are one and the same deliberate fault:
  `mutsu -e 'use NativeCall; sub strdup(int64) is native(Str) {*}; strdup(0)'` — a NativeCall test
  that segfaults on purpose. Every job in every run produces it. It is noise, and it is *why* nobody
  reads this artifact.
- **1** is genuine, and it is not this ticket's file — see below.
- **0** are `roast/S17-procasync/stress.t`, in either the parent or an `is_run` child.

So the parent-vs-child question this ticket poses is still open, and will stay open until the crash
recurs on a build that both writes a report and surfaces it.

### 2. The one genuine crash in the window: heap corruption in `advent2014-day05.t`, silently retried away

CI run [32116354874](https://github.com/tokuhirom/mutsu/actions/runs/32116354874) wrote
`tmp/crash/49110.txt`:

```
signal: 6 (SIGABRT)          si_code: -6
pid: 49110   tid: 49127      thread: mutsu
argv: target/release/mutsu roast/integration/advent2014-day05.t
  0: mutsu::crash_report::report::write_report
  1: mutsu::crash_report::handler::handler
  3: pthread_kill
  4: gsignal
  5: abort
  6..9: <unknown>            (libc +0x297b6, +0xa90d5, +0xab46c, +0xae101)
```

Frames 6-9 are inside glibc's allocator (`__libc_message` → `malloc_printerr` and the `malloc`/`free`
consistency checks that call it). That is **the allocator aborting on a corrupted heap**, on a
non-main thread (`tid != pid`) — i.e. genuine memory unsafety in the interpreter, the same failure
class as this ticket, differing only in which check tripped first (glibc's rather than the MMU's).

It was invisible for three compounding reasons, and all three also apply to a recurrence of *this*
ticket's SIGSEGV:

1. `roast/integration/advent2014-day05.t` is quarantined in `flaky-tests.txt`.
   `scripts/flaky-retry.sh` re-ran it, the retry passed, and the roast log for that very run reads
   `roast/integration/advent2014-day05.t ..................... ok`. The job failed for an unrelated
   reason.
2. `flaky-retry.sh` treats a signal death exactly like an assertion failure. A child killed by a
   signal surfaces as `rc = 128 + signum` (134 for SIGABRT, 139 for SIGSEGV) and the script's only
   test is `[ $rc -eq 0 ]`. Meanwhile `flaky-tests.txt`'s own preamble says *"Crashes, wrong answers,
   and anything whose root cause is unknown are never quarantined; they are bugs to fix."* The
   mechanism is currently laundering a memory-corruption crash into a green test.
3. `.github/workflows/ci.yml`'s "Crash reports" steps are `if: failure()`, and
   `scripts/report-crash-reports.sh` deliberately always exits 0 because *"the crash itself has
   already failed the job that produced it"*. Reason 2 breaks that assumption: after a retry the
   crash has **not** failed anything. So on a run that goes green because of a retry, the report is
   written and uploaded but never printed into the job log — and the job log is retained far longer
   than the 7-day artifact.

### 3. Reproduction attempts — all negative

Built with `cargo build --profile profiling` (release-optimised, with debuginfo, so any fault would
have produced a symbolised backtrace). 12-core machine.

| What | Configuration | Runs | Result |
|---|---|---|---|
| Inner 1200-iteration program alone (`tmp/procasync-stress.raku`) | default | 10 rounds × 6 concurrent = 60 | 60/60 exit 0, `pass` |
| Full `roast/S17-procasync/stress.t` | default | 6 × 4 = 24 | 24/24 clean, 24/24 subtests each |
| Full `roast/S17-procasync/stress.t` | `MUTSU_GC=on MUTSU_GC_EVERY_CANDIDATE=1024 MUTSU_GC_VERIFY=1` (the gc-stress job) | 4 × 3 = 12 | 12/12 clean, zero `VERIFY FAIL` |
| `roast/integration/advent2014-day05.t` (the §2 crash) | `MUTSU_JIT=on MUTSU_JIT_THRESHOLD=2` (the jit-stress job) | 8 × 3 = 24 | 24/24 clean |

That is ~72,000 `Proc::Async` spawns in the standalone loop alone, plus 36 full-file runs, with no
fault. Combined with the ticket's earlier 22 clean runs: **a local repro loop is not the way in, and
the collector's own `MUTSU_GC_VERIFY` invariant checks do not fire on this workload either.** Do not
spend another session on it.

### 4. Ruled out by audit — do not redo this

- **Thread/GC registration.** Every thread the `Proc::Async` path creates is a registered GC mutator
  via `builtins_system::spawn_gc_helper_thread` / `spawn_user_thread` (which raise
  `enter_mutator_worker` parent-side, `preregister_worker_quiescent` to close the birth window, and
  drop through a panic-safe `WorkerGuard`): the child-wait/promise thread, the stdout and stderr
  reader threads, the stdin supply pump (`src/runtime/native_proc_async.rs`), the signal reader
  (`src/runtime/signal_watcher.rs`), the shared timer driver
  (`src/runtime/native_methods/interval_timer.rs`), the react driver (`src/vm/vm_react_loop.rs`) and
  the worker pool (`src/runtime/worker_pool.rs`, which delegates to `spawn_user_thread`). Their
  blocking waits are wrapped in `gc::block_quiescent`. So this is **not** a
  `gc-survivor-purple-verify-violation`-class unregistered-mutator bug.
- **The single raw `std::thread::spawn` in the runtime**, `src/runtime/native_proc_async.rs:385`, is
  the stdin byte-writer. Its captured state is a `Vec<u8>` and an `Arc<Mutex<ChildStdin>>` — no `Gc`
  value — so leaving it unregistered is correct (registering it would starve stop-the-world on a
  pipe write that can block forever).
- **`Promise.in(5)` does not spawn a thread per timer.** It registers on the shared deadline heap
  (`interval_timer::register_once`), driven by one long-lived registered thread. So the 1200
  five-second timers do not create 1200 threads, and thread exhaustion is not the mechanism.
- **A plain cross-thread refcount race is impossible by construction**: `Gc<T>` is backed by `Arc`
  with an `AtomicUsize` strong count (`src/gc/gc_ptr.rs`).
- **User-level container data races are not the mechanism.** `tmp/race-array.raku` (4 `start`
  threads × 20,000 `@a.push` / `%h{...} = ...` on the *same* shared `@a`/`%h`) completes with exactly
  80,000 / 20,000 elements and never faults — the shared-lexical store serialises them.
- **`src/runtime/signal_watcher.rs` never unregisters.** `register_signal` pushes a
  `SignalRegistration` (a `Value` plus a `SupplySender`) into a process-global map and nothing ever
  removes it, so this test leaks 1200 entries and reinstalls the SIGTERM handler 1200 times. That is
  a real leak worth its own ticket, but it is **not** the crash: the collector needs no root
  enumeration (`src/gc/stw.rs`: *"Bacon-Rajan trial deletion needs no remote root enumeration —
  quiescence alone makes the scan sound"*), and the retained `Arc` handle keeps the node's strong
  count above zero, so a leaked registration keeps a node *alive*, never prematurely freed.

### 5. Root cause of the *diagnosis* failure, and the design

The crash report has a `thread:` field precisely so a report names the culprit subsystem. It is
useless today: `runtime::thread_compat::spawn_thread` builds every thread with
`std::thread::Builder::new()` and **never calls `.name()`**, so no thread ever calls
`pthread_setname_np` and every one of them inherits the process comm. Both real reports in the
window duly read `thread: mutsu` with `tid != pid` — "it died on some thread, we cannot say which".
For a crash whose entire difficulty is *which of a dozen background threads owns the fault*, that is
the single most valuable missing bit.

Four slices, in priority order. All are small and none needs an ADR.

1. **Name every thread** (`src/runtime/thread_compat.rs`). Give `spawn_thread` a `&'static str` and
   pass it through `spawn_registered_thread` / `spawn_gc_helper_thread` / `spawn_user_thread`. Linux
   truncates the comm to 15 bytes, so use short names: `mutsu-main`, `proc-wait`, `proc-out`,
   `proc-err`, `proc-in`, `signal-rd`, `timer`, `react`, `pool`, `sock-async`, `sock-conn`,
   `io-path`. Roughly a dozen call sites (`git grep -n 'spawn_gc_helper_thread(\|spawn_user_thread('`).
   After this, the next occurrence's `thread:` line alone says whether the fault is in the reaper,
   a reader, the timer, the react driver, or the pool.
2. **Stop retrying a signal death** (`scripts/flaky-retry.sh`). After `rc=$?`, treat `rc -ge 128` as
   fatal: log it, print the attempt's output, exit `$rc` immediately with a
   `# flaky-retry: <file> died of signal N -- NOT retried (see docs/flaky-test-policy.md)` comment.
   This is not a new policy, it is the enforcement of the one `flaky-tests.txt` already states.
   Record it in `docs/flaky-test-policy.md` too. Note that this alone would have turned §2's abort
   into a red CI on the day it happened.
3. **Surface crash reports on green runs** (`.github/workflows/ci.yml`). Flip the three "Crash
   reports" steps from `if: failure()` to `if: always()` so the report reaches the job log (retained
   far longer than the 7-day artifact), and have `scripts/report-crash-reports.sh` **fail the job**
   when a report's `argv:` is not on a small allowlist. The allowlist needs exactly one entry today:
   the deliberate `strdup(0)` NativeCall probe (105 of the 106 reports in the window). Without that
   filter the signal is 99% noise and nobody will ever read it — which is exactly what happened.
4. **Re-measure the `advent2014-day05.t` quarantine.** Its `flaky-tests.txt` reason attributes the
   failures to CPU-contention timing; §2 is hard evidence that at least one of them was heap
   corruption. Per the ledger's own bar the entry should be pulled (or at minimum re-justified) once
   slice 2 lands and the crash stops being invisible.

### 6. Recommended next action

Land slices 1-3 as one small PR. Then **stop chasing this file directly**: it is a ~1-in-several-dozen
CI event that survived ~96 targeted local runs across four configurations. The productive move is to
make the next crash — in *any* file — self-diagnosing, and to un-mute the two mechanisms that are
currently hiding crashes that already happen. A sanitizer job or core dumps are only worth building
if a named-thread report still leaves the subsystem ambiguous.
