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

1. **DONE (2026-08-19, #6695).** **Name every thread** (`src/runtime/thread_compat.rs`). Gave `spawn_thread`
   a `name: &str` parameter, threaded through `spawn_registered_thread` / `spawn_gc_helper_thread` /
   `spawn_user_thread` and every call site: `mutsu-main`, `proc-wait`, `proc-out`, `proc-err`,
   `proc-in`, `signal-rd`, `timer`, `react`, `pool`, `sock-async`, `sock-conn`, `io-path`,
   `promise-wait`, `promise-comb`, `raku-thread`. Verified live: `ps -eLo pid,tid,comm` on a running
   `Proc::Async` react loop shows `mutsu-main`, `proc-wait`, `proc-out`, `proc-err`, `promise-wait` as
   distinct threads instead of all reading `mutsu`. The next occurrence's `thread:` line alone now
   says whether the fault is in the reaper, a reader, the timer, the react driver, or the pool.
2. **DONE (2026-08-19, #6695).** **Stop retrying a signal death** (`scripts/flaky-retry.sh`). After `rc=$?`,
   `rc -ge 128` now exits immediately with a
   `# flaky-retry: <file> died of signal N -- NOT retried (see docs/flaky-test-policy.md)` comment,
   no retry. Enforced by `tests/flaky_retry.rs::quarantined_test_that_crashes_is_not_retried`
   (a fake SIGABRT-killing test proves it fails on the first attempt, not the third). Documented in
   `docs/flaky-test-policy.md` §4. This is exactly what would have turned §2's abort into a red CI on
   the day it happened.
3. **DONE (2026-08-19, #6695).** **Surface crash reports on green runs** (`.github/workflows/ci.yml`). The
   three "Crash reports" steps are now `if: always()`, and `scripts/report-crash-reports.sh` fails the
   job (`exit 1`) when a report's `argv:` is not on `ALLOWLISTED_ARGV_SUBSTRINGS`, which today holds
   exactly the deliberate `strdup(0)` NativeCall probe (`roast/S29-os/system.t`). Manually verified
   against a fabricated allowlisted report (exit 0) and a fabricated `advent2014-day05.t`-shaped
   report (exit 1, `::error::` annotation).
4. **DONE (2026-08-20).** **Re-measure the `advent2014-day05.t` quarantine.** See §7. Its `flaky-tests.txt` reason
   attributes the failures to CPU-contention timing; §2 is hard evidence that at least one of them was
   heap corruption. Per the ledger's own bar the entry should be pulled (or at minimum re-justified)
   now that slices 2-3 have landed and a recurrence can no longer hide. This needs the crash to
   actually recur (or a deliberate repro) under the new, named-thread reporting before it can be
   root-caused — nothing here reproduces it yet.

### 6. Recommended next action

~~Land slices 1-3 as one small PR.~~ **Done 2026-08-19, #6695** — see §5.
~~Slice 4.~~ **Done 2026-08-20** — see §7. What remains:

1. **Stop chasing `roast/S17-procasync/stress.t` directly.** It is a ~1-in-several-dozen CI event that
   survived ~96 targeted local runs across four configurations, and the productive move was always to
   make the next crash — in *any* file — self-diagnosing rather than burn more cycles on a repro loop
   that has not worked. That diagnostics work (named threads, non-launderable signal deaths, an
   un-muted crash-report step) is now in place. A sanitizer job or core dumps are only worth building
   if a future named-thread report still leaves the subsystem ambiguous.

## 7. Cross-check against `promise-spawn-segv-under-load`, 2026-08-20

The two SEGV tickets were suspected of sharing one root cause (spawn/thread stack depth under
concurrency load). **They do not.** The sibling is closed; this one is unchanged.

### 7.1 The sibling is resolved, for a reason that does not apply here

`todo/deep/promise-spawn-segv-under-load.md` is now
`news/2026-08/promise-spawn-segv-resolved-by-the-worker-pool.md`. Its crash frames
(`drop_in_place<JoinHandle<()>>` -> `pthread_detach`, called from
`spawn_callable_promise`) stopped existing on 2026-08-05, one day after it was filed:
`9e91bc37b` (ADR-0020 slice 1) replaced the per-promise `spawn_user_thread` with
`worker_pool::submit`, which returns no handle. Measured on the current tree,
`roast/S17-lowlevel/semaphore.t`'s 8000 `Promise.start` calls peak at **4** OS threads instead of
one per promise, and it is 144/144 clean in the exact `jit-stress` 12-concurrent configuration that
previously crashed at 6-8%.

None of that touches this ticket. `Proc::Async` never held a `JoinHandle` on the crashing path, its
threads are `spawn_gc_helper_thread` service threads that run no user VM code (§4), and it does not
go through `worker_pool`. Its own crash was on the 2026-07-30 tree, five days *before* the worker
pool existed, so the worker pool cannot have fixed it either — this ticket's non-reproduction long
predates that change and is unexplained rather than resolved.

Also worth carrying forward: the sibling's "promise threads inherit the default stack" premise was
factually wrong — `spawn_user_thread` gave them the same 256 MiB as `mutsu-main`. Do not reuse
"a VM thread ran out of its default stack" as a hypothesis for this ticket without checking which
spawn wrapper the thread actually came from.

### 7.2 This file: 12 more clean runs

`roast/S17-procasync/stress.t`, profiling build, `MUTSU_JIT=on MUTSU_JIT_THRESHOLD=2 MUTSU_FUDGE=1`,
2 rounds x 6 concurrent: 12/12 clean. Running total across all sessions: 22 (2026-07-30) + ~96
(2026-08-19) + 12 = **~130 clean runs, zero faults.** §6's advice stands unchanged: do not spend
another session on a repro loop.

### 7.3 Slice 4 closed

`roast/integration/advent2014-day05.t` stays quarantined, with its `flaky-tests.txt` reason extended
to state the scope explicitly. The reasoning: the entry documents an *assertion-level* race the test
itself writes down (`isnt $a, $times` after a racy `$*SCHEDULER.cue`), which is quarantine-eligible
under `docs/flaky-test-policy.md` §2. The thing that was not eligible — §2's SIGABRT heap corruption
being retried green — is no longer possible at all: since #6695 `scripts/flaky-retry.sh` refuses to
retry any `rc >= 128` signal death and fails on the first attempt. So the quarantine can now only
launder what it was justified for, and a crash in that file fails CI loudly. The underlying heap
corruption remains un-root-caused and still awaits a recurrence under the named-thread reports; that
is tracked by this ticket's §2, not by the ledger entry.

### 7.4 Incidental finding

The audit for this cross-check turned up the one site in non-test `src/` that still spawns a
user-code-running thread outside `spawn_registered_thread`:
`src/runtime/slang_activation.rs:57` builds a raw `std::thread::Builder` with no stack size, runs a
full `Interpreter::new()` + `use_module()` on it, and joins it without `gc::block_quiescent`. That is
an unregistered GC mutator of exactly the class §4 ruled out for the `Proc::Async` paths — §4's audit
simply did not cover the slang path. Filed as
`todo/tickets/slang-activation-thread-is-unregistered-and-default-stack.md`. It is not a candidate
explanation for this ticket (`stress.t` loads no slang module), but it is a live soundness gap.

## 8. The recurrence §7.3 was waiting for arrived — and wrote no crash report, 2026-08-26

§7.3 closed slice 4 with "the underlying heap corruption remains un-root-caused and still awaits a
recurrence **under the named-thread reports**". That recurrence has now happened, and the important
part of it is that the named-thread report mechanism **did not fire**.

### 8.1 What was observed

CI run [32968762746](https://github.com/tokuhirom/mutsu/actions/runs/32968762746), attempt 1,
`test` job (id 98177315406), on PR #7018's branch:

```
roast/integration/advent2014-day05.t   (Wstat: 35584 (exited 139) Tests: 5 Failed: 0)
  Non-zero exit status: 139
  Parse errors: Bad plan.  You planned 7 tests but ran 5.
Result: FAIL
```

`139 = 128 + 11`, i.e. SIGSEGV, surfaced as an exit status because roast runs each file through
`scripts/run-roast-test.sh`. As in every prior instance this is `Failed: 0` — no assertion failed;
the interpreter died partway through.

Three things distinguish this instance from the SIGABRT one recorded in `flaky-tests.txt`:

1. **It is SIGSEGV, not SIGABRT-inside-the-allocator.** The earlier instance
   (CI run 32116354874, `tmp/crash/49110.txt`) aborted inside glibc's allocator, which is the
   heap-corruption signature. This one segfaulted.
2. **It died after test 5, not at test 3.** Test 3 is the `#?rakudo skip "sometimes hangs, sometimes
   segfaults"` case that the quarantine reason is written around, and with `MUTSU_FUDGE=1` that skip
   IS honoured (the serial verbose re-run in the same job shows `ok 3 - # SKIP sometimes hangs,
   sometimes segfaults`). Five tests emitted, so the death is in the tail of the file — the
   `Supplier`/`.act` tap section that tests 6 and 7 cover — not in the racy `$*SCHEDULER.cue` block.
3. **The retry machinery behaved correctly.** #6695's rule held: the signal death was not laundered
   green, and the job failed. The `Failed roast files — serial verbose re-run` diagnostic step then
   ran the file alone and it passed 7/7, which is the usual shape.

### 8.2 The actionable finding: no crash report was produced

`MUTSU_CRASH_DIR` was set for the job (`/home/runner/work/mutsu/mutsu/tmp/crash`), and
`scripts/report-crash-reports.sh` ran. It found **exactly one** report, and that report is unrelated
to this file — it is the deliberate, allowlisted NativeCall crash:

```
mutsu crash report
signal: 11 (SIGSEGV)   si_code: 1   fault-addr: 0x0
thread: mutsu-main
argv: target/release/mutsu -e use NativeCall; sub strdup(int64) is native(Str) {*}; strdup(0)
  4: strdup
  5: ffi_call_unix64
  ...
  9: mutsu::runtime::nativecall::call_native_with_out_args
```

```
-> known deliberate crash (argv matches the allowlist), not treated as a failure.
All 1 crash report(s) match the allowlist (deliberate, expected crashes) -- not failing the job.
```

So the `advent2014-day05.t` process died of SIGSEGV and left **no** report behind. That is the gap
worth chasing next, because it is what makes every future recurrence uninformative: the diagnostic
this ticket has been waiting on is not capturing the very crash it was installed for. Candidate
explanations, in the order they are cheapest to eliminate:

- The fault happened on a thread with no handler installed, or on one whose alternate signal stack
  was unavailable, so the handler could not run. §4/§7.4 already establish that not every
  user-code-running thread goes through `spawn_registered_thread`.
- The handler ran but faulted itself (a second fault inside `write_report` is fatal and silent).
- `MUTSU_CRASH_DIR` did not reach that process, or the report was written somewhere the collector
  step does not look.

Note this is a *diagnostics* bug and can be attacked on its own, without reproducing the underlying
crash: install a deliberate fault on each thread class mutsu spawns and assert that a report appears
for each. `tests/crash_report.rs` already exercises the mechanism, so extending it to cover
non-`mutsu-main` threads is a contained piece of work.

### 8.3 Is it a regression?

Almost certainly not. It surfaced on PR #7018 (`my`/`our $.x` class-level attributes inside method
bodies), which touches class-level attribute storage and has no connection to the scheduler, to
`Supplier`, or to thread spawning. The file has a documented history of intermittent signal deaths on
unrelated PRs (§7.3, and the roast test's own `sometimes hangs, sometimes segfaults` skip comment),
and the same job passed the file cleanly on re-run. Recorded here so the evidence is not lost to a
green re-run, per `CLAUDE.md`'s rule that a crash-class failure is never dismissed as noise.
