# ADR-11617: A worker's `exit` is handed over to the main thread

- **Status**: Accepted (user approval 2026-10-13); implemented (2026-10-12, #11617); the gaps in §4 remain
  ([#13052](https://github.com/tokuhirom/mutsu/issues/13052))
- **Date**: 2026-10-12
- **Deciders**: tokuhirom, Claude
- **Addresses**: [#11617](https://github.com/tokuhirom/mutsu/issues/11617)
- **Related**: [ADR-0020](0020-shared-worker-pool.md) (the worker pool a `start` runs on),
  [ADR-0105](0105-promise-resolution-dispatches-through-the-promise-scheduler.md) (how a
  main-thread `await` parks), [ADR-10779](10779-interpreter-subsystems-and-upward-call-traits.md)
  (no new `Interpreter` fields)

## Context

`exit` ends the process in Raku wherever it is called, and the process runs its END phasers on
the way out:

```raku
END { say "end ran" }
start { exit 3 }
sleep 1; say "alive"
```

Rakudo prints `end ran` and exits 3. The same holds for a sunk `start` that dies: the scheduler
reports the exception and exits 1, and END runs.

mutsu's END phasers belong to the main program's interpreter (`control.end_phasers`; a thread
clone made by `fork_for_thread` has an empty list), and they run in `Interpreter::finish` on the
main program's exit path. A worker's `exit` used to flush the worker's output and call
`std::process::exit` directly, so END never ran. It did that because the main thread may be stuck
in a long computation that never polls `GLOBAL_EXIT_REQUESTED`
(`roast/APPENDICES/A01-limits/overflow.t`: `start { sleep 2; exit }; EVAL 'say 1.0000001 **
10**8'`), and a process that does not end when it is told to hangs.

Two designs were considered:

1. **Run END on the worker**, which is what Rakudo does. It needs the main interpreter's END queue
   and a coherent view of the main env from another thread, while the main thread keeps
   executing. That is a new cross-thread access to the main interpreter's state, the kind of
   sharing ADR-10779 and the thread-clone model avoid.
2. **Hand the `exit` over to the main thread** at a point where it is safe for the main
   interpreter to stop, and keep the direct exit as a bounded fallback.

This ADR takes (2).

## Decision

### 1. A process-global handover

`crate::exit_handover` (a std-only leaf module, so the value layer's waits can read it) holds the
state machine. Every transition is made under one mutex; the hot read is a relaxed atomic load.

```text
IDLE --request--> REQUESTED --adopt--> EXITING
  |                   `--grace runs out--> ABANDONED
  |--adopt (main called `exit` itself)--> EXITING
  `--adopt (main ran off its end)--> FINISHED
```

### 2. The worker requests and waits

`Interpreter::exit_from_worker` (reached from the thread-clone branch of
`request_process_exit`, which covers `exit` and the sunk-`start`-dies report, and from an act loop
whose callback exits or dies) does this:

- It writes the worker's buffered output and *takes* the shared thread buffers, so the main
  thread's `finish` does not print them a second time.
- It requests the handover.
  - `Made`: it waits, counted GC-quiescent, up to `GRACE` (1 s) for adoption. Once adopted it
    parks for good, and the main thread ends the process. If nobody adopts it, it marks the
    handover `ABANDONED` and calls `std::process::exit` itself, without END, as before.
  - `AlreadyExiting`: the process is already exiting. It parks, and the first `exit` keeps the
    status (Rakudo's `the-end-is-nigh` latch).
  - `Closed`: the main program is past END (joining its outstanding threads), or the handover was
    abandoned. It exits directly.

The browser build runs `start` blocks on its single thread, so it keeps the direct exit.

### 3. The main interpreter adopts

`main.rs` calls `Interpreter::adopt_worker_exits`. That sets `ControlState::adopts_worker_exits`
(a flag on the existing `control` subsystem, not a new `Interpreter` field) and marks the main
thread as the adopter.

- `is_halted()` is also true while a request is `REQUESTED` on that interpreter. The VM checks
  it after every op, so the mainline unwinds as if it had called `exit` itself. Only the main
  program's interpreter pays for the extra atomic load (§5).
- The main thread's blocking waits give up on a pending request. Otherwise they could wait
  forever, because the promise may belong to the worker that exited. This covers `sleep` (all
  durations, woken through the handover's condvar), `SharedPromise::wait` (`await`, `.result`),
  and `Channel.receive`. An interrupted wait returns Nil, and the VM unwinds right after the op.
- `run` adopts a pending request right after the mainline returns, and drops any error the
  interrupted op left behind. MAIN dispatch is skipped. `finish` closes the handover before the
  LEAVE and END phasers: it adopts a request that is still pending, and moves the state to
  `EXITING`, or to `FINISHED` when the program ran off its end without `exit`.
- A status the main program already latched by its own `exit` wins over an adopted one.

## Consequences

### 4. Known gaps

- A main thread blocked where it neither runs bytecode nor waits through one of the interrupted
  waits does not adopt in time. Examples: a native computation such as `[+] ^5000000` or a big
  `sort`, a native read, a `Thread.finish` join, a `Lock` acquisition, or any other condvar wait.
  It gets the fallback after `GRACE`: the process exits with the worker's status, without END.
  Each such wait can be taught to give up the same way as the three above.
- With the GC disabled, `stw_aware_wait` waits on the condvar without a timeout, so an
  interrupted `await`/`receive` only notices the request when something else notifies it, and
  falls back after `GRACE`.
- Rakudo runs END *on the exiting thread*. With mutsu's handover it runs on the main thread. The
  difference is visible only through `$*THREAD` inside END.

### 5. Cost

On the main program's interpreter, `is_halted()` adds one relaxed load of a process-global byte
to every executed op. The byte is written at most a few times per process, so the load always
hits a shared cache line. Every other interpreter (thread clones, in-process `is_run`) skips the
load behind the `adopts_worker_exits` flag.

Measured with callgrind on `--profile profiling` builds, warm, one run after a discarded one
(2026-10-12). The workload is a 200k-iteration `while` loop that calls a sub on every
iteration:

| Build | Instructions |
| --- | --- |
| `main` @ 90a8c09b | 1,680,920,514 |
| with the handover | 1,687,918,760 |

The handover adds +0.42%. The cost is accepted for a correctness fix. If it ever matters, the
check can move from `is_halted()` onto the poll network (back-edges and calls, ADR-0106), which
gives up the per-op granularity.

## Amendment (2026-10-13, #13052): more waits give up

§4's first two gaps are narrowed. The main program's thread now also gives up on a pending
request in:

- `Thread.finish`. A join cannot be woken, so the adopter thread polls `is_finished`, starting
  at 50 µs and doubling up to `exit_handover::POLL` (10 ms). A 200-join loop measures the same
  as a blocking join. On a pending request it keeps the handle and unwinds.
- `Lock` acquisition (`acquire_lock`, so `.lock` and `.protect`), `Semaphore.acquire` and
  `Lock::ConditionVariable.wait`. The condition-variable wait re-checks every `POLL` and treats
  a timeout as no signal. None of these returns a value that would let the caller go on as if
  it held the lock. They raise `RuntimeError::exit_handover_pending()` instead. Since the
  interpreter is halted, that only unwinds: it is not reported, and no `CATCH` runs.
- With the GC off, `stw_aware_wait` on the adopter thread waits in `POLL` steps rather than
  untimed, so `await`, `receive` and the waits above see the request without another wake-up.

Still in the gap: a long native computation that never polls, a native read, an `act_loop`
completion wait and a `Proc` wait.
