# The `spawn_callable_promise` SEGV was resolved by the worker pool (ADR-0020)

`todo/deep/promise-spawn-segv-under-load.md` recorded a SIGSEGV in
`roast/S17-lowlevel/semaphore.t` (whitelisted, 2 subtests, 4000 `Promise.start`
closures each) that reproduced at **6-8% under CPU contention** in the
`jit-stress` configuration. It is fixed, and it was fixed the day after the
ticket was written — by a change made for entirely unrelated reasons.

## The crash frame no longer exists

The ticket's crash report named the innermost frames precisely:

    3: ___pthread_detach at ./nptl/pthread_detach.c:36:6
    4: core::ptr::drop_in_place<std::thread::join_handle::JoinHandle<()>>
    5: mutsu::runtime::builtins_system::…::spawn_callable_promise

At the measured commit (`d0233c8ce`, 2026-08-04) `spawn_callable_promise` ended
in a bare `spawn_user_thread(move || { … })` whose returned `JoinHandle` was
discarded on the spot — so every `Promise.start` created **one real OS thread**
and immediately detached it from inside a nested VM dispatch chain.

`9e91bc37b` ("perf(threads): elastic worker pool for `start` and one-shot cue",
ADR-0020 slice 1, **2026-08-05** — one day after the ticket was filed) replaced
that with `crate::runtime::worker_pool::submit(move || { … })`. `submit` returns
`()`: as its own doc says, *"There is no join handle: completion is observed
through whatever the task itself resolves"*. Completion is observed through the
`SharedPromise`. Frames 3 and 4 of the crash report are therefore unreachable
today — there is no `JoinHandle` on the `Promise.start` path at all, and nothing
calls `pthread_detach` from a deep VM stack.

The workload changed shape just as drastically. Measured on the current tree,
`roast/S17-lowlevel/semaphore.t` — 8000 `Promise.start` calls across its two
subtests — now peaks at **4 OS threads** (`main`, `mutsu-main`, and a couple of
pooled workers), where before it created one thread per promise, i.e. 4000
concurrent threads per subtest, each reserving `USER_THREAD_STACK_SIZE` =
256 MiB of address space. Thread churn was the load the crash rode on, and it
is gone.

## Verification: 144/144 clean

Release-optimised build (`cargo build --profile profiling`), 12-core box, the
exact `jit-stress` configuration the ticket measured
(`MUTSU_JIT=on MUTSU_JIT_THRESHOLD=2 MUTSU_FUDGE=1`), 12 concurrent independent
processes per round — the ticket's own driver shape, since it does not reproduce
serially or through `prove -j12`:

| rounds x concurrency | runs | SEGV |
| --- | --- | --- |
| 4 x 12 | 48 | 0 |
| 8 x 12 | 96 | 0 |
| **total** | **144** | **0** |

Against the ticket's measured 6-8% per-run rate, the probability of seeing zero
crashes in 144 runs is about `0.93**144` ~= 3e-5. The two trees it measured
(3/48 and 4/48) are excluded at that confidence.

## Correction: the ticket's stack-size premise was factually wrong

The ticket filed itself as *deep* on the strength of a design question:

> **How much stack a VM thread gets, and who decides.** The main VM thread is
> spawned with an explicit stack size; the threads `Promise.start` creates
> inherit whatever the runtime default is.

That second clause was not true, even at the commit it was measured on. Reading
`src/runtime/builtins_system.rs` at `d0233c8ce`: `spawn_callable_promise` called
`spawn_user_thread`, and `spawn_user_thread` is defined as
`spawn_registered_thread(Some(USER_THREAD_STACK_SIZE), f)` with
`USER_THREAD_STACK_SIZE = 256 * 1024 * 1024`. The crashing thread had the same
256 MiB reservation as `mutsu-main`, not the ~2 MiB Rust default.

This matters because it is decisive, not pedantic: a thread with a 256 MiB stack
running a non-recursive `for ^4000 { … }` loop did not exhaust its stack budget.
The ticket read `SEGV_MAPERR` just below a large aligned boundary as a guard-page
hit and inferred stack overflow; the other reading that fits the same evidence is
a **detach against a recycled thread descriptor** — glibc places the thread
descriptor at the top of the thread-stack mapping, so touching a freed one faults
exactly there, and 4000 rapid spawn-then-immediately-detach cycles is precisely
the workload that exercises glibc's stack-cache reuse. Both candidate mechanisms
required the per-promise `JoinHandle` that ADR-0020 removed, so distinguishing
them is now moot — but the "promise threads get the default stack" premise should
not be carried forward into any future design work, because it was never true.

## Relationship to `todo/deep/procasync-stress-segv.md`

The two were suspected of sharing a root cause (spawn/thread stack depth under
concurrency load). They do not.

- This one had a symbolised crash site, a measured reproduction rate, and a
  specific code shape that has since been deleted.
- `procasync-stress-segv` has never reproduced: 22 runs (2026-07-30), ~96 runs
  across four configurations (2026-08-19), and 12 more in this pass, all clean.
  Its `Proc::Async` threads are `spawn_gc_helper_thread` service threads that run
  no user VM code, it never held a `JoinHandle` on the crashing path, and its
  §4 audit had already ruled out the GC-registration class. It stays open on its
  remaining diagnostics slice.

## The one live residue of the design question

The ticket's question — *who decides how much stack a thread that runs user code
gets* — is answered correctly everywhere on the promise path, but exactly one
site in the tree still gets it wrong: `src/runtime/slang_activation.rs:57` spawns
a raw `std::thread::Builder` with no stack size **and** without going through
`spawn_registered_thread`, then runs a full `Interpreter::new()` + `use_module()`
on it. That is recorded separately as
`todo/tickets/slang-activation-thread-is-unregistered-and-default-stack.md`.
