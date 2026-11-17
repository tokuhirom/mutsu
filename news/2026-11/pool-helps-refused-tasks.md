# A task the OS refuses a thread for waits, and a blocked worker runs it

mutsu's `await` holds its OS thread, so N parked `start` blocks need N threads. The pool
used to reject a task once the OS refused that many, which broke the task's promise.
The ecosystem sandbox runs as an unprivileged user under `ulimit -u 400`. There,
Log::Dispatch's `t/040-threaded.rakutest` hung: its 500 tasks each run
`$ready.keep; await $starter`, and the refused tasks never kept their `$ready` (#17392).
Local runs passed only because root is exempt from `RLIMIT_NPROC`. Running the
reproduction under `setpriv --reuid=65534` with `ulimit -u 400` made it hang here too.

The pool now handles a refused thread as ADR-0123's §7 amendment (Proposed) describes:

- **The task waits instead of failing.** A refused task is rejected only when the pool
  has no live worker at all. Otherwise it stays queued, as rakudo's bounded pool queues
  past `max_threads`.
- **A blocked worker runs it.** A pool worker blocked in `gc::wait_until` is a *helper*.
  When it sees a starved queue, it runs one task nested on its own stack, and then waits
  again. The wasm build already handles every wait this way. A helper helps only while no
  worker is running, and only with at least half a fresh worker's stack to spare. The
  main thread never helps.
- **No thread past the budget when a helper can take the task.** That thread would only
  eat the heap the parked tasks need: 3000 parked `start`s under `ulimit -v 6000000` used
  to abort with "memory allocation failed".

`MUTSU_USER_THREAD_LIMIT=N` injects a thread limit, since `ulimit -u` is not enforced for
root. `t/concurrency/thread-lock/pool-helps-refused-thread.t` uses it.

Measured on this container:

- `ulimit -u 400`, non-root: 500 and 2000 parked tasks finish in under a second, where
  they used to hang. Log::Dispatch's `t/040-threaded` passes.
- `ulimit -v 6000000`: 3000 parked tasks finish on both a debug and a release build.
- `ulimit -v 1500000`, release build: 1000 finish (800 used to hang). 3000 still exceed
  what the helpers' stacks can hold. Running more than that needs a parked task that
  costs no thread, which is ADR-15967 §4.2's stackful-task direction.
