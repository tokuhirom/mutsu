# A `start` burst under `ulimit -v` queues instead of panicking

`Concurrent::PriorityQueue` 0.0.2's `t/02-concurrent.rakutest` starts 64 `start` blocks. Inside
the ecosystem sandbox (`ulimit -v 6000000`) mutsu panicked while spawning them —
`failed to spawn worker thread: ... Resource temporarily unavailable` — because every pool worker
reserved a 256 MiB stack and the pool grew a worker per queued task, so the burst asked for 16 GiB
of address space ([#9377](https://github.com/tokuhirom/mutsu/issues/9377)).

[ADR-0123](../../docs/adr/0123-bounded-user-thread-stack-reservations.md) changes three things:

- **Stacks have a budget.** `runtime::stack_budget` reserves user-thread stacks against half of
  `RLIMIT_AS` (unlimited when there is no limit; `MUTSU_STACK_BUDGET_MB` overrides it), checked
  *before* a thread is created — waiting for `pthread_create` to fail leaves the heap no room
  either, and a failed allocation is an abort.
- **The pool grows only when it has to.** A worker that enters a blocking wait (`await`, channel
  receive, `sleep`, a join) is counted as blocked, at the same `gc::block_quiescent` /
  `gc::wait_until` chokepoints the stop-the-world already uses. The pool grows freely up to
  `8 × cores` active workers (rakudo's `max_threads`) while a full stack fits the budget; past
  that it queues work for a running worker. Only when every worker is blocked does it grow
  regardless — stepping down to a 64 or 32 MiB stack if it must — so nested `start` + `await`
  still cannot deadlock. The ADR-0100 recursion guard scales its reserve to the smaller stacks.
- **A refused thread is an exception.** `thread_compat::spawn_thread` returns a `Result`; `start`
  breaks its promise with `X::AdHoc` ("Could not create a new Thread: ..."), and `Thread.start`,
  `Supply.Promise`, slang activation and `Promise.allof` raise it.

A 64-task CPU-bound burst under a 3 GB limit now runs on 8 workers; under
`MUTSU_STACK_BUDGET_MB=0`, on one. Pinned by `t/concurrency/worker-pool-stack-budget.t`.
