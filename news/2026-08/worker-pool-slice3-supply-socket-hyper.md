# Worker pool slice 3: supply emitters, socket pumps, hyper/race batches (ADR-0020)

The third slice of the ADR-0020 shared-worker-pool campaign routes the
remaining user-code spawn sites through the elastic pool. After this slice,
`Thread.start` is the only `spawn_user_thread` call site left — deliberately,
per ADR-0020 §3.6: a `Thread.start` thread has user-visible identity
(`$*THREAD.id`) that must stay stable for its whole lifetime.

What moved onto the pool, case by case:

- **Short one-shots** — the per-value `Supply.start` block runner (fires per
  emitted value, the hottest supply spawner), promise-waiter dispatch
  (`SharedPromise::dispatch_waiters`, fired on every resolution with queued
  `.then`/`.andthen` waiters; ordering is unchanged because all of a promise's
  waiters run in registration order inside one pooled task), the
  uncaught-handler callback, and both `Supply.throttle` coordinators.
- **Joined fan-outs** — hyper/race batch workers and `Supply.throttle`'s
  concurrency-limited workers. These needed a new `worker_pool::submit_joinable`
  API: natively it pairs a pooled task with a result channel (a panicking task
  drops the sender during unwind, so `join` reports `Err` exactly like a
  dedicated thread's join); on wasm32 it delegates to the cooperative
  scheduler's `JoinHandle`, whose `join` *runs* the queued task — a channel
  wait would spin forever there, which is why the cfg fork lives inside
  `worker_pool.rs` rather than at call sites. Inter-item synchronization
  (Promises between hyper items) still works: the submit-side starvation check
  gives every batch its own worker, the same concurrency as thread-per-batch.
- **Supply-lifetime pumps** — the zip / zip-latest coordinators, both
  `run_supply_act_loop` drivers (live `.act`/`.tap`, whenever socket sources),
  and the bare-tap socket accept driver. A pump borrows a worker until its
  supply is done, so the steady-state thread cost is unchanged; the win is
  warm reuse across create/tear-down churn — exactly the Cro-style
  per-connection pipeline shape, where each connection previously paid a
  fresh 256 MiB-stack thread spawn.

`MUTSU_POOL=off` still restores thread-per-task at every site (the joinable
path falls back to a dedicated thread that feeds the same channel).
