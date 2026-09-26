# A Promise now carries the scheduler it was constructed under

ADR-0105 slice S1 (decision D1, towards #8380's `Test::Time` deadlock) gives
every promise the scheduler Rakudo binds it to. `Promise.new(:scheduler)`,
`Promise.in`/`.at` (whose explicit `:scheduler` argument used to be dropped),
`Promise.start(:scheduler)`, `start`, `.then`/`.andthen`/`.orelse` results,
`Promise.allof`/`.anyof`/`.kept`/`.broken` and `Supply.Promise` bind the
explicit scheduler or a user-defined `$*SCHEDULER`, and the new
`Promise.scheduler` accessor reads it back. A built-in scheduler still binds as
"none" and keeps the native deadline-heap and worker-pool paths.

Under a user `$*SCHEDULER`, a `start` body is now cued through that scheduler's
`.cue`, as in Rakudo, so a virtual-time scheduler such as `Test::Scheduler`
decides when the block runs; `Promise.start(:scheduler($orig), ...)` escapes it,
which is how `Test::Time`'s `:auto-advance` loop is written.

Pinned by `t/concurrency/thread-lock/promise-scheduler-binding.t`; the S0
oracle test's `Promise.scheduler` and `start` assertions are no longer `todo`.
Promise resolution still does not dispatch through the bound scheduler — that
is slice S2.
