# A critical-section refresh skips names the thread made its own

Entering a critical section (`Lock.protect`) pulls the values other threads
committed in earlier critical sections into the entering thread's env, by bare
name. The pull did not apply the exclusion `sync_shared_vars_to_env` already
has: a name the thread re-declared, or captured from its spawn, is its own
binding, and the store's entry under that name belongs to another binding of
it. Red's `Guard.start` hit it: a worker's `$guard` became a previous
`red-do` block's guard after its first database statement, so the worker's
`$guard.commit` found a committed scope (Red t/99 "An asynchronous task can
outlive the red-do block").
