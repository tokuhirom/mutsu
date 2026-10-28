# Entering a critical section no longer replaces a same-named lexical

`Lock.protect` (and `Semaphore`) refresh the entering thread's env from the
shared store for every name written inside an earlier critical section, so
the next holder reads what the previous one committed. The refresh went by
bare name and also pulled the thread's own earlier writes and mere spawn-time
seeds, which replaced an unrelated lexical of the same name in the frame that
entered the section. Red's nested `red-do :transaction, -> :$guard! { ... }`
blocks hit it: after an inner transaction block ran a query, the outer block's
`$guard` was the inner guard, so the outer `$guard.commit` found a rolled-back
scope (Red t/99 "Savepoint participants are independent").

The refresh now pulls only a value another thread committed, as
`sync_shared_vars_to_env` already did.
