# `.wrap` is seen by every thread

ADR-17216's second landed slice shares the routine `.wrap` tables between
threads. Before, a spawned thread got a copy of them, so

```raku
sub f { "f1" }
await start { EVAL q[&f.wrap(-> { "wrapped" })] };
say f();   # mutsu: f1, raku: wrapped
```

printed the unwrapped answer, an unwrap on a worker left the main thread's
routine wrapped, and two workers wrapping the same routine each kept only
their own wrapper.

The tables (the chains, the name and package ledgers, the callable-id
ledger) now live in a `WorldCell`: one process-wide slot holding an
`Arc` of the table, written read-copy-update under a lock, plus a per-thread
snapshot that a read refreshes only when the slot's generation moved (one
atomic load per read). Every read-modify-write of a wrap or unwrap is one
locked write, so concurrent wraps are never lost, and wrap handle ids come
from one process-wide counter. The call ops compare the generation with the
one their name-keyed call caches were filled under and drop those caches
when another thread wrapped, so a call site already warm on one thread
still runs the wrapper another thread installed.

`WorldCell` is the shared-table building block the remaining ADR-17216
slices (the type registry, module state, `our` variables) reuse.
`t/concurrency/concurrent-wrap-tables-shared.t` pins the cases against
`raku`.
