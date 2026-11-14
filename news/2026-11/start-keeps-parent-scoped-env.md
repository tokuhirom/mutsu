# A `start` no longer merges a routine's lexicals with its caller's

Spawning a thread (`start`, `Promise.start`, a hyper batch) used to flatten the parent's scoped
environment in place before it seeded the shared-variable store and cloned the env for the
child. That flattening merged the overlay of a running module routine with the frames beneath it,
so a routine's `my Int $n = 0` and the script's own `my $n` became one entry, and the routine read
`Int` back once its scope was restored. A bare `start` two method calls deep was enough; no `await`
was involved (#17747).

The spawn now flattens a copy for the seeding walk and the child only, and leaves the parent's
scoped env alone. Rakuish's `t/06-sandbox-agent.t`, whose agent server counts requests in a
`my Int $n` while each request runs in a `start`, now matches rakudo.
