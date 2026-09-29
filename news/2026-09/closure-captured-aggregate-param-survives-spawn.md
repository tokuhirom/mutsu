# A closure's captured `%`/`@` parameter keeps writing to the caller after it spawns a thread

A closure that captured a hash or array **parameter** of its defining routine,
and that itself ran a `start` block, stopped writing into the caller's
container after the first spawn:

```raku
sub mk(%g) { -> $id { %g{$id} = 1; start { 1 } } }
my %gates;
my &f = mk(%gates);
my &h = mk(%gates);
f('a'); h('b');
say %gates.keys.sort;   # rakudo: (a b) — mutsu printed (a)
```

The spawn copies the spawning frame's lexicals into the name-keyed cross-thread
store. A parameter-bound aggregate was already kept out of that store, because
the store is seeded once per name and a parameter is a fresh binding on every
call. That exclusion only applied when the spawned block *named* the
parameter, though. `start { 1 }` never mentions `%g`, so `%g` was still seeded
(as an ADR-0039 §8.6 transient entry). The next `%g{$id} = ...` found the name
in the store and took the atomic hash lane, which writes a copy of the hash and
rebinds `%g` to that copy. From then on, the caller's `%gates` never saw
another store.

The exclusion now covers every parameter-bound aggregate that is live in the
spawning frame, whether or not the block names it. It compares containers
through the closure machinery's `ContainerRef` cell, because that is how a
captured parameter sits in the env. A parameter is per-invocation whoever
names it, and its container is already the object every alias holds.

This was found through the `JobQueue` distribution, drawn by the ecosystem
roulette. Its `t/02-coordinator.rakutest` passes one `%gates` hash to two
queues' runner closures, then waits on gates that the second queue had been
registering into the detached copy. The file now passes, which puts all four
of the distribution's test files at parity with rakudo. A narrower variant
remains: two live bindings of one parameter name to *different* hashes still
share a lane entry. That is tracked as #10076.
