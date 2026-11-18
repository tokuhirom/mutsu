# The type registry is shared by every thread

ADR-17216's slice 2b makes the declaration registry (classes, roles, enums,
subsets, method tables, attribute and build-plan metadata) one world table per
program. Before, every spawned thread got a copy-on-write snapshot, so a
declaration made after the spawn stayed on one side:

```raku
await start { EVAL 'class FromThread { method hi { "hi" } }' };
say ::('FromThread').hi;   # mutsu before: No such symbol; raku/now: hi
```

The registry now lives in a `WorldCell`: a read takes this thread's snapshot
(one atomic load; refreshed only after another thread published a write), a
write holds the shared write lock and publishes on drop.
`registry_write_generation()` folds the world generation into the per-thread
one, so a memo keyed by it is invalidated by a declaration on any thread. The
routine tables stay per execution context (slice 2a), because mutsu uses them
to emulate lexical routine scopes.

Sharing the table made every needless registry write visible: a `but R` whose
role body had already run still inserted its "body ran" marker, which in a
multi-threaded program copied the whole registry once per `but` (a
200-task benchmark went from 201 registry copies to 3 once the marker is
probed before it is written). Role-body lexicals of a mixin composed on
another thread are found through the published mixin recipe until
`package_lexicals` itself becomes world state (slice 4).

A write copies what another thread's snapshot still shares, so a registry
copied whole on every declaration made Red's concurrent-query test
(`t/098`, several threads minting types per query) ten times slower: 258 s
against 26 s, one ~4 ms copy per write. Every table of the registry is now
held by reference (`CowTable`), and every keyed map is a `ShardedMap` of 16
copy-on-write shards, so a write copies one shard of one table; the class
short-name and lexical type-name indexes are kept while the key sets they
summarise are unchanged instead of being rebuilt by every thread after every
write. Red t/098 now runs in 39 s. Instruction counts against `main`: a
400-`start` script +0.9%, a method-call loop +0.6%, `say "hello"` +3%
(building the sharded builtin registry), an 8-thread OO + mixin script -14%.

A type declaration registers under a lock of its own declaration site (two
threads running the same `class :: { ... }` must not see each other's half),
not one process-wide lock: the latter, held while a class body runs, deadlocked
against any user `Lock` the body took, which raku does not
(`t/concurrency/thread-lock/concurrent-type-declaration-lock-order.t`).

The shared registry also exposed a long-standing mismatch: a role's type
object boolified through the role's own `Bool` once something had punned the
role, where rakudo answers `Mu.Bool` (False). Red's `if $ast.?filter` on the
`Red::AST` type object then rendered an empty `WHERE` about one run in two
(`t/oo/role/role-type-object-boolification.t`; the rest of the role punning
exceptions is #18062).

`t/concurrency/concurrent-type-registry-shared.t` pins the cross-thread
cases against `raku`.
