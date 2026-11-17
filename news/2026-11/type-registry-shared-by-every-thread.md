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

`t/concurrency/concurrent-type-registry-shared.t` pins the cross-thread
cases against `raku`.
