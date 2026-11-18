# ADR-17216: One program world, shared live by every thread

- **Status**: Accepted (by the maintainer, 2026-11-17)
- **Date**: 2026-11-17
- **Carrier issue**: #17216 (correctness), with #17217 (cost) as its perf close condition
- **Implements**: ADR-15967 §4.1 "one world, long-lived execution contexts, small tasks", whose
  §6 recommendation 2 asks for this design ADR once ADR-16595 slice 4 has landed (slices 4 and 5
  have)
- **Related**: ADR-0068 (a cross-thread write is synchronised at the structure), ADR-12814
  (concurrency follows rakudo; the memory-safety bar), ADR-16595 (lexicals reach other threads
  through their cells; no lexical in the lineage store, §6), ADR-12529 (the dynamic link),
  ADR-10779 (Interpreter subsystems), ADR-0020/0105/0123 (the pool)

## 1. Context

A spawned thread (`start`, `Promise.start`/`.then`, supply and `whenever` drivers, the scheduler,
hyper/race batches) runs on an `Interpreter` built by `spawn_clone`
(`src/runtime/runtime_thread.rs`). For the program's *world* (the state rakudo keeps once per
process) the child gets, as measured on 2026-11-17:

| world table | today | write path |
|---|---|---|
| type registry: classes, roles, enums, subsets, the routine table (`functions`, `proto_functions`), method tables and method wrap chains (`types.registry`, `Arc<RwLock<Arc<Registry>>>`) | copy-on-write snapshot: a **new outer lock** around the shared inner `Arc`; the first write on either side diverges | `registry_mut()` → `Arc::make_mut` (480 sites in 106 files; `functions_mut()` 77) |
| type metadata, stash hiding, type aliases, method fallbacks, ... (`types.*`) | COW snapshots | `cow_table_mut` |
| routine wrappers, operator tables (`dispatch.wrap_chains`, ...) | COW snapshots; `wrap_sub_names` and kin deep-cloned | `cow_table_mut` (9 sites on `wrap_chains`) |
| loaded modules, exports, lib paths, unit pads (`module.*`) | COW snapshots; `loaded_unit_pads` deep-cloned | `cow_table_mut` |
| `our` variables (`lexicals.our_vars`) | deep clone of the map (containers shared, keys and rebinds diverge) | `set_our_var` (26 sites) |
| package and compunit statics (`package_lexicals`, `unit_lexicals`, ...) | COW snapshots (their scalar cells are shared, ADR-0024/#12825) | `cow_table_mut` |
| IO handle table (`io.io_handles`) | filtered deep clone of the open handles the child can reach, new lock; ids continue from the parent's `next_id` | `io_handles_mut()` (19 sites) |

Already shared live, each under its own lock: the serialization-context registry (`Mutex`), the
`once` store, `PROCESS::` dynamics (`ProcessStash`, `RwLock`), published mixins and minted types,
thread output.

So anything a thread adds to the world after the spawn is invisible to the other side (#17216):

```raku
await start { EVAL 'class FromThread { method hi { "hi" } }' };
say (try ::('FromThread').hi) // 'not visible';           # raku: hi
sub f { "f1" }; await start { EVAL q[&f.wrap(-> { "wrapped" })] }; say f();  # raku: wrapped
```

The IO table is patched by a transfer: a finishing worker returns the handles it opened
(`ThreadPromisePayload`) and a single-Promise `await` merges them. That transfer is itself wrong
three ways: sibling workers allocate the **same** handle ids from the copied `next_id` and the merge
(`entry(id).or_insert`) silently drops the second; an `await` of an array of promises (or any
other wait path) never merges; and a close or seek on an inherited handle stays on the worker's
copy.

Two things made the fork necessary when it was introduced, and both are gone:

- Lexicals travelled through the forked env and a name-keyed lane; since ADR-16595 a lexical is
  reached only through the cell its closure captured, and the lineage store holds no lexical.
- The world tables are mutated through `&mut self` (`registry_mut`, `cow_table_mut`), which is
  only sound if no other thread can see the table. A fork made that true by construction.

## 2. Decision

**The world exists once per process. Every thread reads and writes the same world tables, each
behind its own lock, and a spawn shares them instead of forking them.** What stays per thread is
the execution context: frames, VM registers, locals, the topic and `$/`/`$!`, the current-package
pointer, the dynamic link to the spawn point, memo caches, GC mutator state.

1. **One handle per world table, shared by `Arc`.** Each world table above becomes a process-wide
   `Arc<RwLock<...>>` (or the table's existing lock, made shared instead of re-created per
   child). `spawn_clone` clones the `Arc`, never the contents. The inventory in §1 is the list;
   a table not on it is execution state unless a later review moves it.
2. **Reads take a snapshot, writes publish (read-copy-update).** A read takes the read lock just
   long enough to clone the inner `Arc` and works on that snapshot, so a method dispatch or a
   type check sees one consistent registry even while another thread declares a class. A write
   takes the write lock, applies `Arc::make_mut` to the current version and stores the result:
   later readers on every thread see it. This is the shape `registry_mut()` already has; what
   changes is that the outer lock is the same object on every thread.
3. **Generation counters are shared.** Per-thread memo caches keyed by a world generation
   (`registry_write_gen` and kin) read one process-wide `AtomicU64` per table, bumped by every
   publish, so a cache filled on one thread is invalidated by another thread's write. The caches
   themselves stay per thread.
4. **The IO handle table is shared, handle by handle.** One table, one id allocator
   (`AtomicUsize`), each handle's state behind its own lock (a read on one handle does not block
   another). `ThreadPromisePayload`'s handle transfer is deleted, and with it the id collision
   and the single-Promise-only merge.
5. **`our` variables and package/compunit statics are world.** A new `our` key, a rebind or a
   new static published on one thread is visible on every thread, through the same RCU table
   rule.
6. **Synchronisation follows ADR-0068 and ADR-12814.** A world write is synchronised at the
   table; no `&self → &mut` access to a world table without its lock (`docs/security.md`). A
   program racing two declarations of one name without synchronisation may see either order (as
   on rakudo) but never a torn table.
7. **Out of scope here:** the per-worker long-lived execution context and the end of the
   per-task `Interpreter` clone (the cost side, #17217) and non-blocking `await` (ADR-15967
   §4.2, #17790). Both need this ADR first; neither is decided by it.

## 3. Migration

Each slice is its own PR, gated by the full `t/`, the whitelisted roast and the battery, with a
`t/concurrency/` test that does the cross-thread thing and matches `raku`.

1. **IO handle table** (§2.4). Smallest, self-contained, and it removes a known wrong answer
   (the id collision) as well as the transfer. Acceptance: the handle half of #17216's repro, a
   sibling-workers-open-files test, an array-`await` test.
2. **Type registry and routine table** (§2.1-2.3). Share the outer lock; make
   `registry_write_gen` a shared atomic; audit the 480 `registry_mut` sites for writes that
   assume they are alone (read-modify-write across two guard scopes must become one guard).
   Acceptance: the `EVAL`'d-class half of #17216; ParaSeq, Cro and Red battery runs; a perf
   check that a read-heavy single-thread benchmark does not regress (one uncontended read lock
   and an `Arc` clone per snapshot).
3. **Dispatch tables: wrappers and operators.** Acceptance: the `&f.wrap` half of #17216.
4. **Module state, `our` variables, package/compunit statics.** Acceptance: a module `require`d
   on a worker is usable on the main thread; an `our` declared on one thread is visible on
   another.
5. **Delete the fork.** `fork_for_thread` keeps only execution state; `spawn_clone` stops
   copying world tables. This is #17216's close condition ("the spawn no longer forks the type
   registry or the handle table").

### 3.1 Implementation slices (accepted plan)

The maintainer accepted this ADR on 2026-11-17. The slices below refine §3 into PRs; each is
claimed on #17216 as its own branch, carries the regression tests named in its row (each a
`t/concurrency/` file whose expected output was checked against `raku`), a `news/` entry, and an
ecosystem re-run of the thread-heavy distributions (Red, Monad, ParaSeq, MCP::Server*,
Concurrent::*) against the branch's build before it is published. A slice may not add a
slow-path fallback, an `unsafe impl Send`/`Sync`, or a `&self → &mut` access to a world table
outside its lock (`docs/security.md`).

| Slice | Scope | Acceptance |
| ---: | --- | --- |
| 1 | IO handle table (§2.4): one process-wide table shared by `Arc`, an `AtomicUsize` id allocator, each handle's state behind its own lock; delete `ThreadPromisePayload`'s handle transfer and the merge on `await` | sibling workers' handles get distinct ids; a handle opened on a worker is usable (and closable) on the main thread after any form of `await`, including an array of promises; a close or seek on an inherited handle is seen by its owner |
| 2 | Type registry, routine table, method tables and method wrap chains (§2.1-2.3): the outer `RwLock` is shared, not re-created per spawn; `registry_write_gen` becomes one process-wide atomic; every `registry_mut` read-modify-write that spans two guard scopes is folded into one | `await start { EVAL 'class FromThread {...}' }` then `::('FromThread').hi` on the main thread; a method added through the MOP on a worker is dispatched on the main thread; a single-thread read-heavy benchmark does not regress in Ir |
| 3 | `types.*` side tables, `dispatch.*` (routine wrappers, operator tables, `wrap_sub_names`) and `module.*` (loaded modules, exports, lib paths, unit pads) | `&f.wrap(...)` inside a `start` is seen by `f()` on the main thread; a module `require`d on a worker is usable on the main thread |
| 4 | `our` variables (`lexicals.our_vars`) and package/compunit statics (`package_lexicals`, `unit_lexicals`) | an `our` declared or rebound on one thread is visible on another; a package static created by `EVAL` on a worker is visible on the main thread |
| 5 | Delete the fork: `fork_for_thread` keeps only execution state, `spawn_clone` copies no world table; measure the spawn cost and close #17217 by its own condition | #17216's close condition; #17217's stated ratio |

Order: slice 1 is independent and goes first; slices 2-4 each depend only on the shared
generation-counter pattern slice 2 establishes; slice 5 needs 1-4.

### 3.2 Slice 2 refinement: the routine tables are lexical, the type tables are world

Found while starting slice 2 (2026-11-17). `Registry` holds two kinds of state:

- **Package-scoped declarations** — classes, roles, enums, subsets, the method tables
  and method wrap chains, attribute and build-plan metadata, the HOW and representation
  tables. A `class`/`role`/`enum`/`subset` is `our`-scoped: rakudo installs it in its
  package, once per process. This is world state (§2.1).
- **The routine tables** — `functions`, `proto_functions`, `proto_subs`,
  `our_scoped_functions`, `token_defs`, `proto_tokens` and their indexes and memos
  (`fn_transitions`, `module_functions_present`, `our_scoped_restore_memo`,
  `token_captures`, `proto_token_params`, `proto_gen`). mutsu uses them as a *lexical
  scope emulator*: a routine scope or `EVAL` snapshots them and puts the snapshot back
  when it ends (`snapshot_routine_registry` / `restore_routine_registry`), and an import
  scope undoes its routine installs at block exit (`ImportScopeSnapshot::import_writes`).
  ADR-0113 and ADR-8751 retired the commonest cases into frame lexicals and lexical
  import sets; the rest remain.

Shared as world state, the routine tables would be wrong in two ways a synchronised
program can observe: a routine scope on one thread would install its inner `my sub`
where a concurrent call of the same routine on another thread resolves it (each
thread then runs the other's closure), and a wholesale restore would delete a
declaration another thread made meanwhile. So slice 2 splits `Registry`:

1. The routine tables move into their own per-execution-context structure (`RoutineTables`,
   reached as `routines()` / `routines_mut()`), forked copy-on-write per spawn exactly as
   the whole registry is today. A routine is lexical in Raku; what a thread can call is
   what its closures' scopes declared, which the fork carries.
2. Everything else in `Registry` becomes one world table (§2.1-2.3), behind a
   `WorldCell`-style slot shared by every interpreter: reads take the thread's snapshot,
   writes publish under the lock and move the process-wide generation.
3. Per-thread memos that live inside `Registry` today and are written on read paths
   (`mixin_what_cache`, `proto_compiled_cache`, `lexical_key_scans`, ...) move out of
   the world table into the thread's caches, so a memo fill is not a world write.
4. An import scope's class rollback (`ImportScopeSnapshot::scope_classes`) undoes the
   keys the scope added instead of putting a whole class table back, so it cannot drop
   another thread's declaration.

Out of this refinement, left for slice 4: an `our sub` created at run time on a worker
(`EVAL 'our sub f {...}'`, a `require` on a worker) is published to the package's world
stash; until then it stays visible on the thread that made it.

Slice 2 lands in two PRs: 2a moves the routine tables out (no behaviour change), 2b
shares what is left of `Registry`.

### 3.3 Slice 2b refinement: sharded tables, per-site declaration locks (2026-11-18)

Measured on slice 2b (§4 anticipated this): with the registry shared, a write
copies whatever another thread's snapshot still shares, and every reader
thread holds one. With most tables held inline, every declaration copied the
whole `Registry` (~4 ms with Red loaded); Red t/098 (eight threads minting
projection types per query) went from 26 s to 258 s. So:

- Every table of `Registry` is a `CowTable` (copying the registry is a
  reference-count bump per table), and every keyed map is a `ShardedMap`
  (`runtime/registry_sharded_map.rs`): 16 copy-on-write shards selected by key
  hash, so a write copies one shard of one table. Values that carry an AST
  (`roles`, `role_candidates`) are behind their own `Arc`, so a shard copy
  never deep-copies a definition. These are the "finer-grained tables" of §4,
  without a dependency on a persistent-map crate; reads stay a hashbrown probe
  (plus one hash for the shard).
- Indexes derived from the type tables (class short names, lexical type
  names, the lexical key scan memo) are tagged with the tables' key
  generation and kept while it holds, instead of being dropped by every write
  and rebuilt by every thread.
- A type declaration's registration (site-reuse check, method table) is
  serialised per **declaration site**, not by one process-wide lock. The
  registration runs user code (a class body, traits); a process-wide lock held
  across it is a lock-order inversion with any user lock that code takes
  (raku runs the case in `concurrent-type-declaration-lock-order.t`; mutsu
  deadlocked). Rakudo serialises composition per type the same way.

Result: Red t/098 39 s; Ir against `main`: 400 `start`s +0.9%, a method-call
loop +0.6%, `say "hello"` +3% (the sharded builtin registry), an eight-thread
OO + mixin script -14%. What remains of the gap on Red is memo invalidation:
every world write moves every thread's registry generation, where a
per-thread registry moved only the writer's. Finer invalidation (a write that
only adds a type need not drop memos about other types) is the next step if a
workload needs it.

### 3.4 Slice 4a: package and class-body statics (2026-11-18)

`package_lexicals` (package-block `my` lexicals and class-body statics) and
its two companion tables (`class_body_static_names`,
`class_body_static_types`) are `WorldCell`s. Writes go through
guard-scoped closures (`package_lexicals_write`). No `&mut` into the table
outlives its lock: an in-place mutation of a package-block `@`/`%` lexical
boxes the entry into a cell under the write lock
(`package_scope_lexical_cell`), and the mutating frame pins the cell under a
`MetaNs::PackageLexicalPin` env key while it holds a reference into it, so
another thread's rebind cannot free it mid-mutation. TRIR's free-variable
cache is keyed on the table's world generation as well.

Left for later slices: `our` variables and runtime-created `our` routines
(4b), and retiring `PublishedMixins::role_lexical`.

**`PublishedMixins::role_lexical` retired (2026-11-18).** Nothing had to be
published under the role's name: with `package_lexicals` world state, a
method of a mixin composed on another thread finds the role body's lexicals
where the composing thread filed them, as on the composing thread itself.
The stopgap was also wrong: keyed by role name with the first record
winning, it gave every later composition of the role (another class mixing
it in on another thread) the first composition's lexicals, where rakudo runs
the role body once per composed type.

## 4. Consequences

- `start` and every other spawn see the program rakudo would show them: declarations, wraps,
  module loads, `our` writes and opened handles made on any thread are visible on every other.
- A world write costs a lock acquisition and, while another thread holds a snapshot, a copy of
  the table being written (`Arc::make_mut`). Declarations are rare after start-up; a program that
  declares in a hot loop across threads pays it. Slice 2 measures it; finer-grained tables
  (per-package maps) are the remedy if it shows up, not a return to the fork.
- A world read costs one uncontended read lock plus an `Arc` clone per snapshot. The snapshot is
  taken per operation (a dispatch, a lookup), not per access.
- Spawn cost drops by the tables no longer copied (the COW snapshots were cheap, the deep clones
  of `our_vars`, `loaded_unit_pads` and the handle table were not).
- Every world write site becomes a synchronisation point: the audit in slice 2 is the large part
  of the work, and a missed read-modify-write across two guard scopes is a logical race (lost
  declaration), not a memory-safety hole, because each publish is under the write lock.

## 5. Alternatives rejected

- **Keep the fork and propagate world writes to every live thread** (a broadcast of each
  declaration). Every thread would hold a stale copy until the broadcast lands, so a reader can
  still miss a write that "happened before" it by any program-visible ordering (a `Promise` kept
  after the declaration). It also multiplies every write by the thread count.
- **Transfer world changes on `await` (the handle transfer, generalised).** Only reaches the
  awaiting thread, only at the join, and already shows the id-collision and missing-merge bugs
  for handles.
- **A single global lock around the whole Interpreter world.** Simple, but every dispatch on
  every thread would serialise on it; per-table RCU keeps reads parallel.
- **Persistent (structurally shared) maps for every table now.** Removes the copy-on-write cost
  of a write but is a large change to every table's type; deferred until slice 2's measurement
  says the copy matters.

## 6. Open questions

- Whether `instance_type_metadata` (per-object metadata) is world or should simply move onto the
  object it describes.
- Whether the slang tables (`slang_declarator_hows`, `defined_slang_declarators`), which a spawn
  currently wipes, are world (rakudo: a slang is lexical to the compilation, so a running thread
  does not change it) or need nothing.
- Whether the pragma flags kept in module state (`strict`, `fatal`) belong with the world or are
  lexical and leave it.
