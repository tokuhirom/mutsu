# ADR-17216: One program world, shared live by every thread

- **Status**: Proposed
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
