# ADR-8069: A read of a shared container does not write a shared cache line

- **Status**: Proposed
- Date: 2026-11-22
- Relates to: [ADR-0068](0068-cross-thread-container-writes-need-a-synchronized-store.md)
  (the cross-thread container lock; its *read side* is what this ADR replaces),
  [ADR-12814](12814-concurrency-memory-safety-follows-rakudo.md) (the memory-safety contract),
  [ADR-0001](0001-gc-strategy-and-phasing.md) §7 layer 3c (biased reference counting),
  [ADR-17216](17216-one-program-world-shared-by-every-thread.md) (one program world),
  [#8069](https://github.com/tokuhirom/mutsu/issues/8069) §4.3-4.4 (the measurements this
  generalizes).

## 1. Context

`bench-threads` is the bench row mutsu loses by the widest margin (median ratio 2.54 over
2026-11-14..21). Its write section was fixed by giving a `:=`-bound container's element store
its lane back under the container lock (#18609). What is left is the read section: four
workers reading one shared `:=`-bound array and one shared bound scalar.

Measured on a 4-core container, release build, 1.2M reads split across W workers
(the scaling probe of #8069 §6):

| loop body | W=1 | W=4 | scaling |
| --- | ---: | ---: | ---: |
| no container | 170 ms | 53 ms | 3.2x |
| worker-local array | 338 ms | 121 ms | 2.8x |
| shared plain `@` lexical | 318 ms | 465 ms | **0.68x** |
| shared `:=`-bound array + scalar | 375 ms | 466 ms | **0.80x** |
| shared `:=`-bound scalar only (`$s += $sref`) | 154 ms | 331 ms | **0.47x** |

On one thread the read is as fast as rakudo's (600k reads: mutsu ~270 ms, raku 190-300 ms).
With four, every shared read gets *slower* the more threads read it, and reading one bound
`Int` is the worst case. rakudo scales the same loops (W=4 read section 127 ms vs 162 ms at
W=1).

Two experiments rule out the obvious lever:

- **Making ADR-0068's stripes reader/writer locks** (readers take a shared guard): no change
  (W=4 219-231 ms vs 213 ms).
- **Removing the stripe acquisition from the read paths entirely** (unsafe, measurement only):
  no change either (W=4 234-247 ms).

So exclusion is not what costs. What costs is that a read **writes**: every read of a shared
cell performs atomic read-modify-writes on cache lines that every reading thread shares, so
the line bounces between cores on every read:

1. `ContainerCell::value` is a `Mutex<Value>`; every read locks and unlocks it (two RMWs on the
   cell's line).
2. ADR-0068's stripe for the cell (and for the node, on an element read): two more RMWs per
   acquisition, on a line shared by every thread reading that cell.
3. The read clones the held `Value` out: for a container node that is a refcount increment and
   a matching decrement when the clone drops, on the node's header. `GcBox` keeps two counters
   in lockstep (the backing `Arc`'s and its own `header.strong`), so it is four RMWs.

Removing (2) alone leaves (1) and (3), which is why the experiments moved nothing. A read that
scales has to perform no store to a shared line at all.

## 2. Decision

**A read of a shared container performs no atomic read-modify-write on memory other threads
read.** Concretely:

### D1. A cell's slot is one atomically loaded word

`ContainerCell::value` becomes an atomic word holding the `Value`'s NaN-boxed bits (a `Value`
is one 64-bit word). A read is an `Acquire` load. A write is a `swap` under the writer's
existing exclusion (ADR-0068's stripe, unchanged for writers), so concurrent writers still
serialize and a racing pair still sees one order (ADR-12814). The cell's other fields
(constraint, readonly kind, default) are unchanged.

### D2. Displaced values are reclaimed after every reader is done with them

A word loaded without a refcount can name a node that a writer has just displaced. The writer
therefore does not drop what it displaced: it retires it to an **epoch-based deferred-free
list**, and a retired value is released only once every thread that could have loaded it has
passed a quiescent point. A reader *pins* the current epoch for the duration of one read (a
thread-local store, no shared RMW) and unpins after. This is the mechanism MoarVM's readers get
from a tracing GC: the node cannot be freed while it is being looked at.

The epoch machinery is either `crossbeam-epoch` (a new dependency, to be vetted under
`docs/security.md`) or a small in-tree implementation tied to the existing interpreter
safepoints (the VM already polls at loop back-edges and calls); §6 leaves that choice open.

### D3. A read borrows; it does not clone

An element read of a shared container (`@a[$i]`, `$s` through a bound cell) reads the slot under
the pin and copies out only the *element* (an immediate `Int`/`Num`/`Bool` needs no refcount; a
heap element takes the one refcount the result needs). The container node itself is not cloned
and dropped around the read, so its header is not written.

### D4. ADR-0068's read side retires onto D1-D3

The read paths that take a stripe today (`Value::with_deref`, `ContainerCell::load`, the indexed
read in `vm_var_index_ops.rs`, `container_read::shared_snapshot` and kin) stop taking it once the
value they read is pinned. Writers keep the stripe: ADR-0068's write-side guarantee and its
at-most-one-lock rule are unchanged. ADR-0068 is superseded for its read side only.

### D5. Out of scope

- The name-keyed cross-thread lanes for plain `@`/`%` lexicals (`runtime_shared_vars.rs`,
  #8069 §4.2): their read cost has the same shape (a lineage chain of `RwLock<HashMap>` read per
  access) and collapsing them into the container model is its own decision. D1-D3 are what that
  collapse would reach for.
- Biased reference counting (ADR-0001 layer 3c, #8069 §4.4). D3 removes the refcount traffic
  from the read path; 3c would remove it from every *other* clone. It stays a separate decision.

## 3. Invariants (docs/security.md)

- Memory safety holds for a program that synchronizes its shared data and for everything the
  program cannot see (ADR-12814): no reader can dereference a freed node, because every node a
  reader can reach is either held by a cell or retired and not yet released.
- Every `unsafe` block (the word-to-`Value` reinterpretation, the borrow under a pin) carries a
  `// SAFETY:` comment naming the pin it relies on. No `unsafe impl Send`/`Sync`.
- A thread that never pins (a single-threaded program) pays nothing: the deferred list is only
  used once a second mutator thread exists (ADR-0068 §4 step 1's latch), and before that a
  write drops what it displaces immediately, as today.
- Deferred frees are bounded: the list is drained at every quiescent point, so retired memory
  is proportional to the writes since the slowest reader's last read, not to program length.

## 4. Consequences

- `bench-threads`' read section should scale like the worker-local row (2.8-3.2x on 4 cores)
  instead of 0.5-0.8x; that is the remaining gap of #8069 after #18609.
- Every program that shares a container between threads benefits, not one benchmark: every
  `Promise`/`start` program that reads a bound or captured container in its workers.
- The cell's API changes shape: code that today holds `cell.lock()` across a read migrates to a
  pinned load. The 86 `ContainerStructGuard` acquisition sites are re-classified into readers
  (move to a pin) and writers (keep the stripe); that audit is the bulk of the work.
- A write is slightly more expensive after the first spawn (a swap plus a retire instead of a
  store plus a drop); writes are the minority on every measured workload.
- The cycle collector (ADR-0001 §7) must treat retired-but-unreleased values as roots until they
  are released, or it can collect a node a pinned reader is looking at.

## 5. Alternatives rejected

- **Reader/writer stripes** (#8069 §4.3 "immediately"): measured, no gain — a shared lock still
  writes the lock word.
- **Dropping the read-side stripe only**: measured, no gain — the cell mutex and the refcount
  traffic remain.
- **A seqlock per cell**: a reader would still have to clone the `Value` (a refcount RMW) inside
  the read section, and a clone of a value being displaced is exactly the use-after-free D2
  prevents.
- **Thread-local caches of shared containers**: reintroduces the per-thread snapshot ADR-17216
  just removed; a write on one thread would not be seen by another.

## 6. Open questions

- `crossbeam-epoch` or an in-tree epoch tied to the VM's safepoints. The dependency is proven;
  the in-tree one avoids a new crate in the trust base and can reuse the poll points.
- Whether a `Hash` node read (`%h{$k}` on a shared hash) can use the same borrow, given that a
  hash read hashes the key while the node may be rehashed by a writer (it is pinned, so the old
  table stays valid, but the reader may then miss a just-inserted key; ADR-12814 allows that
  for an unsynchronized race).
- How the JIT's inlined element read (#8069 §4.5) interacts with a pin.
