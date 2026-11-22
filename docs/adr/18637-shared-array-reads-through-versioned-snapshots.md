# ADR-18637: A shared array's element read goes through a versioned, epoch-held snapshot

- **Status**: Proposed (2026-11-22)
- Date: 2026-11-22
- Amends: [ADR-8069](8069-shared-container-reads-without-a-contended-write.md) D3 (the
  borrowing element read), for `Array` nodes
- Relates to: [ADR-0068](0068-cross-thread-container-writes-need-a-synchronized-store.md)
  (the stripes writers keep), [ADR-12814](12814-concurrency-memory-safety-follows-rakudo.md)
  (the memory-safety contract), [#8069](https://github.com/tokuhirom/mutsu/issues/8069),
  [#18637](https://github.com/tokuhirom/mutsu/issues/18637)

## 1. Context

ADR-8069 decided that a read of a shared container writes no shared cache line. Its D1 (a
cell's slot as one atomically loaded word) is implementable for a payload-free value without
any reclamation at all. Slice 1 does that: shared bound-scalar reads at W=4 went from
516-634 ms to 35-38 ms on a 4-core container.

D3 is where the plan breaks. It reads an element by borrowing the container node under an
epoch pin, without ADR-0068's stripe. But every writer mutates an array's elements **in
place**, through `&mut Vec<Value>`: `ArrayData::items_mut()` (54 call sites) and
`DerefMut for ArrayData`. A reader that loads a slot, or reads the `Vec`'s pointer and
length, while a writer stores to it non-atomically is a data race. That is undefined behaviour
under Rust's memory model whether or not the buffers a writer *replaces* are retired through
the epoch, because the race is on the live buffer. A sound D3 would need atomic element slots
in `ArrayData`, which is every array operation in the interpreter.

Without an array-side answer, slice 1 alone makes `bench-threads` slower: 640 ms to 725 ms
(medians of 9). Once scalar reads stop serializing, the `@alias[$i]` reads contend harder on
the stripe, the cell mutex and the node refcount (futex calls 16.5k to 23.8k).

## 2. Decision

### S1. Every mutation of an array's elements bumps the node's version

`ArrayData::items_mut()` and the other `&mut self` methods that change `items` already form
the single chokepoint every in-place write goes through (it is where the `head` offset is
reset). After ADR-0068's sticky "second mutator thread" latch is set, that chokepoint
increments a per-node `AtomicU64` mutation version. Before the latch it costs one relaxed
load. Only writers write this line, and they already hold the node exclusively.

### S2. A celled array publishes an immutable snapshot

A `ContainerCell` holding an `Array` can publish a **snapshot**: a separate copy of the node,
tagged with the source node's version counter and the version it was copied at, owned by the cell's
published word and reclaimed through `crossbeam-epoch` (ADR-8069 §6.1) when it is replaced.
Nothing mutates a snapshot after it is built.

A reader pins, loads the snapshot, and checks two things: the cell's published word still
names it (slice 1's guard sync clears the word whenever the slot is replaced under the mutex),
and the source node's version still equals the snapshot's. If both hold, it reads the element
from the immutable copy and clones only that element. Otherwise it takes today's locked path.
A reader that passes the check just before a writer's bump sees the value from before the
write, which is the legal outcome of an unsynchronized race (ADR-12814).

### S3. Snapshots are rebuilt only when reads pay for them

The locked path counts slow reads since the cell's snapshot was last invalidated, and it
rebuilds the snapshot only once that count reaches `len / 16 + 1`. The copy is then amortized
to at most about sixteen element clones per read. A read-mostly shared array (`bench-threads`,
fan-out over a shared input) reads lock-free. A write-heavy one never reaches the threshold
and behaves exactly as it does today.

### S4. Scope

- `Array` nodes reached through a cell (`:=`-bound, captured). Hash nodes have the same shape
  through `HashData`'s mutable accessor and follow once arrays are measured.
- The name-keyed plain-lexical lanes stay out of scope, as in ADR-8069 D5.
- A cell that `descend_container_ref` escaped (slice 1's `PUB_ESCAPED`) publishes no snapshot
  in the first implementation. An element write through the escaped pointer would be caught by
  S1's version, but a whole-slot replacement through it is invisible to the cell. Making
  escaped cells eligible, by having the escaping writers report a slot replacement, is left
  to §7.

## 3. Invariants (docs/security.md)

- A reader dereferences only a snapshot (immutable, held by the published word or by the
  epoch until no pinned reader remains) and the version counter the snapshot itself keeps alive
  (§6.1). No reader reads a buffer that a writer can be writing.
- Every `unsafe` block (turning the published word back into a snapshot reference under a
  pin) carries a `// SAFETY:` comment naming the pin. There is no `unsafe impl Send`/`Sync`.
- A single-threaded program pays one relaxed load per mutation and nothing per read.
- Memory is bounded: at most one live snapshot per cell plus the retired ones the epoch has not
  released yet.
- The cycle collector treats a snapshot as an external root of the elements it holds (it holds
  their references untraced), which is conservative. A cycle through a published snapshot is
  collected once the snapshot is dropped, which happens on the next write or when the cell is
  dropped.

## 4. Consequences

- `bench-threads`' read section stops writing shared lines for the array, the last part of
  #8069's gap.
- A write to a shared array costs one more atomic increment after the latch.
- A published snapshot holds element references beyond the node's own, so an element that the
  array has dropped lives until the snapshot is replaced.

## 5. Alternatives rejected

- **D3 as written** (borrow the live node without exclusion): a data race on the live buffer
  (§1).
- **Atomic element slots in `ArrayData`**: sound, but it changes every array operation in the
  interpreter, and every single-threaded element access would pay for it.
- **Copy-on-write on every write to a shared array**: O(n) per element store, quadratic for a
  worker that fills a shared array.
- **Reader/writer locks per node**: ADR-8069 §1 measured that a shared lock still writes its
  lock word.

## 6. Soundness of the snapshot read, and how it is tested

### 6.1 Why a snapshot read cannot race

What a fast-path reader touches, and why none of it can be written concurrently or freed under
it:

1. **The cell's published word**: an `AtomicU64`, loaded with `Acquire`. Only a holder of the
   cell's mutex stores to it (ADR-8069 slice 1's protocol, unchanged). A word is either a
   payload-free value (slice 1) or a tagged pointer to a snapshot. The tag lives in the
   NaN-box niche (page 0), which no value uses.
2. **The snapshot** (`CellSnapshot { items: Box<[Value]>, version: Arc<AtomicU64>, seen: u64 }`):
   built completely by the rebuilding reader while it holds the cell's mutex and stripe, then
   published with a `Release` store of its pointer. The reader's `Acquire` load synchronizes
   with that store, so every byte of the snapshot happens-before the read. After publication
   nothing ever writes it: it is reachable only through the published word and the epoch's
   retired list, and neither hands out `&mut`. A read of memory that no one writes after a
   happens-before edge is not a data race.
3. **The snapshot's lifetime**: the reader is pinned (`crossbeam_epoch::pin()`) from the load
   to its last use. A snapshot leaves the word only through a swap made under the cell's mutex
   (a write, a slot replacement, an escape, or a rebuild), and the remover hands it to
   `Guard::defer_destroy`. crossbeam-epoch guarantees that a deferred destructor runs only
   after every thread that was pinned when the object was unlinked has unpinned. The reader's
   pin precedes its load, so it is one of those threads. This is the one `unsafe` step
   (`Shared::deref`), and its `// SAFETY:` comment names the pin.
4. **The version counter**: an `Arc<AtomicU64>` shared by the source node and every snapshot
   of it. The snapshot holds its own `Arc`, so the counter outlives the node if it has to, and
   the reader never dereferences the node itself (which a writer may be mutating). Writers bump
   it with `fetch_add(1, Release)` at the `items_mut()` chokepoint, before the mutation; readers
   load it with `Acquire`.
5. **The element it returns**: cloned out of the immutable `items`. The snapshot owns one
   reference to each element, and the snapshot is alive (point 3), so the clone's refcount
   increment lands on a live node. That is the same rule every `Value` clone follows.

Freshness, which is separate from safety: a writer bumps the version before it mutates, and
its write completes after that. A reader whose load is ordered after the write's completion
(by any synchronization: `await`, a `Lock`, a `Channel`) therefore sees the new version and
takes the locked path. A reader racing the writer may return the pre-write element, which
ADR-12814 allows for an unsynchronized race. A *lost* bump cannot happen, because the
counter is only ever changed with an RMW. Without one, two racing unsynchronized writers could
leave a stale snapshot valid indefinitely.

What is *not* claimed: the snapshot read does not order element reads of two different
arrays, just as rakudo does not without a lock.

### 6.2 Testing

- **Miri** (the CI `miri` job, `-Zmiri-disable-isolation`): unit tests in the snapshot module
  run real threads. Each has one writer that bumps, mutates and republishes, and readers that
  pin, validate and clone, over a few hundred iterations. Miri's data-race detector and its
  use-after-free checks cover points 2-4 directly. The module's tests get their own
  `cargo miri test` filter in the job, since `gc::` does not match `value::` paths. That is a
  workflow change, so it is CODEOWNERS-reviewed.
- **loom**: crossbeam-epoch builds against loom under `--cfg crossbeam_loom`. A loom model of
  the publish/validate/retire protocol (two threads, one rebuild, one write) checks every
  interleaving of the five accesses in 6.1. It runs as a `#[cfg(loom)]` test target outside the
  default `cargo test`, documented next to the module. Adding loom as a dev-dependency is part of
  the implementation PR, under the same supply-chain review as crossbeam-epoch.
- **ThreadSanitizer** (docs/security.md tooling): `t/concurrency` and the slice's stress files
  under a `-Zsanitizer=thread` build, once per slice.
- **Stress**: `t/concurrency` files that race element writes, pushes, splices and whole-array
  assignment against readers, each checked for value integrity and post-`await` visibility, and
  run 40 times on a release build before each PR.

## 7. Open questions

- Escaped cells (S4): which `descend_container_ref` callers can replace the whole slot, and
  whether they can re-sync the cell's word through a guard instead of a raw pointer.
- Whether the `len / 16` threshold should be measured on ParaSeq and on the Supply dists
  before it is fixed.
