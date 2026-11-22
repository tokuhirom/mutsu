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
tagged with the source node's address and the version it was copied at, owned by the cell's
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
  to §6.

## 6. Open questions

- Escaped cells (S4): which `descend_container_ref` callers can replace the whole slot, and
  whether they can re-sync the cell's word through a guard instead of a raw pointer.
- Whether the `len / 16` threshold should be measured on ParaSeq and on the Supply dists
  before it is fixed.

## 3. Invariants (docs/security.md)

- A reader dereferences only a snapshot (immutable, held by the published word or by the
  epoch until no pinned reader remains) and the source node's version word (the reader holds
  the cell, which holds the node). No reader reads a buffer that a writer can be writing.
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
