# ADR-18752: An in-range element store to a shared array takes no exclusive lock

- **Status**: Accepted (2026-11-22, by the maintainer), with conditions (D6 C1-C4: soundness rows for the collector and the pre-latch path, a <= 150 ms prototype gate with a stop rule, D4 only after the second-mutator latch, implementation after the release freeze)
- Date: 2026-11-22
- Supersedes: [ADR-0068](0068-cross-thread-container-writes-need-a-synchronized-store.md)'s
  write side for in-range element stores (its exclusion stays for every structural write)
- Relates to: [ADR-8069](8069-shared-container-reads-without-a-contended-write.md),
  [ADR-18637](18637-shared-array-reads-through-versioned-snapshots.md),
  [ADR-12814](12814-concurrency-memory-safety-follows-rakudo.md),
  [#8069](https://github.com/tokuhirom/mutsu/issues/8069),
  [#18752](https://github.com/tokuhirom/mutsu/issues/18752)

## 1. Context

ADR-8069 and ADR-18637 (#18693) made reads of a shared container stop writing shared cache
lines. What is left of #8069 is `bench-threads`' write section: four workers each store into
their own 16 slots of one `:=`-bound 64-element array, 80k stores in total.

| measured (4-core container, release) | time |
| --- | ---: |
| 4 workers, disjoint slots | 260-310 ms |
| the same 80k stores on one thread, after a spawn | ~50 ms |
| raku, 4 workers | ~92 ms |

The work is cheap; the cost is that every store holds, exclusively, locks that every other
writer of the array also needs: the cell's `Mutex<Value>` (to reach the node), then ADR-0068's
cell and node stripes (`Mutex<()>`), for the whole store. Four writers hand those locks to one
another 80k times, and a contended `std::sync::Mutex` parks the losers on a futex.

Making the critical section shorter does not help. Reading the node address under the cell
mutex instead of cloning the whole value saved 590 instructions per store and made the
contended run *slower* (311 -> 438 ms), because two locks then contended instead of one. A
store has to stop excluding the other stores.

### Why "no lock at all" is unsound

The direction approved for this ADR was "write the slot directly, with no shared lock". On its
own that is a data race. Most of the interpreter reads a live array node through
`&[Value]` (`ArrayData::items()`: iteration, `.elems`, `.join`, slices, `eqv`, the locked
element read that ADR-18637's snapshot falls back to) and holds the node stripe while it does.
A store that writes a slot while one of those readers reads it is a concurrent non-atomic read
and write of the same word. That is undefined behaviour under Rust's memory model, whatever the
hardware does. The store can drop the exclusive lock only if those readers can no longer run
while it writes.

## 2. Decision

### D1. The stripes become reader/writer locks, used the other way round

ADR-0068's stripe table holds `RwLock<()>` instead of `Mutex<()>`. The roles are inverted from
the usual read/write meaning:

- **Shared side:** an *in-range element store*, meaning a store to an index below the node's
  length into a plain `Array` with no native backing, which neither grows, shrinks nor
  reallocates the node.
- **Exclusive side:** everything that holds a stripe today. That is every structural write
  (push, pop, splice, growth, whole-container assignment, `make_mut` copies) and every live-node
  reader (iteration, `.elems`, the locked element read).

Two in-range stores of one array then run concurrently. A store never overlaps a structural
write or a live-node read, which keep exactly today's exclusion.

### D2. The slot is written with an atomic swap

Two in-range stores can target the same slot (an unsynchronized program racing on one element).
So the store does not write through `&mut Value`; it swaps the slot's 64-bit NaN-box word
through `AtomicU64::from_ptr` on the element's address, which is derived from the node's buffer
pointer without creating a `&mut ArrayData`. The swap returns the displaced word, and the store
drops it. No other thread can be reading that word, because:

- live-node readers hold the exclusive side, so none is running;
- another store that swapped the same slot got a different word back, its own predecessor;
- ADR-18637 snapshot readers read the snapshot's own copy, never the live slot.

So no epoch is needed for the displaced element. The node itself cannot be freed or reallocated
during the store, because both of those are structural writes, which take the exclusive side.

### D3. The ADR-18637 version is bumped only when a snapshot exists

An in-range store bumps the node's `snapshot_version` exactly as `Trace::note_mutable_access`
does today: only once a snapshot of the node has ever been published. A write-heavy array never
earns a snapshot (ADR-18637 S3), so its stores pay no shared atomic increment.

### D4. Reaching the node without the cell mutex

The store currently reaches the node through `descend_container_ref`, which locks the cell's
`Mutex<Value>`. That mutex would then be the remaining contended lock. A cell that holds an
`Array` therefore also publishes the node's address in its ADR-8069 word: a tagged,
epoch-retired word, as ADR-18637's snapshot word is, and owning one reference to the node. The
store pins, loads the node from the word, and takes the node's stripe on the shared side. It
then checks that the word still names that node before it writes; a cell re-seated meanwhile
sends it to today's path. An escaped cell (`PUB_ESCAPED`) publishes nothing and keeps today's
path.

### D5. Scope

- Plain `Array` nodes with no native backing, no shape and no laziness. A typed or defaulted
  element store that has to check or decay a value does so before the swap, and a store that
  would autovivify a hole or grow the node is structural.
- Hashes are out of scope. A hash store can rehash, so it is always structural.
- The stripe stays keyed on the node address (and on the cell's for structural writes,
  unchanged). ADR-0068's at-most-one-stripe-per-thread rule holds for both sides.

## 3. Soundness (docs/security.md)

| access | what excludes it |
| --- | --- |
| a store's swap vs. a live-node read of the same slot | the reader holds the stripe exclusively; the store holds it shared |
| a store vs. a structural write (buffer reallocated or freed) | the same exclusion |
| a store vs. another store, same slot | both are atomic swaps on one word, and each drops only the word its own swap returned |
| a store vs. another store, different slots | distinct words; no `&mut ArrayData` is created, only element pointers |
| a store vs. a snapshot reader | the reader never touches the live node (ADR-18637 §6.1) and revalidates against the version (D3) |
| the store's node lookup vs. a cell re-seat | the node word is held by the epoch (D4); the store re-checks it after taking the stripe |

The `unsafe` steps are: deriving the element pointer from the buffer pointer while holding the
shared stripe, `AtomicU64::from_ptr` on it, and rebuilding the displaced `Value` from the
returned word. Each carries a `// SAFETY:` comment naming the stripe mode it relies on. There
is no `unsafe impl Send`/`Sync`.

ADR-12814 is unchanged in what it allows. A program racing two stores on one element sees one
of them win, as on rakudo. Nothing in that race can free or tear memory.

## 4. Testing

- **Miri** (the `miri-cell-reads` job, Tree Borrows as there): threaded unit tests with
  in-range stores to distinct and to identical slots, racing a structural writer (push) and a
  live-node reader (iteration). Miri's race detector checks D1 and D2 directly.
- **loom**: a model in `crates/loom-cell-snapshot` that adds the shared/exclusive node lock,
  two slot swappers and one exclusive reader, and checks that every swapped-out word is dropped
  exactly once.
- **ThreadSanitizer** over `t/concurrency` (docs/security.md), compared against `main`.
- **Stress**: `t/concurrency` files racing disjoint-slot stores, same-slot stores, pushes and
  iteration on one shared array, each checked for value integrity and post-`await` visibility,
  and run 40 times on a release build.
- **Bench**: `bench-threads`' write section (4 workers), before and after.

## 5. Consequences

- Stores to disjoint slots of one shared array scale with cores instead of serializing. The
  expected write section is close to the single-thread figure divided by the core count, plus
  the shared-lock acquisition: the `RwLock`'s reader count is still one cache line that every
  store writes, but it never parks a thread.
- Live-node readers pay an `RwLock` exclusive acquisition instead of a `Mutex` one, which is
  about the same cost.
- A store that misses the in-range shape takes the exclusive side, exactly as today.

## 6. Alternatives rejected

- **No lock at all** (the literal direction): a data race with every live-node reader (§1).
- **Atomic element slots everywhere**: every `&[Value]` user of `ArrayData` would change, and
  single-threaded code would pay for it.
- **A shorter critical section under the same exclusive locks**: measured slower (§1).
- **Per-slot stripes**: a live-node reader would have to take every slot's stripe.

## 7. Open questions

- Whether an `RwLock` writer-preferring policy starves stores while a reader iterates in a
  loop. `std`'s `RwLock` on Linux prefers writers, which here are the *exclusive* side, so a
  steady stream of stores cannot starve a reader.
- Whether to publish the node word (D4) for every celled array or only once a second mutator
  thread exists. The latter matches ADR-8069 §3's "a single-threaded program pays nothing".
