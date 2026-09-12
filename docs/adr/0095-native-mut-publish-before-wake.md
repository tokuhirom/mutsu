# ADR-0095: A mutable native instance method publishes before it wakes another thread

- Status: Accepted (implemented)
- Date: 2026-09-12
- Supersedes: nothing
- Extends: [ADR-0013](0013-container-interior-mutability-cellvalue.md) (the shared
  attribute cell this publishes through)
- Related: [#7943](https://github.com/tokuhirom/mutsu/issues/7943) (consequence 1 —
  the visibility window), [#7923](https://github.com/tokuhirom/mutsu/issues/7923)
  (consequence 2 — the lost update, fixed by the delta commit)

## Context

Every mutable native instance method in mutsu is a read-modify-write over a
**private copy** of the receiver's attribute map. `call_native_instance_method_mut_in_place`
reads the shared `Gc<InstanceAttrs>` cell into an owned `AttrMap`, hands that map
to the per-class handler by value, and commits what the handler hands back:

```rust
let working = attributes.to_map();
let before = working.bits_image();
let (result, updated) = self.dispatch_native_instance_method_mut(...)?;
attributes.commit_attrs_delta(&before, &updated);
```

`commit_attrs_delta` (ADR-0013's cell, #7923's fix) already solved the *lost
update*: the commit stores only the keys whose boxed word changed, so a key
another thread wrote in between survives.

It does not solve the other half of #7943. The private copy is also a
**visibility window**: nothing the handler writes reaches any other thread until
it returns. A handler that wakes another thread *mid-flight* — keeps a promise,
emits to a supply, sends on a channel, spawns — has published none of its writes
at the moment the woken thread starts reading the receiver.

`Proc::Async.start` is the case that bit first. It sets `started` and `pid`,
keeps the `.ready` promise as soon as the child is spawned, and only then
returns. A thread blocked on `await $p.ready` woke, read `started` straight back
off the instance, saw the pre-spawn map, and `.kill` threw
`X::Proc::Async::MustBeStarted` — 1 failure in 80 runs of
`roast/S17-procasync/kill.t`, and the `.ready`-racing-`.start` variant hung about
3 runs in 4.

That was patched twice, both times locally. `fd3f1df3` moved the state onto a
constructor-built `SharedPromise` (shared by reference, so every snapshot reaches
the same object) and said so itself: *"This is targeted at Proc::Async, not
general."* A later fix wrote the two keys straight through the cell
(`live.insert("started", Value::TRUE)`) next to each working-map write. Neither is
a convention; the second is also subtly wrong (below).

## Decision

**Keep the private working map, and make the publish point first-class.**

1. The working map stays private and stays the default. It is what makes most
   handlers simple and correct: a handler that fails part-way publishes nothing,
   so a `?` cannot leave half a transition on the shared object. Converting every
   handler to read-modify-write the cell directly — the `with_attr_mut` route
   #7943 first proposed — would trade that for a per-handler audit of partial
   failure, and would change behaviour in every handler, not just timing.

2. Every mutable native handler receives an `AttrPublisher`
   (`src/runtime/native_methods/attr_publish.rs`), opened once by the single
   dispatcher that owns the snapshot-dispatch-commit triple. The convention is
   one rule:

   > **A mutable native handler must publish before any operation that can wake
   > another thread which reads the receiver.**

3. `AttrPublisher::publish(&working)` commits the delta accumulated so far **and
   rebases the before-image onto it**. This is the part a raw `cell.insert` gets
   wrong: writing through the cell publishes the value but leaves the
   dispatcher's before-image describing the map as it was *read*, so the commit
   at return still sees that key as changed and stores it a second time — over
   whatever another thread wrote to it in between. That is precisely the lost
   update `commit_attrs_delta` exists to prevent, reintroduced by the workaround
   for the other half of the same ticket. After a rebase, a key the handler
   published and never touched again is no longer part of its delta, so the
   other thread's later write survives.

4. `AttrPublisher::detached()` is the honest no-op for the two sites that run a
   mutable handler purely for its return value and discard the map (the immutable
   `IO::CatHandle` and `Supply.tap` entries): there is no shared receiver to
   publish to.

## Consequences

- The publish point is uniform. A class that needs cross-thread visibility no
  longer has to invent its own latch, as `Proc::Async` did with its
  `SharedPromise`; the three hand-rolled `live.insert` calls in
  `native_proc_async_mut` are now `publish.publish(&attrs)` and are covered by
  the rebase.
- `Supplier.done` and `Supplier.quit` gained publish points. Both write the
  terminal state (`done`, `quit_reason`) and then run tap callbacks — user code
  that can reach the same `Supplier` from another thread — and a concurrent
  `.emit` gates on exactly that state through `supply_is_terminated`, which falls
  back to the attribute map for a `Supplier` with no registry id. `Supplier.emit`
  deliberately did *not* get one: the only key it writes, `emitted`, is appended
  to and never read back anywhere, so a publish there would be a delta commit per
  emission bought for nothing. That is the rule working as intended — the
  question a publish point asks is "does anything the wake releases read this
  key", not "did I write a key".
- A publish is **atomic across keys**: one call commits the whole accumulated
  delta under one write lock, so a handler that must publish a multi-key
  transition indivisibly writes every key and then publishes once. That answers
  #7943's `started`-plus-`pid` question, and shows why `Proc::Async.start`
  deliberately does *not* do it: `started` has to be visible *during* the spawn,
  and the pid does not exist until the spawn returns, so the two are separate
  transitions rather than one.
- Publishing is explicit, so it is auditable: a handler with no publish call
  makes no cross-thread claim. Handlers that never wake anybody take the
  parameter as `_publish`, which is the documentation that they do not.
- The cost of a publish is one delta commit (one write lock, only the changed
  keys) plus one `bits_image` rebase — `u64` per key, no `Value` clones. Handlers
  that do not publish pay nothing: the parameter is a reference.
- This does not make instance attributes a memory model. Two threads racing on
  the same key still tie-break last-commit-wins, as `commit_attrs_delta`
  documents. What the rule buys is the *happens-before* case, which is the one
  Raku programs actually rely on: everything written before a wake is visible to
  whoever the wake released.

## Alternatives rejected

- **Write-through working map** (publish on every `attrs.insert`). Removes the
  audit, but also removes the rollback: a handler that returns `Err` after a
  partial transition would leave that transition committed, and which handlers
  depend on that rollback is exactly what is undocumented. Making every write a
  publish would also change the *order* other threads observe a multi-key
  transition in — `Proc::Async.start`'s `started` and `pid` become separately
  visible by construction — which is the one question #7943 asked that an
  explicit publish point lets a handler answer for itself.
- **Convert all handlers to `with_attr_mut` on the cell.** The honest full fix,
  and the reason #7943 was filed `todo:deep`. Rejected for now on the same
  partial-failure grounds, and because it is ~8000 lines of handler across nine
  files whose privacy assumptions are undocumented. The publish point makes the
  incremental version of that conversion possible: a handler can be moved onto
  the cell one method at a time without changing the dispatcher.
- **Automatic publish inside the wake primitives** (`SharedPromise::keep`,
  `supplier_emit`, …). No safe way to reach the handler's working map from
  there, and it would publish on wakes that have nothing to do with the receiver.
