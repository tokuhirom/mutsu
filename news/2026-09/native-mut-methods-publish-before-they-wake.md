# A mutable native instance method now publishes before it wakes another thread

Every mutable native instance method in mutsu is a read-modify-write over a
**private copy** of the receiver's attribute map: the dispatcher reads the shared
`Gc<InstanceAttrs>` cell into an owned `AttrMap`, the per-class handler mutates
that copy, and the dispatcher commits the delta when the handler returns.

[#7923](https://github.com/tokuhirom/mutsu/issues/7923) fixed the *lost update*
half of that convention — `commit_attrs_delta` stores only the keys whose boxed
word changed, so a key another thread wrote in between survives.
[#7943](https://github.com/tokuhirom/mutsu/issues/7943) kept the other half open:
the private copy is also a **visibility window**. Nothing the handler writes
reaches another thread until it returns, so a handler that wakes a thread
*mid-flight* — keeps a promise, emits to a supply, sends on a channel, spawns —
has published none of its writes at the moment the woken thread starts reading
the receiver.

`Proc::Async.start` is the case that bit: it sets `started` and `pid`, keeps the
`.ready` promise as soon as the child is spawned, and only then returns. A thread
blocked on `await $p.ready` woke, read `started` off the instance, saw the
pre-spawn map, and the `.kill` that followed threw
`X::Proc::Async::MustBeStarted`. That had been patched twice, both times for one
class only: first by moving the state onto a constructor-built `SharedPromise`
(shared by reference, so every snapshot reaches the same object), then by writing
the two keys straight through the cell next to the working-map write.

## What changed

The publish point is now a first-class part of the convention.
`src/runtime/native_methods/attr_publish.rs` introduces `AttrPublisher`, opened
once by `call_native_instance_method_mut_in_place` — the single dispatcher that
owns the snapshot-dispatch-commit triple — and passed to **every** mutable native
handler. The rule it carries is one line:

> A mutable native handler must publish before any operation that can wake
> another thread which reads the receiver.

`publish(&working)` commits the delta accumulated so far **and rebases the
before-image onto it**. That rebase is the part the hand-rolled
`cell.insert(...)` got wrong: writing through the cell publishes the value but
leaves the dispatcher's before-image describing the map as it was *read*, so the
commit at return still sees that key as changed and stores it a second time —
over whatever another thread wrote to it in between. Fixing the visibility window
by hand had quietly reintroduced the lost update the delta commit exists to
prevent. `a_published_key_is_not_re_committed_over_a_later_writer` pins it.

The working map stays private, and stays the default: it is what lets a handler
fail part-way without leaving half a transition on the shared object. Handlers
that wake nobody take the parameter as `_publish`, which is the documentation
that they make no cross-thread claim. `AttrPublisher::detached()` is the honest
no-op for the two sites that run a mutable handler purely for its return value
and discard the map (the immutable `IO::CatHandle` and `Supply.tap` entries).

`Proc::Async.start`'s three hand-rolled `live.insert` calls became
`publish.publish(&attrs)`, and `Supplier.done` and `Supplier.quit` gained publish
points: both write the terminal state and then run tap callbacks — user code that
can reach the same `Supplier` from another thread — and a concurrent `.emit`
gates on exactly that state. `Supplier.emit` deliberately did not get one: the
only key it writes is appended to and never read back, so a publish there would
cost a delta commit per emission and buy nothing.

## Pins

Five unit tests in `attr_publish.rs` cover the mechanism: a mid-flight write is
invisible until published and visible after; a published key is not re-committed
over a later writer; a write *after* the publish still lands at return; a removal
after the publish reaches the cell; a detached publish point does nothing.
`t/concurrency/procasync-ready-publishes-started.t` remains the end-to-end pin.

The decision, the alternatives weighed (write-through working map, converting
every handler onto `with_attr_mut`, publishing from inside the wake primitives),
and what this deliberately does *not* promise are recorded in
[ADR-0095](../../docs/adr/0095-native-mut-publish-before-wake.md).
