# A celled container's element store now excludes concurrent threads

Sharing one `@`/`%` across threads corrupted the interpreter's heap. Not
occasionally, and not only under the elaborate stress harness ADR-0068 built for
it: on an ordinary debug build, this program failed **93 times out of 96**, with
`double free or corruption`, `corrupted size vs. prev_size`, `Arc counter
overflow`, `Gc::drop strong-count underflow`, and silent short counts.

```raku
{
    my @a;
    sub put-it($i) { @a[$i] = 1 }
    await (^20).map: -> $t { start { for ^50 -> $k { put-it($t * 50 + $k) } } };
    say @a.grep(*.defined).elems;   # want 1000; got 906, or a core dump
}
```

It is now **0 out of 240** at 24-way oversubscription, and the same holds for
the hash twin, for the `.tap` fizzbuzz idiom of
`roast/integration/advent2014-day05.t`, and for `Thread.start` bodies —
ADR-0068's routes 1 and 4. `t/concurrent-celled-container-store.t` pins all
four shapes.

## Why the container was unprotected

mutsu has two mechanisms for a container that more than one thread can reach.
The name-keyed lanes in `runtime/runtime_shared_vars.rs` hold a write lock
across the whole read-modify-write, which is what makes twenty concurrent
`start { @a[$i] = 1 }` blocks all land. The other is the `ContainerRef` cell the
closure machinery boxes a captured container into.

The two do not hand off. `assign_array_elem_to_shared_var` declines for a celled
container, on the recorded premise that it "is already shared through the
Mutex", and lets the general assignment path write through it. The general path
does not inherit that lock: `descend_container_ref` takes the cell's
`Mutex<Value>` only long enough to derive a raw pointer into the slot, releases
it, and the rest of the statement mutates through `gc_contents_mut` with nothing
held.

Two writers is the obvious hazard. The one that actually dominated is subtler:
`Value::with_deref` / `into_deref`, the read chokepoint for every celled
container, take that same `Mutex<Value>` and clone the inner `Value` out. Reader
and writer therefore synchronised on a lock the writer had already dropped —
i.e. on nothing — so a reader could clone a `Value` mid-overwrite and take a
refcount on a node the writer had just released. That is where the freed-twice
allocations came from.

## The fix

`src/value/container_lock.rs` adds one small mechanism: a table of 64 striped
mutexes, keyed by the **address of the shared `ContainerCell`**. Both the
element store and the two read chokepoints take it.

The cell, not the container node, is the key — and that correction is the
substance of the change. Instrumenting the store showed that twenty threads
writing one celled array reach **thirteen distinct `Gc<ArrayData>` addresses**,
because `Gc::make_mut` copies a node whose strong count is above one. A lock
keyed on the node excludes nothing, and measurably did not: it left the failure
rate at 13/96. Keyed on the cell — the one thing every alias genuinely shares —
and applied to readers as well, it goes to zero.

It is not the cell's own `Mutex<Value>`. `descend_container_ref` has to take
that lock to produce the pointer it returns, so a guard holding it across the
mutation would self-deadlock on the next read of the same cell. A separate lock
with the same *key* gets the exclusion without the reentrancy.

Two properties keep it safe and free:

- **A thread holds at most one of these locks at a time**; a nested acquisition
  is a no-op. That makes a lock-ordering deadlock between two threads
  structurally impossible and lets a store re-enter another store.
- **Nothing locks until a VM mutator thread has been spawned** — ADR-0068 §4's
  process-global `AtomicBool`, set on the first registered spawn. A
  single-threaded program pays one `Relaxed` load per celled read or store and
  never touches a mutex.

## What is still open

This is ADR-0068 steps 1 and 2. Step 3 — widening to the remaining reasons the
name-keyed lane declines (attributes and twigils, a container that was never in
a spawning frame's env, a write that is not name-keyed at all such as
`$obj.attr[$i]`) — is still open, and so are route 3 (object attributes,
unclassified), route 5 (blocked behind a Channel-supply delivery bug) and the
unexplained `S17-procasync/stress.t` SIGSEGV. They stay tracked in
`todo/deep/gc-contents-mut-cross-thread-aliased-writes.md`.

The other correction worth carrying forward is methodological, and it is
recorded in ADR-0068 §7.1. The previous investigation concluded that CPU
oversubscription was the necessary ingredient and that hand-shrunk probes were
untrustworthy. Neither holds. What decides whether a probe reproduces is **which
store path it takes**: a `start` block that mentions the container lexically is
excluded from celling by `thread_escaping_captures` and lands on the safe lane,
which is why five earlier shrunk probes came back clean. Reach the container
through a **named sub the thread body merely calls** — a route the thread-escape
analysis cannot see, per ADR-0039 §8.6 — and it fails on the first run, in a
debug build, with the GC off.
