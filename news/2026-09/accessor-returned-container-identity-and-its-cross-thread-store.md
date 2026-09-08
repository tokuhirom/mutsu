# A user-written attribute accessor hands back the container, not a copy of it

`todo/deep/gc-contents-mut-cross-thread-aliased-writes.md` (ADR-0068 §4 step 3)
had one route left open, and had flagged it as needing "a decision, not a
patch": an element store through a container returned by a **user-written**
accessor.

```raku
class Holder { has @.items; method bag() { @!items } }
```

Twenty threads writing 1000 distinct indices through `$h.bag[$i] = 1` landed
**418 / 543 / 304** of them (rakudo: 1000) — silent loss, no crash. An earlier
attempt keyed the funnel's guard on the shared invocant and still measured 24/24
wrong, and concluded the exclusion would have to cover the accessor dispatch
itself, which the funnel's own comment had deliberately kept outside its region.

Both halves of that conclusion were right. Neither could work until a
**deterministic bug underneath them** was removed.

## The accessor assignment was rebinding the attribute

`method items { @!items }` hands back the attribute's own `Array` — in raku it
*is* that object — so `$obj.items = LIST` and `$obj.items[$i] = v` store **into**
that container. mutsu rebuilt the whole attribute map around a fresh node
instead (`assign_method_lvalue_with_values`'s `rw_attr_target` branch →
`Value::write_back_sharing`). One thread is enough to see it:

```raku
my @alias := $h.bag;  $h.bag = (1, 2, 3);
say @alias;    # raku: [1 2 3]     mutsu: []
```

The *generated* accessor for the same attribute never had this bug, which is
exactly why ADR-0068 §8 measured `has @.seen` at 0/240 while
`method seen { @!seen }` lost half its writes: it was never really the same
route.

The concurrency consequence is what made the route look unfixable. Because the
attribute was rebound on every write, **the container's address moved on every
write** — four distinct `Gc<ArrayData>` addresses in six writes from two threads
— so the ADR-0068 store guard, which keys on the container node when the
container is not celled, locked a different stripe each time and excluded
nothing. On top of that, `write_back_sharing` commits a *copy of the whole
attribute map* into the shared cell, so a slow thread could clobber concurrent
writes to unrelated attributes as well.

`ArrayData::adopt_state_from` / `HashData::adopt_state_from` now perform the
store in place, keeping the node (and hence `.WHICH`) and dropping the map
commit entirely.

## The guard moved to the invocant, and now precedes the accessor

With the node churn gone the container is stable again, but it is still the
wrong key: the read that races is the accessor's own read of the live
container's elements, and that read happens before any guard exists. Removing
the churn without moving the guard turned the lost updates into a NaN-box tag
panic and `double free or corruption` — honest exposure, not a fix.

The invocant's `Gc<InstanceAttrs>` is the right key. Measured, it is identical
on every write from every thread that reaches the same object, and no store
moves it. `builtin_index_assign_method_lvalue` now takes the guard on it
**before** the accessor dispatch, so the accessor call, the element modify and
the store-back are one critical section. A non-instance invocant (a `Pair`
value, a package path accessor) keeps the previous container/cell key.

The cost of that decision is that user code — the accessor body — runs inside
the region. `container_lock`'s at-most-one-lock-per-thread rule makes it safe
against the obvious re-entrancy: an accessor that itself stores into a container
takes no second lock, so it cannot self-deadlock. What stays uncovered is an
accessor that *blocks on another thread* while this one holds the stripe; no
such accessor exists in the suite, and the alternative is leaving a live
container's element read racing with a `Vec` reallocation.

## Measured

Debug build, `MUTSU_GC=on MUTSU_GC_EVERY_CANDIDATE=1024 MUTSU_GC_VERIFY=1`,
96 processes at 24-way on 12 cores:

| Route | Before | After |
|---|---|---|
| `$h.bag[$i] = 1`, user accessor, array | 304-543 of 1000 writes landed | 0 / 96 failures |
| `$h.bag{$k} = 1`, user accessor, hash | — | 0 / 96 |
| `$h.bag.push($v)`, user accessor | — | 0 / 96 |
| `$obj.attr[$i] = v` written INLINE in the thread body | lost updates on 4 of 5 runs | 0 / 96 |
| ADR-0068 §8's named-sub attribute store (regression check) | 0 / 240 | 0 / 96 |

The inline row is a route §8 never probed: with no routine closing over the
invocant the container never becomes celled, so neither the celled guard nor the
name-keyed lane applied to it. The same change covers it.

Pins: `t/attribute-accessor-container-identity.t` (8 deterministic rows, green
under raku) and three new rows in `t/concurrent-attribute-element-store.t`.
Recorded as ADR-0068 §10.

Still open in step 3: route 5 (`Channel.Supply` tap captures) behind the
Channel-supply delivery bug, and ADR-0068 §3.1's unexplained
`S17-procasync/stress.t` SIGSEGV.
