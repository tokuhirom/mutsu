# Mutable native-method dispatch commits a delta, not the whole attribute map

Two threads working on one `Proc::Async` could lose each other's attribute
writes, and `roast/S17-procasync/kill.t` test 7 failed intermittently on `main`
because of it — blocking unrelated PRs. Both halves of that are now fixed
([#7923](https://github.com/tokuhirom/mutsu/issues/7923), closing
[#7892](https://github.com/tokuhirom/mutsu/issues/7892)).

## The lost update

Every mutable native method is a read-modify-write over the receiver's whole
attribute map. The dispatcher cloned the map (`to_map()`), handed the clone to
the per-class handler, and stored whatever came back (`commit_attrs()`). The two
locks are individually safe and the pair is not: any key a *different* thread
committed in between was silently replaced by the snapshot's older version. That
shape routed `Promise`, `Channel`, `Supply`, `Supplier`, `Proc`, `Proc::Async`,
`Encoding::Decoder`, `ThreadPoolScheduler` and `IO::CatHandle`, from four
separate call sites — `Proc::Async` is only where roast deliberately puts two
threads on one instance, so it is where the hole showed first.

The ticket asked for a decision between a delta commit and serializing the whole
dispatch per instance. **Delta commit won.** Serializing would hold one lock
across the handler, and these handlers block for a long time — `Proc::Async.start`
spawns a process, `IO::CatHandle` reads files — so it trades a lost update for a
stall, and still does nothing about writes arriving from outside this path.

So a mutable native method's returned `AttrMap` now means *the new state of the
keys it touched*, not the instance's complete new state.
`InstanceAttrs::commit_attrs_delta` takes the boxed-word before-image of the map
as it was read (`AttrMap::bits_image`) and, under one write lock, stores the keys
whose word changed, removes the keys that are gone, and leaves everything else
alone — so a key neither side touched keeps the other thread's value. Where both
threads rewrote the same key, the last commit wins; there is no happens-before
between them to prefer, and it is what a per-key write lock would have given.

"Did the handler touch this key" is answered by comparing NaN-box words rather
than values: cloning a `Value` preserves the word (a pointer clone bumps the
refcount but keeps the address), so an entry carried through untouched compares
equal, and the before-image costs one `u64` per attribute instead of a second
full map clone. It errs in the safe direction — a word that differs for a
semantically equal value just commits a write that the old code made anyway.

The one entry point, `call_native_instance_method_mut_in_place`, now owns the
snapshot-dispatch-commit triple, and the function that let a call site commit by
hand is gone. The bug cannot be reintroduced at a fifth call site.

## The late publication

The delta commit alone does not fix `kill.t`. Measuring the failure showed the
killer thread's `await $p.ready` returning and `$p.started` reading `False`
immediately after — not a lost write but a write that had not landed yet.
`Proc::Async.start` keeps the `.ready` promise as soon as the child is spawned
and then carries on (stdin registry, reader threads, the waiter thread) before
returning, so the thread it just woke reads the instance while `started` and
`pid` still live only in the handler's local copy. Committing at return is too
late no matter what the commit does.

`start` therefore publishes `started`, `pid` and `spawn_error` straight through
the shared cell before the keep that hands them to another thread. Rakudo sets
its `$!started` first thing in `start` for the same reason, and that write is on
the shared object too.

## Measurements

The ticket's repro (~1.5% per run) was too weak to judge a fix by, so the race
was probed directly: one thread on `await $p.ready` reading `$p.started`, with
both handles tapped so `start` has real work left to do after the keep. That
loses the race on 7-12 of 12 rounds before the fix and 0 after, in 0.02s — it is
the new `t/concurrency/procasync-ready-publishes-started.t`, which caught the bug
on 10 of 10 pre-fix runs and passed 30 of 30 post-fix runs, idle and under 4x CPU
load. `roast/S17-procasync/kill.t` went 25 for 25 under the same load.

The delta commit has its own deterministic pins in
`src/value/value_instance/tests.rs`: a concurrent writer's key surviving (and the
old whole-map path losing it), removals still applying, the contended-key
tie-break, an untouched map committing nothing at all, and four threads hammering
one cell.
