# A supply block now publishes its reactions in the order their sources produced them

`t/whenever-callback-recompile-semantics.t` subtest 5 failed roughly half the
time ([#7811](https://github.com/tokuhirom/mutsu/issues/7811)). Its shape is a
nested `whenever` created once per emitted value:

```raku
my $out = supply {
    whenever $src -> $v {
        my $p = Promise.new;
        whenever $p { emit $v * 10 }
        $p.keep;
    }
};
```

Three values in, and `@fired` came out as `10 30 20` about as often as
`10 20 30`. `raku` produced `10 20 30` 20 times out of 20 on the same snippet,
including under 2x CPU oversubscription.

## Two mechanisms

**The supply block's serialize lock decides the order, and nothing fed it one.**
Raku guarantees "you can only be in one `whenever` block at a time" per
`supply {}` instance, and mutsu enforces that with a per-block group lock held
across the callback. But the lock is first-come-first-served, so the order it
publishes is the order threads happen to arrive in.

**And promise-driven reactions arrived in no order at all.** A nested
`whenever <Promise>` body runs on whichever thread resolves the promise
(`SharedPromise::dispatch_waiters`), and each resolution is submitted as its own
pooled task. With three promises kept in quick succession, three pooled workers
raced each other's wake-up latency for which body reached the supply block
first.

An early attempt made the group lock itself a strict FIFO ticket lock, on the
theory that Rakudo's `Lock::Async` queues its waiters and mutsu should too. That
only halved the failure rate -- the *arrival* order at a fair lock was itself the
race -- and it cost real throughput: a hot `Supply.act` stream pays a context
switch per handoff under a strict queue, which dropped Log::Async's
`05-concurrent` from 400 delivered messages to 159 inside its two-second window.
The lock stays first-come-first-served.

The order those reactions should come out in is fixed long before any worker
wakes: it is the order the promises were resolved in, on one thread, in program
order. So that is where the place in the queue is now taken. A waiter registered
via the new `on_resolve_in_supply_group` carries the enclosing block's serialize
group; `dispatch_waiters` reserves a `SupplyTicket` for it on the resolving
thread, before the pooled task is even submitted, and the worker waits for that
ticket's turn before taking the lock. The sequencer that hands out the tickets
sits beside the group lock under its own mutex rather than inside it, so a
reaction that took no ticket -- every `Supply.act` callback, every ordinary
`whenever` -- neither waits on it nor advances it, and pays nothing. Each parked
ticket holder gets its own condvar, so a handoff wakes exactly the one thread
whose turn it is. Nothing about the pool changes — tasks
still run wherever there is a worker, they just enter the supply block in the
order their sources produced them. An unredeemed ticket (a panicking task, a
waiter dropped during unwind) cancels itself on drop, so a dispatch that never
arrives cannot wedge the group.

Measured over 40 runs at three values and 20 runs at thirty: ordered every time,
before and after loading the box to 2x its cores. mutsu is in fact steadier here
than the oracle — `raku` reorders about 3 runs in 25 once the stream is long
enough (`10 20 30 ... 230 250 240 260 ...`), which is worth knowing: the strict
assertion in subtest 5 is pinning a property Raku only *usually* delivers, so
the guarantee mutsu now has is the stronger one.

Pinned by `t/supply-serialize-fifo.t`, which also covers a promise kept *before*
its nested `whenever` sees it (the body then runs synchronously on the
registering thread and must still land in sequence).
