# An already-resolved promise now takes a SupplyTicket too

`t/supply-serialize-fifo.t` test 3 — the pin added for #7811 — failed about one
run in six on `main` (11 of 30 on an idle release build here), which made the
fatal `t/` suite redden PRs at random. The failure was always the same shape:

```
# expected: '1 2 3 4 5 6'
#      got: '2 4 6 1 3 5'
```

Not noise, and not a regression from #7811: it is the half of that fix that was
never written.

#7811 orders the reactions of a supply block by having the thread that resolves
a promise reserve the reaction's place in the block's sequencer (a
`SupplyTicket`) before the pooled worker that will run it has even been woken.
But `SharedPromise::on_resolve_in_supply_group` only did that on the path where
the promise was still `Planned`: the ticket is taken in `dispatch_waiters`, and
a promise that is **already** resolved when the nested `whenever` registers
never reaches `dispatch_waiters`. Its `group` argument was simply dropped, and
the reaction ran inline on the registering thread — ahead of every reaction
already ticketed and queued for a worker. The two paths were therefore ordered
against each other by luck, which is exactly what the test alternates between
(even values keep the promise before the nested `whenever` sees it, odd values
after), and why the wrong answer came out cleanly separated rather than
interleaved.

The already-resolved branch now reserves its ticket too, on the registering
thread — the ordering point, since that thread is running the enclosing supply
block's reactions in order — and hands the body to a pooled worker exactly as
`dispatch_waiters` does. Both paths enter the block through the one sequencer,
in the order the reactions were created.

Redeeming the ticket inline instead, and keeping the body on the caller's
thread, is not an option: that thread is typically running the enclosing supply
block's own reaction and so already holds the group lock, so parking it behind
an earlier ticket — whose worker is itself blocked on that lock — deadlocks.
This mirrors the reason the *pending* path hands off to a worker rather than
running user code on the resolver, which is what deadlocked `Cro::HTTP`'s
middleware pipeline when it was tried there.

Consequence for the API contract: `on_resolve_in_supply_group` now returns
`false` for an already-resolved promise when a group is given, because the
reaction genuinely has not run yet. Plain `on_resolve` (no group) keeps its
synchronous fast path and its `true`.

40 consecutive runs of the file are green after the fix. `t/supply-serialize-fifo.t`
grows a fourth test that keeps *every* promise before its nested `whenever`
registers, so the already-resolved path is exercised on its own rather than only
in alternation with the ticketed one.

Closes #7831.
