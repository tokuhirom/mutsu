# `.head(N)` on a channel-backed live Supply drops every emission

```raku
my @res;
my $done;
Supply.interval(0.1).head(3).tap({ @res.push($_) }, :done({ $done = True }));
for ^40 { last if $done; sleep .1 }
say "done=$done res=@res[]";
```

```
$ raku                     $ mutsu
done=True res=0 1 2        done=True res=
```

## What the repro isolates

The **tap callback never runs at all** — a counter incremented inside it stays
at 0 and a `say` inside it prints nothing — while the `:done` callback fires
immediately. So nothing is lost in transit; the derived Supply is simply empty
and already finished.

`.head` on a *Supplier*-backed supply is fine (it sets a `head_limit` the tap
registration honours), and `.head` on a materialized supply is fine
(`Supply.from-list(0,1,2).head(3)` works). Only the third kind of source
breaks: a **channel-backed** live Supply, which carries a `supply_id` rather
than a `supplier_id` — `Supply.interval` without a `:scheduler`, `Proc::Async`
output, an async socket.

The cause is in `native_supply_dispatch.rs`'s `"head"` arm. It branches on
`attributes.get("supplier_id").is_some()`, so a `supply_id` source takes the
*materialized* branch: it reads `values` (empty — nothing has been emitted, the
Supply has not even been tapped yet), takes `count` of it, and hands the result
to `make_supply_from_values`, which builds a fresh Supply carrying neither
`supply_id` nor `supplier_id`. Tapping that empty Supply fires `done` at once.

## Why it is more than a one-line branch

Adding `supply_id` to the `has_supplier` test is not enough: `head_limit` is
consumed by `register_supplier_tap_with_head_limit`, which is a *supplier*
registry concept, while a channel-backed supply is drained by the thread
`run_supply_act_loop` spawns after `take_supply_channel`. That loop honours no
limit and fires no `done` — it just forwards until the channel closes. Making
`.head` work there means either teaching the act loop about a limit and a
completion signal, or giving channel-backed supplies a derived-supply mechanism
of the sort `lines` approximates with `parent_supply_id`.

Note the channel is single-consumer (`take_supply_channel` moves it), so
whatever shape is chosen has to decide what a second tap on the same source
means — today the second tap silently gets nothing.

The same gap presumably affects every other combinator that funnels through
`make_supply_from_values` with a channel-backed source; `.head` is just the one
with a repro.

## History

This was previously filed as "a tap callback's `@array.push` from a
timer/scheduler thread is lost", on the theory that the `@`-aggregate lane of
the cross-thread shared store dropped the pushes — the `:done` callback's write
to a *scalar* crossing correctly seemed to point that way. Measured on
2026-08-02: the callback never runs, so no push is ever attempted and the
shared store is not involved. It was also listed as the blocker behind five of
the six `roast/S17-supply` regressions in the `Test::Tap` retirement; those had
five unrelated causes and are fixed (`news/2026-08/live-supply-combinators.md`,
`news/2026-08/scheduler-driven-supply-interval.md`).
