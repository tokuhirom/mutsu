# `Channel.close` completes the taps on its `Supply`

Tapping the `Supply` view of a `Channel` and then closing the channel delivered
every value but never ran the tap's `done` callback:

```raku
my $c = Channel.new;
my $p = Promise.new;
$c.Supply.tap(-> $v { }, done => { $p.keep });
$c.send(1);
$c.close;
sleep 1;
say "kept=", $p.status;   # mutsu: Planned    raku: Kept
```

`Channel.close` did mark every supplier the channel had been bridged to as
done, and that was easy to mistake for the whole job — `supplier_done` raises
the terminal flag, stamps a terminal sequence number and wakes the registered
sinks. What it does *not* do is run anything: a `.tap(&emit, done => { ... })`
parks its completion callback in the supplier's pending-callback list, and only
an explicit `take_supplier_done_callbacks` drain invokes it. `Supplier.done`
performs that drain; the channel path never did, so for a channel-backed Supply
the callbacks sat in the list until the process exited.

`Channel.fail` was a wider miss on the same completion edge. It never touched
the suppliers at all, so a tap's `quit => { ... }` handler — the only thing that
observes a failing channel — could not fire either.

Both are now signalled from `dispatch_channel_method`: `close` drains and
invokes the done callbacks of every bridged supplier after closing the channel,
and `fail` records the quit reason on each supplier and runs its quit handlers.
Marking still happens before `ch.close()` exactly as before, so the existing
`whenever`/react channel-drain path (which reads the queue and never consults
the supplier ids) is untouched.

Completion is per tap and exactly once, whether several taps share one
`Channel.Supply` value or each comes from its own `.Supply` call, and it lands
after the last value rather than instead of it. `t/channel-supply-tap-completion.t`
pins all five properties through `Promise` status rather than printed output, so
it does not depend on how writes from different threads interleave; the file
passes unchanged on rakudo.

This unblocks what `Channel.Supply`'s delivery fix (`187fc2eff`) left standing:
route 5 of the `gc_contents_mut` cross-thread aliased-write investigation needs a
tap that can be measured to a deterministic end, which a tap that never signals
completion could not provide.

Note that the value-distribution difference visible with two taps on one
`Channel.Supply` — mutsu broadcasts each value to every tap where rakudo hands
it to exactly one — is a separate bug tracked in its own issue, and is not
touched here.

Closes #7584.
