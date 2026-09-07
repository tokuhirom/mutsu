# `Channel.Supply` is pumped, not bridged at send time

`Channel.Supply` was a *bridge*: `Channel.send` called `supplier_emit` on every
supplier id the channel had been bridged to, synchronously, before it even put
the value on the channel. A `send` was therefore an emit into the tap stream.
Rakudo's `Channel.Supply` is pumped — the value sits on the channel until the
scheduler moves it — and the difference was measurable in both directions.

## The data-loss half

```raku
my $c = Channel.new;
$c.send(1); $c.send(2);
react {
    whenever $c.Supply -> $x { @got.push($x) }
    whenever Promise.in(0.3) { done }
}
# raku: [1, 2]     mutsu was: []
```

Values sent *before* anything bridged the channel to a Supply were lost
entirely: `ch.supplier_ids()` was empty at `send` time, so no emit happened,
and the later `$c.Supply` tap had no backlog to replay.

## The too-eager half

```raku
react {
    my $t = do whenever $c.Supply -> $x { @got.push($x) };
    $c.send(1); $c.send(2);
    $t.close;
    whenever Promise.in(0.3) { done }
}
# raku: []         mutsu was: [1, 2]
```

Rakudo drops them because the pump never ran before the close; mutsu had
already counted them as emitted at `send`.

## The fix

The machinery was already there — `ReactSubscription::channel`, polled by the
react drive loop, is what a bare `whenever $channel` uses. `Channel.Supply` now
records the channel on the Supply instance it returns (a `channel` attribute
alongside `supplier_id`), and the `whenever` subscription builder
(`src/vm/vm_react_loop.rs`) recognises it and builds a channel-drain
subscription instead of a supplier-sink one.

That single routing change answers both halves, because the drive loop reads
the queue rather than a stream of past emissions: a backlog is simply still
there, and a value sent during the react body is not delivered until the loop
runs (so a tap closed before that first round retires with nothing owed, via
the existing `whenever_closed_seq` retirement). The channel's own
`is_drained_closed()` already completes the subscription and fires its `LAST`
phasers, so `Channel.close` needs nothing new.

The send-time bridge itself is deliberately left in place for the other
consumers of `ch.supplier_ids()` — a plain `$c.Supply.tap(...)` outside a react,
and a combinator chain such as `$c.Supply.grep(...)` whose derived Supply no
longer carries the channel. Those keep working exactly as before, and no
consumer sees a value twice: a `whenever` on the channel-backed Supply
registers no supplier sink at all now.

Pinned by `t/channel-supply-is-pumped.t`, whose 4 assertions pass unchanged
under rakudo. `roast/S17-supply/{Channel,syntax,head,start,supplier-preserving}.t`
and `roast/S17-channel/basic.t` are unmoved, as is the whole `t/supply-*`,
`t/react-*`, `t/whenever-*` family.
