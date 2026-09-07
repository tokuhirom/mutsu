# `Channel.send` emits into the bridged Supply immediately, where rakudo pumps asynchronously

`Channel.Supply` in mutsu is a *bridge*: `Channel.send`
(`dispatch_channel_method`, `src/runtime/methods_promise.rs:359-379`) calls
`supplier_emit` on every supplier id the channel is bridged to, synchronously,
before it even puts the value on the channel. So in mutsu a `send` is an emit
into the tap stream.

Rakudo's `Channel.Supply` is pumped: the value sits in the channel until the
scheduler moves it, so nothing has been emitted to a tap at `send` time.

Two measurable consequences, both against raku v2026.07 and mutsu at the
`Tap.close` ordering fix (`news/2026-09/whenever-tap-close-is-ordered-against-the-emit.md`):

```raku
# 1. mutsu delivers where rakudo does not
my $c = Channel.new;
my @got;
react {
    my $t = do whenever $c.Supply -> $x { @got.push($x) };
    $c.send(1); $c.send(2);
    $t.close;
    whenever Promise.in(0.3) { done }
}
say @got.raku;    # raku: []      mutsu: [1, 2]
```

Rakudo drops them because the pump never ran before the close; mutsu had already
counted them as emitted. The same shape with the close moved into a later pump
round (`whenever Promise.in(0.1) { $t.close }`) agrees in both — `[1, 2]` — so
the *close ordering* rule is right in both implementations; only the moment a
channel value counts as emitted differs.

```raku
# 2. mutsu delivers nothing where rakudo delivers everything
my $c = Channel.new;
$c.send(1); $c.send(2);
my @got;
react {
    whenever $c.Supply -> $x { @got.push($x) };
    whenever Promise.in(0.3) { done }
}
say @got.raku;    # raku: [1, 2]  mutsu: []
```

Values sent *before* anything bridged the channel to a Supply are lost entirely:
`ch.supplier_ids()` is empty at `send` time, so no `supplier_emit` happens, and
the later `$c.Supply` tap has no backlog to replay. Rakudo's pump reads them off
the channel when the tap arrives. This one is a plain data-loss bug and is the
more important half.

Both fall out of the same design choice. The fix is to stop bridging at `send`
and instead have the `whenever`/`tap` side drain the channel — mutsu already has
the machinery (`ReactSubscription::channel`, polled by the drive loop, is used
for a bare `whenever $channel`), so the work is to route `Channel.Supply`
through it rather than through an eager supplier bridge, and to keep the
already-sent backlog visible to the first tap.

Sized as a ticket rather than a deep item because the shape of the answer is
clear; it is nonetheless not a one-liner, since `ch.supplier_ids()` has other
consumers (`Channel.close` fans a `supplier_done` out the same way) and
`Supply.Channel` (the reverse direction, `state_supplier.rs:62`) must keep
working.
