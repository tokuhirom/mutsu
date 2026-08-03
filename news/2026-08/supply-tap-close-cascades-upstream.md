# Closing a tap now closes the whole supply chain

In raku, closing a tap closes the supply block that produced it, which closes the
`whenever` subscriptions inside it, which closes *their* sources — all the way
down to the original `Supplier` or listener. mutsu closed only the block the tap
was taken on:

```raku
my $src = Supplier.new;
my $upstream = supply {
    whenever $src.Supply -> $v { emit $v * 10 }
    CLOSE { say "UPSTREAM closed" }
};
my $mid = supply {
    whenever $upstream -> $v { emit $v + 1 }
    CLOSE { say "MID closed" }
};
my $tap = $mid.tap(-> $v { say "GOT $v" });
$src.emit(1);      # GOT 11
$tap.close;        # raku: UPSTREAM closed, MID closed
                   # mutsu: MID closed
$src.emit(2);      # raku: nothing
                   # mutsu: GOT 21   <- delivered to a CLOSED tap
```

So the upstream block kept running: its `CLOSE` phasers never fired, and values
still reached the closed tap callback.

## Fix

The on-demand `tap` path now records every subscription it creates upstream on
the Tap handle it returns (`upstream_taps`): a `[supplier_id, tap_id]` pair for
each `whenever` source and for the outer callback registered on the block's own
emitter, plus the nested Tap handle when the source is itself a chained
on-demand supply. `Tap.close` walks that list — `close_supplier_tap` for pairs,
recursion for nested handles — before firing its own `CLOSE` callbacks, so a
chain's `CLOSE` phasers run source-first exactly as raku's do. (The nested-handle
half closes a long-standing `TODO: thread the inner Tap into the outer Tap
handle` in `native_supply_mut_methods.rs`.)

Pin: `t/supply-tap-close-cascades-upstream.t` (5 tests, green under `raku` too).

## Effect on Cro

`Cro::Service.stop` is exactly this shape — `$!service-tap.close` on a pipeline
whose bottom is a TCP listener — so a "stopped" Cro server kept serving and a
second server on the same port never saw a request:

```raku
my $app1 = route { get -> { content 'text/plain', "APP-ONE"; } };
my $app2 = route { get -> { content 'text/plain', "APP-TWO"; } };
# ...start, request, stop the first; then start the second on the same port
# before: R1: APP-ONE / R2: APP-ONE
# now:    R1: APP-ONE / R2: APP-TWO
```

`t/http-middleware.rakutest`'s first subtest goes from 2/4 to 4/4 as a result.

**Known consequence:** the same file now *hangs* on its later subtests instead of
completing with wrong answers. That is a second, pre-existing bug this fix
exposes — re-`listen`ing on one port in a loop leaks listeners and eventually
fails to bind, because `'localhost'` resolves to both `127.0.0.1` and `[::1]` and
different rounds bind different ones. It reproduces identically on `main` with no
supplies involved at all, and is filed as
`todo/tickets/async-listener-not-freed-when-relistening-in-a-loop.md` with an
`ss`-level diagnosis. Until it is fixed, a stale listener answering for a stopped
server is traded for no listener answering — a deterministic failure instead of a
silently wrong one.
