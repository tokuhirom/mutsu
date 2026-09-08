# `Channel.Supply` taps compete for values instead of each getting a copy

Two taps on a `Channel`'s `Supply` are **competing consumers** in rakudo — each
sent value is delivered to exactly one of them — but mutsu **broadcast** the
value to all of them, so every tap saw the whole stream:

```raku
my $c = Channel.new;
my (@a, @b);
my $s = $c.Supply;
$s.tap: { @a.push($_) }
$s.tap: { @b.push($_) }
$c.send($_) for 1..6;
$c.close;
sleep 0.5;
say "a={@a.join(',')} b={@b.join(',')}";
# raku:            a=1,3,5 b=2,4,6
# mutsu (before):  a=1,2,3,4,5,6 b=1,2,3,4,5,6
```

A single tap — by far the common case — was unaffected, which is why it went
unnoticed. But a program fanning work out to several workers over one channel
did every unit of work N times instead of once: silent duplicated side effects,
not a wrong scalar.

## Why the two shapes differ

A `Supplier` is a genuine broadcast point, and mutsu already agreed with rakudo
there. A `Channel` is a queue, so `Channel.Supply` is a view onto a `.receive`
loop — a value one consumer takes is gone for the others.

The `whenever` route already got this right: a react drive loop drains the
channel queue itself, so two `whenever`s on `$c.Supply` partitioned the stream
exactly as rakudo does. The divergence was confined to the *eager send-time* tap
dispatch in `Channel.send`, which looped over every one of the channel's
supplier ids and ran every tap registered on each.

## The fix

`Channel.send` now enumerates the channel's live taps across all of its
`Supply` objects (`supplier_live_tap_indices`, which applies the same
closed / `head_limit` skips the emit loop does) and hands the value to exactly
one of them, chosen by a round-robin cursor kept on the channel
(`ChannelState::supply_turn`). `supplier_emit_callbacks_for_tap` is the
single-tap form of the existing `supplier_emit_callbacks`, so competing delivery
goes through the same per-tap machinery — `head`, `unique`, `batch`, `lines`,
`produce` and the rest all behave as before, they are just reached by one tap
per value.

The per-supplier `supplier_emit` bookkeeping (the sinks a react loop polls, and
the recorded emission list) is untouched, so nothing that reads a supplier's
state changes behaviour.

## Pins

`t/channel-supply-competing-consumers.t` covers the shared-`$s` spelling, two
separate `$c.Supply` calls, the `Supplier` broadcast control, and a single tap
seeing the whole stream in order. Because which tap wins a given value is not
specified, it asserts the *partition* — every value delivered exactly once, and
the union being the whole stream — rather than a particular split, so it passes
under real rakudo too.

This also removes one source of noise from the ADR-0068 §3 route-5 concurrency
probe: the three-tap-writer FizzBuzz idiom recorded there cannot fill its array
under rakudo's competing-consumer semantics, so part of that "values
dropped/misordered" reading was this divergence seen from the other side.

Closes [#7604](https://github.com/tokuhirom/mutsu/issues/7604).
