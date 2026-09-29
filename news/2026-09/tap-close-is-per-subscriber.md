# `Tap.close` closes only its own tap

Closing one tap of a channel-backed Supply — `Supply.interval`, a
`Proc::Async` output Supply, the other Rust-fed sources — used to stop the
source for every tap, because all taps shared one close flag on the
broadcast point (ADR-0074 left per-subscriber close out of scope).

The close flag now lives on each tap's own queue. A closed tap receives
nothing more and no longer counts as a listener, so the producer keeps
running for the remaining taps and retires only when the last tap is closed
or dropped — exactly when rakudo stops a live Supply's producer.

```raku
my $s = Supply.interval(0.05);
my (@a, @b);
my $t1 = $s.tap({ @a.push($_) });
my $t2 = $s.tap({ @b.push($_) });
sleep 0.3; $t1.close; sleep 0.5;
say @b.elems > @a.elems;   # True (was False)
```

Closes #9899.
