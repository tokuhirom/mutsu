# `Supply.interval(:$scheduler)` cues its ticks on the scheduler

`Supply.interval(period, delay, :$scheduler)` hands its clock to the given
`Scheduler` rather than starting a timer, which is how a test can drive a stream
deterministically. mutsu recorded the scheduler on the Supply and then never
called it: the emissions existed only inside mutsu's *native* `tap-ok`, which
recognized the `scheduler` attribute and asked a *native* `FakeScheduler` for a
counter. Neither half exists once real code is involved, so a user-written
scheduler — including roast's `Test::Tap::FakeScheduler` — received nothing and
the tapped Supply produced an empty stream.

Tapping such a Supply now cues a tick block on the scheduler through the
ordinary interface, `$scheduler.cue(&code, :every($period), :in($delay))`, so
any Scheduler drives it:

```raku
my $scheduler = TestScheduler.new;     # any class with .cue / its own clock
my @res;
Supply.interval(1, :$scheduler).tap({ @res.push($_) });
$scheduler.progress-by(4.5);
say @res;                              # [0 1 2 3 4]
```

The block handed to `.cue` is synthesized rather than parsed — its body is a
call to an internal `__mutsu_interval_tick` method on a `Supplier` literal — so
the scheduler holds an ordinary first-class `Callable` it can store and invoke
whenever its clock says to. That is the same idiom `promise_keeper_block` uses
for `Promise.in` on a user scheduler. The value emitted is the number of ticks
so far, so the stream is `0, 1, 2, …` exactly as a timer-driven interval
produces, and an interval that nobody taps cues nothing.

The counter-mode `FakeScheduler` cue that existed only to serve the native
`tap-ok` is gone with it; the fake scheduler now stores and runs plain
callbacks like any other.

Pinned by `t/supply-interval-scheduler.t`, which drives the whole path through
a scheduler class written in Raku.
