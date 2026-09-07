# `Channel.Supply.tap`'s `done` callback never fires on `.close`

Tapping the `Supply` view of a `Channel` and closing the channel does not run
the tap's `done` callback. Values are delivered correctly; only the completion
signal is missing.

## Repro

```
my $c = Channel.new;
$c.Supply.tap({ say "v$_" }, done => { say "DONE" });
$c.send(1);
$c.close;
sleep 1;
say "end";
```

- mutsu: `v1` / `end`
- raku:  `v1` / `DONE` / `end`

The emit form does not matter — a pointy block (`-> $v { say "v$v" }`) gives
the same split. The cleanest observable, because it does not depend on output
ordering across threads, is the status of a `Promise` the handler keeps:

```
my $c = Channel.new;
my $p = Promise.new;
$c.Supply.tap(-> $v { }, done => { $p.keep });
$c.send(1);
$c.close;
sleep 1;
say "kept=", $p.status;
```

- mutsu: `kept=Planned`
- raku:  `kept=Kept`

## What is NOT wrong

Two neighbouring behaviours were measured and are correct, so the bug is
specifically the completion signal and not the tap or the channel:

- **Value delivery is correct and ordered.** `$c.Supply.tap({ @got.push($_) })`
  with `$c.send($_) for ^20` collects `0 … 19` in order, 3/3 runs. (This was
  itself a real bug once and was fixed by `187fc2eff`, "Channel.Supply is
  pumped, not bridged at send time".)
- **`await` on a never-kept `Promise` blocks correctly** in both
  implementations (`timeout 10` → exit 124 for mutsu and raku alike). An
  earlier reading that `await` returned immediately here was a measurement
  artifact of a shell pipeline masking the exit code — it does not.

Note that `tap({}, done => { … })` is **not** a valid probe: raku rejects the
empty hash as the `&emit` positional (`expected Callable but got Hash`). Use a
block or a pointy block.

## Why it matters beyond the obvious

`todo/deep/gc-contents-mut-cross-thread-aliased-writes.md` lists route 5
(`Channel.Supply` tap captures) as blocked behind "a deterministic
Channel-supply delivery bug". That delivery bug is fixed; **this** is what now
stands between that file and a route-5 probe, because a tap that never signals
completion cannot be measured to a deterministic end.

## Where to look

The `done`/completion path of the `Channel` → `Supply` pump introduced by
`187fc2eff`. Delivery was moved to a pump; the channel's closed state
apparently does not propagate to the tap's `done` handler at the end of the
pumped stream. Check that the pump distinguishes "no value available right
now" from "the channel is closed", and that the latter runs the tap's `done`
callback exactly once per tap.

## Acceptance

- Both repros above match raku.
- `done` runs exactly once per tap, after the last value, including with
  several taps on the same `Channel.Supply`.
- A `quit` path equivalent (`$c.fail(...)` reaching the tap's `quit`) is
  checked in the same pass, since it is the same completion edge.
- A `t/` pin covering the `Promise.status` form, which is order-independent.
