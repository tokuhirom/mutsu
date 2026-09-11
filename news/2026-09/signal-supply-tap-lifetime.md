# A `signal()` Supply now arms on tap and disarms on the last untap

`signal(SIGTERM)` registered an OS watcher the moment it was *called* and never
unregistered it. Two things followed from that, and both were wrong.

The first was an unbounded leak ([#7917](https://github.com/tokuhirom/mutsu/issues/7917),
split out of the [#7609](https://github.com/tokuhirom/mutsu/issues/7609) SIGSEGV
investigation). Every call pushed a `SignalRegistration` — a supply sender clone
and the `Signal` value to emit — into a process-global registry that nothing
ever removed, and parked the supply's event channel (an mpsc receiver, a
subscriber list, a waker set, a condvar) in the supply-channel map for the life
of the process. `roast/S17-procasync/stress.t`'s rakudo#3299 block creates 1200
of them, so 1200 dead registrations were walked on every delivered signal and
1200 dead broadcast points were held until exit. The `SignalRegistration`
struct's `supply_id` field was marked `#[allow(dead_code)]` — direct evidence
that teardown had never been wired up.

The second was a set of behaviour divergences that the leak had been hiding.
rakudo's signal Supply is a live supply: the handler is armed while a tap is in
force and the previous disposition comes back when the last tap goes. Measured
against `raku`, mutsu differed in both directions:

| | `raku` | mutsu before | mutsu now |
| --- | --- | --- | --- |
| SIGUSR1 while tapped | caught | caught | caught |
| SIGUSR1 after `$tap.close` | kills the process | swallowed | kills the process |
| SIGTERM after the `react` that tapped it ended | kills the process | swallowed | kills the process |
| re-tapping the same Supply after a close | delivers to the new tap | delivers nothing | delivers to the new tap |

So a program that stopped listening was still quietly eating its signals, and
one that started listening again was not being given them.

## What changed

`SupplySender::is_retired()` answers, without an event in hand, the same
question `send` already answers with one: can this sender still reach anybody?
It is retired once `Tap.close` has set the close flag, the broadcast point is
gone, or every tap it ever had has been dropped (ADR-0074's "producer retires"
condition). A supply that has never been tapped is deliberately *not* retired —
its first tap may still be coming.

The signal watcher uses that to sweep: a registration whose supply is retired is
dropped, its channel entry is released, and a signal number that has lost its
last registration is handed back to the disposition it displaced (captured from
`sigaction`'s `old` when the first registration installed our handler, so a
crash-reporter or user handler is restored rather than blanket `SIG_DFL`). The
sweep runs where taps actually go away — `Tap.close`, the end of a `react` drive
loop — and again on the next `signal()` call and on each delivered signal, so
nothing depends on a single teardown path being taken. Programs that never call
`signal()` skip it on an atomic load.

Re-arming is the other half. A signal Supply now carries its own spec (the
`Signal` values it was created with) in its attributes, so a tap arriving after
the watcher retired it re-registers instead of listening to a channel nothing
feeds — which is how the re-tap row above turned green.

Measured over 2900 `signal()`/tap/close cycles on a debug build, peak `VmRSS`
growth went from 25.5 MB to 3.4 MB; the registry itself now holds one entry per
live tap instead of one per call, which a unit test over the process-global
registry pins.

The same investigation found `crash_report::install_alt_stack` rebuilding its
`Vec` in `Drop` with `ALT_STACK_SIZE` rather than the capacity `with_capacity`
actually handed out — exact today, a deallocation-layout mismatch the moment
`RawVec` rounds a request up. It now carries the real capacity through the
guard.

## What this is not

It is not the #7609 SIGSEGV. That crash remains a single unreproduced CI event;
the leak could only ever keep nodes *alive*, never free one early. #7609 stays
open.

One approximation remains: a signal delivered when the last tap has just gone,
before any sweep point has been reached, is still consumed by our handler rather
than taking the default action — the sweep at delivery time retires the
registration, so the *next* one behaves. Closing a tap, ending a react, and
calling `signal()` again all sweep, so the window is the span between those.
