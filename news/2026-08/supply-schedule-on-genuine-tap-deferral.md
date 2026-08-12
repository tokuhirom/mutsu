# `Supply.schedule-on()` now genuinely defers tap/done/quit delivery

`Supply.schedule-on($scheduler)` used to be a near no-op: it stashed the
scheduler into an attribute that was consulted in exactly one place
(`Supply.interval`'s own timer wiring), so a value emitted upstream still
reached `.tap()`'s callback synchronously, on the emitting call stack, for
every other Supply shape. Real Raku's `.schedule-on()` genuinely reschedules
delivery, decoupling the emitting thread from the tap callback — the whole
documented point of the method. The gap collapsed two independent-in-Raku
executions back onto one call stack, which changed program behavior whenever
the tap callback did something blocking.

This produced a real deadlock, confirmed against
`Cro::HTTP::ResponseParser`'s test helper: a *blocking* call inside a
`.schedule-on($*SCHEDULER)`-wrapped tap callback ran on the same thread that
still needed to run its own next statement (`$fake-in.done()`) — whose
effect the blocking call was waiting on. Real Raku deadlocks identically
*without* `.schedule-on()` and passes *with* it, proving the deferral was
exactly the missing semantics.

Design: [ADR-0028](../../docs/adr/0028-supply-schedule-on-deferred-tap-delivery.md).
Implementation wraps `tap_cb`/`done_cb`/`quit_cb` at the single `"tap" |
"act"` registration chokepoint (`native_supply_mut_methods.rs`) rather than
any of the ~33 emit call sites, with a scheduler-kind fork:

- **`CurrentThreadScheduler`** stays fully synchronous (Rakudo's own `.cue`
  runs inline, so this is the correct semantics, not a shortcut).
- **`ThreadPoolScheduler`** (including the default `$*SCHEDULER`) gets a
  serialized per-tap drain: a fresh `supply_event_channel()` pair is created
  at tap time, the emit/done/quit callbacks are substituted for shims that
  forward into the channel and return immediately, and a pooled worker
  (`run_supply_act_loop`, reusing the existing channel-backed live-supply
  drain and ADR-0020's worker pool) invokes the real callbacks in order. A
  naive per-emit pool submit would let two deliveries race across workers
  and reorder them — the single channel + single drain worker is the sound,
  cannot-go-flaky form of the same guarantee.
- **Any other Scheduler** (a user-written one, roast's `FakeScheduler`)
  routes through its own `.cue`, exactly like `Supply.interval`'s existing
  scheduler wiring — never bypassed with a hardcoded pool submit.

Two adjustments surfaced only once the mechanism was actually exercised: the
generic method-call resolver gates native dispatch on a per-class
`ClassDef.native_methods` allowlist in `runtime_init.rs`, a layer separate
from (and in addition to) the `native_methods/mod.rs` dispatch table, so the
new internal `__ScheduledTapPump` class needed its own entry; and a bare,
undefined scheduler type object used directly (`CurrentThreadScheduler`
without `.new`) is a `ValueView::Package`, not a `ValueView::Instance` — the
scheduler-kind classification had to read `class_name` off both.

Pinned in `t/supply-schedule-on-defer.t` (5 cases: the deadlock repro,
emission-order preservation through a `ThreadPoolScheduler` pump, quit
routing, `Tap.close` reclaiming the drain, and a user Scheduler's `.cue`
being honored rather than bypassed) — every case was verified against real
`raku` first, since several plausible-looking simplifications of the
original repro did not actually reproduce the deadlock. Existing coverage
(`t/supply-schedule-on.t`, `t/schedule-on-whenever-env.t`,
`t/supply-interval-scheduler.t`, `roast/S17-supply/schedule-on.t`) stays
green unchanged.

Re-running `t/http-response-parser.rakutest` from the vendored Cro checkout
confirms the fix against the original motivating case: both previously
timing-out subtests ("Response with body terminated by close of connection",
"Connection close with incomplete body throws") now pass, taking the file
from a mid-run timeout to 155/156. The Cro::HTTP suite moves to 34/35
fully-green files (from 33/35); Cro::Core stays 9/9. The one remaining
`http-response-parser.rakutest` failure is a different, narrower bug (a
`Content-length`-driven body parser not throwing
`X::Cro::HTTP::RawBodyParser::ContentLength::TooShort` on early connection
close), filed separately:
`todo/tickets/http-response-parser-content-length-too-short-not-thrown.md`.

ADR-0028's Slice 2 (an audit of the `whenever`/react-loop/`Promise` code
paths that bypass the `"tap"|"act"` chokepoint) remains open, tracked by the
ADR itself.
