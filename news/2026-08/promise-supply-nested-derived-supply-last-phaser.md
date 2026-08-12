# `Promise(supply { whenever <derived-supply> { ...; LAST {...} } })` no longer loses values

`Promise(supply { ... })` coercion had its own hand-rolled subscription
classifier (`supply_promise_on_demand`, `src/runtime/supply_promise.rs`)
separate from the shared react/supply drive mechanism. It recognized a
`Promise` source, a `Supply` with an already-registered channel, and a live
`Supplier`-backed `Supply` — but not a *derived* supply (a `whenever`'s
source that is itself another `supply { ... }` block). That case fell through
to a synchronous static replay (`replay_static_whenever_promise`), which
intentionally drops a still-live nested subscription rather than replaying
it — so the whole `whenever` body, including its `LAST` phaser, silently
never ran. A `Promise(supply { my $joined = 0; whenever $inner -> $x {
$joined += $x; LAST emit $joined } })` resolved with the LAST phaser's
untouched initial value (or, without `LAST`, with no value at all) instead of
the accumulated result.

Fixed by routing a derived-supply source through the same
`register_nested_on_demand_source` mechanism the react loop and `.tap()`
already use to wire a nested `supply { }` stage as a live streaming pipeline
stage, then handing the streaming registrations this classifier
(`self.supply_stream_consumers`) collects on the calling thread over to the
cloned interpreter that actually drives the background poll — without that
handoff, the live-forwarding wiring is registered on the wrong `Interpreter`
instance and never observed.

Two further gaps in the shared mechanism itself, previously unreachable via
`react`/`.tap()`, were also fixed while pinning this:

- A nested stage's own live subscription completing normally never resolved
  that stage's `on_demand_done` promise (only a QUIT/die path was wired to
  it), so a `LAST` phaser waiting on that promise's resolution never fired.
- A source-less "shadow" subscription carrying only `LAST` callbacks (no
  `quit_callbacks`) was marked `done` on the very first drive-loop poll,
  before its `on_demand_done` promise could possibly have resolved —
  orphaning its `LAST` phaser permanently.
- A purely-synchronous nested source (a `supply { emit ...; emit ...; }`
  with no `whenever` of its own — no live subscription at all) never
  resolved its own `on_demand_done` promise either, since nothing observed
  its (already-complete) synchronous run as a completion signal.

Pin: `t/promise-supply-nested-derived-supply-last.t` (three cases: LAST +
accumulator, plain emit with no LAST, LAST-only body), all verified against
`raku`.

This was the root cause of two `Cro::HTTP::ResponseParser`-adjacent roast
failures found in the 2026-08-12 Cro session, since
`Cro::MessageWithBody.body-blob` uses exactly this pattern to accumulate a
response body. Re-measuring the vendored Cro suite after the fix also
unlocked three previously broken/hanging files that turned out to share the
same root cause: `http-router.rakutest` (was hanging, now 439/439),
`http-router-plugin.rakutest` (was 5/7, now 7/7), and `http-middleware.rakutest`
(was hanging, now 24/24). `http-response-parser.rakutest`'s own two failures
turned out to be a *different*, deeper bug in a longer `.tap()`/
`Supplier::Preserving` chain — filed separately as
`todo/deep/preserved-tap-chain-loses-body-when-terminated-by-connection-close.md`.
