# `Supply.lines.tap(...)` no longer drops everything on a channel-backed Supply

A direct `.tap()` on a `.lines`-derived Supply silently dropped every value
when the source Supply's values arrived through a **channel** — the shape
used by real TCP sockets (`IO::Socket::Async` connections and the listener's
accept stream), as opposed to the in-process supplier registry
(`Supplier.new.Supply`):

```raku
my $listener = IO::Socket::Async.listen('127.0.0.1', 0);
$listener.tap(-> $conn {
    $conn.Supply.lines.tap(-> $l { say "line: $l" });   # never fired
});
```

## Root cause

`native_supply_dispatch.rs`'s `"lines"` arm builds the derived Supply with a
**fresh** `supply_id` and carries the source's id forward only as an inert
`parent_supply_id` attribute. A real-TCP `.Supply` has no `supplier_id`; its
values are pushed down a channel registered under the *source's* `supply_id`.
The react/`whenever` drive loop already followed `parent_supply_id` back to
the source when resolving which channel to drain (this half of the mechanism
was already correct), but the **direct `.tap()` chokepoint**
(`native_supply_mut_methods.rs`, used by any code that calls `.tap()` outside
a `react`/`whenever` context) looked up the channel by the derived Supply's
own `supply_id` — which never has a channel registered under it — so the tap
silently found nothing to drain. Even a fixed lookup would still have been
wrong: the background reader (`run_supply_act_loop`, shared by every
channel-backed `.tap()`) forwarded each raw chunk straight to the callback
with no line-buffering at all, so a line split across two TCP `write`s would
have come out truncated.

## Fix

- `native_supply_mut_methods.rs` gained a small `resolve_tap_channel_supply_id`
  helper (mirroring the drive loop's existing `parent_supply_id`-then-`supply_id`
  resolution) and now uses it at both the live-channel tap-registration site
  and the Proc::Async collected-output replay site.
- `run_supply_act_loop` (`native_methods/encoding.rs`) gained `is_lines` /
  `line_chomp` parameters. When set, each received chunk is appended to a
  carry-over buffer and split into complete lines with
  `take_complete_lines_from_buffer` (the same splitter already used elsewhere)
  instead of being forwarded verbatim; a trailing partial line is flushed once
  more when the source signals `Done`/`Quit`. All three call sites of this
  loop were updated — the plain live-channel driver, the nested-whenever
  channel-source driver (reached when a `supply { whenever <channel> {…} }`
  body is tapped directly, outside `react`), and the `ThreadPoolScheduler`
  pump drain (which intentionally passes `is_lines: false` — its values are
  already the *post-split* result of an upstream shim).

## Verification

Confirmed with a real two-process TCP round trip (`t/supply-lines-channel-backed-tap.t`,
modeled on `t/io-socket-async-real-connect.t`): a listener taps
`$conn.Supply.lines` directly (no `react`/`whenever` involved), a separate
`Proc::Async` client writes `"hel"` then `"lo\nworld\n"` in two separate
writes to force a chunk boundary mid-line, and both `hello` and `world` arrive
as correctly reassembled lines. The full `t/*supply*.t`/`t/*socket*.t` suite
(91 files, 420 tests) was re-run with no regressions.

## Related findings (filed separately, not part of this fix)

Two unrelated bugs turned up while investigating this:

- `todo/tickets/whenever-target-var-binds-wrong-value-in-react.md`: `my $tap =
  whenever <live Supply> -> $x {...}` inside a `react` block binds `$tap` to
  the literal string `"whenever"` instead of the real `Tap`.
- An initial hypothesis that `whenever IO::Socket::Async.listen(...) -> $conn
  { ... }` registered directly inside a `react` body's own synchronous
  statements never actually accepts a connection turned out to be a
  measurement artifact of this investigation's own test harness (a background
  server process's short `Promise.in(N) { done }` deadline elapsing between
  separately-issued tool calls before the client connected) — not a real bug.
  `t/io-socket-async-listen.t` already exercises and passes this exact idiom.
