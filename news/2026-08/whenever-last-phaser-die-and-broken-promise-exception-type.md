# `whenever` LAST-phaser dies now convert to quit, and broken-promise exceptions keep their real type

`Cro::HTTP::ResponseParser`'s `http-response-parser.rakutest` had one
remaining failure after ADR-0028 Slice 1/2 fixed `Supply.schedule-on()`'s
tap-delivery deadlock and its bypass paths: "Connection close with
incomplete body throws" (check 4), which expects `.body-text` to throw
`X::Cro::HTTP::RawBodyParser::ContentLength::TooShort` when a response
declares a `Content-length` larger than the bytes actually received before
the connection closes.

Root-caused to three separate, compounding general bugs, each confirmed
against real `raku` with a Cro-free minimization before fixing:

1. **A `die` escaping a `whenever`'s `LAST` phaser did not convert to a
   quit.** `Cro::HTTP::RawBodyParser::ContentLength`'s parser is exactly the
   shape `whenever $raw-blobs -> $blob { emit $blob; $expected -= ...; LAST
   { die ... if $expected != 0 } }`. mutsu's LAST-phaser callbacks are
   registered as ordinary "done callbacks" on the supplier
   (`native_supply_mut_methods.rs`), and the generic invocation path
   (`invoke_done_callback` in `native_supply_methods.rs`) silently discarded
   any error the callback raised — so the die vanished and the supply
   completed via its normal `done` path instead of quitting. Fixed with a
   new `invoke_done_callback_or_quit` wrapper (mirroring the established
   whenever-body-emit die-to-quit conversion already used for a `whenever`
   body's own `SupplierEmitAction::Call` errors) at the three call sites
   that previously propagated the (never-actually-raised) error with `?`;
   it now routes a real die to the supplier's registered quit callbacks and
   stops delivering the rest of that done-callback batch, so a supply
   terminates via either `done` or `quit`, never both. Pinned in
   `t/whenever-last-phaser-die-converts-to-quit.t`.

2. **A nested-derived-source quit left `Promise(supply {...})` `Planned`
   forever.** `Cro::MessageWithBody.body-blob` is `Promise(supply { whenever
   self.body-byte-stream -> $blob { ...; LAST emit $joined } })`, where
   `body-byte-stream` is itself a `preserve()`d (Supplier::Preserving-backed)
   derived supply — a "live Supplier-backed" `whenever` inside the Promise
   coercion's drive loop. When that upstream source quit with no explicit
   `quit =>` handler of its own, two independent spots in
   `vm/vm_react_subscriptions.rs` (the `on_demand_done` Broken check, and
   `dispatch_waker_events`'s `SinkEvent::Quit` arm) returned an `Err` meant
   for a real `react {}` block's die — but under `SupplyDrivePolicy::Promise`
   this drive loop runs detached on its own thread whose `Result` the caller
   already discards (`supply_promise_on_demand`'s `spawn_user_thread`), so
   the error had nowhere to go and the promise simply never resolved. Both
   sites now check the policy and call `promise.break_with(...)` directly
   when it's the `Promise` variant. Pinned in
   `t/promise-supply-nested-quit-breaks.t`.

3. **Broken-promise exceptions with a user class name were re-wrapped in
   `X::AdHoc`, losing their type.** Both `.cause` and `.result` (on a Broken
   promise) checked `class_name.resolve().contains("Exception") ||
   starts_with("X::")` before deciding whether to pass an exception instance
   through unchanged — a name-based heuristic that cannot see a user class's
   `is Exception` ancestry when its name follows neither convention (e.g. a
   bare `class TooShort is Exception {...}`; Cro's own
   `X::Cro::HTTP::RawBodyParser::ContentLength::TooShort` happens to satisfy
   it, which is why this compounded rather than being the sole cause).
   `.result` did not even have the check — it unconditionally wrapped every
   broken reason. Both now use the same shape-based check
   `Supplier.quit()`'s reason handling already established elsewhere in the
   codebase: any object `Instance` passes through untouched; only a plain
   `Str`/other non-instance reason gets wrapped in `X::AdHoc`. This also
   fixes `Cro::MessageWithBody.body-text`, whose `self.body-blob.then: ->
   $p { $p.result; ... }` shape depends on `.result` preserving the type.
   Pinned in `t/promise-result-broken-exception-type.t` and covered by
   `t/promise-supply-nested-quit-breaks.t`.

**Result:** `http-response-parser.rakutest` is now 156/156 fully green
(previously 155/156). Cro::HTTP suite: 34/35 fully-green files (the sole
remaining gap is the unrelated, already-tracked
`todo/deep/http2-concurrent-streams-first-body-blob-loses-value.md`).
Cro::Core stays 9/9. `make test` (3103 files / 28830 tests) and the
`t/supply*.t`/`t/whenever*.t`/`t/react*.t`/`t/promise*.t` suites (109 files /
508 tests) are green.

A minor debugging footnote from this session: a test-description string
containing a literal `{...}` inside a double-quoted Raku string (e.g.
`"...Promise(supply{...})..."`) is not a typo to fix later — Raku's string
interpolation parses `{...}` as an embedded code block whose body is the
three-dot stub statement, so *running* the test throws "Stub code executed"
in both `raku` and mutsu identically. Not a mutsu bug; just don't write
literal `{...}` inside interpolated test descriptions.
