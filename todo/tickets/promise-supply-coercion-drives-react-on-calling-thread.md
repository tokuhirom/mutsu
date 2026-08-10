# `Promise(supply { ... })` blocks the calling thread — deadlocks Cro body promises terminated by connection close

## Affected tests
- `t/http-response-parser.rakutest` subtest 111 ("Response with body terminated by close of connection") — flunked via the harness's 10s "Response parser failed to emit a HTTP response" path
- `t/http-response-parser.rakutest` subtest 120 ("Connection close with incomplete body throws") — same flunk (its `pass` and checks 1-3 appear as subtests 116-119; check 4 hangs)

These are the only two cases in the file whose BODY completion depends on the source supply's `done` (UntilClosed body / ContentLength-too-short + close). All content-length/chunked cases complete from data alone and pass.

**Likely also the cause of `t/http2-request-parser.rakutest` subtest 44 ("Header1 + Header2 + Data1 + Data2", check 4)**, found while verifying the fix for `supplier-preserving-backlog-destroyed-by-done-immutable-lane` (PR #6166, 2026-08-10). After that fix, this file's other backlog-loss flakes disappeared and only this one subtest remains, now deterministic every run. Debug instrumentation showed `.body-blob.result` for the second (later-registered) concurrent stream resolves to an **empty** buffer even though the producer did emit+done the expected bytes — i.e. the `Promise(supply { whenever self.body-byte-stream {...} })` coercion is returning before observing any chunks, consistent with this ticket's synchronous-drive mechanism racing against the still-emitting producer thread. Not yet reduced to a minimal non-Cro repro (a hand-built two-`Supplier::Preserving` analogue in `tmp/repro-two-stream-body-blob.raku` did NOT reproduce it, so the trigger needs the real `Cro::HTTP2::GeneralParser`/HPACK-decoder shape, not just two independent Preserving suppliers).

## Repro (verified)
Minimal, no Cro (`tmp/repro-promise-supply-coerce.raku`):

```raku
my $s = Supplier.new;
note "before coercion";
my $p = Promise(supply {
    my $acc = '';
    whenever $s.Supply -> $v { $acc ~= $v; LAST emit $acc; }
});
note "after coercion: {$p.^name} status={$p.status}";
start { $s.emit('a'); $s.emit('b'); $s.done; }
say "result: ", await $p;
```

- raku: prints `after coercion: Promise status=Planned`, then `result: ab`.
- mutsu (release): prints `before coercion` and hangs forever — the coercion itself blocks, the `start` never runs, deadlock.

Cro-shape repro with the deadlock as seen in the test (`tmp/repro-bodytext-blocks.raku`, uses vendored Cro::HTTP::ResponseParser):
- raku: `S: emitting` → emit returns → done sent → body resolves.
- mutsu: `S: emitting` → `T: tap got response` (tap callback runs synchronously INSIDE `$in.emit` on the emitting thread — `.schedule-on($*SCHEDULER)` does not decouple) → `$r.body-text` never returns → `$in.done()` never executes → 10s timeout. Circular wait: the body promise can only resolve on `done`, and `done` can only be sent by the thread blocked waiting for the body promise.

Chain in Cro: `body-text` → `body-blob` = `Promise(supply { whenever self.body-byte-stream {...} })` (`tmp/cro-work/C_RO_CRO_CORE_*/lib/Cro/MessageWithBody.rakumod:36-44`).

## Root cause
`Supply.Promise` for an on-demand supply (`supply { ... }` block) is implemented synchronously: dispatch at `src/runtime/native_supply_dispatch.rs:431-456` calls `supply_promise_on_demand` (`src/runtime/supply_promise.rs:330`), which runs the supply body inline and then **drives the react subscriptions on the calling thread** via `drive_react_subscriptions` with `SupplyDrivePolicy::Promise { deadline: now + 30s, .. }` (`src/runtime/supply_promise.rs:527-538`, loop in `src/vm/vm_react_loop.rs`). The coercion therefore does not return a Planned promise; it blocks until the supply is done or 30s elapse.

Aggravating factor (secondary): mutsu tap callbacks run synchronously on the emitting thread; `.schedule-on` merely stores a `scheduler` attribute without changing delivery (`src/runtime/native_supply_dispatch.rs:457-468`). That is what puts the blocking coercion ON the producer thread and closes the deadlock cycle. (Repro `tmp/repro-schedule-on.raku`; note raku may also reuse the same pool thread, but its `emit` returns before the tap body runs — mutsu's does not.)

Related minor anomaly noticed while bisecting (worth a one-line check in the same campaign): a tap's `done` callback can fire twice (`tmp/repro-class-transformer.raku` prints `OUT DONE` twice under mutsu, once under raku).

## Fix direction
Make on-demand `Supply.Promise` asynchronous:
- In `supply_promise_on_demand`, after `run_on_demand_body` has collected the subscriptions (this part can stay synchronous — it must run the body to register whenevers), do NOT drive the react loop on the calling thread. Instead hand the `react_subs` + policy to a background drive: either
  (a) spawn a GC-registered helper thread (`spawn_gc_helper_thread`, used at `supply_promise.rs:433` already — remember the "Gc-touching threads must be registered" rule) that owns a thread-clone interpreter and runs `drive_react_subscriptions_nested`, keeping/breaking the promise; or
  (b) for live Supplier-backed sources, reuse the existing promise registry path (`supplier_register_promise`, `supply_promise.rs:351`) so the emitting thread itself resolves the promise on emit/done/quit — this is the same mechanism the `supplier_id_from_attrs` fast path at `native_supply_dispatch.rs:433-434` already uses for live supplies, extended to on-demand bodies whose whenevers hang off live suppliers.
- Keep the synchronous drive for the `await $supply` path if it is load-bearing there (await is ALLOWED to block its thread — but note in this codebase `await` on a Supply likely routes through `.Promise` too; if so, `await` should block on the returned promise, not on the coercion).
- Preserve the existing synchronous resolutions for static/finite sources (`react_subs.is_empty()` branch, `supply_promise.rs:396-401` / `:510-519`) — they resolve immediately and cannot deadlock; changing them would churn many passing tests.

Risks: high-traffic machinery; the ordering guarantees of the current inline drive (values emitted synchronously before return) may be relied on by roast S17 supply tests. Time-box a survey of `SupplyDrivePolicy::Promise` callers first. A wrong thread-clone for the background drive can resurrect the shared-store collisions of the sibling tickets — prefer option (b) where possible since it adds no new thread.

## Verification
- `tmp/repro-promise-supply-coerce.raku`: prints `after coercion: Promise status=Planned` then `result: ab`.
- `tmp/repro-bodytext-blocks.raku`: prints `S: after emit` / `S: after done` and `T: body promise resolved: Kept`.
- `tmp/repro-respparser-untilclosed.raku`: C1 body Kept with `"hello\n"`; C2 body Broken with `X::Cro::HTTP::RawBodyParser::ContentLength::TooShort`.
- `t/http-response-parser.rakutest`: 164/164 (subtests 111 and 120 pass; 116-119 keep passing).
- roast S17-supply / S17-promise whitelist files locally, full roast via CI.
