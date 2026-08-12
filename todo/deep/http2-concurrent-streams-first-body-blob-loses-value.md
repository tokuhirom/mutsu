# HTTP/2 multiplexed request parsing: the FIRST of two concurrent streams' `.body-blob` resolves wrong, the second is fine

## Symptom

`tmp/cro-work/C_RO_CRO_HTTP_*/t/http2-request-parser.rakutest`, test "Header1
+ Header2 + Data1 + Data2" (two HTTP/2 streams — id 3 and id 5 — each with a
`Headers` + single `Data` frame, multiplexed on one connection): the FIRST
stream's (id 3) `check 4` (`*.body-blob.result eq $payload`) fails
deterministically; the SECOND stream's (id 5) equivalent `check 4`
(`*.body-blob.result eq $payload ~ $payload`) passes. Reproduces 5/5 runs
(release binary, 2026-08-12) — not flaky, a real ordering/concurrency bug.

Confirmed independent of `news/2026-08/promise-supply-nested-derived-supply-last-phaser.md`
(the `Promise(supply { whenever <derived-supply> {...} })` fix landed the same
session): `Cro::HTTP2::GeneralParser` (`lib/Cro/HTTP2/GeneralParser.rakumod`
line 87) backs each stream's request body with a raw `Supplier::Preserving`
directly (`set-body-byte-stream($body.Supply)`), NOT a nested `supply { }`
block — so `.body-blob`'s `Promise(supply { whenever self.body-byte-stream
-> $blob {...} })` sees a live-`Supplier`-backed source (already-working
code path, confirmed via `t/promise-of-supply-live-supplier.t` and this
session's `tmp/nested-supply-promise-baseline.raku` repro), not a derived
one. This is therefore a *different* bug from the one just fixed.

## What's set up

`Cro::HTTP2::GeneralParser`'s `supply { whenever $in {...} }` (frame-level
demux loop) creates a fresh `Supplier::Preserving` per HTTP/2 stream as
`Headers` frames arrive (keyed by stream id in `%streams`), appends `Data`
frame bytes to the matching stream's supplier as they arrive (demuxed by
`stream-identifier`), and `.done`s it on `END_STREAM`. Two DIFFERENT
`Cro::HTTP::Request` objects (one per stream) each independently call
`.body-blob` (via the test's `check 4`), each spinning up its own
`Promise(supply { my $joined = Buf.new; whenever self.body-byte-stream ->
$blob { $joined.append($blob); LAST emit $joined } })` — i.e. two
CONCURRENT `supply_promise_on_demand` background drives, each targeting a
DIFFERENT `Supplier::Preserving` instance (different `supplier_id`s), racing
against the SAME frame-demux loop still processing the connection.

## Hypotheses (not investigated further)

1. A `emitter_supplier_id`/`supplier_id` collision or stale-registry read
   between the two concurrent `Promise(supply{...})` drives — e.g. the
   *first* stream's `done_promise`/`supplier_register_promise` entry getting
   overwritten or consumed by the second's registration before the first's
   background thread observes it. Grep `next_supplier_id`/
   `supplier_register_promise`/the global supplier registry
   (`native_supplier_methods.rs`) for anything keyed loosely enough that two
   concurrent registrations could cross-talk.
2. A timing race specific to being the FIRST stream processed: the first
   stream's body-blob Promise may start driving (spawn its background
   thread) WHILE the connection's shared frame-demux `supply { whenever $in
   {...} }` loop is still actively running (processing the second Headers +
   both Data frames), vs the second stream's Promise starting only once the
   demux loop has quiesced. If `Supplier::Preserving`'s buffering/replay
   depends on when exactly a tap/whenever registers relative to `.emit()`
   calls arriving on ANOTHER thread, the first stream's registration could
   lose an early buffered chunk that the second stream's (registering later,
   after all frames are already fully buffered) does not.
3. `body-blob` on the FIRST stream's Request might be racing its own
   `.done()` call (`END_STREAM` on stream 3 arrives before stream 5's
   Headers/Data are even parsed) against `Promise(supply{...})`'s classifier
   still running `run_on_demand_body` synchronously — check whether
   `session's stream_consumers_base`-style handoff (see the just-landed fix)
   has an analogous gap for the plain-live-Supplier branch (not the
   on-demand-nested branch) when `.done()` fires extremely early.

## Suggested next steps

1. Minimize outside Cro: two `Supplier::Preserving` instances, each fed one
   `Data`-equivalent chunk + `.done()` from a single "demux" thread/loop,
   each independently `.body-blob`'d (or a hand-rolled equivalent
   `Promise(supply { whenever $preserving.Supply -> $b {...; LAST emit
   $b} })`) — see if the FIRST one to be both fed-and-queried loses its
   value while the second does not, with no Cro/HTTP2 code involved at all.
2. If step 1 reproduces, `rust-gdb` break in `supply_promise_on_demand`
   (`src/runtime/supply_promise.rs`) with a condition on which
   `emitter_supplier_id`/`Supplier::Preserving` instance is being processed,
   and compare timing/ordering between the two concurrent invocations.
3. If step 1 does NOT reproduce, the bug is specific to
   `Cro::HTTP2::GeneralParser`'s OWN frame-demux `whenever $in {...}` shape
   (interleaved multi-stream state in `%streams`, plus its own `whenever
   $cancellation` per stream) — minimize THAT shape instead.

## Reproduce

```
DIST=$(echo /home/tokuhirom/work/mutsu-roast/tmp/cro-work/C_RO_CRO_HTTP_*)
INC=$(cat /home/tokuhirom/work/mutsu-roast/tmp/cro-work/inc-paths.txt)
cd "$DIST"
timeout 60 /home/tokuhirom/work/mutsu-roast/target/release/mutsu $INC -I "$DIST/lib" -I "$DIST/t" t/http2-request-parser.rakutest
```

Expected (raku): all tests pass. Actual (mutsu, release build, 2026-08-12):
test 50 (`not ok 50 - check 4`, first of the two-stream "Header1 + Header2 +
Data1 + Data2" case) fails 5/5 runs; the sibling check for the second stream
(test 52) always passes.

Requires the vendored Cro checkout under `tmp/cro-work/` from prior sessions
(not part of this repo's tracked test suite).

## Also present: `http2-response-serializer.rakutest` (1 failure, not yet
diagnosed)

Same suite sweep (2026-08-12) shows `http2-response-serializer.rakutest`
at 28/29 (`notok=1`), unrelated to this session's other work and not yet
looked at — may or may not share this same root cause (both are HTTP/2
multi-stream files). Check it once this ticket's root cause is known.
