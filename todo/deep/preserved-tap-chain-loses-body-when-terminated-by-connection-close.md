# `Cro::HTTP::ResponseParser` loses a body terminated by connection close (via `.tap()`/`Supplier::Preserving`, not `Promise()`)

## Symptom

`tmp/cro-work/C_RO_CRO_HTTP_*/t/http-response-parser.rakutest` has two
persistent failures (release binary, 2026-08-12, after the
`register_nested_on_demand_source` / `Promise(supply{...})` fixes in
`news/2026-08/promise-supply-nested-derived-supply-last-phaser.md`):

- "Response with body terminated by close of connection" (line ~281)
- "Connection close with incomplete body throws" (line ~296)

Both fail with `# Response parser failed to emit a HTTP response` — the
test's own `$test-completed` Promise (kept by the response-parser's `.tap()`
callback) never resolves within the test's 10-second `Promise.in(10)` guard.

## Why this looks related but is NOT the same bug

The already-fixed ticket
(`news/2026-08/promise-supply-nested-derived-supply-last-phaser.md`, formerly
`todo/deep/last-phaser-loses-outer-var-mutations-when-whenever-source-is-a-nested-supply.md`)
covered `Promise(supply { whenever <derived-supply> { ...; LAST {...} } })`
losing values. `Cro::MessageWithBody.body-blob` uses exactly that pattern,
and fixing it was expected to fix this test too — but it did not. The
remaining chain here is longer and goes through `.tap()`, not `Promise()`:

`Cro::HTTP::ResponseParser.transformer($in)` (`lib/Cro/HTTP/ResponseParser.rakumod`):

```raku
sub preserve(Supply:D $s) {
    my $p = Supplier::Preserving.new;
    $s.tap: { $p.emit($_) }, done => -> { $p.done }, quit => { $p.quit($_) };
    $p.Supply
}
...
$response.set-body-byte-stream(preserve(
    $raw-body-parser.parser($response, $raw-body-byte-stream.Supply, $leftover)));
```

For a connection-close-terminated body, `$raw-body-parser` is
`Cro::HTTP::RawBodyParserSelector::UntilClosed`, whose `.parser(...)` returns
`supply { whenever $raw-blobs { .emit; } }` — a derived/on-demand supply. But
here it is consumed via `.tap()` inside `preserve()` (buffering into a
`Supplier::Preserving`), NOT via `Promise(supply { whenever ... })` — so this
does not go through `supply_promise_on_demand`/
`register_nested_on_demand_source` at all. The whole test file's `parses()`
helper (line 23) itself also uses `.tap()` on
`$testee.transformer($fake-in.Supply)`, another layer of on-demand supply
composition (`transformer` is itself a `supply { ... }` block with a
`whenever $in -> Cro::TCP::Message $packet { ... }`, several levels of
`fresh-message`/`whenever $cancellation`/emit-per-header-line nesting, and an
inner `preserve()`-wrapped body sub-stream) on top.

Given the fix already landed handles exactly ONE specific shape (a `whenever`
whose direct source is a derived on-demand supply, inside a `Promise(supply
{...})` or react/`.tap()` context via `register_nested_on_demand_source`),
and this chain adds a `Supplier::Preserving`-mediated `.tap()` hop in the
middle, it is plausible (not confirmed) that:

- `Supplier::Preserving`'s own `.tap()`/buffering semantics have a similar
  live-vs-static gap the fix didn't touch, or
- the outermost `parses()`-level `.tap()` on `transformer()`'s own multi-stage
  on-demand supply has a DIFFERENT gap (not the `Promise()`-coercion one) in
  how nested `whenever`s inside a body that itself does per-header-line
  `emit`+conditional `fresh-message`/`next`/`last` looping are driven.

## Suggested next steps (not investigated further)

1. Minimize outside Cro: build a 3-level chain — `Supplier` → `supply {
   whenever $raw.Supply { emit } }` (an UntilClosed stand-in) → `preserve()`
   (a `Supplier::Preserving` tap-forward, reimplement inline, no Cro import
   needed) → `.tap()` the preserved supply directly (not via `Promise()`) —
   and see if a plain `.tap()` on the FINAL stage ever receives anything.
   This isolates whether `Supplier::Preserving`/`preserve()` itself is the
   break, independent of `ResponseParser`'s own `whenever $in -> $packet
   {...}` complexity.
2. If step 1 reproduces, `rust-gdb` into `Supplier::Preserving`'s `.emit`/
   `.tap` native method implementations (`native_supplier_methods.rs` or
   wherever `Preserving` is handled — grep `Preserving`) and compare against
   the now-fixed `register_nested_on_demand_source` path for how a `.tap()`
   on a live/derived source gets its consumer wired.
3. If step 1 does NOT reproduce, the bug is specific to
   `ResponseParser.transformer`'s own body shape (`whenever $in -> $packet
   {...}` with an inner `loop { ... emit $response ... }` and conditional
   `next`/`last`, PLUS a nested `whenever $cancellation` registered from
   `fresh-message` each time a new response starts) — minimize THAT shape
   directly instead (a `supply { whenever $in -> $x { loop { ...; emit
   $something; last } } }` body that also emits a body sub-stream some
   OTHER consumer taps).
4. `MUTSU_TRACE=supply` (or whatever the closest trace category is; check
   `src/trace.rs`) while running the isolated repro from step 1 may show
   whether `try_stream_emit`/`SinkEvent` ever fires for the innermost stage
   at all.

## Reproduce

```
DIST=$(echo /home/tokuhirom/work/mutsu-roast/tmp/cro-work/C_RO_CRO_HTTP_*)
INC=$(cat /home/tokuhirom/work/mutsu-roast/tmp/cro-work/inc-paths.txt)
cd "$DIST"
timeout 60 /home/tokuhirom/work/mutsu-roast/target/release/mutsu $INC -I "$DIST/lib" -I "$DIST/t" t/http-response-parser.rakutest
```

Expected (raku): all tests pass. Actual (mutsu, release build,
2026-08-12): tests 111 and 120 (`not ok`) — "Response parser failed to emit
a HTTP response" for both connection-close-terminated body cases.

Requires the vendored Cro checkout under `tmp/cro-work/` from prior sessions
(not part of this repo's tracked test suite — Cro itself is intentionally
not bundled, see `handoff-cro-next-steps` project memory / `PLAN.md`).
