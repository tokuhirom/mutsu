# Supplier::Preserving backlog is destroyed by `done` (immutable-lane `supplier_reset`), so a tap registered afterwards sees nothing

## Affected tests

- `t/http2-response-parser.rakutest` subtest 5 ("Headers + Data") — deterministic: the check `*.body-blob.result == $random` hangs because `body-blob` never resolves; `$test-completed` times out.
- `t/http2-request-parser.rakutest` subtests around 12/19/42/44 — **flaky, ordering-dependent**: the same mechanism hits whenever the Data frame + `body.done` are processed *before* the checking `start` block calls `.body-blob`. Fails as "Headers + Data"/"Headers + Continuation + Data" flunks or as a failing `check 4` (`*.body-blob.result eq $payload`). Under parallel load it fails; run serially it usually passes.
- `t/http2-request-serializer.rakutest` subtest 12 ("POST with set-body round-trips correctly over HTTP/2") — occasional: round-trip body goes through the same body-byte-stream machinery.

Mechanism in Cro: `Cro::HTTP2::GeneralParser` builds `my $body = Supplier::Preserving.new`, stores `$body.Supply` via `set-body-byte-stream`, then on a Data frame does `$stream.body.emit: .data` and `$stream.body.done` (GeneralParser.rakumod lines 74-82, 87-97). `Cro::MessageWithBody.body-blob` (Cro::Core) later taps that supply via `Promise(supply { whenever self.body-byte-stream { ... LAST emit $joined } })`. Shadow probes confirmed both sides use the same supplier object (`Supplier::Preserving|778`), emit(123 bytes)+done complete BEFORE the tap registers, and the tap then receives neither the backlog nor `done` — so the `Promise(...)` coercion's drive loop blocks (30s internal deadline; the test's 5s `Promise.in` wins).

## Repro

Pure Raku (`tmp/h2-preserving-accessor.raku`):

```raku
class Holder { has $.body }

# Case E: emit/done through an instance accessor chain, tap afterwards
my $s = Supplier::Preserving.new;
my $h = Holder.new(body => $s);
$h.body.emit(Buf.new(1,2,3));
$h.body.done;
my $got = False;
$s.Supply.tap: -> $b { say "E got {$b.elems}"; $got = True },
    done => { say "E done" };
say "E result: ", $got;

# Case F: same calls on the bare variable
my $s2 = Supplier::Preserving.new;
$s2.emit(Buf.new(4,5));
$s2.done;
$s2.Supply.tap: -> $b { say "F got {$b.elems}" }, done => { say "F done" };
```

- mutsu: `E result: False` (no replay, no done callback), while F works (`F got 2` / `F done`).
- raku: `E got 3` / `E done` / `E result: True` / `F got 2` / `F done`.

The full-chain hang is reproduced by `tmp/h2-resp-parse.raku` (via `bash tmp/croflake.sh`, absolute `MUTSU_BIN`): mutsu prints `in start, calling body-blob` and then hangs (`status: Planned`); raku completes with `match: True`.

## Root cause

mutsu has two "done" lanes for a Supplier instance:

- **Immutable lane** — `native_supplier` `"done"` arm, `src/runtime/native_supplier_methods.rs:367-467`. It ends with `supplier_reset(supplier_id)` (line 465), and `supplier_reset` (`src/runtime/native_methods/state.rs:558-568`) clears `state.emitted`, `emitted_seq`, AND the `done` flag in the process-global `SupplierRuntimeState`.
- **Mutable lane** — `native_supplier_mut` `"done"` arm (same file, ~line 867+), which updates the instance's own attrs (`done => True`) and resets conditionally.

A method call chained off an accessor (`$h.body.done`, `$stream.body.done`) dispatches down the immutable lane; a call on a bare variable (`$s.done`) takes the mutable lane — hence case E vs case F. For a `Supplier::Preserving` with **no tap/sink registered at done time**, the immutable lane's reset destroys exactly the state that Preserving exists to keep: the buffered values (`state.emitted`, watermarked by `preserved_consumed`, state.rs:175-183) and the terminal event. A later tap registration replays via `supplier_take_preserved_backlog` (state.rs:450) and `supplier_take_preserved_terminal` (state.rs:475) — both find a virgin state, so the tap gets nothing and a drive loop waiting on the supplier (`supplier_sink_register`, state.rs:220 — which replays `state.emitted` + `state.done`) blocks forever.

This is the remaining piece of the known "done's supplier_reset wipes the buffer" family (see `news/` for #6129, which fixed the sink-ordering variant); the no-consumer-at-done case is still broken.

## Fix direction

In the immutable-lane `"done"` arm (`native_supplier_methods.rs:465`): when `attributes.contains_key("preserving")`, do **not** `supplier_reset` — leave `state.emitted` and `state.done` intact so the existing `preserved_consumed` / `preserved_terminal_delivered` watermarks give exactly-once replay to the next tap (that machinery already exists and is exercised by the passing case F path). Check the mutable-lane arm (~line 988 `supplier_reset(sid)`) for the same guard so the two lanes stay in parity; ideally extract one shared done-finalization helper.

Risk: `supplier_reset` exists so a plain (non-preserving) Supplier can be reused after done — keep that behavior for non-preserving suppliers. Regression surface is the react/whenever done ordering fixed in #6129 (`tcp.rakutest` hang) and the S17 supply tests; also `t/supply-*.t`.

## Verification

- `tmp/h2-preserving-accessor.raku`: case E prints `E got 3` / `E done` / `E result: True`.
- `tmp/h2-resp-parse.raku` (croflake.sh): prints `blob elems: 123 match: True`, `status: Kept`.
- `t/http2-response-parser.rakutest`: 5/5 (currently 4/5 + flunk).
- `t/http2-request-parser.rakutest`: 54/54 stable under load — run it ~5x and once with 4 concurrent instances; the flaky body-blob check-4 failures should disappear.
- `t/http2-request-serializer.rakutest` subtest 12 stable across repeats.
- `make test` (t/lock.t, t/supply-batch-period.t are nearby supply-state pins).
