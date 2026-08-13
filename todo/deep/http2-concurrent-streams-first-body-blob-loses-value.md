# HTTP/2 multiplexed request parsing: the SECOND of two concurrently-registered streams' `.body-blob` silently resolves empty

## Correction (2026-08-13)

The original title/symptom below (filed 2026-08-12) claimed the FIRST
stream's `.body-blob` fails and the second is fine. That was a
misreading of the TAP test-number output under concurrent execution —
each arriving `Cro::HTTP::Request` spawns its OWN `start {}` thread to run
its checks (`t/http2-request-parser.rakutest`'s `test()` harness), so the
printed `ok N`/`not ok N` numbers interleave non-deterministically between
the two streams' check threads and do NOT correspond to "stream A's checks
then stream B's checks" in file order.

Re-diagnosed 2026-08-13 with a harness patch that tags each printed line
with the request's arrival-order index (`$current-counter`, captured
synchronously in the tap callback before either check-thread starts, so it
reliably identifies stream 3 vs stream 5 regardless of which check-thread's
`ok()` call lands first) — **it is deterministically the SECOND stream
(id 5, arrival index 1) whose `.body-blob.result` comes back wrong**, 5/5
runs. The first stream (id 3, index 0) always resolves correctly.

Also: `http2-response-serializer.rakutest`, listed below as "not yet
diagnosed" in the original filing, is now **29/29** (was 28/29) as of
2026-08-13 — likely fixed as a side effect of the `whenever`
LAST-phaser/quit work landed the same day (PR #6350) or the ADR-0028 Slice 2
bypass-path fixes (PR #6346). Not re-investigated separately; drop it from
this ticket's scope.

## Symptom (corrected)

`tmp/cro-work/C_RO_CRO_HTTP_*/t/http2-request-parser.rakutest`, test "Header1
+ Header2 + Data1 + Data2" (two HTTP/2 streams — id 3 and id 5 — each with a
`Headers` frame carrying `END_HEADERS` but not `END_STREAM`, followed later
by a `Data` frame carrying `END_STREAM`, multiplexed on one connection): the
SECOND stream's (id 5) `check 4` (`*.body-blob.result eq $payload ~
$payload`) fails deterministically — `.body-blob.result` resolves to an
**empty 0-byte `Buf`** instead of the real 123/246-byte payload. The first
stream's (id 3) equivalent check always passes. Reproduces 5/5 runs (both
debug and release binaries, 2026-08-13).

## Root cause, as far as diagnosed

Confirmed via three layers of debug instrumentation (all removed again —
none committed):

1. A copy of `Cro::MessageWithBody.body-blob`/`.body-byte-stream`/
   `.set-body-byte-stream` (`Cro::Core`'s `lib/Cro/MessageWithBody.rakumod`,
   patched under `tmp/cro-work/` only, not part of this repo) logging
   `self.WHERE` (request identity), the stored/read Supply's `.WHERE` and
   `.defined`, at both write time (`set-body-byte-stream`, called from
   `GeneralParser`'s frame-demux loop at Headers-frame time) and read time
   (`body-byte-stream`, called later from `body-blob`'s background
   `Promise(supply{...})` on a different thread).
2. Output for the failing stream shows the write-time call itself already
   receiving an **undefined `Supply` type object** as its argument:
   `set-body-byte-stream($body.Supply)` — where `$body` is a freshly
   constructed `Supplier::Preserving.new` for THIS stream — evaluates
   `$body.Supply` to `Supply` (the undefined type object) instead of a
   defined instance, ONLY for the second such call within the same running
   `whenever $in {...}` frame-demux loop. The WHERE address of this
   undefined value is identical across multiple failing runs (consistent
   with it being a canonical type-object singleton, not a stray real
   instance).
3. `body-byte-stream()`'s `with $!body-byte-stream {...} else { supply {} }`
   then correctly falls to the `else` branch (since the attribute really is
   undefined), returning an always-empty static `supply {}` — `body-blob`'s
   `whenever` over that never receives any chunk, so its `LAST emit $joined`
   fires with `$joined` still an empty `Buf.new`. This is NOT a
   registration-timing race in the supplier-sink/react-drive machinery (no
   evidence of that layer even being reached wrong) — the corruption
   happens earlier, at the `$body.Supply` call site itself, before
   `Promise(supply{...})` is ever involved.

So the actual defect is: **calling `.Supply` on a `Supplier::Preserving`
instance, from inside a repeatedly-executed `whenever` body (the SECOND
time that particular call site executes within one running program),
sometimes returns the undefined `Supply` type object instead of invoking
the native `"Supply"` method** (`src/runtime/native_supplier_methods.rs`
line 23, which unconditionally returns
`Value::make_instance(Symbol::intern("Supply"), supply_attrs)` — reading
that function in isolation, it can never itself produce an undefined
value). The bug is therefore somewhere in *method dispatch* deciding NOT to
call that native handler for this specific call, not in the handler.

## What was ruled out

- **Not the supplier-sink/react-drive registration race** originally
  hypothesized (see "Original hypotheses" below) — the corruption is
  visible before any of that machinery runs, right at the `.Supply` call.
- **Not a `class_mro` dispatch-table gap**: `call_native_instance_method`
  (`src/runtime/native_methods/mod.rs`) has an initial fast-path `matches!`
  that lists `"Supplier"` but NOT `"Supplier::Preserving"` (the mutable
  counterpart, `call_native_instance_method_mut`, correctly lists both) —
  this looked like a promising lead but is a dead end: the function's
  fallback (`self.class_mro(class_name)`-based lookup) still finds
  `"Supplier"` in `Supplier::Preserving`'s MRO and sets `dispatch_class =
  Some("Supplier")`; and even if that lookup somehow failed entirely,
  the final `match dispatch_class.as_deref().unwrap_or(class_name) { ...
  "Supplier" | "Supplier::Preserving" => self.native_supplier(...) }` would
  still match on `class_name` itself. Dispatch to `native_supplier` is
  therefore unconditional either way for this class. (The missing entry in
  the fast-path list is still worth adding for consistency/`class_mro`
  overhead's sake, but it is not this bug's cause.)
- **Not reproducible in ~5 independent minimization attempts outside the
  real `Cro::HTTP2::GeneralParser` file**, despite closely mirroring its
  shape each time:
  1. Two bare `Supplier::Preserving` instances, both fed+done before either
     is queried via a hand-rolled `body-blob`-equivalent — no repro.
  2. Same, but with the feeding done through an intermediate `supply {
     whenever $frames {...} }` demux loop matching the real frame-dispatch
     shape — no repro.
  3. Same, with real thread concurrency (`start {}` feeder with `sleep`s
     between frames, `body-blob` promise created mid-feed) — no repro, ran
     30 iterations.
  4. A `Msg`/`Stream` class pair mirroring `Cro::MessageWithBody`'s
     `set-body-byte-stream`/`body-byte-stream`, with the demux loop reading
     `.body-byte-stream` synchronously right after writing it (same thread)
     — no repro (all defined).
  5. Same, but reading via an externally-tapped `start {}` thread (matching
     the real cross-thread `body-blob` read) with 40 iterations, including
     a nested `whenever $cancellation {...}` inside the same `unless
     %streams{...}:exists {...}` block (mirroring `GeneralParser`'s own
     per-stream cancellation subscription) — still no repro.

  This matches a known "bisect-resistant" pattern already documented
  elsewhere in this project's history (see CLAUDE.md's roast-triage
  section, "全ファイル限定バグは兄弟stmt削りbisect") — some bugs only
  reproduce with the full surrounding statement bulk/complexity of the
  original file, not in a reduced form. The minimal trigger has NOT been
  found; only the real `Cro::HTTP2::GeneralParser.transformer` method
  reproduces it.

## Suggested next steps

1. `rust-gdb` break directly in `native_supplier_methods.rs`'s `"Supply" =>`
   arm (line ~23) with a hit counter, and separately break wherever a
   generic/fallback `.Supply` coercion for `Any`/`Mu` might live (grep for
   another handler that could plausibly produce an undefined `Supply` type
   object as a "no known way to get a Supply from this receiver" fallback).
   Confirm whether the native arm is even reached for the failing call, or
   whether some OTHER dispatch path (a builtin coercion stub, a role method
   resolution picking a different candidate, a cached/memoized method-table
   entry) intercepts it instead — this is the open question the debug
   instrumentation above did not answer.
2. If the native arm IS reached but still yields undefined output somehow
   (e.g. a second look at `supplier_id_from_attrs`/`next_supply_id`
   returning something that collides), add gdb watchpoints on the specific
   `AttrMap` backing the second stream's `$body` Supplier instance across
   the two calls.
3. If the native arm is NOT reached, the bug is a method-resolution
   correctness issue specific to a native class's method being called
   twice, close together, on two DIFFERENT instances, from inside a
   `whenever` body that runs multiple times within one process — worth
   checking whether any monomorphic/per-call-site inline cache exists for
   method dispatch that might be keyed too coarsely (by method name and
   call-site only, not by receiver identity/class), especially given the
   call site (`$body.Supply`) is textually the SAME AST node on both the
   first (working) and second (failing) executions.
4. Given repro requires the real file, prefer instrumenting
   `Cro::HTTP2::GeneralParser.rakumod`/`Cro::MessageWithBody.rakumod`
   copies under `tmp/cro-work/` (NOT the read-only vendored `roast/`, and
   NOT this repo's tracked source) rather than trying another from-scratch
   minimization — five attempts at that have already failed.

## Original hypotheses (2026-08-12, superseded — kept for reference)

These assumed the FIRST stream was the failing one and pointed at the
supplier-sink/react-drive registration timing; both premises are now known
to be wrong (see "Root cause" above), but are kept here in case the
`.Supply`-returns-undefined finding turns out to be a red herring and this
angle needs revisiting:

1. A `emitter_supplier_id`/`supplier_id` collision or stale-registry read
   between the two concurrent `Promise(supply{...})` drives.
2. A timing race in when the react-drive loop's sink registration happens
   relative to the frame-demux loop's `.emit()`/`.done()` calls.
3. `body-blob` racing its own stream's `.done()` against
   `run_on_demand_body` still running synchronously.

## Reproduce

```
DIST=$(echo /home/tokuhirom/work/mutsu-roast/tmp/cro-work/C_RO_CRO_HTTP_*)
INC=$(cat /home/tokuhirom/work/mutsu-roast/tmp/cro-work/inc-paths.txt)
cd "$DIST"
timeout 60 /home/tokuhirom/work/mutsu-roast/target/release/mutsu $INC -I "$DIST/lib" -I "$DIST/t" t/http2-request-parser.rakutest
```

Expected (raku): all tests pass. Actual (mutsu, both debug and release,
2026-08-13): one `not ok - check 4` inside the "Header1 + Header2 + Data1 +
Data2" subtest, always the SECOND stream (id 5) — confirm by patching the
harness (`t/http2-request-parser.rakutest`'s `test()` sub, in a scratch copy
under `tmp/`) to log `$current-counter` alongside each check result, since
raw TAP test numbers do not reliably identify which stream failed under
concurrent `start {}` execution.

Requires the vendored Cro checkout under `tmp/cro-work/` from prior sessions
(not part of this repo's tracked test suite). Building this checkout
requires ALL of `tmp/cro-work/{C_RO_CRO_HTTP_*,C_RO_CRO_CORE_*,
C_RO_CRO_TLS_*,IO-Socket-Async-SSL,J_SO_JSON_JWT_*,C_BO_CBOR_SIMPLE_*,
Log-Timeline,DateTime-Parse,TinyFloats}/lib` on `-I` (the full set in
`tmp/cro-work/inc-paths.txt`) plus this repo's bundled
`modules/HTTP-HPACK/lib` — a partial `-I` set can fail confusingly with
"Could not find <unrelated top-level module>" even though the actual
missing dependency is several `use` statements deeper.
