# Closure-capture same-name family — RESOLVED CORE (ADR-0025 slice 1); two narrowed residuals + slice 2 remain

## Status after ADR-0025 slice 1 (2026-08-11)

The core defect is **diagnosed differently than this ticket originally
claimed, and fixed**. The original TL;DR blamed "read-only captures get no
cell"; the sharpened diagnosis (see
`docs/adr/0025-captured-scalar-cells-value-kind-blind.md`, Context) is:

- The real files' `$encoder` IS captured-and-mutated (declare-then-assign,
  reassigned between test blocks), so `captured_mutated_locals` held it and
  the vouch refusal was correct; the check closures ARE escaping-deemed
  (array-literal elements compile under `with_escape(true)`), so
  `needs_cell_locals` fired.
- The single defense that failed was the **value-kind skip** in
  `box_captured_lexicals` / `box_decl_local_cell`: a slot holding an
  `Instance` at boxing time was never boxed. HPACK encoders, `Instant`
  (`$fake-now`), and session objects are all Instances, which is why both
  the hijack direction AND the staleness direction hit exactly this family.
- Slice 1 removed `Instance` from the skip (2 lines). Pins:
  `t/closure-capture-instance-cell.t` (6 tests, raku-validated), built from
  the minimal in-repo repros `tmp/cap-hijack-instance.raku` /
  `tmp/cap-hijack-str-ab.raku` / `tmp/cap-stale-worker-first.raku` — the
  synthetic-repro drought this ticket reported is over (the missing
  ingredient was an Instance-holding capture plus a caller-chain shadow
  that owns an env KEY, forced via a nested capturing closure).

Cro measurements (main → slice 1): `http2-request-serializer.rakutest`
notok 3 → **0**; `http2-response-serializer.rakutest` 3 → 1;
`http2-request-parser.rakutest` 1 → 1.

## Remaining work tracked here

1. **`http2-response-serializer.rakutest` test 14** ("check 4" of
   'Header + Data'): the `$encoder` cell now delivers the right object, so
   the residual mismatch is elsewhere. Suspects: `@headers` liveness
   through the capture (`@headers` is reassigned between blocks; `@` is
   outside slice 1), or HPACK dynamic-table state trajectory (the test-side
   encoder is warm from the previous block's check while the per-`test()`
   serializer is cold). Needs shadow-bisect with byte-level comparison.
2. **`http2-request-parser.rakutest` test 44** ("check 4" of
   'Header1 + Header2 + Data1'): the failing check is
   `*.body-blob.result eq $payload` — NOT the encoder family at all (the
   old "DATA frame content mismatch" label was right for THIS file).
   Suspects: cross-stream DATA demux (streams 3 and 5 interleave, stream 5
   carries `$payload ~ $payload`) or `Buf`-capture. Independent diagnosis.
3. **ADR-0025 slice 2** (escape verdict must stop being a correctness
   gate — decl-site cells for every vouch-refused captured scalar) and
   slice 3 follow-ups: design and gates are in the ADR; implementation
   pending.
4. **Session acceptance criterion: RESOLVED (2026-08-11).** The rc=139
   crash was an unrelated pre-existing bug (`supply_promise_on_demand`'s
   drive thread used the default ~2 MiB stack instead of the 256 MiB
   user-code stack, overflowing on deep grammar/regex recursion —
   `news/2026-08/supply-promise-ondemand-whenever-drive-thread-stack-size.md`).
   With it fixed, `http-session-inmemory.rakutest` runs to a full 13/13,
   confirming the staleness mechanism slice 1 fixed (pinned by
   `t/closure-capture-instance-cell.t` tests 3-4) also covers "Session
   expires appropriately". `http-session-persistent.rakutest` no longer
   crashes either, but still fails its own test 13
   (`X::Cro::HTTP::Error::Client`) — a separate, undiagnosed issue.

Related (third direction, separate compiler bug, root cause now verified —
see its ticket):
`todo/tickets/closure-for-loop-param-hijacked-by-same-named-captured-outer.md`.
