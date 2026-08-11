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

0. **RESOLVED (PR #6238): the "test 14"/"test 44" labels were artifacts of a
   TAP counter desync.** A thread spawned before the first test call never
   shared the TAP counter (`TapState::clone_for_thread` only shares an
   EXISTING `TestState`, created lazily by the first `ok`), so the first
   tap's increments were lost and every subsequent test number shifted by
   the first check batch — all three HTTP/2 files failed prove with "Tests
   out of sequence" even where every assertion passed. Fixed; pin
   `t/test-counter-spawn-before-first-test.t`. With correct numbering,
   `http2-request-serializer.rakutest` passes COMPLETELY, and the two
   residuals below are tests 18 and 49 respectively.
1. **`http2-response-serializer.rakutest` test 18** ("check 4" of
   'Header + Data - Content-Length unspecified', block 3 — NOT block 2, and
   NOT `@headers`): re-diagnosed 2026-08-11 with byte-level probe
   (`tmp/h2rs-probe.raku`). The captured `@headers` is correct (2 entries),
   but the check closure computes `$encoder.encode-headers(@headers)` =
   `88 BE` — an HPACK dynamic-table reference, meaning it used the OLD
   warm encoder: WHICH-trace shows mainline holds the fresh
   `Encoder|1068` (created line 74) while the tap-thread check — and a
   `$probe = { $encoder.WHICH }` closure created right after the line-74
   rebind and invoked from inside the check — both resolve `$encoder` to
   the stale `|928`. Crucially, a `$pre = { $encoder.WHICH }` closure
   created BEFORE the line-74 rebind sees the NEW object when invoked
   from mainline — **the ContainerRef cell exists and mainline
   write-through works** (slice 1 did its job; a plain snapshot would
   show the old object in mainline too). The SAME `$pre` closure invoked
   from the tap thread sees the old `|928`. So the defect is on the
   worker-side resolution chain: a stale PLAIN `encoder` entry beats (or
   replaced) the cell during cross-thread dispatch — exactly the
   ADR-0025 "cross-thread audit rider" family
   (`sync_shared_vars_to_env` / `set_env_with_main_alias_sym` /
   spawn-seeding staleness). The spawn-seeding walk itself
   (`clone_for_thread_excluding`) seeds the raw env value, which
   preserves a cell, so the stale-plain lane is one of the other sites —
   pinpoint with rust-gdb on `tmp/h2rs-probe.raku` (break on the
   closure-dispatch merge and on `sync_shared_vars_to_env`, watch who
   installs `encoder` in the tap thread's env). Note: a Cro-free
   reduction (`tmp/tap-check-stale.raku`: named sub + supply/whenever
   pass-through + tap + start-driven emit + rebind between calls) does
   NOT reproduce — the missing ingredient is in Cro's transformer
   internals (ConnectionState nested whenevers / `Supplier::Preserving`),
   which may link this to the nested-whenever adoption bug in
   `todo/deep/nested-whenever-registration-clobbers-sibling-event-aggregate-writes.md`.
   **Fix-order implication: ADR-0025 slice 2's "cross-thread audit
   rider" (generalize the cell-preserving rule to every
   frame-independent assign/sync utility) is load-bearing for this file
   and may be a smaller targeted fix than the full decl-site-cell set
   extension.** The ADR's block-2/HPACK-trajectory and `@headers`
   suspects are exonerated (probe shows `+@headers == 2`, correct).
   FINAL datum (decisive for the gdb session): inserting ANY mainline
   `start { }` spawn between the line-74 rebind and the block-3 `test()`
   call — even one that references NOTHING — makes every reader
   (including the tap thread) see the fresh object and the check pass.
   A mainline spawn's most relevant side effect is
   `clone_for_thread_excluding`'s `self.env = self.env.flattened()` on
   the PARENT env, so the prime suspect is scoped-env overlay staleness:
   the rebind lands in an overlay tier, and some captured-env clone or
   merge path (used by the tap-dispatch chain but not by direct mainline
   calls) resolves `encoder` from a stale base tier until a flatten
   collapses the overlay. Start the gdb session by checking
   `self.env.is_scoped()` at the block-3 closure creations and at the
   tap-dispatch merge.
2. **`http2-request-parser.rakutest` test 49** ("check 4" of
   'Header1 + Header2 + Data1 + Data2'): root-caused 2026-08-11 — NOT a
   capture bug and NOT expected to be fixed by slice 2. A nested
   `whenever` registered inside a `whenever` body makes a later sibling
   event's `%streams{...}` write resolve to a stale forked container,
   clobbering the supply block's hash; DATA frames are then demux-dropped.
   Dependency-free 22-line repro + bisect matrix in
   `todo/deep/nested-whenever-registration-clobbers-sibling-event-aggregate-writes.md`.
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
