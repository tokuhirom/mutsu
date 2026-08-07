# TRIAGE — prioritized snapshot of todo/ (2026-08-07)

A ranked index of every open finding under `todo/tickets/` and `todo/deep/`,
so a session can pick the next ticket without re-reading all of them.

This is a **snapshot, not a ledger**. Resolving a ticket does *not* require
editing this file — that would reintroduce exactly the shared-file merge
conflicts `todo/` exists to avoid. A stale row is fine; the per-ticket files
stay the source of truth. Regenerate the whole file when it has drifted too
far (re-survey every ticket, re-score, rewrite).

Surveyed 2026-08-07: **77 files** (26 `deep/`, 51 `tickets/`). Previous
survey (2026-08-05, 85 files) has seen heavy churn since: roughly 20 tickets
resolved and moved to `news/`, several new ones filed from the ADR-0019 C6e
(legacy-body removal) and for-loop-shadow campaigns.

## How the ranking works

Each ticket gets two independent scores, both shown in the tables:

- **Axis** — which project goal the fix advances. Weights follow PLAN.md:
  - `batteries §1` — bundled libraries, Cro, dist compat, mzef. The main effort; highest weight.
  - `Test-vendor §1` — the vendor-real-`Test` campaign specifically (a batteries item large enough to be its own axis).
  - `soundness` — SEGV, lost updates, silently-swallowed exceptions, memory unsafety. Always high weight.
  - `correctness §6` — wrong answers or missing diagnostics that roast does not see.
  - `concurrency §5` — supply/whenever/thread semantics.
  - `errors §5` — error-message and backtrace quality.
  - `perf §4` — de-prioritized polish; mutsu already beats raku almost everywhere.
  - `roast §3` — mined out; lowest weight.
  - `record` — a decision/measurement record, not actionable work.
- **Effort** — implementation size: **S** (narrow, one session), **M** (one
  session, several subsystems), **L** (multi-session), **XL** (needs an
  ADR/design pass first).

**Tier = axis weight × measured impact.** Effort is displayed but does not
lower a tier — an XL item with P1 impact stays P1, with the ADR as its first
deliverable. Items whose *sound* fix is blocked on a mechanism that does not
exist yet (array/hash element cells, cell-based closure capture) drop to
Icebox regardless of impact, so nobody burns a session re-discovering the
blocker. ⚡ marks S-effort quick wins.

Tiers:

- **P1 — now.** Directly advances a PLAN §1 goal or fixes a crash-class bug, and is actionable today.
- **P2 — next.** Concrete blocked tests/dists or wrong-answer bugs; pick when P1 is saturated.
- **P3 — later.** Cosmetic, low-leverage, or perf polish. Batch the ⚡ ones.
- **Icebox.** Blocked on a design campaign or an explicit user decision. Do not start ad hoc.

## P1 — now

Two campaigns dominate this tier; most P1 rows are named blockers of one of
them, so progress compounds.

### Campaign: Cro (the web-framework battery slot, PLAN §1 B1)

| Ticket | Axis | Effort | Why here |
|---|---|---|---|
| [async-listener-not-freed-when-relistening-in-a-loop](tickets/async-listener-not-freed-when-relistening-in-a-loop.md) | batteries §1 | L | Third+ round of re-binding a port to a Cro server gets empty bodies; blocks the whole multi-server auth/session/log-file suite family. |
| [cro-middleware-await-body-text-dies-coercing-any-into-promise](tickets/cro-middleware-await-body-text-dies-coercing-any-into-promise.md) | batteries §1 | M | Last remaining, deterministic, isolated failure in `http-middleware.rakutest`. |
| [pair-namedness-is-a-value-property-not-a-call-site-property](deep/pair-namedness-is-a-value-property-not-a-call-site-property.md) | correctness §6 | XL | Breaks real Cro client usage (`headers => [...]` dies where raku returns 200). 32 consumer sites; first deliverable is the ADR choosing call-site-mask vs value-default. |
| [for-multi-param-array-hash-shadow-clobbers-outer-container](tickets/for-multi-param-array-hash-shadow-clobbers-outer-container.md) | correctness §6 | M | The `@`/`%`-sigil sibling of the fixed scalar shadow-clobber bug (same Cro `for @c.kv -> $i, $comp` family) — silent in-place corruption of an outer container. |

### Campaign: vendor the real `Test` module (PLAN §1, batteries policy)

| Ticket | Axis | Effort | Why here |
|---|---|---|---|
| [vendor-real-test-module](tickets/vendor-real-test-module.md) | Test-vendor §1 | XL | The campaign header: regression count driven from 343 down to a handful of remaining files. Read this before any row below. |
| [interpreter-call-path-in-hot-loops](deep/interpreter-call-path-in-hot-loops.md) | perf §4 | L | The one perf axis where mutsu loses to raku, and the real blocker for the flip: real-`Test` inflates heavy roast files past the 30s budget (`state.t` 67× deficit traced here). Attack row B (file-scope sub call) next. |
| [use-fatal-leaks-out-of-a-sub-or-do-block](tickets/use-fatal-leaks-out-of-a-sub-or-do-block.md) | Test-vendor §1 | M | `use fatal` leaks out of `sub`/`do{}`/closures and poisons the next assertion under real Test. |
| [use-inside-a-block-leaks-to-the-enclosing-scope](tickets/use-inside-a-block-leaks-to-the-enclosing-scope.md) | Test-vendor §1 | M | Remaining env half of import scoping; defeats selective imports in real-Test roast files. |
| [cache-on-a-lazy-seq-must-not-answer-seq](deep/cache-on-a-lazy-seq-must-not-answer-seq.md) | soundness | M | Crash-class: real `is-deeply(Seq,Seq)` recurses to a stack-overflow abort because `.cache` still answers `Seq`. |
| [deferred-seq-materialization-destroys-the-original](deep/deferred-seq-materialization-destroys-the-original.md) | correctness §6 | M | Even `.defined` guts a deferred Seq; breaks any `is $fh.lines, <A B C>` under the real module. |

### Soundness: blocks the legacy_body-removal architecture goal (ADR-0019 C6e)

| Ticket | Axis | Effort | Why here |
|---|---|---|---|
| [compiled-fns-default-breaks-nested-subs-outside-methods](deep/compiled-fns-default-breaks-nested-subs-outside-methods.md) | soundness | L | ~17 call sites pass an empty `CompiledFns::default()` instead of the real table — nested-sub declarations silently no-op once the `legacy_body` tree-walk fallback is dropped. Confirmed live reproducer; explicitly "do NOT re-attempt dropping legacy_body before this lands." |

### Standalone quick wins (ordinary-code wrong answers, S effort)

| Ticket | Axis | Effort | Why here |
|---|---|---|---|
| [metaop-over-range-base-is-unsupported](tickets/metaop-over-range-base-is-unsupported.md) ⚡ | correctness §6 | S | `Z..`/`X..` parse fine but the metaop runtime handler has no Range entry — ordinary Raku silently unusable. |
| [named-capture-absent-from-current-match-leaks-stale-value](tickets/named-capture-absent-from-current-match-leaks-stale-value.md) | correctness §6 | S-M | `$<name>` absent from the *current* match leaks a stale value from an earlier match instead of Nil — silent wrong data for any `.defined`-branching code. |
| [retire-native-test-util-overrides](tickets/retire-native-test-util-overrides.md) ⚡ | Test-vendor §1 | S | Mechanical: add missing `use Test::Util` to `t/` callers, then delete the dead native handlers. |

## P2 — next

| Ticket | Axis | Effort | Why here |
|---|---|---|---|
| [yamlish-grammar-layer](tickets/yamlish-grammar-layer.md) | batteries §1 | L | YAML battery candidate fails deep in its own grammar; 5 upstream test files with partial pass rates. |
| [template-engines-blocked-on-mutsu](deep/template-engines-blocked-on-mutsu.md) | batteries §1 | L | Cluster survey; Mustache (chosen slot) now 11/13, Jinja2 load-blocker fixed. Template6 is the next natural pick. |
| [template-mojo-residual-failures](tickets/template-mojo-residual-failures.md) ⚡ | batteries §1 | S | Nearly resolved; the one open item is really a pointer to the sigspace ticket below. |
| [rule-sigspace-does-not-consume-trailing-whitespace](tickets/rule-sigspace-does-not-consume-trailing-whitespace.md) | correctness §6 | S-M | `rule`/`:sigspace` doesn't insert the implicit `<.ws>` after the last atom — blocks Template::Mojo's `03-capture.rakutest` layer 2. |
| [nativehelpers-blob-moarvm-guts](deep/nativehelpers-blob-moarvm-guts.md) | batteries §1 | L | Gates the database battery slot (DBIish/DB::SQLite). Design settled (ADR-0015 Accepted); P3b/P3c execution remains. |
| [cold-supply-whenever-source-replayed-not-tapped](deep/cold-supply-whenever-source-replayed-not-tapped.md) | batteries §1 | L | Last known blocker for the Test::Scheduler dist (T-037): a cold supply must be tapped, not replayed. |
| [forward-captured-code-var-snapshot](tickets/forward-captured-code-var-snapshot.md) | batteries §1 | M | Last blocker for a full CBOR::Simple `cbor-decode` round-trip. |
| [dist-test-suite-failures-batch](tickets/dist-test-suite-failures-batch.md) | batteries §1 | XL | A triage *queue* — several root causes already pulled out into their own deep/ tickets below; remainder: Math::Interval, Native::Overflow, App::SudokuHelper, P5tie, Mathematica::Serializer::Encoder, Hash::Restricted, Crypt::RC4, Random::Choice. |
| [listops-are-not-real-multi-subs](deep/listops-are-not-real-multi-subs.md) | correctness §6 | XL | `splice`/`push`/etc. are special-cased opcodes, not real multi-subs — a user/module `multi` for these names is unreachable. Blocks String::Splice entirely. |
| [sigilless-constant-invisible-in-nested-sub-inside-module](tickets/sigilless-constant-invisible-in-nested-sub-inside-module.md) | correctness §6 | L | `constant \NAME` inside non-unit `module`/`package` invisible to a nested `sub`; blocks all 16 subtests of the RSV dist. Root cause fully traced, needs a design choice. |
| [bare-block-as-infix-operand-not-recognized](deep/bare-block-as-infix-operand-not-recognized.md) | correctness §6 | L | A leading `{ ... }` before an infix never looks ahead to see if it should be a term — blocks PSpec dist's `xxx` custom-operator idiom. |
| [user-postcircumfix-index-not-dispatched-for-instances](deep/user-postcircumfix-index-not-dispatched-for-instances.md) | correctness §6 | XL | A user `multi sub postcircumfix:<[ ]>` is never consulted for `@obj[...]` — real, general, spec'd operator-overload gap; blocks Array::Rounded (16/35 failing). |
| [promise-spawn-segv-under-load](deep/promise-spawn-segv-under-load.md) | soundness | L | Reproducible SEGV (guard-page stack overflow on `Promise.start` threads, ~6-8% under contention); fix direction is a uniform spawned-thread stack budget. |
| [supply-lines-drops-channel-backed-supplies](tickets/supply-lines-drops-channel-backed-supplies.md) | concurrency §5 | M | `.lines` on a real-socket Supply silently emits nothing — the most natural socket idiom. |
| [head-on-a-channel-backed-supply-drops-every-value](tickets/head-on-a-channel-backed-supply-drops-every-value.md) | concurrency §5 | M | Same family: every combinator through `make_supply_from_values` drops channel-backed sources; `.head` is the repro. |
| [procasync-stdout-is-not-incremental](tickets/procasync-stdout-is-not-incremental.md) | concurrency §5 | M | Output only arrives at child exit → parent/child handshakes deadlock; the streaming-reader shape already exists for sockets. |
| [supply-block-lexical-leaks-through-thread-lane](tickets/supply-block-lexical-leaks-through-thread-lane.md) | concurrency §5 | M | Residual cross-thread half of a mostly-fixed lexical-privacy bug; the needed info (`authoritative_captures`) already exists. |
| [supply-block-scalar-lexical-invisible-to-last-phaser](tickets/supply-block-scalar-lexical-invisible-to-last-phaser.md) | concurrency §5 | M | `LAST` reads a stale accumulator written from `whenever` — breaks the fold-and-emit idiom; same writeback-drain machinery as the row above. |
| [module-file-scope-array-and-hash-still-share-the-caller](tickets/module-file-scope-array-and-hash-still-share-the-caller.md) | Test-vendor §1 | L | Sibling of a fixed scalar bug: a module's file-scope `@`/`%` still shares the caller's env key. Costs a whole roast integration file. |
| [local-tests-rely-on-a-lenient-native-is](tickets/local-tests-rely-on-a-lenient-native-is.md) | Test-vendor §1 | M | Six remaining `t/` files in the "raku fails it too" bucket, each an independent triage. |
| [callframe-line-and-file-come-from-different-frames](tickets/callframe-line-and-file-come-from-different-frames.md) | errors §5 | M | Failure locations under the real `Test` point into unrelated frames — affects `throws-like` reporting in at least 4 sweep files. |
| [eval-context-frame-owns-the-return-target](deep/eval-context-frame-owns-the-return-target.md) | Test-vendor §1 | M | Real `throws-like '<code with return>'` reports "did not die"; three coordinated changes, all specified. |
| [sinking-a-try-blocks-discarded-value-escapes-the-try](tickets/sinking-a-try-blocks-discarded-value-escapes-the-try.md) | Test-vendor §1 | L | Aborts `roast/integration/advent2009-day20.t` after 11/21 assertions under real Test; two independent wrongs. |
| [exception-class-hierarchy-is-mostly-unregistered](deep/exception-class-hierarchy-is-mostly-unregistered.md) | correctness §6 | XL | 124 unregistered `X::` classes; mutsu's own compiler emits one of them. Needs the role-vs-prefix parentage design first. |
| [expression-position-my-has-no-scope](tickets/expression-position-my-has-no-scope.md) | correctness §6 | L | Expression-position `my` has no scope at all (silent lexical leak); one roast test currently passes *because* of the bug. |
| [multi-candidates-declaration-order](tickets/multi-candidates-declaration-order.md) | correctness §6 | M | `&foo.candidates` order is hash-bucket-dependent, not declaration order — can dispatch the wrong candidate. Reader-side sort is trivial but may be cheaper after ADR-0019 phase C/D. |
| [parameter-objects-have-no-stable-identity](tickets/parameter-objects-have-no-stable-identity.md) | correctness §6 | M | `Signature.params` builds a fresh `Parameter` every access; the Cro-blocking case already shipped via a narrower replay mechanism, this is the honest cached-Parameter version. |

## P3 — later

| Ticket | Axis | Effort | Why here |
|---|---|---|---|
| [wasm-start-and-channel-trap](deep/wasm-start-and-channel-trap.md) | batteries §1 | M | Two tutorial-site lessons; small mechanism but the synchronous-`start` semantics need thought. |
| [http-server-tiny-async-serving-remainder](tickets/http-server-tiny-async-serving-remainder.md) | concurrency §5 | L | Humming-Bird is no longer the web target; the general whenever/control-flow bugs it names are tracked in the concurrency family above. |
| [digest-dist-blockers](tickets/digest-dist-blockers.md) | batteries §1 | M | Dist already bundled and ~90% of this file is struck-through "FIXED"; residue is wide-buffer bit accessors and a `with`-modifier placeholder gap. Candidate to trim/archive (see Housekeeping). |
| [nativecall-surface-gaps](tickets/nativecall-surface-gaps.md) ⚡ | batteries §1 | S | Only the `NativeCall::Types::` prefix naming remains open; duplicate of the row below. |
| [nativecall-pointer-short-name](tickets/nativecall-pointer-short-name.md) | batteries §1 | M | Cosmetic `.^name`; must be one deliberate slice with the row above (candidate merge — see Housekeeping). |
| [procasync-stress-segv](deep/procasync-stress-segv.md) | soundness | L | Real memory unsafety but ~1-in-dozens CI-only, no local repro; *monitor* — the crash reporter now uploads artifacts, wait for the next occurrence. |
| [state-write-through-is-skipped-in-a-jit-compiled-range](tickets/state-write-through-is-skipped-in-a-jit-compiled-range.md) | soundness | M | No deterministic repro today; the `state_vars` rekey half is worth doing on its own merits. |
| [closure-capture-shadowed-by-colliding-callee-parameter](deep/closure-capture-shadowed-by-colliding-callee-parameter.md) | correctness §6 | L | Real trap (three ingredients needed) but two narrow fixes already regressed — belongs to the env-layering cluster, do not poke at it narrowly. |
| [stored-regex-loses-its-defining-scope-lexicals](tickets/stored-regex-loses-its-defining-scope-lexicals.md) | correctness §6 | L | Two hard divergences, nothing measured blocked today. |
| [code-var-mention-remakes-the-sub](tickets/code-var-mention-remakes-the-sub.md) | correctness §6 | L | `&f.WHICH` unstable; entangled with `wrap_chains` identity — decide where the canonical Sub lives first. |
| [duplicated-prefix-question-mark](tickets/duplicated-prefix-question-mark.md) | roast §3 | M | Single roast test divergence; needs the `Z??`/`X??` CannotMeta sorrow to become the primary diagnosis first. |
| [repeat-call-loses-backtrace-frame](tickets/repeat-call-loses-backtrace-frame.md) | errors §5 | L | Second call loses its frame; wants `RoutineFrame` symbol-interning first (hot path). |
| [module-parse-warning-reported-twice](tickets/module-parse-warning-reported-twice.md) | errors §5 | M | Cosmetic duplicate warning with wrong attribution; fix needs new origin-tracking plumbing. |
| [bare-precedes-placeholder-nested-block](tickets/bare-precedes-placeholder-nested-block.md) | errors §5 | M | False-negative diagnostic only, no miscompile; re-express on the existing placeholder collectors. |
| [two-terms-in-a-row-is-not-a-parse-error](tickets/two-terms-in-a-row-is-not-a-parse-error.md) | errors §5 | M | Missing diagnostic; per-site guard-list re-decisions, and a wrong guard *rejects valid programs* — full roast as review. |
| [test-assertion-trait-is-not-introspectable](deep/test-assertion-trait-is-not-introspectable.md) | Test-vendor §1 | L | Only costs wrong line numbers in failure output; three coupled mechanisms (trait resolution ordering, `.^mixin`, backtrace walk). |
| [our-var-and-its-package-name-are-two-slots](tickets/our-var-and-its-package-name-are-two-slots.md) | roast §3 | L | One roast test; the sound fix is a shared cell (container-representation family) — near-Icebox, listed here because the repro is tiny. |
| [remaining-language-feature-gaps](tickets/remaining-language-feature-gaps.md) | correctness §6 | mixed | A container: multi-line feeds (S) and `exits-ok` (S) are pickable; the typed-exception rows need scope analysis (L each). |
| [bare-package-symbolic-deref-and-stash-routines](tickets/bare-package-symbolic-deref-and-stash-routines.md) | roast §3 | M | `pseudo-6e.t` only; needs a semantics decision (SymbolicDeref vs stash-index) first. |
| [typed-buf-native-interop-holes](tickets/typed-buf-native-interop-holes.md) ⚡ | correctness §6 | S | Items 2-4 already fixed; item 1 doesn't currently reproduce — low-value residue, candidate to close (see Housekeeping). |
| [magic-vars-should-be-built-lazily](tickets/magic-vars-should-be-built-lazily.md) | perf §4 | M | Startup metric polish; slice 1 done, profile before designing slice 2. |
| [bench-ctor-construction-parity](tickets/bench-ctor-construction-parity.md) | perf §4 | L | The only bench where mutsu is slower (1.17-1.35×); remaining slices lean on the closure-env-capture-cost Icebox item. |
| [digest-ripemd-start-per-block-overhead](tickets/digest-ripemd-start-per-block-overhead.md) | perf §4 | L | `t/ripemd.t` 295s→119s after major perf work but still exceeds the 120s CI gate margin; profile is now flat, needs one more diminishing-return lever. |
| [yaml-parse-throughput](tickets/yaml-parse-throughput.md) | perf §4 | XL | Correct (81/81) but ~5× raku; next round is structural (ADR-0016 P2/P5), not another call site. |
| [adr0016-p5-match-consumer-inventory](deep/adr0016-p5-match-consumer-inventory.md) | perf §4 | L | The 72-site inventory that gates lazy `Match` (feeds the row above). |
| [c6d-interpreter-body-sites-are-mostly-token-bodies](deep/c6d-interpreter-body-sites-are-mostly-token-bodies.md) | perf §4 | L | Nearly complete: most sub-items landed; remaining scope is grammar token/rule bodies (belongs to ADR-0009/Phase D). |

## Icebox — blocked on a design campaign or an explicit decision

| Ticket | Axis | Blocked on |
|---|---|---|
| [needs-env-sync-blanket-removal](deep/needs-env-sync-blanket-removal.md) | perf §4 | Explicitly a fused campaign (lexical-slot + per-slot precision); a narrow probe deterministically broke four pinned mechanisms. De-prioritized 2026-07. |
| [captured-outer-pair-container-alias](deep/captured-outer-pair-container-alias.md) | correctness §6 | ADR-0001 element-cell / container-representation mechanism. |
| [subscript-p-pair-is-a-snapshot-not-a-container](deep/subscript-p-pair-is-a-snapshot-not-a-container.md) | correctness §6 | Same: needs an `array_element_cell` API (ADR-0001); the tempting locals-scan patch is explicitly wrong. |
| [inline-start-blocks-clobber-a-later-declared-variable](tickets/inline-start-blocks-clobber-a-later-declared-variable.md) | correctness §6 | Cell-based capture work (write back only what the thread mutated); no call-site special case allowed. |
| [otf-compilation-gate-leftovers](tickets/otf-compilation-gate-leftovers.md) | perf §4 | Per-call capture cells / caller-slot mechanism; "just remove the gate" frontier is exhausted. |
| [closure-env-capture-cost](deep/closure-env-capture-cost.md) | perf §4 | Two-tier capture + epoch design; belongs with the Slice F env work. Cheap shapes are ruled out as unsound. |
| [cue-loop-lexical-shared-lane-residue](tickets/cue-loop-lexical-shared-lane-residue.md) | concurrency §5 | ADR-0010/Track-B-adjacent per-binding cell mechanism; a loop-redeclared lexical mutated inside a `.cue` callback retains the previous iteration's value. |
| [bundle-json-tiny-instead-of-emulating](tickets/bundle-json-tiny-instead-of-emulating.md) | batteries §1 | A deliberate decision: real JSON::Tiny is >1000× slower on zef's metadata path; JSON::Fast needs 42 `nqp::` ops. Ask the user before moving. |
| [rakuast-remaining](deep/rakuast-remaining.md) | correctness §6 | Multi-campaign backlog (ADR-0011); pick slices by user impact, not cadence. |
| [nativecall-cannot-be-vendored](deep/nativecall-cannot-be-vendored.md) | record | Not actionable — a measurement record with explicit reopen conditions. Keep. |

## Housekeeping

- Resolved and moved to `news/` since the 2026-08-05 survey (partial list):
  `package-short-name-alias-is-global` (PR #6019, importer-scoped aliasing),
  `schedule-on-whenever-env-loss`, `done-in-a-whenever-body-does-not-stop-later-emits`,
  `for-multi-param-shadow-clobbers-outer-lexical` (scalar half — array/hash
  sibling survives as `for-multi-param-array-hash-shadow-clobbers-outer-container`),
  `shared-worker-pool-adr` (ADR-0020 complete), `miri-gate-for-adr-0013`,
  `compound-assign-as-call-argument-yields-pair`, `code-lexical-does-not-shadow-a-builtin`,
  `say-swallows-an-exception-from-gist`, `for-loop-multi-param-types-unenforced`,
  `literal-parameters-are-not-enforced-at-bind`, `duckmap-does-not-itemize-a-nested-descend`,
  `labelled-bare-block-is-not-a-loop-construct`, `compile-errors-that-name-no-exception-class`,
  `object-hash-raku-does-not-parenthesise-keys`, `cas-on-scalar-attribute-loses-updates`.
- Container tickets that are queues, not single fixes:
  [dist-test-suite-failures-batch](tickets/dist-test-suite-failures-batch.md),
  [remaining-language-feature-gaps](tickets/remaining-language-feature-gaps.md),
  [digest-dist-blockers](tickets/digest-dist-blockers.md). Pull one row out,
  fix it as its own PR, and note it in the container file only if the row list
  changes.
- Near-resolved residue files worth a trim/close pass in a future session
  (not done here — this is an index regen, not a cleanup pass):
  `digest-dist-blockers.md` (~90% struck-through FIXED sections),
  `typed-buf-native-interop-holes.md` (item 1 no longer reproduces),
  `template-mojo-residual-failures.md` (only open item duplicates
  `rule-sigspace-does-not-consume-trailing-whitespace.md`),
  `c6d-interpreter-body-sites-are-mostly-token-bodies.md` (only C6d-2 and a
  Phase-D handoff remain), `nativecall-surface-gaps.md` /
  `nativecall-pointer-short-name.md` (same open item, tracked twice).
