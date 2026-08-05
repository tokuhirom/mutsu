# TRIAGE — prioritized snapshot of todo/ (2026-08-05)

A ranked index of every open finding under `todo/tickets/` and `todo/deep/`,
so a session can pick the next ticket without re-reading all of them.

This is a **snapshot, not a ledger**. Resolving a ticket does *not* require
editing this file — that would reintroduce exactly the shared-file merge
conflicts `todo/` exists to avoid. A stale row is fine; the per-ticket files
stay the source of truth. Regenerate the whole file when it has drifted too
far (re-survey every ticket, re-score, rewrite).

Surveyed 2026-08-05: **85 files** (23 `deep/`, 62 `tickets/`), one of which is
already resolved (see Housekeeping).

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
| [package-short-name-alias-is-global](tickets/package-short-name-alias-is-global.md) | batteries §1 | XL | The live blocker for Cro's Response/RequestParser (`http-response-parser` 129/154, `http-request-parser` 93/108). Wants a design pass first — that pass is the deliverable. |
| [schedule-on-whenever-env-loss](tickets/schedule-on-whenever-env-loss.md) | batteries §1 | M | Same Cro parser files: ~25 failures + duplicate-run TAP via `.schedule-on` losing the supply block's lexicals. |
| [async-listener-not-freed-when-relistening-in-a-loop](tickets/async-listener-not-freed-when-relistening-in-a-loop.md) | batteries §1 | L | Empty bodies from round 3 of re-binding a port; blocks the Cro middleware/session/auth suite family. |
| [cro-middleware-await-body-text-dies-coercing-any-into-promise](tickets/cro-middleware-await-body-text-dies-coercing-any-into-promise.md) | batteries §1 | M | The single remaining failure in `http-middleware.rakutest`; deterministic and isolated. |
| [for-multi-param-shadow-clobbers-outer-lexical](tickets/for-multi-param-shadow-clobbers-outer-lexical.md) | correctness §6 | M | Multi-param `for` clobbers same-named outer lexicals and leaks cross-frame — hit by Cro's own `for @c.kv -> $i, $comp`, and easy to hit in any ordinary code. |
| [done-in-a-whenever-body-does-not-stop-later-emits](tickets/done-in-a-whenever-body-does-not-stop-later-emits.md) ⚡ | concurrency §5 | S | Side effects keep firing after `done`; the missing half is already specified (close `upstream_taps`). |
| [pair-namedness-is-a-value-property-not-a-call-site-property](deep/pair-namedness-is-a-value-property-not-a-call-site-property.md) | correctness §6 | XL | Breaks real Cro client usage (`headers => [...]` dies where raku returns 200). 32 consumer sites; first deliverable is the ADR choosing call-site-mask vs value-default. |

### Campaign: vendor the real `Test` module (PLAN §1, batteries policy)

| Ticket | Axis | Effort | Why here |
|---|---|---|---|
| [vendor-real-test-module](tickets/vendor-real-test-module.md) | Test-vendor §1 | XL | The campaign header: 113 roast files still regress under `MUTSU_REAL_TEST=1` (down from 343). Read this before any row below. |
| [interpreter-call-path-in-hot-loops](deep/interpreter-call-path-in-hot-loops.md) | perf §4 | L | The one perf axis where mutsu loses to raku, and the real blocker for the flip: real-`Test` inflates heavy roast files past the 30 s budget (`state.t` 67× deficit). Attack row B (file-scope sub call), not the declaration site. |
| [use-fatal-leaks-out-of-a-sub-or-do-block](tickets/use-fatal-leaks-out-of-a-sub-or-do-block.md) | Test-vendor §1 | M | Open systemic cause #3 of the campaign: the pragma escapes `throws-like { use fatal; … }` and poisons the next assertion. |
| [use-inside-a-block-leaks-to-the-enclosing-scope](tickets/use-inside-a-block-leaks-to-the-enclosing-scope.md) | Test-vendor §1 | M | Remaining env half defeats selective imports (`S32-list/skip.t` aborts under the real module). |
| [cache-on-a-lazy-seq-must-not-answer-seq](deep/cache-on-a-lazy-seq-must-not-answer-seq.md) | soundness | M | Crash-class: real `is-deeply(Seq,Seq)` recurses to a stack-overflow abort because `.cache` still answers `Seq`. |
| [deferred-seq-materialization-destroys-the-original](deep/deferred-seq-materialization-destroys-the-original.md) | correctness §6 | M | Even `.defined` guts a deferred Seq; breaks any `is $fh.lines, <A B C>` under the real module. |
| [miri-gate-for-adr-0013](tickets/miri-gate-for-adr-0013.md) ⚡ | soundness | S | PLAN §2 names it; the startup blocker is gone, so it is now a small CI job + one stale header rewrite. |

### Standalone quick wins (ordinary-code wrong answers, S effort)

| Ticket | Axis | Effort | Why here |
|---|---|---|---|
| [compound-assign-as-call-argument-yields-pair](tickets/compound-assign-as-call-argument-yields-pair.md) ⚡ | correctness §6 | S | `@r.push($x += 5)` pushes the Pair `x => 5` — silent wrong data in ordinary code; localized to one argument-evaluation arm. |
| [code-lexical-does-not-shadow-a-builtin](tickets/code-lexical-does-not-shadow-a-builtin.md) ⚡ | correctness §6 | S | A lexical `&emit` loses to the control-flow builtin, which hijacks the enclosing routine; the governing rule already exists for qualified calls. |

## P2 — next

| Ticket | Axis | Effort | Why here |
|---|---|---|---|
| [yamlish-grammar-layer](tickets/yamlish-grammar-layer.md) | batteries §1 | L | The YAML battery candidate fails in its own grammar; reduce `basic.rakutest` (0/7) first, Template::Mojo-style. |
| [template-engines-blocked-on-mutsu](deep/template-engines-blocked-on-mutsu.md) | batteries §1 | L | Mustache (the chosen slot) is fixed; Template6 0/12 is the named next row of the cluster. |
| [template-mojo-residual-failures](tickets/template-mojo-residual-failures.md) ⚡ | batteries §1 | S | Two narrow, already-localized bugs; 3 assertions across 2 files from parity. |
| [nativehelpers-blob-moarvm-guts](deep/nativehelpers-blob-moarvm-guts.md) | batteries §1 | L | Gates the database battery slot (DBIish/DB::SQLite via NativeLibs). Design settled (ADR-0015); remaining is P3b/P3c execution + the Buf half. |
| [cold-supply-whenever-source-replayed-not-tapped](deep/cold-supply-whenever-source-replayed-not-tapped.md) | batteries §1 | L | Last known blocker for the Test::Scheduler dist (T-037): a cold supply must be tapped, not replayed. |
| [forward-captured-code-var-snapshot](tickets/forward-captured-code-var-snapshot.md) | batteries §1 | M | Last blocker for a full CBOR::Simple round-trip (`cbor-decode` on any input). |
| [dist-test-suite-failures-batch](tickets/dist-test-suite-failures-batch.md) | batteries §1 | XL | A triage *queue* (~17 root causes), not one fix — pull individual rows out of it, raku-baseline first. |
| [promise-spawn-segv-under-load](deep/promise-spawn-segv-under-load.md) | soundness | L | Reproducible SEGV (guard-page stack overflow on `Promise.start` threads, ~6-8 % under contention); fix direction is a uniform spawned-thread stack budget. |
| [cas-on-scalar-attribute-loses-updates](tickets/cas-on-scalar-attribute-loses-updates.md) | soundness | M | CAS retry loops lose ~25 % of updates in debug builds, masked by whitelisted roast files passing in release. |
| [say-swallows-an-exception-from-gist](tickets/say-swallows-an-exception-from-gist.md) | soundness | M | `say` eats a genuine user exception from `.gist` (program exits 0 instead of dying). Blast radius = everywhere; wants its own PR + full roast. |
| [typed-buf-native-interop-holes](tickets/typed-buf-native-interop-holes.md) ⚡ | correctness §6 | S | Four narrow one-liners, two of them silent-corruption class (wrong-shape buffer, NULL to C). |
| [supply-lines-drops-channel-backed-supplies](tickets/supply-lines-drops-channel-backed-supplies.md) | concurrency §5 | M | `.lines` on a real-socket Supply silently emits nothing — the most natural socket idiom. |
| [head-on-a-channel-backed-supply-drops-every-value](tickets/head-on-a-channel-backed-supply-drops-every-value.md) | concurrency §5 | M | Same family: every combinator through `make_supply_from_values` drops channel-backed sources; `.head` is the repro. |
| [procasync-stdout-is-not-incremental](tickets/procasync-stdout-is-not-incremental.md) | concurrency §5 | M | Output only arrives at child exit → parent/child handshakes deadlock; the streaming-reader shape already exists for sockets. |
| [supply-block-lexical-leaks-through-thread-lane](tickets/supply-block-lexical-leaks-through-thread-lane.md) | concurrency §5 | M | Residual writeback path clobbers the caller's same-named lexical; the needed info (`authoritative_captures`) already exists. |
| [supply-block-scalar-lexical-invisible-to-last-phaser](tickets/supply-block-scalar-lexical-invisible-to-last-phaser.md) | concurrency §5 | M | `LAST` reads a stale `my $sum` — breaks the fold-and-emit idiom; remaining half of #5704, same machinery as the row above. |
| [shared-worker-pool-adr](deep/shared-worker-pool-adr.md) | concurrency §5 | XL | PLAN §5 explicitly asks for this Proposed ADR (thread-per-task → pool; the `await` fork is the core question). The ADR itself is the session-sized deliverable. |
| [module-file-scope-array-and-hash-still-share-the-caller](tickets/module-file-scope-array-and-hash-still-share-the-caller.md) | Test-vendor §1 | L | The last identified `@`/`%` blocker of the vendor campaign; costs a whole roast integration file. Cheapest after the chokepoint work. |
| [local-tests-rely-on-a-lenient-native-is](tickets/local-tests-rely-on-a-lenient-native-is.md) | Test-vendor §1 | M | Six remaining `t/` files in the "raku fails it too" bucket, each an independent triage. |
| [callframe-line-and-file-come-from-different-frames](tickets/callframe-line-and-file-come-from-different-frames.md) | errors §5 | M | Failure locations under the real `Test` point into `Test.rakumod` (line 666 in a 106-line file). |
| [eval-context-frame-owns-the-return-target](deep/eval-context-frame-owns-the-return-target.md) | Test-vendor §1 | M | Real `throws-like '<code with return>'` reports "did not die"; three coordinated changes, all specified. |
| [sinking-a-try-blocks-discarded-value-escapes-the-try](tickets/sinking-a-try-blocks-discarded-value-escapes-the-try.md) | Test-vendor §1 | L | Costs 10 assertions in a roast integration file; two independent wrongs, sink-rule research needed first. |
| [exception-class-hierarchy-is-mostly-unregistered](deep/exception-class-hierarchy-is-mostly-unregistered.md) | correctness §6 | XL | 124 unregistered `X::` classes; mutsu's own compiler emits one of them. Needs the role-vs-prefix parentage design first — the naive rule is wrong more often than right. |
| [expression-position-my-has-no-scope](tickets/expression-position-my-has-no-scope.md) | correctness §6 | L | Expression-position `my` has no scope at all (silent lexical leak); one roast test currently passes *because* of the bug. |
| [for-loop-multi-param-types-unenforced](tickets/for-loop-multi-param-types-unenforced.md) ⚡ | correctness §6 | S | Fix shape fully specified in the ticket; pairs naturally with the P1 shadow-clobber sibling. |
| [retire-native-test-util-overrides](tickets/retire-native-test-util-overrides.md) ⚡ | Test-vendor §1 | S | Mechanical: add missing `use Test::Util` to `t/` callers, then delete the dead native handlers. |

## P3 — later

| Ticket | Axis | Effort | Why here |
|---|---|---|---|
| [wasm-start-and-channel-trap](deep/wasm-start-and-channel-trap.md) | batteries §1 | M | Two tutorial-site lessons; small mechanism but the synchronous-`start` semantics need thought. |
| [http-server-tiny-async-serving-remainder](tickets/http-server-tiny-async-serving-remainder.md) | batteries §1 | L | Humming-Bird is no longer the web target; the general bugs it names are tracked in the whenever/supply family above. |
| [digest-dist-blockers](tickets/digest-dist-blockers.md) | batteries §1 | M | Dist already bundled; residue is several independent narrow bugs (with-modifier placeholders, qualified proto, `&`-param vs builtin). |
| [nativecall-surface-gaps](tickets/nativecall-surface-gaps.md) ⚡ | batteries §1 | S | Only the `NativeCall::Types::` prefix naming remains here; bigger items live in ADR-0015. |
| [nativecall-pointer-short-name](tickets/nativecall-pointer-short-name.md) | batteries §1 | M | Cosmetic `.^name`; must be one deliberate slice with the row above. |
| [procasync-stress-segv](deep/procasync-stress-segv.md) | soundness | L | Real memory unsafety but ~1-in-dozens CI-only, no local repro; *monitor* — the crash reporter now uploads artifacts, wait for the next occurrence. |
| [state-write-through-is-skipped-in-a-jit-compiled-range](tickets/state-write-through-is-skipped-in-a-jit-compiled-range.md) | soundness | M | Latent residue with no deterministic repro; the `state_vars` rekey half is worth doing on its own merits. |
| [closure-capture-shadowed-by-colliding-callee-parameter](deep/closure-capture-shadowed-by-colliding-callee-parameter.md) | correctness §6 | L | Real trap (three ingredients needed) but two narrow fixes already regressed — belongs to the env-layering cluster, do not poke at it narrowly. |
| [stored-regex-loses-its-defining-scope-lexicals](tickets/stored-regex-loses-its-defining-scope-lexicals.md) | correctness §6 | L | Two hard divergences, nothing measured blocked today. |
| [code-var-mention-remakes-the-sub](tickets/code-var-mention-remakes-the-sub.md) | correctness §6 | L | `&f.WHICH` unstable; entangled with `wrap_chains` identity — decide where the canonical Sub lives first. |
| [literal-parameters-are-not-enforced-at-bind](tickets/literal-parameters-are-not-enforced-at-bind.md) | correctness §6 | M | Wrong behaviour but no measured blocker; fix should cover the whole bind-time-constraint family. |
| [multi-candidates-declaration-order](tickets/multi-candidates-declaration-order.md) | correctness §6 | M | Reader-side sort is trivial but trustworthy `decl_order` may be cheaper after ADR-0019 phase C/D. |
| [parameter-objects-have-no-stable-identity](tickets/parameter-objects-have-no-stable-identity.md) | correctness §6 | M | The Cro-blocking case already shipped via replay; this is the honest cached-Parameter version. |
| [duckmap-does-not-itemize-a-nested-descend](tickets/duckmap-does-not-itemize-a-nested-descend.md) ⚡ | correctness §6 | S | `deepmap` already has the fix (`itemize_result`); nothing blocked. |
| [metaop-over-range-base-is-unsupported](tickets/metaop-over-range-base-is-unsupported.md) ⚡ | correctness §6 | S | `Z..` is ordinary Raku and silently unusable; parse is already right. |
| [labelled-bare-block-is-not-a-loop-construct](tickets/labelled-bare-block-is-not-a-loop-construct.md) ⚡ | correctness §6 | S | mutsu is lenient where rakudo errors; check `t/` reliance in the same commit. |
| [compile-errors-that-name-no-exception-class](tickets/compile-errors-that-name-no-exception-class.md) ⚡ | errors §5 | S | 6 of 7 rows now pass; what is left is an attribute pass on `X::Temporal::OutOfRange`. |
| [two-terms-in-a-row-is-not-a-parse-error](tickets/two-terms-in-a-row-is-not-a-parse-error.md) | errors §5 | M | Missing diagnostic; per-site guard-list re-decisions, and a wrong guard *rejects valid programs* — full roast as review. |
| [bare-precedes-placeholder-nested-block](tickets/bare-precedes-placeholder-nested-block.md) | errors §5 | M | False-negative diagnostics only; re-express on the existing placeholder collectors. |
| [repeat-call-loses-backtrace-frame](tickets/repeat-call-loses-backtrace-frame.md) | errors §5 | L | Second call loses its frame; wants `RoutineFrame` symbol-interning first (hot path). |
| [module-parse-warning-reported-twice](tickets/module-parse-warning-reported-twice.md) | errors §5 | M | Cosmetic duplicate warning with wrong attribution. |
| [test-assertion-trait-is-not-introspectable](deep/test-assertion-trait-is-not-introspectable.md) | Test-vendor §1 | L | Only costs wrong line numbers in failure output; three coupled mechanisms (trait resolution ordering, `.^mixin`, backtrace walk). |
| [object-hash-raku-does-not-parenthesise-keys](tickets/object-hash-raku-does-not-parenthesise-keys.md) ⚡ | correctness §6 | S | Cosmetic `.raku` output; ticket itself says low priority. |
| [bare-package-symbolic-deref-and-stash-routines](tickets/bare-package-symbolic-deref-and-stash-routines.md) | roast §3 | M | `pseudo-6e.t` only; item 1 needs a semantics decision first. |
| [duplicated-prefix-question-mark](tickets/duplicated-prefix-question-mark.md) | roast §3 | M | `S03-operators/misc.t` tests 35/36; three upstream-scanner corrections, one already tried and reverted. |
| [our-var-and-its-package-name-are-two-slots](tickets/our-var-and-its-package-name-are-two-slots.md) | roast §3 | L | One roast test; the sound fix is a shared cell (container-representation family) — near-Icebox, listed here because the repro is tiny. |
| [remaining-language-feature-gaps](tickets/remaining-language-feature-gaps.md) | correctness §6 | mixed | A container: multi-line feeds (S) and `exits-ok` (S) are pickable; the typed-exception rows need scope analysis (L each). |
| [magic-vars-should-be-built-lazily](tickets/magic-vars-should-be-built-lazily.md) | perf §4 | M | Startup metric polish; slice 1 done, profile before designing slice 2. |
| [bench-ctor-construction-parity](tickets/bench-ctor-construction-parity.md) | perf §4 | L | The only bench where mutsu is slower (1.17-1.35×); remaining slices lean on Slice F / ADR-0016. |
| [digest-ripemd-start-per-block-overhead](tickets/digest-ripemd-start-per-block-overhead.md) | perf §4 | L | One un-whitelistable battery test file; the real lever is per-`start` cost → follows the worker-pool ADR. |
| [yaml-parse-throughput](tickets/yaml-parse-throughput.md) | perf §4 | XL | Correct but ~5× raku; next round is structural (ADR-0016 P2/P5), not another call site. |
| [adr0016-p5-match-consumer-inventory](deep/adr0016-p5-match-consumer-inventory.md) | perf §4 | L | The 72-site inventory that gates lazy `Match` (feeds the row above). |
| [c6d-interpreter-body-sites-are-mostly-token-bodies](deep/c6d-interpreter-body-sites-are-mostly-token-bodies.md) | perf §4 | L | A scoping correction for ADR-0019 C6d; 83 % of sites belong to the grammar execution model, not OTF. |

## Icebox — blocked on a design campaign or an explicit decision

| Ticket | Axis | Blocked on |
|---|---|---|
| [needs-env-sync-blanket-removal](deep/needs-env-sync-blanket-removal.md) | perf §4 | Explicitly a fused campaign (lexical-slot + per-slot precision); a narrow probe deterministically broke four pinned mechanisms. De-prioritized 2026-07. |
| [captured-outer-pair-container-alias](deep/captured-outer-pair-container-alias.md) | correctness §6 | ADR-0001 element-cell / container-representation mechanism. |
| [subscript-p-pair-is-a-snapshot-not-a-container](deep/subscript-p-pair-is-a-snapshot-not-a-container.md) | correctness §6 | Same: needs an `array_element_cell` API (ADR-0001); the tempting locals-scan patch is explicitly wrong. |
| [inline-start-blocks-clobber-a-later-declared-variable](tickets/inline-start-blocks-clobber-a-later-declared-variable.md) | correctness §6 | Cell-based capture work (write back only what the thread mutated); no call-site special case allowed. |
| [otf-compilation-gate-leftovers](tickets/otf-compilation-gate-leftovers.md) | perf §4 | Per-call capture cells / caller-slot mechanism; "just remove the gate" frontier is exhausted. |
| [closure-env-capture-cost](deep/closure-env-capture-cost.md) | perf §4 | Two-tier capture + epoch design; belongs with the Slice F env work. Cheap shapes are ruled out as unsound. |
| [bundle-json-tiny-instead-of-emulating](tickets/bundle-json-tiny-instead-of-emulating.md) | batteries §1 | A deliberate decision: real JSON::Tiny is >1000× slower on zef's metadata path; JSON::Fast needs 42 `nqp::` ops. Ask the user before moving. |
| [rakuast-remaining](deep/rakuast-remaining.md) | correctness §6 | Multi-campaign backlog (ADR-0011); pick slices by user impact, not cadence. |
| [nativecall-cannot-be-vendored](deep/nativecall-cannot-be-vendored.md) | record | Not actionable — a measurement record with explicit reopen conditions. Keep. |

## Housekeeping

- [whenever-owned-lexical-outlives-the-react-block](tickets/whenever-owned-lexical-outlives-the-react-block.md)
  is **Status: Resolved** (#5773/#5776) — per the lifecycle it should be
  `git mv`'d to `news/2026-08/` and rewritten as an accomplishment.
- Container tickets that are queues, not single fixes:
  [dist-test-suite-failures-batch](tickets/dist-test-suite-failures-batch.md),
  [remaining-language-feature-gaps](tickets/remaining-language-feature-gaps.md),
  [digest-dist-blockers](tickets/digest-dist-blockers.md). Pull one row out,
  fix it as its own PR, and note it in the container file only if the row list
  changes.
