# TRIAGE — prioritized snapshot of todo/ (2026-08-19)

A ranked index of every open finding under `todo/tickets/` and `todo/deep/`,
so a session can pick the next ticket without re-reading all of them.

This is a **snapshot, not a ledger**. Resolving a ticket does *not* require
editing this file — that would reintroduce exactly the shared-file merge
conflicts `todo/` exists to avoid. A stale row is fine; the per-ticket files
stay the source of truth. Regenerate the whole file when it has drifted too
far (re-survey every ticket, re-score, rewrite).

Surveyed 2026-08-19: **80 files** (66 `deep/`, 14 `tickets/`) — down sharply
from the 2026-08-13 survey's 132, meaning ~52 tickets resolved or were
folded into other tickets in six days. This regen also **changes the ranking
axis**. The 2026-08-13 file ranked primarily by *campaign* (which multi-PR
effort a ticket belonged to). That grouping is gone. This file ranks
primarily by two axes instead:

1. **Correctness class** — does the bug corrupt memory/data silently
   (soundness), produce a wrong answer in common code (broad correctness),
   produce a wrong answer only in a narrow/rare construct (narrow
   correctness), or just produce a bad diagnostic (diagnostics)?
2. **Breadth of impact** — does it affect "any Raku program using construct
   X", or does it block one specific dist/battery, or is it a narrow repro
   with nothing currently depending on it?

**Performance items are pulled into their own section**, separate from the
correctness ranking, because the right unit of work for them is different:
several are mid-measurement (a round of profiling already in progress, a
specific next lever already identified) and the productive move is to batch
them into one profiling-heavy session rather than interleave them with
correctness fixes. A ticket only lives in the perf section if its *own next
step* is measurement/profiling, or the fix is perf-only or blocked purely
on a design/perf tradeoff — a perf-flavored ticket that also fixes a real
wrong answer stays in the correctness tables.

Old campaign identities (ADR-0019 Phase E, ADR-0025, vendor-real-Test, Cro,
...) are preserved as a parenthetical on each row where relevant, since they
still tell you which other tickets share root causes — but they no longer
determine the row's position.

## How the ranking works

- **Tier S — Soundness.** Crashes (SEGV/panic), memory unsafety, or *silent
  data corruption* (a variable, container, or environment write that is
  wrong and nothing detects it). Always highest priority regardless of
  effort — these are the ones that erode trust in "if it doesn't crash, the
  answer is right."
- **Tier B — Correctness, broad impact.** A wrong answer or missing
  capability in a common/fundamental construct (closures, dispatch,
  Supply/whenever, basic types), or a bug that blocks an entire dist/battery
  slot. Subdivided into B1 (broad language-construct correctness — the
  highest-leverage fixes, since they aren't tied to one dist) and B2
  (batteries/dist-blocking — real but scoped to unblocking one bundled
  library).
- **Tier N — Correctness, narrow impact / diagnostics.** Wrong answer in a
  rare/edge-case construct with no known blocked test, or a
  wrong/missing *error message* (not a wrong answer).
- **Perf — needs measurement.** Batch these into one profiling session; see
  the section intro above.
- **Icebox.** Blocked on an ADR/design decision, an explicit user call, or
  is a pure decision/measurement record with no action attached. Also
  houses very-low-priority optional polish (zero behavior change).

**Effort** (S/M/L/XL) is shown but does not change tier — an XL soundness
bug still outranks an S-effort narrow-correctness bug.

---

## Tier S — Soundness (crashes, memory unsafety, silent data corruption)

Ordered roughly by how confirmed/reproducible the defect is, then by how
common the triggering pattern is.

| Ticket | Breadth | Effort | Why here |
|---|---|---|---|
| [native-array-storage-sync-unsound-interior-mutation](deep/native-array-storage-sync-unsound-interior-mutation.md) | Any NativeCall pointer write-through into a native array's backing store | L | **Confirmed genuine UB** (`*const→*mut` cast on a non-`Cell` field) causing a deterministic *release-only* miscompile — independent of any one test, could manifest unpredictably as compiler/inlining changes. |
| [inline-start-blocks-clobber-a-later-declared-variable](deep/inline-start-blocks-clobber-a-later-declared-variable.md) | `Promise.allof(start{...}, start{...})` — an idiomatic concurrency pattern | XL | Confirmed, reproducible silent corruption of a variable to `Nil`. Three separate fix attempts across three sessions each found a deeper layered bug (`needs_env_sync` vs. atomic-cell RMW); explicitly "no longer pick-up-and-finish." |
| [attribute-bind-severed-by-unrelated-later-call](deep/attribute-bind-severed-by-unrelated-later-call.md) | `$!x := $var`-style attribute bind aliasing, inside any framework callback (test harness, event handler) | L | An attribute bind silently stops tracking its source after an *unrelated* later method call touches other env state — real-world risk for the common "bind an attribute to a caller container" idiom, not `Test`-specific. |
| [attr-bind-source-write-lost-through-nested-sub-call-chain](deep/attr-bind-source-write-lost-through-nested-sub-call-chain.md) | Same bind family, triggered by a multi-frame call chain (e.g. `lives-ok { $obj.bind() }`) | L | Root-caused to `VmCallFrame` having no per-frame `CompiledCode`/locals-name reference, so cross-frame free-variable container promotion silently no-ops. Same underlying disease as the row above; investigate together. |
| [env-var-write-invisible-to-spawn-after-a-thread](deep/env-var-write-invisible-to-spawn-after-a-thread.md) | Any program combining `%*ENV` writes, a spawned OS thread (Supply/socket/timer tap), and a later subprocess spawn with default env inheritance | M-L | A `%*ENV` write silently stops reaching a spawned child's *default* environment once any thread has ever run — `std::env::set_var`'s documented cross-thread UB hazard, or a mutsu-thread-setup interaction, not yet distinguished. |
| [control-warn-resume-list-assign-first-target-stale-on-repeat-call](deep/control-warn-resume-list-assign-first-target-stale-on-repeat-call.md) | A `CONTROL { when CX::Warn { ...; .resume } }` sub, called 2+ times, whose result feeds a caller-side multi-value list assignment | M-L | Confirmed deterministic (not a race): the first LHS slot of the caller's list-assignment goes stale on the second call, only after `use Test;` loads the real vendored module — general `.resume`/list-assign interaction, not `Test`-specific. |
| [cue-loop-lexical-shared-lane-residue](deep/cue-loop-lexical-shared-lane-residue.md) | A loop-redeclared lexical mutated inside a `.cue`/`cas`/atomic callback | XL | Confirmed root cause (a stale plain-`env` value read as the atomic base on the first tick of a new round), but **three independent fix attempts each broke something else** (FD ownership, free-var-completeness, a `Nil`-mask interaction never root-caused) — needs an ADR-level design pass covering the whole `clone_for_thread*` family, not another patch attempt. |
| [expr-decl-writes-through-captured-cell](tickets/expr-decl-writes-through-captured-cell.md) | Any *method* (class/role/sub/multi/private) that expression-declares a `my` whose name matches a cell-boxed caller lexical | S | Silent corruption of the CALLER's outer variable through a captured `ContainerRef` cell. Re-verified 2026-08-20: the sub/`EVAL` shapes are fixed, only method dispatch survives, and the fix is prototyped and validated (`make test` clean, both roast pins green) — **moved to `tickets/`, ready for direct implementation**, no design needed. |
| [bare-name-type-constraint-store-is-scope-blind](deep/bare-name-type-constraint-store-is-scope-blind.md) | `@`/`%`-typed lexicals in routines; typed scalars inside `if`/`while`/loop bodies | M-L | Silent loss of type enforcement across scope boundaries (a callee's typed lexical can poison or lose enforcement for a same-named caller variable). Scalars in routines/genuine blocks are already fixed; containers and `if`/`while`/loop bodies remain. |
| [adr0025-slice2-implementation-plan](deep/adr0025-slice2-implementation-plan.md) | Closures capturing a creator-reassigned scalar via call-arg storage; a live Cro test race | XL | The ADR-0025 soundness campaign's concrete next step (decl-site cells for every vouch-refused captured scalar), including its own Step-0 cross-thread race fix in `http2-response-serializer.rakutest` (confirmed genuinely racy — 4/8 pass, 4/8 fail on a pristine binary — not deterministic). |
| [closure-read-only-capture-loses-to-caller-env-same-name](deep/closure-read-only-capture-loses-to-caller-env-same-name.md) | Same family; core defect **already fixed** (slice 1) | M | Kept open only as the historical record for the still-open Step-0 race (now tracked in the row above) and a spun-off `for`-loop hijack ticket. Don't re-diagnose the core defect — it's resolved. |
| [compiled-fns-default-breaks-nested-subs-outside-methods](deep/compiled-fns-default-breaks-nested-subs-outside-methods.md) | Any routine (not just methods) declaring a nested `sub`, once `legacy_body` is dropped | L | Currently masked by the `legacy_body` tree-walk fallback (~17 call sites pass an empty `CompiledFns` table instead of the real one). A confirmed live reproducer exists (`Test::Util`'s own `_is-eqv` shape). **Landmine, not yet live-broken** — but explicitly blocks the ADR-0019 C6e architecture goal of removing `legacy_body`; do not attempt that removal before this lands. |
| [promise-spawn-segv-under-load](deep/promise-spawn-segv-under-load.md) | Many concurrent `Promise.start` (4000+) under CPU contention | M-L | Confirmed reproducible SEGV (guard-page stack overflow on a spawned-thread's `JoinHandle` drop), ~6-8% under load in the jit-stress config. Fix direction is known: a uniform spawned-thread stack budget, and/or not holding a `JoinHandle` on the deep dispatch path. |
| [panic-unwind-leaks-side-channel-call-state](deep/panic-unwind-leaks-side-channel-call-state.md) | Any panic caught mid-call whose resuming code depends on `current_package`/pragma state | M | The `call_frames`/locals corruption half of this is **already fixed**; what remains is `current_package` and pragma state, saved/restored as plain Rust locals outside `VmCallFrame`, so they leak on unwind too. Narrower now, but a real gap. |
| [seq-cache-does-not-narrow-to-list-stack-overflow](deep/seq-cache-does-not-narrow-to-list-stack-overflow.md) | `.cache` on a deferred `SeqBody` or a cat-pull `LazyList`, under the experimental `MUTSU_REAL_TEST=1` flag | M | **Root-caused 2026-08-20; design in [ADR-0038](../docs/adr/0038-seq-cache-returns-a-list-and-the-seq-list-view-is-a-property-of-the-value.md), ready for implementation.** `Seq.cache` must return a `List`; mutsu returns something still binding `Seq:D`, so the real `Test.rakumod`'s `is-deeply` narrowing candidates re-select themselves until the Rust stack overflows (4 files, exit 134). Not CatHandle-specific — two of the four involve none. Phase 2 (~2 lines) clears half. Not release-reachable (native `Test` is the default), but blocks the vendor-real-Test campaign flip. |
| [procasync-stress-segv](deep/procasync-stress-segv.md) | `roast/S17-procasync/stress.t`'s 1200-iteration `Proc::Async`+signal+timer churn test | — | One CI-only SEGV observed, not reproduced in 22 local runs or on retry. **Not actionable without a fresh crash-report artifact** — monitor via the crash-reporter (`tmp/crash/<pid>.txt`), don't spend a session chasing it blind. Do NOT quarantine — a crash is a poor quarantine candidate. |

## Tier B — Correctness, broad impact

### B1 — broad language-construct correctness (not tied to one dist)

| Ticket | Breadth | Effort | Why here |
|---|---|---|---|
| [method-calls-never-push-caller-frame](deep/method-calls-never-push-caller-frame.md) | Any method body using `CALLER::`, `callframe()`, `PROCESS::`, or `DYNAMIC::` (a common OO + dynamic-var idiom) | L/XL | All 5 method-dispatch paths never push a caller-env frame, so dynamic-var/introspection reads from inside ANY method are wrong or `Nil`. Confirmed with flat, non-role repros. Blocks the `Log::Timeline` battery. |
| [blind-slip-flattening-in-fixed-arity-calls](deep/blind-slip-flattening-in-fixed-arity-calls.md) | Any fixed-arity sub/method call whose argument merely *evaluates to* an `Empty`/Slip-shaped value (not just `\|EXPR`) | L | A runtime value-shape-based flattening mechanism silently drops such arguments instead of passing them as one value — cross-cutting VM change across 4 call-dispatch files. |
| [element-itemization-lost-in-scalar-binding](deep/element-itemization-lost-in-scalar-binding.md) | `.raku`/`.gist` of arrays-of-arrays, implicit-topic iteration, hash-value reads, element arity in list context | M per slice | **Root-caused and designed 2026-08-20 in [ADR-0040](../docs/adr/0040-array-hash-elements-are-itemized-at-the-store.md); ready for implementation.** Array/hash elements are stored bare instead of as Scalar containers. The decision is to itemize at the *store* via the existing `Value::item()`; measurement showed the feared fallout class is empty (25/25 behavioural + 10/10 raku-exact probes on hand-itemized elements), so the campaign is the store-site enumeration only, not a representation change. The "list-destructuring bind write-through" symptom in the original file is **misfiled** — it is a desugar bug (ADR-0040 §1.7), not itemization. |
| [pair-namedness-is-a-value-property-not-a-call-site-property](deep/pair-namedness-is-a-value-property-not-a-call-site-property.md) | Any code that iterates Pairs and hands them to a typed `multi` (e.g. `Cro::HTTP::Client`'s header handling) | mostly done, residual M | mutsu encodes named-vs-positional on the *value*; raku decides it at the *call site*. ADR-0021 P1-P3a/P3 already shipped (method-boundary parity, hash-derived pairs); P4/P5 cleanup remains. Re-verify the originally-blocking Cro repro before re-scoping down. |
| [for-loop-rw-element-alias-lost-through-deferred-closure](deep/for-loop-rw-element-alias-lost-through-deferred-closure.md) | `for @arr -> $v is rw {...}` combined with a closure that escapes the loop and is called later — an ordinary closure-capture pattern | XL | Binds a snapshot instead of a live alias; wrong answer in fairly ordinary code. Root cause is architectural (no element-level `ContainerRef`), needs its own share-vs-bind design at the element-store layer. |
| [deferred-seq-materialization-destroys-the-original](deep/deferred-seq-materialization-destroys-the-original.md) | Any deferred/lazy `Seq` inspected twice (even just `.defined` then `.Str`); blocks the vendor-real-Test flip broadly, plus a large family of `try`-block laziness divergences that will self-resolve once this lands | M | Materializing a deferred Seq abandons the original `Arc`, so a second access on the SAME variable reports "already consumed." raku reifies in place via `.cache`. The sound fix (write pulled items back through the original `Arc`) also fixes ~13 documented `try`/laziness divergences for free — but will make mutsu *stricter* in those cells, so a full roast-whitelist sweep is mandatory when it lands. |
| [listops-are-not-real-multi-subs](deep/listops-are-not-real-multi-subs.md) | Any user/module `multi` sub sharing a name with `splice`/`push`/etc. (blocks `String::Splice` entirely) | L | `splice`/`push`/... are hardcoded VM opcodes, not real multi-subs, so a user candidate for these names is unreachable — architecturally significant gap. |
| [whenever-expression-position-needs-real-design](deep/whenever-expression-position-needs-real-design.md) | Any code holding onto a live `whenever`'s `Tap` handle from inside `react`/`supply` (Cro-style or hand-written Supply/Channel consumers managing their own tap lifecycle) | L/XL | Two independent bugs: the parser doesn't recognize `whenever` as an expression term at all (fragments into 4 orphaned statements), AND the one existing narrow mechanism that partially supports it clobbers the wrong variable and doesn't even bind a real `Tap`. Needs design across parser/AST/compiler/VM — not a one-shot patch. |
| [schedule-on-live-transform-operators-bypass-deferral](deep/schedule-on-live-transform-operators-bypass-deferral.md) | `.map`/`.grep`/`.do`/`.flat` applied to a `.schedule-on()`'d Supply | L | Still deliver synchronously despite scheduling (ADR-0028 slice 1 fixed direct taps; this is the remaining immediate-registration transform-op category). Design is sketched concretely (a new `__ScheduledTransformApply` shim), 4 files, needs its own test matrix. |
| [module-file-scope-array-and-hash-still-share-the-caller](deep/module-file-scope-array-and-hash-still-share-the-caller.md) | Any module whose file-scope `@`/`%` collides by name with the loading scope's own variable (the scalar case is already fixed) | XL | Silent data corruption of module state on name collision. Blocks the vendor-real-Test campaign (`Test.rakumod`'s own `@vars`), costs a whole roast integration file. Fix is a large canonical-slot refactor across ~120+ call sites — deferred twice already. |
| [exception-class-hierarchy-is-mostly-unregistered](deep/exception-class-hierarchy-is-mostly-unregistered.md) | Any `throws-like X ~~ Y`-shaped hierarchy check; mutsu's own compiler emits one of the unregistered classes | XL | 124 of mutsu's own `X::` exception classes aren't registered types (`.new`/`isa`/`~~` fail on them). Needs a role-vs-prefix parentage design before mechanical generation. |
| [sigilless-alias-assignment-skips-type-constraint](deep/sigilless-alias-assignment-skips-type-constraint.md) | Any `my \x := $typed_var; x = ...` sigilless bind-alias write (blocks `Native::Overflow`'s whole suite: plans 30, runs 0) | M-L | A compile-time name-keyed type-constraint map has no entry for a sigilless alias name, so the type check is silently skipped. Sound fix is container-carried type constraints (same family as ADR-0013's interior-mutability work) — narrow compile-time patch would miss cross-function-boundary and computed-index cases. |
| [nextsame-tail-call-is-not-a-real-return-signal](tickets/nextsame-tail-call-is-not-a-real-return-signal.md) | `nextsame`/`nextwith` inside a bare-Block `.wrap()` wrapper (a `sub`/`method` wrapper already behaves correctly) | **S — ready to implement** | Re-verified 2026-08-20: the old "no lexical return target exists" framing was stale — plain `return` in the same position is already correct. The real defect is that `dispatch_next_candidate`'s 12 `tail_call` legs build the unwind by hand without `control: Control::Return`, so the existing `is_return()`-gated lexical re-target arm skips it. Fix (`RuntimeError::return_signal(result)`) measured green against raku and the full wrap/`callsame` corner. The architectural residue split out to ADR-0050. |
| [eval-context-frame-owns-the-return-target](deep/eval-context-frame-owns-the-return-target.md) | Real `Test.rakumod`'s `throws-like '<code with return>'` (every such assertion under the vendored module) | M | `EVAL ..., context => $frame` takes the *package* from the context but not the *frame*, so a `return` in the snippet returns from the wrong routine instead of raising a catchable `X::ControlFlow::Return`. |
| [return-outside-routine-uncatchable-inside-nested-run](deep/return-outside-routine-uncatchable-inside-nested-run.md) | `gather { return 1 }` (or similar) evaluated via `EVAL`, inside a `try`/`CATCH` | M-L | An escaping `return` inside a nested run (EVAL, `throws-like`) is never converted to a catchable exception until the TRUE top of the program — it aborts past any surrounding `try`/`CATCH`. The existing guard was already tuned once for a different regression; needs a repro matrix covering both directions before touching it again. |
| [pseudo-method-which-why-user-override-ignored-in-bareword-and-dynamic-form](deep/pseudo-method-which-why-user-override-ignored-in-bareword-and-dynamic-form.md) | Any class overriding `.WHICH`/`.WHY` (a documented custom-value-identity idiom), called via bareword or `.$m()`/`."$m"()` | M | Two independent, redundant "skip native pseudo dispatch" mechanisms across 5 call sites, neither aware `WHICH`/`WHY` (unlike the other 6 MOP pseudo-methods) are genuinely overridable. Real but likely rare in practice. |
| [array-literal-nil-not-decayed-at-construction](deep/array-literal-nil-not-decayed-at-construction.md) | `[Nil]` array-literal construction (`eqv`, sub-arg passing, typed-array element checks) | M | A real semantic gap, but low-moderate severity: narrow practical surface (most code never round-trips a bare `Nil` array element through `eqv`), and the naive fix has a known second casualty (a typed-array leniency mutsu currently has that raku doesn't) requiring its own audit. |
| [regex-alternation-ltm-longest-literal-prefix](deep/regex-alternation-ltm-longest-literal-prefix.md) | Any regex/grammar relying on `\|` LTM tie-breaking | **verify, likely resolved** | ADR-0022 (all 5 slices) shipped 2026-08-09 per the design doc's own header, but this file's body predates that and the originally-blocking Cro repro / roast subtests were not re-verified after the ship. Re-run before trusting either direction. |
| [when-nonmatch-value-outside-map-grep](deep/when-nonmatch-value-outside-map-grep.md) | A `when`/`default`-tail block used as a direct closure call, `do { when ... }`, or a bare `given`/`when` statement (the `.map`/`.grep`/`.first` fast paths are already fixed) | M | A non-matching `when`-tail block still evaluates to `Nil`/`Any` instead of the correct falsy value in every context except the 4 already-fixed inline fast paths. The general fix (push the value in `exec_when_op`) is a statement-sequencing invariant change across 3 compiler call sites that must move in lockstep — a point fix was explicitly rejected as unsafe by its own prior investigation. |

### B2 — batteries / dist-blocking correctness (real bugs, scoped to unblocking one bundled library)

| Ticket | Blocks | Effort | Why here |
|---|---|---|---|
| [p5tie-container-protocol-and-array-parse-bug](deep/p5tie-container-protocol-and-array-parse-bug.md) | `P5tie` dist entirely | L | Needs a real `Stash.BIND-KEY` container-tie protocol (MOP-level, not a stopgap per BATTERIES.md rung-3 ban) plus an unrelated parse bug in one file. |
| [trait-mod-does-not-callable-and-no-variable-mop](deep/trait-mod-does-not-callable-and-no-variable-mop.md) | `Hash::Restricted` dist (dies immediately on load) | L | Needs a real `Variable` MOP object plus `trait_mod:<does>` as a genuinely callable multi sub — check corpus-wide need before investing, per its own note. |
| [user-postcircumfix-index-not-dispatched-for-instances](deep/user-postcircumfix-index-not-dispatched-for-instances.md) | `Array::Rounded` (16/35 still failing) | M-L (down from XL — read path fixed) | The *read*-path dispatch gap for user `postcircumfix:<[ ]>` is fixed and pinned. Still open: the assignment path, the native-term capture (`&postcircumfix:<[ ]>` doesn't exist as a callable), and an unrelated constant-alias `is`-trait gap. |
| [nativecall-types-package-qualification](deep/nativecall-types-package-qualification.md) | `.^name`/`.raku`/error-message consistency for NativeCall types; blocks nothing currently whitelisted | M/L | `Pointer`/`CArray`/etc. report bare names where raku reports `NativeCall::Types::`-qualified ones. The "real" fix (qualify the registry key) has a **confirmed correctness trap**: it breaks ordinary `Pointer[T]`/`CArray[T]` marshalling that the OpenSSL/`IO::Socket::SSL` battery depends on. Needs an ADR choosing between display-only qualification (safe, permanent divergence) and the real fix (needs ~15 call-site audits first). |
| [nativehelpers-blob-moarvm-guts](deep/nativehelpers-blob-moarvm-guts.md) | Database battery slot (`DBIish`/`DB::SQLite`) | S | Re-measured 2026-08-19: the MoarVM-guts blocker is **gone** (ADR-0015 P0-P3b all landed; module bundled, 4/5 upstream files on the battery gate). One blocker left, root-caused and designed in the file: `pointer-to(array:D)` fails because mutsu's **bare** `array` type constraint matches no value at all, so the `multi` never selects — storage, `.REPR`, `BODY_OF` and C-write-through all verified working. Gap A is small and additive; Gap B (`int:D`/`array:D` in term position) and Gap C (`array ~~ Array` over-reporting) are separable, C deferred. |
| [template-engines-blocked-on-mutsu](deep/template-engines-blocked-on-mutsu.md) | Template battery slot candidates (`Template6`, `Template::Jinja2`, `Template::HAML`, `SP6`, `Template::Classic`) | L (cluster) | `Template::Mustache` (chosen slot) and `Template::Mojo` are essentially done. `Template6` is the natural next pick (runner-up candidate, would give the survey a real second option); the rest are ordinary compat work, each a fresh data point on real-world grammar/list divergence. |
| [cbor-simple-typed-array-and-diagnostic-format-gaps](tickets/cbor-simple-typed-array-and-diagnostic-format-gaps.md) | `CBOR::Simple`'s own upstream suite (doesn't block Cro/Log::Timeline, which only need a narrow slice) | M | Actively being worked — 3 of ~4 root causes found and fixed this week (forward-captured-code-var, a `my constant`+`elsif` parser bug, a BigInt-negate downcast bug). Residual: a nested-array decode aliasing bug + a stack overflow in `01-basic.rakutest` test 68+, and `03`/`04`/`06` not yet triaged. |
| [cold-supply-whenever-source-replayed-not-tapped](deep/cold-supply-whenever-source-replayed-not-tapped.md) | `Test::Scheduler` dist (last known blocker) | L | A cold on-demand Supply used as a `whenever` source is replayed synchronously instead of tapped, so async emissions (promises, timers) never reach the outer body — architecture change (give it a supplier, tap it) touching completion accounting used by many other consumers. |
| [subtest-compiled-dispatch-async-middleware-regression](deep/subtest-compiled-dispatch-async-middleware-regression.md) | Blocks re-landing #6499's `subtest` perf win (already reverted, so nothing is currently broken) | L | Re-measured 2026-08-20: compiled-first dispatch fails **16 of 24** subtests in Cro::HTTP's `http-middleware.rakutest`, not just the one recorded. Root cause is NOT declaration lifetime — class decls, `LEAVE` phasers and async `supply` transforms in a subtest body all behave identically on both paths. Bisect from the frame-setup end (`call_compiled_closure` vs `call_sub_value`), not from Cro. The escaped-type half of the original finding split out into ADR-0047. |
| [second-preserving-instance-body-blob-returns-empty-in-same-supply-body](deep/second-preserving-instance-body-blob-returns-empty-in-same-supply-body.md) | Cro::HTTP2's `http2-request-parser.rakutest` (sole remaining failure) | L | The second of two concurrently-open HTTP/2 streams' `Supplier::Preserving`-backed body reads empty despite correct `emit`/`done`. No Cro-independent minimal repro found yet after two attempts — needs a closer repro (emit-before-data-arrives timing) before a real fix attempt. |

## Tier N — Correctness, narrow impact / diagnostics

| Ticket | Category | Effort | Why here |
|---|---|---|---|
| [keep-undo-decided-by-value-truthiness-not-completion](tickets/keep-undo-decided-by-value-truthiness-not-completion.md) ⚡ | correctness-narrow | S-M | `KEEP`/`UNDO` should be decided by normal-completion-vs-exception, not the block's trailing value's truthiness — a block ending in `0`/`False` should still run `KEEP`. Fix site is precisely named (`should_run_success_queue`); a good quick win, and an existing test asserts the wrong behavior and needs correcting alongside the fix. |
| [infix-in-term-position-not-diagnosed](tickets/infix-in-term-position-not-diagnosed.md) | diagnostics | M | `X::Syntax::InfixInTermPosition` is registered but nothing in the parser ever raises it — falls back to a generic `X::Syntax::Confused`. Needs careful parser-combinator surgery (many "expected a term" sites share code) verified against full `t/`+roast. |
| [mark-context-flags-leak-across-live-call-boundary](deep/mark-context-flags-leak-across-live-call-boundary.md) | correctness-narrow (broad latent risk) | L | A `:=` bind's VM "mark context" flag leaks into a subsequent real function call, corrupting typed-array declarations inside the callee. Narrow trigger today (blocks `Crypt::RC4`), but 7 sibling one-shot flags share the same unscoped-to-call-frame architecture — worth fixing generally, not just for this one flag. |
| [e8a-deferral-shadow-sequence-is-role-blind](tickets/e8a-deferral-shadow-sequence-is-role-blind.md) | instrument accuracy (not a dispatch bug) | S | Residual of the closed `method-entries-never-covers-unpunned-roles` finding (now `news/2026-08/`): the ADR-0019 E8a deferral shadow check builds its comparison sequence without the role fallback the real E9a walker uses, so every role-in-MRO deferral is reported as a mismatch. Ready for direct implementation, no design needed. |
| [placeholder-scope-loop-while-block-boundaries](deep/placeholder-scope-loop-while-block-boundaries.md) | correctness-narrow / diagnostics | M | `while`/`loop`/bare-`{}` each have a genuinely *different* raku placeholder-scoping rule (not one shared "boundary" fix) — no roast test currently depends on any of the three, but needs individual raku-verification per construct before touching the shared boundary-detection code (which affects arity of unrelated existing blocks). |
| [plain-classes-answer-cool-only-builtin-methods](deep/plain-classes-answer-cool-only-builtin-methods.md) | diagnostics (mutsu too lenient) | M-L | **Designed — see ADR-0051.** `G.new.uc` should die but answers `"G()"` via a stringify fallback (26 divergences, all string-family). The file's own diagnosis is falsified: the "needs a new row-existence predicate" blocker is stale (`native_method_row_exists`/`e2_native_method_exists` shipped with ADR-0019 E7), and the 6 regressions were not coincidental names — `Instant`/`Duration`/`IO::Path`/`Match` genuinely *are* `Cool` in raku and mutsu's ancestry data omits it. Root cause is 12 independently-maintained ancestry tables. Fix data first, gate second. |
| [ltm-inline-unbounded-quantifier-vs-array-tie](deep/ltm-inline-unbounded-quantifier-vs-array-tie.md) | correctness-narrow | L | On a runtime-length LTM tie, rakudo structurally prefers an inline unbounded quantifier; mutsu's two independently-evolved LTM engines just compare end positions. No known blocked test. |
| [adr0019-role-composition-memo-guard-raku-case-table](deep/adr0019-role-composition-memo-guard-raku-case-table.md) | correctness question, unresolved | S-M | Whether raku memoizes a role's deferred-body re-execution per-role, per-target, or not at all on recomposition is not yet established against real raku — build the case table before deciding a fix. |
| [is-run-after-raku-read-swallows-child-spawn](deep/is-run-after-raku-read-swallows-child-spawn.md) | correctness-narrow | M | `t/`-only repro (2nd+ `is_run` call referencing `$*RAKU`+`use v6.x` never spawns its child); doesn't reproduce in any current whitelisted test. |
| [begin-rat-divzero-escapes-wrapping](tickets/begin-rat-divzero-escapes-wrapping.md) | correctness-narrow, deliberately deprioritized | S | A single-case divergence from an apparent Rakudo implementation quirk (lazy `Rat` div-by-zero not wrapped in `X::Comp::BeginTime`, unlike every other exception shape). Its own investigation recommends NOT chasing this — near-zero real-world benefit, would couple the phaser-wrap mechanism to `Rat`'s internals. |
| [process-dynamic-write-nil-not-decayed-to-any](tickets/process-dynamic-write-nil-not-decayed-to-any.md) | correctness-narrow | S | `PROCESS::<$x> = Nil` stores a literal `Nil` instead of decaying to `Any`, unlike ordinary scalar assignment. Narrow, no roast dependency. |
| [wide-buffer-bit-accessor-width-divergence](tickets/wide-buffer-bit-accessor-width-divergence.md) | correctness-narrow, **recommend not fixing** | — | `read-ubits`/`write-bits` on a >1-byte-element buffer diverges from MoarVM in a way that looks like leaked, non-portable MoarVM memory-layout behavior rather than a defined Raku semantic (probed extensively). Zero roast coverage, zero real dist impact (every `Digest` call site uses width-1 buffers). Leave unfixed unless something concrete surfaces. |

---

## Perf — needs measurement (batch these into one profiling-heavy session)

These are all mid-campaign: each has already had at least one measurement
round, and the productive next step is more profiling/benchmarking, not a
guessed code change. Running them together amortizes the profiler-setup
cost and keeps perf work from interleaving with correctness fixes above.

| Ticket | Status | Why batch it here |
|---|---|---|
| [yaml-parse-throughput](tickets/yaml-parse-throughput.md) | Round 9 of an ongoing, heavily-instrumented campaign; correct (81/81) but 5-35x raku on some files | The methodology (VM_STATS diffing, gdb hit-count sweeps to find the dominant `view()`/materialization call site) is well-established and has closed several real drift regressions already. **ADR-0016 (all 5 phases, including P5 lazy `Match`) fully landed 2026-07-31** and delivered a real ~4x wall-clock win (round 6 confirmed) — round 5's "next round is ADR-0016 P2/P5 territory" prediction is stale; the file is now past that, chasing a post-P5 drift (ratio crept back from ~25 to ~34-46) with no dominant call site identified yet. |
| [digest-ripemd-start-per-block-overhead](tickets/digest-ripemd-start-per-block-overhead.md) | `t/ripemd.t` 295s → 119s already (9/9 correct); still exceeds the 120s CI gate margin | The flat post-optimization profile (no single dominant function: refcount 4.7%, symbol caches 5.5%, malloc+free 12%, ...) means the next lever needs a fresh profile, not a guess. One more ~20% win needed to whitelist. |
| [adr0019-g3-diffuse-bless-allocation-cost](deep/adr0019-g3-diffuse-bless-allocation-cost.md) | bench-ctor/bench-class construction cost is diffuse across many small alloc/hash/GC-refcount ops | Explicitly blocked on getting a working call-graph profiler or counting allocator — two smaller findings from the same investigation already landed. |
| [bench-ctor-construction-parity](tickets/bench-ctor-construction-parity.md) | The only bench where mutsu is slower than raku (1.17-1.35x); S1 and S5 landed | S2 is confirmed blocked on the closure-upvalue-cell prerequisite (`docs/vm-single-store.md`); S3's suggested directions are already implemented. What's left needs the same profiler work as the G3 row above — pair them. |
| [interpreter-call-path-in-hot-loops](deep/interpreter-call-path-in-hot-loops.md) | Mostly resolved (13.8x → ~2x for the isolated hot-loop shape) | The file's own conclusion redirects further work to `eval-block-value-recompiles-every-call.md` below — don't re-measure the already-closed shape, go straight to that ticket. |
| [eval-block-value-recompiles-every-call](deep/eval-block-value-recompiles-every-call.md) | A real, measured perf issue (per-call AST recompile for any first-class block/closure Value); one fix attempt already reverted after finding a 2.4x regression on a different shape | Has a documented, specific verification protocol (VM_STATS before/after on BOTH a no-nested-sub and a nested-sub repro, since CI doesn't catch perf regressions) — exactly the kind of ticket that needs a dedicated measurement session, not a quick patch. A 2026-08-14 Fable design consultation left a concrete recommended sequencing (cache compiled results first, since it fixes both named costs as one mechanism). |

### Perf — direction already known (lower priority than the above; no fresh measurement needed)

| Ticket | Effort | Why here |
|---|---|---|
| [call-compiled-closure-lacks-merge-all-and-dual-persistence-store](deep/call-compiled-closure-lacks-merge-all-and-dual-persistence-store.md) | L | Architecture prerequisite for the `eval-block-value-recompiles-every-call` "larger fix" — a recommended ADR framing and sequencing already exists (bug-parity fixes → `CapturePriority` mode → dispatch-eligibility fix → unconditional fork → delete the tree-walk branch). Pick up once the perf-measure row above reaches that stage. |
| [c6d-interpreter-body-sites-are-mostly-token-bodies](deep/c6d-interpreter-body-sites-are-mostly-token-bodies.md) | L | Nearly complete — C6d-1/3/4/5 all landed. Remaining scope (C6d-2, grammar token/rule bodies) is a regex-execution-model question that should be scoped against ADR-0009, not the OTF gate. |
| [adr0019-e2-e4-resolver-core](deep/adr0019-e2-e4-resolver-core.md) | XL | E3/E4 closed; only E2 (exact handler-ID catalog) remains, and it no longer gates anything (a structural fallback replaced the coverage gate). Demote out of active work. |
| [adr0019-f1-f2-introspection-canonical-source](deep/adr0019-f1-f2-introspection-canonical-source.md) | S | Mechanism slice (Sub-vs-Instance unification, `.package`/`.signature` defaults) already landed. Remaining work is reactive per-case fidelity overrides on native-method introspection — fix opportunistically when a real assertion demands it, not proactively. |

---

## Icebox — blocked on design/decision, or explicit low-priority record

| Ticket | Blocked on / why |
|---|---|
| [captured-outer-pair-container-alias](deep/captured-outer-pair-container-alias.md) | Needs the ADR-0001 element-cell / container-representation mechanism. Explicitly: do not special-case Pair construction or snapshot-copy around it. |
| [subscript-p-pair-is-a-snapshot-not-a-container](deep/subscript-p-pair-is-a-snapshot-not-a-container.md) | Same blocker — needs an `array_element_cell` API. The tempting "widen the env scan" patch is explicitly the wrong direction (doubles down on an already-wrong heuristic). |
| [shared-store-bare-name-collision-across-unrelated-frames](deep/shared-store-bare-name-collision-across-unrelated-frames.md) | Re-verified 2026-08-13: every concrete instance found so far has been fixed elsewhere (multi-param `for` loop binding, two env-key fixes). **No known blocked test currently drives the store-keying redesign** — re-measure before starting rather than picking this up speculatively. |
| [rakuast-remaining](deep/rakuast-remaining.md) | A multi-campaign backlog (ADR-0011). Its own text says to pick slices by user impact, not cadence — needs a user/strategic decision on which read/construction/lowering gap matters next, not more investigation. |
| [nativecall-cannot-be-vendored](deep/nativecall-cannot-be-vendored.md) | Not actionable — a measurement record with explicit reopen conditions (NQP/QAST, MoarVM dispatch programs, 61 missing `nqp::` ops). Keep as reference, don't re-derive. |
| [call-compiled-closure-missing-rw-lazylist-tail](tickets/call-compiled-closure-missing-rw-lazylist-tail.md) | Confirmed by static audit only — not yet reduced to a failing repro despite two rounds of trying plausible shapes. Low priority until a concrete repro exists; a documented next debugging step (breakpoint at the exact fork site) is recorded for whoever picks it up. |
| [bare-failure-sink-is-consumption-time-not-creation-time](tickets/bare-failure-sink-is-consumption-time-not-creation-time.md) | Architecturally large (raku decides fatal-vs-soft at Failure *construction*, stamped per-instance; mutsu decides at ~33 scattered consumption sites with zero knowledge of `fatal_mode`). Confirmed pre-existing, narrow practical trigger — not blocking anything in progress. |
| [adr0019-d10-precompute-stub-and-swallow-flags](tickets/adr0019-d10-precompute-stub-and-swallow-flags.md) ⚡ | Zero-behavior-change polish. Its own text: "do opportunistically if touching these files for another reason, otherwise skip." |
| [adr0019-d2c5-collapse-default-eval-env-setup](tickets/adr0019-d2c5-collapse-default-eval-env-setup.md) | Explicitly optional in the ADR-0019 checklist. Pure de-duplication (3 near-identical env-setup shapes), no missing feature. Gated on raku-verifying one special case first. |
| [adr0019-method-body-compile-dedup-remnants](tickets/adr0019-method-body-compile-dedup-remnants.md) | Low priority; one of its two items may already be dead code post the Phase E dispatch-resolver unification — confirm reachability before touching. |

---

## Housekeeping notes

- **[adr0016-p5-match-consumer-inventory](deep/adr0016-p5-match-consumer-inventory.md) is stale and should be archived.**
  Verified directly against `docs/adr/0016-span-based-captures-and-lazy-match.md`:
  ADR-0016's status line says **all five phases have landed** (P1-P5, last one
  2026-07-31), and this file's own header says to `git mv` it to `news/` once
  P5's seam lands — it has. Not a live blocker for `yaml-parse-throughput`
  (that ticket has since moved past ADR-0016 into a post-P5 drift
  investigation, corrected above); this file is pure historical record now.
- **This regen did not verify every "likely resolved" flag from the
  2026-08-13 survey.** [regex-alternation-ltm-longest-literal-prefix](deep/regex-alternation-ltm-longest-literal-prefix.md)
  (Tier B1) is the one carried-over case — its design doc (ADR-0022) says
  shipped, the ticket file itself doesn't reflect that. Re-run its named
  repros before either closing it or trusting it's still broken.
- **[vendor-real-test-module](deep/vendor-real-test-module.md)** (XL, its own axis — not tabulated in Tier
  B above because it's a 2300-line campaign log, not a single fix) continues
  to be the single largest source of newly-discovered general interpreter
  bugs in this survey: several Tier S and Tier B1 rows above
  (`deferred-seq-materialization-destroys-the-original`,
  `module-file-scope-array-and-hash-still-share-the-caller`,
  `eval-context-frame-owns-the-return-target`,
  `return-outside-routine-uncatchable-inside-nested-run`,
  `seq-cache-does-not-narrow-to-list-stack-overflow`,
  `subscript-p-pair-is-a-snapshot-not-a-container`,
  `comp-group-multi-error-bundling-unsupported` (since verified stale and
  closed out — see `news/2026-08/comp-group-multi-error-bundling-closeout.md`),
  `infix-in-term-position-not-diagnosed`) were found by that campaign's `t/`
  residue sweep, not independently. Fixing any of them also moves that
  campaign forward — read `vendor-real-test-module.md`'s own tail section
  before picking one, since it tracks which roast files each residual gap
  blocks.
- **13 tickets/files referenced by the 2026-08-13 survey no longer exist**
  in `todo/tickets/`/`todo/deep/` as of this regen (e.g.
  `async-listener-not-freed-when-relistening-in-a-loop`,
  `named-parameter-type-constraints-are-not-enforced`,
  `wrap-chain-skipped-inside-foreign-wrap-dispatch`,
  `callsame-to-native-mu-methods-nil`, the whole ADR-0019 E8/E9
  raku-divergence byproduct table, most of the P2/P3 rows from that
  survey). This regen did not individually trace each to its resolving PR
  or `news/` entry — assume fixed/merged/closed unless a fresh repro says
  otherwise. If you need the paper trail for one of them, `git log
  --diff-filter=D -- todo/` on the filename is the fastest way to find the
  deleting commit.
