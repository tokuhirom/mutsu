# TRIAGE — prioritized snapshot of todo/ (2026-08-13)

A ranked index of every open finding under `todo/tickets/` and `todo/deep/`,
so a session can pick the next ticket without re-reading all of them.

This is a **snapshot, not a ledger**. Resolving a ticket does *not* require
editing this file — that would reintroduce exactly the shared-file merge
conflicts `todo/` exists to avoid. A stale row is fine; the per-ticket files
stay the source of truth. Regenerate the whole file when it has drifted too
far (re-survey every ticket, re-score, rewrite).

Surveyed 2026-08-13: **132 files** (52 `deep/`, 80 `tickets/`). Since the
2026-08-10 survey (111 files): **13 resolved** (all fixed, merged into a
sibling ticket, or closed as non-reproducing — see Housekeeping for the
per-file mapping) and **34 newly filed**, dominated by two fast-moving
campaigns: ADR-0019 Phase E's E8/E9 raku-ground-truth verification pass
(closed E8 outright and spawned ~10 method-dispatch/wrap divergence
tickets, several already fixed same-day) and the new ADR-0025
captured-scalar-cells cluster (a soundness campaign with a live Cro
blocker). Two previously-P1 tickets (`regex-alternation-ltm-longest-literal-prefix`,
`pair-namedness-is-a-value-property-not-a-call-site-property`) look
resolved by ADR-0022/ADR-0021 shipping but were not re-verified this pass —
flagged in Housekeeping rather than silently dropped.

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

Five campaigns dominate this tier; most P1 rows are named blockers of one of
them, so progress compounds.

### Campaign: Cro (the web-framework battery slot, PLAN §1 B1)

Down from 8 rows to 4 since 2026-08-10 — five blockers fixed this window
(slurpy-mask thread survival, `Promise(supply{})` off-thread drive, session
counter isolation, subset-type nominalization, a pointy-block trait misparse).
`docs/batteries/web-framework.md`'s own baseline numbers (Cro::HTTP 1/28,
Cro::Core 1/9) predate this window and are stale; re-measure before quoting them.

| Ticket | Axis | Effort | Why here |
|---|---|---|---|
| [second-preserving-instance-body-blob-returns-empty-in-same-supply-body](deep/second-preserving-instance-body-blob-returns-empty-in-same-supply-body.md) | concurrency §5 | L | NEW. Sole remaining failure ("check 4") in Cro::HTTP2's `http2-request-parser.rakutest`: a second concurrently-open stream's `Supplier::Preserving`-backed body reads empty despite correct emit/done — no minimal repro found yet. |
| [async-listener-not-freed-when-relistening-in-a-loop](tickets/async-listener-not-freed-when-relistening-in-a-loop.md) | batteries §1 | L | Unresolved: the original stale-`$tap`/multi-address bugs are fixed, but the 3rd+ round of re-binding a port to a Cro server still gets empty response bodies — blocks the whole multi-server auth/session/log-file suite family. |
| [for-multi-param-array-hash-shadow-clobbers-outer-container](tickets/for-multi-param-array-hash-shadow-clobbers-outer-container.md) | correctness §6 | L | Unresolved: the scalar case fix was incomplete for slot-less names, and the `@`/`%`-sigil case is fully unfixed (container-handle aliasing bug in `vm_for_loop_body.rs`). |
| [named-parameter-type-constraints-are-not-enforced](tickets/named-parameter-type-constraints-are-not-enforced.md) | correctness §6 | M | Unresolved: type constraints on *named* params are parsed then ignored (positionals are checked) — Cro router's 400 Bad Request branch is unreachable, bad requests fall through to 404. |

### Campaign: ADR-0025 — captured scalar cells are value-kind blind (soundness)

New since 2026-08-10. [ADR-0025](../docs/adr/0025-captured-scalar-cells-value-kind-blind.md)
is Accepted, slice 1 shipped. Slice 1 fixed the core hijack/staleness defect
(a closure's captured read-only scalar losing to a caller-scope variable of
the same name); slice 2 is the concrete next step, widening decl-site cell
coverage — which will also surface more instances of the adjacent bug below.

| Ticket | Axis | Effort | Why here |
|---|---|---|---|
| [adr0025-slice2-implementation-plan](deep/adr0025-slice2-implementation-plan.md) | soundness | XL | The campaign's concrete next step: decl-site cells for every vouch-refused captured scalar, plus its own Step 0 (a cross-thread race fix for `http2-response-serializer.rakutest` test 18 — a live Cro test) and mandatory perf-canary gates. |
| [closure-read-only-capture-loses-to-caller-env-same-name](deep/closure-read-only-capture-loses-to-caller-env-same-name.md) | soundness | M-L | Core defect fixed by slice 1; kept open only for the same Step-0 race (now owned by the slice2 plan above) and a spun-off nested-whenever ticket. |
| [expr-decl-writes-through-captured-cell](deep/expr-decl-writes-through-captured-cell.md) | soundness | L | Adjacent, not campaign-blocked: expression-position `my` writes through an inherited `ContainerRef` cell instead of shadowing it — slice 2's broader cell coverage will expose this more often, so worth fixing alongside it. |

### Campaign: vendor the real `Test` module (PLAN §1, batteries policy)

| Ticket | Axis | Effort | Why here |
|---|---|---|---|
| [vendor-real-test-module](tickets/vendor-real-test-module.md) | Test-vendor §1 | XL | The campaign header. Regression count driven 343→315→301→255→190→113 (last hard full-sweep number, 2026-08-04) via continuous fixes; step 3 (flip the default) hasn't started. Read this before any row below — its own text explicitly retracts three earlier "almost done" calls each disproven by the next session's clustered fixes. |
| [interpreter-call-path-in-hot-loops](deep/interpreter-call-path-in-hot-loops.md) | perf §4 | L | The one perf axis where mutsu loses to raku, and the real blocker for the flip: real-`Test` inflates heavy roast files past the 30s budget (`state.t` 67× deficit traced here). Attack row B (file-scope sub call) next. |
| [use-inside-a-block-leaks-to-the-enclosing-scope](tickets/use-inside-a-block-leaks-to-the-enclosing-scope.md) | Test-vendor §1 | M | Remaining env half of import scoping; defeats selective imports in real-Test roast files. |
| [cache-on-a-lazy-seq-must-not-answer-seq](deep/cache-on-a-lazy-seq-must-not-answer-seq.md) | soundness | M | Crash-class: real `is-deeply(Seq,Seq)` recurses to a stack-overflow abort because `.cache` still answers `Seq`. |
| [deferred-seq-materialization-destroys-the-original](deep/deferred-seq-materialization-destroys-the-original.md) | correctness §6 | M | Even `.defined` guts a deferred Seq; breaks any `is $fh.lines, <A B C>` under the real module. |
| [nil-method-warnings-are-not-a-resumable-cx-warn](tickets/nil-method-warnings-are-not-a-resumable-cx-warn.md) | Test-vendor §1 | M | NEW. `Nil.Real`/`.Int`/`.Str` warnings bypass the catchable `CX::Warn` mechanism real `Test::Util`'s `warns-like` needs; narrow residue currently worked around by keeping two files on the native fallback. |

### Campaign: ADR-0019 Phase E — the unified dispatch resolver

**E8 fully closed since the last survey** (E8a/b/c, 2026-08-12: candidates
now carry `level`/`stored_idx`, proto methods folded into `MethodEntry`).
**E9-pre — the mandatory raku-ground-truth campaign — is also done**: 12
scenarios pinned (green under both raku and mutsu), 8 real divergences
found and filed as tickets (the byproduct table below). **E9a landed**
the cross-MRO multi deferral-order fix (two raku-predicted probes, both
exact hits); the `DispatchCursor` struct itself is deferred. **E9b is in
active design as of today** (wrap chains → cursor-prefix entries), already
surfacing two more raku divergences. E5/E6/E7 entry-routing cutover work
continues in parallel (E5b just found a real ~2.4% native-vs-cascade
mismatch rate that blocks a naive "trust the row" cutover). E1/E2(residual)/
E3/E10/E11 remain open.

| Ticket | Axis | Effort | Why here |
|---|---|---|---|
| [adr0019-e8-e11-candidate-sequence-semantics](deep/adr0019-e8-e11-candidate-sequence-semantics.md) | correctness §6 | XL | The live front: E8 closed, E9-pre done, E9a partially landed, E9b design in progress today (2026-08-13). E10 (wraps under the registry generation, killing the global `has_any_wrap_chains` prefilter — a measurable perf win) and E11 (arity-probe retirement) not started. |
| [adr0019-e5-e7-entry-routing](deep/adr0019-e5-e7-entry-routing.md) | correctness §6 | XL | All four E5 measurement slices landed; E5b (`CallMethod` cutover) is mid-flight and just found the native-row candidate does NOT reliably predict the real cascade's outcome (~2.4% mismatch) — a genuine blocker finding that rules out a naive "resolver decides, cascade never runs" cutover shape. |
| [adr0019-e2-e4-resolver-core](deep/adr0019-e2-e4-resolver-core.md) | perf §4 | XL | E4 (one resolver, native rows folded in) is closed; E2b coverage driven ~99% (37904→~400 unmodeled hits) and no longer gates anything (structural fallback replaces the literal-zero gate). Open: E1 TypeId-authoritative cutover, E3 sequence cache (now unblocked). |

**ADR-0019 E8/E9 raku-divergence byproducts** — real, raku-confirmed bugs
the verification campaign surfaced; several already root-caused with a
concrete fix direction, none yet landed (some sibling divergences named in
the design docs, e.g. `role-shadowed-method-in-defer-chain`,
`explicit-child-proto-assumes-parent-candidates`, were apparently fixed
same-day and no longer exist as files — don't go looking for them):

| Ticket | Axis | Effort | Why here |
|---|---|---|---|
| [method-entries-never-covers-unpunned-roles](deep/method-entries-never-covers-unpunned-roles.md) | correctness §6 | L | Found via a real `t/`-suite sweep, not a synthetic probe; feeds 4 production dispatch sites (ctor plan, method cache, private-method resolution, winner selection) — needs a raku-verified per-shape pass before landing. |
| [wrap-chain-skipped-inside-foreign-wrap-dispatch](tickets/wrap-chain-skipped-inside-foreign-wrap-dispatch.md) | correctness §6 | M | E9b design-pass finding (raku-confirmed): the global `is_inside_wrap_dispatch` guard silently drops an unrelated method's wrap chain whenever any other wrap is live. |
| [callsame-in-method-consumes-enclosing-sub-wrap-chain](tickets/callsame-in-method-consumes-enclosing-sub-wrap-chain.md) | correctness §6 | M-L | E9b design-pass finding: cross-stack (sub-wrap vs method) frame-priority bug in `dispatch_next_candidate`; well-scoped fix (E9b's own `dispatch_token` stamping) but no named blocking test yet. |
| [callsame-to-native-mu-methods-nil](tickets/callsame-to-native-mu-methods-nil.md) | correctness §6 | M-L | E9-pre campaign finding: any override of `gist`/`Str`/`raku`/`new` calling `callsame` gets Nil/Any instead of the native Mu implementation — a fairly fundamental OO idiom. |
| [method-wrap-unwrap-restore-noop](tickets/method-wrap-unwrap-restore-noop.md) | correctness §6 | M | E9-pre campaign finding (scenario f'): method wraps can never be removed (`.restore`/`.unwrap` both no-op/throw); interim fix (search-and-remove) is self-contained pending E10a. |
| [proto-method-body-skipped-for-type-object-invocant](tickets/proto-method-body-skipped-for-type-object-invocant.md) | correctness §6 | M | `proto method` bodies with side effects never run for type-object invocants (`P.m(5)`), only for instances. |
| [lastcall-in-wrapper-nextsame-swallows-output](tickets/lastcall-in-wrapper-nextsame-swallows-output.md) | correctness §6 | L | Unscoped probe finding (not yet even a proposed fix); note a sibling `lastcall`-in-wrapper-then-`callsame` divergence was already fixed same-day (#6349) — re-check this one hasn't been folded in before starting. |
| [classhow-lookup-all-candidates-non-multi-mro-gap](tickets/classhow-lookup-all-candidates-non-multi-mro-gap.md) | correctness §6 | S | Split out of the same E7-step-5 introspection work for "one bug per sub-PR" discipline; niche but cheap — bundle with the row below. |
| [classhow-lookup-surfaces-private-methods](tickets/classhow-lookup-surfaces-private-methods.md) | correctness §6 | S | `.^lookup` leaks private methods by bare name; same E7 split as the row above. |
| [nomatch-candidate-signature-slurpy-and-smiley](tickets/nomatch-candidate-signature-slurpy-and-smiley.md) | errors §5 | S | Cosmetic only: duplicate `*%_` and missing `:D`/`:U` in `X::Multi::NoMatch` messages; dispatch itself is already correct. |

### Soundness: blocks the legacy_body-removal architecture goal (ADR-0019 C6e)

| Ticket | Axis | Effort | Why here |
|---|---|---|---|
| [compiled-fns-default-breaks-nested-subs-outside-methods](deep/compiled-fns-default-breaks-nested-subs-outside-methods.md) | soundness | L | ~17 call sites pass an empty `CompiledFns::default()` instead of the real table — nested-sub declarations silently no-op once the `legacy_body` tree-walk fallback is dropped. Confirmed live reproducer; explicitly "do NOT re-attempt dropping legacy_body before this lands." |

### Standalone quick wins (ordinary-code wrong answers / CI health)

| Ticket | Axis | Effort | Why here |
|---|---|---|---|
| [s17-supply-syntax-burns-600-cpu-seconds](tickets/s17-supply-syntax-burns-600-cpu-seconds.md) | perf §4 | M | `S17-supply/syntax.t` burns ~610 CPU-s (11:1 CPU:wall — a busy-wait somewhere in the react runtime) and already produced a jit-stress CI timeout; it is the whole margin of the roast budget. |
| [s17-supply-syntax-gc-stress-budget](deep/s17-supply-syntax-gc-stress-budget.md) | perf §4 / roast §3 | M | NEW sibling of the row above: the same file deterministically blows the gc-stress CI timeout budget (reproduced 4/4), already killed a PR's gc-stress job. |
| [sub-rw-writeback-may-also-leak-attr-shaped-source-into-caller-env](tickets/sub-rw-writeback-may-also-leak-attr-shaped-source-into-caller-env.md) | soundness | S-M | NEW. Unverified sibling of an already-fixed method-path attribute-corruption bug (same shape that broke a real Cro test); soundness axis plus a concrete suggested repro make it worth verifying first. |
| [array-subclass-push-returns-storage-not-self](tickets/array-subclass-push-returns-storage-not-self.md) ⚡ | correctness §6 | S | NEW. Wrong-identity bug on `is Array` subclass `.push` with a concrete fix site named. |
| [class-body-scalar-reassignment-lost](tickets/class-body-scalar-reassignment-lost.md) ⚡ | correctness §6 | S | NEW. A class-body `my $x` write lands under the wrong env key; two named one-line fix directions, found via the CSV campaign but not needed by it. |
| [regex-comment-containing-pipe-char-confuses-top-level-alternation-split](tickets/regex-comment-containing-pipe-char-confuses-top-level-alternation-split.md) ⚡ | correctness §6 | S | NEW. `\|` inside a `#` regex comment is misread as alternation; fix is reusing comment-skip logic already duplicated elsewhere. |
| [role-mixin-hash-attr-default-not-coerced](tickets/role-mixin-hash-attr-default-not-coerced.md) ⚡ | correctness §6 | S | NEW. `has %.h = (...)` default not coerced to Hash on `does`/`but` mixin; one-line fix mirroring 10 other call sites. |
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
| [dist-test-suite-failures-batch](tickets/dist-test-suite-failures-batch.md) | batteries §1 | XL | A triage *queue* — several root causes already pulled out into their own deep/ tickets; remainder: Math::Interval, Native::Overflow, App::SudokuHelper, P5tie, Mathematica::Serializer::Encoder, Hash::Restricted, Crypt::RC4, Random::Choice. |
| [same-role-composed-twice-multi-dispatch-picks-one-candidate](tickets/same-role-composed-twice-multi-dispatch-picks-one-candidate.md) | correctness §6 | M-L | `does R[Int] does R[Str]` composes both multi candidates but every call hits one of them (declaration-order tiebreak, not signature match); roast passes only by luck of exercising one arg type. |
| [mixin-role-order-not-tracked](tickets/mixin-role-order-not-tracked.md) | correctness §6 | L | `MixinOverrides` has no application order, so chained `but`/`does` collisions resolve alphabetically instead of later-wins (`(0 but A) but Z` → mutsu `A`, raku `Z`). Also the E1-V2 nondeterminism the Phase E resolver will consume. |
| [wildcard-handles-loses-to-builtin-cool-methods](tickets/wildcard-handles-loses-to-builtin-cool-methods.md) | correctness §6 | M | `handles *` delegation is consulted only after built-in Cool/Any dispatch, so any delegate method colliding with a builtin never forwards. |
| [when-only-block-nonmatch-value-wrong](tickets/when-only-block-nonmatch-value-wrong.md) | correctness §6 | M | A `.map`/`.grep` block whose only statement is a non-matching `when` falls back to the original topic — `.grep({when Int {True}})` filters nothing. Step 1 is pinning raku's actual non-match value (probes disagree); the fallback is load-bearing for rw map/grep. |
| [role-submethod-runtime-does-parameterized-value](tickets/role-submethod-runtime-does-parameterized-value.md) | correctness §6 | M | A parameterized role's own `$v` parameter is invisible to its BUILD/TWEAK on runtime `does`/`but` targets; needs a case survey of `class_role_param_bindings` reachability first. |
| [native-ctor-gate-reads-is-required-as-type-constraint](tickets/native-ctor-gate-reads-is-required-as-type-constraint.md) | soundness | M | The native-fast-ctor gate binds required-ness where it means to test the attribute's type constraint — wrong fact, effect uncharacterized; mostly repro construction. |
| [whenever-parameter-type-constraint-is-not-enforced](tickets/whenever-parameter-type-constraint-is-not-enforced.md) | errors §5 | M | `whenever $s -> Int $x {}` silently drops the type — cost is diagnosis time (the Cro chunked-body leak surfaced as "No such method" instead of a binding failure). `.tap` already enforces, which narrows the fix. |
| [infix-word-name-is-never-a-listop-call](tickets/infix-word-name-is-never-a-listop-call.md) | correctness §6 | M | A declared sub named after an infix word (`before`, `min`, …) can never be called paren-less; needs the identifier parser to consult the declared-sub table in term position only. Not currently blocking (Cro's `before`/`after` happen to work). |
| [listops-are-not-real-multi-subs](deep/listops-are-not-real-multi-subs.md) | correctness §6 | XL | `splice`/`push`/etc. are special-cased opcodes, not real multi-subs — a user/module `multi` for these names is unreachable. Blocks String::Splice entirely. |
| [sigilless-constant-invisible-in-nested-sub-inside-module](tickets/sigilless-constant-invisible-in-nested-sub-inside-module.md) | correctness §6 | L | `constant \NAME` inside non-unit `module`/`package` invisible to a nested `sub`; blocks all 16 subtests of the RSV dist. Root cause fully traced, needs a design choice. |
| [bare-block-as-infix-operand-not-recognized](deep/bare-block-as-infix-operand-not-recognized.md) | correctness §6 | L | A leading `{ ... }` before an infix never looks ahead to see if it should be a term — blocks PSpec dist's `xxx` custom-operator idiom. |
| [user-postcircumfix-index-not-dispatched-for-instances](deep/user-postcircumfix-index-not-dispatched-for-instances.md) | correctness §6 | XL | A user `multi sub postcircumfix:<[ ]>` is never consulted for `@obj[...]` — real, general, spec'd operator-overload gap; blocks Array::Rounded (16/35 failing). |
| [promise-spawn-segv-under-load](deep/promise-spawn-segv-under-load.md) | soundness | L | Reproducible SEGV (guard-page stack overflow on `Promise.start` threads, ~6-8% under contention); fix direction is a uniform spawned-thread stack budget. |
| [supply-lines-drops-channel-backed-supplies](tickets/supply-lines-drops-channel-backed-supplies.md) | concurrency §5 | M | `.lines` on a real-socket Supply silently emits nothing — the most natural socket idiom. |
| [head-on-a-channel-backed-supply-drops-every-value](tickets/head-on-a-channel-backed-supply-drops-every-value.md) | concurrency §5 | M | Same family: every combinator through `make_supply_from_values` drops channel-backed sources; `.head` is the repro. |
| [procasync-stdout-is-not-incremental](tickets/procasync-stdout-is-not-incremental.md) | concurrency §5 | M | Output only arrives at child exit → parent/child handshakes deadlock; the streaming-reader shape already exists for sockets. |
| [supply-block-lexical-leaks-through-thread-lane](tickets/supply-block-lexical-leaks-through-thread-lane.md) | concurrency §5 | M | Residual cross-thread half of a mostly-fixed lexical-privacy bug; the needed info (`authoritative_captures`) already exists. |
| [schedule-on-live-transform-operators-bypass-deferral](deep/schedule-on-live-transform-operators-bypass-deferral.md) | concurrency §5 | L | NEW. ADR-0028 Supply-scheduling gap: `.map`/`.grep`/`.do`/`.flat` bypass `schedule-on` deferral. Unrelated to ADR-0025 despite superficially similar "captured/deferred" framing. |
| [bare-name-type-constraint-store-is-scope-blind](deep/bare-name-type-constraint-store-is-scope-blind.md) | correctness §6 | L | NEW. Sibling architectural disease to ADR-0025 (a global bare-name map, not a value cell): type constraints on `@`/`%` and mainline blocks still leak cross-scope. |
| [module-file-scope-array-and-hash-still-share-the-caller](tickets/module-file-scope-array-and-hash-still-share-the-caller.md) | Test-vendor §1 | L | Sibling of a fixed scalar bug: a module's file-scope `@`/`%` still shares the caller's env key. Costs a whole roast integration file. |
| [local-tests-rely-on-a-lenient-native-is](tickets/local-tests-rely-on-a-lenient-native-is.md) | Test-vendor §1 | M | Six remaining `t/` files in the "raku fails it too" bucket, each an independent triage. |
| [callframe-line-and-file-come-from-different-frames](tickets/callframe-line-and-file-come-from-different-frames.md) | errors §5 | M | Failure locations under the real `Test` point into unrelated frames — affects `throws-like` reporting in at least 4 sweep files. |
| [eval-context-frame-owns-the-return-target](deep/eval-context-frame-owns-the-return-target.md) | Test-vendor §1 | M | Real `throws-like '<code with return>'` reports "did not die"; three coordinated changes, all specified. |
| [sinking-a-try-blocks-discarded-value-escapes-the-try](tickets/sinking-a-try-blocks-discarded-value-escapes-the-try.md) | Test-vendor §1 | L | Aborts `roast/integration/advent2009-day20.t` after 11/21 assertions under real Test; two independent wrongs. |
| [exception-class-hierarchy-is-mostly-unregistered](deep/exception-class-hierarchy-is-mostly-unregistered.md) | correctness §6 | XL | 124 unregistered `X::` classes; mutsu's own compiler emits one of them. Needs the role-vs-prefix parentage design first. |
| [multi-candidates-declaration-order](tickets/multi-candidates-declaration-order.md) | correctness §6 | M | `&foo.candidates` order is hash-bucket-dependent, not declaration order — can dispatch the wrong candidate. Reader-side sort is trivial but may be cheaper after ADR-0019 Phase E lands (E8 now closed, so this may already be easier — re-check before starting). |
| [parameter-objects-have-no-stable-identity](tickets/parameter-objects-have-no-stable-identity.md) | correctness §6 | M | `Signature.params` builds a fresh `Parameter` every access; the Cro-blocking case already shipped via a narrower replay mechanism, this is the honest cached-Parameter version. |
| [quantified-scalar-regex-interpolation-broken](tickets/quantified-scalar-regex-interpolation-broken.md) | correctness §6 | M-L | NEW. `$s?`/`$s+` never matches — general regex-interpolation gap; the splice approach needs replacing with a real atom/token. Deferred since Text::CSV doesn't need it. |
| [statement-level-begin-side-effects-lost-with-later-vardecl-splits](tickets/statement-level-begin-side-effects-lost-with-later-vardecl-splits.md) | correctness §6 | M | NEW. Silent data loss when a `BEGIN {}` push runs before a later VarDecl split reorders statements; needs bytecode/gdb investigation of container identity. |
| [dollar-dot-dynamic-method-name-should-require-callable](tickets/dollar-dot-dynamic-method-name-should-require-callable.md) | correctness §6 | M | NEW, standalone (not an ADR-0019 byproduct). `.$m()` silently accepts a bare string as a method name instead of requiring Callable/`CALL-ME`. |
| [package-receiver-attribute-accessor-wrong-error](tickets/package-receiver-attribute-accessor-wrong-error.md) | errors §5 | M | NEW, standalone. Classic beginner mistake (`Foo.x` without `.new`) reports "no such method" instead of raku's "did you forget a `.new`" — common pattern. |

## P3 — later

| Ticket | Axis | Effort | Why here |
|---|---|---|---|
| [wasm-start-and-channel-trap](deep/wasm-start-and-channel-trap.md) | batteries §1 | M | Two tutorial-site lessons; small mechanism but the synchronous-`start` semantics need thought. |
| [http-server-tiny-async-serving-remainder](tickets/http-server-tiny-async-serving-remainder.md) | concurrency §5 | L | Humming-Bird is no longer the web target; the general whenever/control-flow bugs it names are tracked in the concurrency family above. |
| [digest-dist-blockers](tickets/digest-dist-blockers.md) | batteries §1 | M | Dist already bundled and ~90% of this file is struck-through "FIXED"; residue is wide-buffer bit accessors and a `with`-modifier placeholder gap. Candidate to trim/archive (see Housekeeping). |
| `nativecall-surface-gaps` (file removed) | batteries §1 | — | RESOLVED-ish: merged 2026-08-10 into `nativecall-pointer-short-name.md` below. |
| [nativecall-pointer-short-name](tickets/nativecall-pointer-short-name.md) | batteries §1 | M | Now absorbs `nativecall-surface-gaps`; only the `NativeCall::Types::` prefix naming (cosmetic `.^name`) remains open. |
| `pointy-block-custom-param-trait-parse-time-check-fails-for-large-modules` (file removed) | batteries §1 | — | RESOLVED 2026-08-12: parser was misparsing `is` trait args on pointy-block params (intermittent "unknown trait" on Cro::HTTP::Router). |
| [procasync-stress-segv](deep/procasync-stress-segv.md) | soundness | L | Real memory unsafety but ~1-in-dozens CI-only, no local repro; *monitor* — the crash reporter now uploads artifacts, wait for the next occurrence. |
| [state-write-through-is-skipped-in-a-jit-compiled-range](tickets/state-write-through-is-skipped-in-a-jit-compiled-range.md) | soundness | M | No deterministic repro today; the `state_vars` rekey half is worth doing on its own merits. |
| [computed-monitor-method-call-in-a-loop-still-leaks-the-topic](tickets/computed-monitor-method-call-in-a-loop-still-leaks-the-topic.md) | correctness §6 | S-M | Only the three-way combination (computed name × monitor × loop) still clobbers `$_`; entry point identified, nothing known blocked (Cro calls monitors by literal name). |
| [closure-capture-shadowed-by-colliding-callee-parameter](deep/closure-capture-shadowed-by-colliding-callee-parameter.md) | correctness §6 | L | Real trap (three ingredients needed) but two narrow fixes already regressed — belongs to the env-layering cluster, do not poke at it narrowly. |
| [stored-regex-loses-its-defining-scope-lexicals](tickets/stored-regex-loses-its-defining-scope-lexicals.md) | correctness §6 | L | Two hard divergences, nothing measured blocked today. |
| [ltm-inline-unbounded-quantifier-vs-array-tie](deep/ltm-inline-unbounded-quantifier-vs-array-tie.md) | correctness §6 | L | On a runtime length tie, rakudo prefers the branch with an inline unbounded quantifier (boundedness-aware NFA); mutsu's two independently-evolved LTM engines both just compare end positions. No known blocked test. |
| [code-var-mention-remakes-the-sub](tickets/code-var-mention-remakes-the-sub.md) | correctness §6 | L | `&f.WHICH` unstable; entangled with `wrap_chains` identity — decide where the canonical Sub lives first (ADR-0019 E10 touches the same store; E10 hasn't started, so still blocked). |
| `role-our-scoped-attribute-not-rejected` (file removed) | errors §5 | — | RESOLVED 2026-08-10: `our $.attr` in a role now dies `X::Declaration::OurScopeInRole` at compile time. |
| [subtest-recompiles-block-from-ast-every-call](tickets/subtest-recompiles-block-from-ast-every-call.md) | perf §4 | M-L | The common `subtest "name" => {…}` call form misses the `SubtestScope` bytecode path and recompiles the block from AST every call — the residual source of nonzero `method_body_runtime_compiles` in subtest-heavy files. Not a correctness bug. |
| [adr0019-d10-precompute-stub-and-swallow-flags](tickets/adr0019-d10-precompute-stub-and-swallow-flags.md) ⚡ | record | S | Zero-behavior-change polish; its own text says "do opportunistically if touching these files for another reason, otherwise skip". |
| [duplicated-prefix-question-mark](tickets/duplicated-prefix-question-mark.md) | roast §3 | M | Single roast test divergence; needs the `Z??`/`X??` CannotMeta sorrow to become the primary diagnosis first. |
| [repeat-call-loses-backtrace-frame](tickets/repeat-call-loses-backtrace-frame.md) | errors §5 | L | Second call loses its frame; wants `RoutineFrame` symbol-interning first (hot path). |
| [module-parse-warning-reported-twice](tickets/module-parse-warning-reported-twice.md) | errors §5 | M | Cosmetic duplicate warning with wrong attribution; fix needs new origin-tracking plumbing. |
| [bare-precedes-placeholder-nested-block](tickets/bare-precedes-placeholder-nested-block.md) | errors §5 | M | False-negative diagnostic only, no miscompile; re-express on the existing placeholder collectors. |
| [two-terms-in-a-row-is-not-a-parse-error](tickets/two-terms-in-a-row-is-not-a-parse-error.md) | errors §5 | M | Missing diagnostic; per-site guard-list re-decisions, and a wrong guard *rejects valid programs* — full roast as review. |
| [test-assertion-trait-is-not-introspectable](deep/test-assertion-trait-is-not-introspectable.md) | Test-vendor §1 | L | Only costs wrong line numbers in failure output; three coupled mechanisms (trait resolution ordering, `.^mixin`, backtrace walk). |
| [our-var-and-its-package-name-are-two-slots](tickets/our-var-and-its-package-name-are-two-slots.md) | roast §3 | L | One roast test; the sound fix is a shared cell (container-representation family) — near-Icebox, listed here because the repro is tiny. |
| [remaining-language-feature-gaps](tickets/remaining-language-feature-gaps.md) | correctness §6 | mixed | A container: multi-line feeds (S) and `exits-ok` (S) are pickable; the typed-exception rows need scope analysis (L each). |
| `typed-buf-native-interop-holes` (file removed) | correctness §6 | — | RESOLVED (closed, no fix needed) 2026-08-10: items 2-4 were already fixed; item 1 re-verified as non-reproducing. |
| [magic-vars-should-be-built-lazily](tickets/magic-vars-should-be-built-lazily.md) | perf §4 | M | Startup metric polish; slice 1 done, profile before designing slice 2. |
| [bench-ctor-construction-parity](tickets/bench-ctor-construction-parity.md) | perf §4 | L | The only bench where mutsu is slower (1.17-1.35×); remaining slices lean on the closure-env-capture-cost Icebox item. |
| [digest-ripemd-start-per-block-overhead](tickets/digest-ripemd-start-per-block-overhead.md) | perf §4 | L | `t/ripemd.t` 295s→119s after major perf work but still exceeds the 120s CI gate margin; profile is now flat, needs one more diminishing-return lever. |
| [yaml-parse-throughput](tickets/yaml-parse-throughput.md) | perf §4 | XL | Correct (81/81) but ~5× raku; next round is structural (ADR-0016 P2/P5), not another call site. |
| [adr0016-p5-match-consumer-inventory](deep/adr0016-p5-match-consumer-inventory.md) | perf §4 | L | The 72-site inventory that gates lazy `Match` (feeds the row above). |
| [c6d-interpreter-body-sites-are-mostly-token-bodies](deep/c6d-interpreter-body-sites-are-mostly-token-bodies.md) | perf §4 | L | Nearly complete: most sub-items landed; remaining scope is grammar token/rule bodies (belongs to ADR-0009/Phase D handoff). |
| [slang-piersing-identifier-name-overrides](tickets/slang-piersing-identifier-name-overrides.md) | batteries §1 | L | NEW. Slangify's `identifier`/`name` rule overrides (trailing `?`/`!`) aren't supported; needs a new parser mode surveyed across many identifier call sites. Not urgent — Slang::Tuxic already provides gate coverage. |
| [pseudo-method-which-why-user-override-ignored-in-bareword-and-dynamic-form](deep/pseudo-method-which-why-user-override-ignored-in-bareword-and-dynamic-form.md) | correctness §6 | L | NEW. `.WHICH`/`.WHY` user overrides ignored except via quoted-literal call; two redundant dispatch mechanisms need auditing together. Real-world usage likely rare. |
| [supply-done-in-tap-callback-load-flaky.t](tickets/supply-done-in-tap-callback-load-flaky.t.md) | concurrency §5 / record | S | NEW. Measured 10/24 failure rate under 24-way parallel load (evidence-standard-satisfying); next step is root-cause or a formal `flaky-tests.txt` quarantine per policy. |

## Icebox — blocked on a design campaign or an explicit decision

| Ticket | Axis | Blocked on |
|---|---|---|
| [needs-env-sync-blanket-removal](deep/needs-env-sync-blanket-removal.md) | perf §4 | Explicitly a fused campaign (lexical-slot + per-slot precision); a narrow probe deterministically broke four pinned mechanisms. De-prioritized 2026-07. |
| [shared-store-bare-name-collision-across-unrelated-frames](deep/shared-store-bare-name-collision-across-unrelated-frames.md) | concurrency §5 | Re-verified 2026-08-13: every concrete instance found so far has still been fixed elsewhere (most recently the multi-param `for` loop + two more env-key fixes brought `http-session-inmemory` to 10/13); no known blocked test drives the store-keying redesign. **Not** the same bug as the (now-resolved) Cro session-counter ticket — that redirected to and was fixed via a different deep ticket entirely; don't conflate the two. |
| [captured-outer-pair-container-alias](deep/captured-outer-pair-container-alias.md) | correctness §6 | ADR-0001 element-cell / container-representation mechanism. |
| [subscript-p-pair-is-a-snapshot-not-a-container](deep/subscript-p-pair-is-a-snapshot-not-a-container.md) | correctness §6 | Same: needs an `array_element_cell` API (ADR-0001); the tempting locals-scan patch is explicitly wrong. |
| [inline-start-blocks-clobber-a-later-declared-variable](tickets/inline-start-blocks-clobber-a-later-declared-variable.md) | correctness §6 | Cell-based capture work (write back only what the thread mutated); no call-site special case allowed. |
| [otf-compilation-gate-leftovers](tickets/otf-compilation-gate-leftovers.md) | perf §4 | Per-call capture cells / caller-slot mechanism; "just remove the gate" frontier is exhausted. |
| [closure-env-capture-cost](deep/closure-env-capture-cost.md) | perf §4 | Two-tier capture + epoch design; belongs with the Slice F env work. Cheap shapes are ruled out as unsound. |
| [cue-loop-lexical-shared-lane-residue](tickets/cue-loop-lexical-shared-lane-residue.md) | concurrency §5 | ADR-0010/Track-B-adjacent per-binding cell mechanism; a loop-redeclared lexical mutated inside a `.cue` callback retains the previous iteration's value. |
| [bundle-json-tiny-instead-of-emulating](tickets/bundle-json-tiny-instead-of-emulating.md) | batteries §1 | A deliberate decision: real JSON::Tiny is >1000× slower on zef's metadata path; JSON::Fast needs 42 `nqp::` ops. Ask the user before moving. |
| [rakuast-remaining](deep/rakuast-remaining.md) | correctness §6 | Multi-campaign backlog (ADR-0011); pick slices by user impact, not cadence. |
| [nativecall-cannot-be-vendored](deep/nativecall-cannot-be-vendored.md) | record | Not actionable — a measurement record with explicit reopen conditions. Keep. |
| [for-loop-rw-element-alias-lost-through-deferred-closure](deep/for-loop-rw-element-alias-lost-through-deferred-closure.md) | soundness | NEW. Array-*element*-level `ContainerRef` aliasing (the old scalar-array-sharing Slice 2b) — needs its own share-vs-bind design at the element-store layer. Superficially ADR-0025-shaped but architecturally unrelated. |
| [element-itemization-lost-in-scalar-binding](deep/element-itemization-lost-in-scalar-binding.md) | correctness §6 | NEW. Store-side Scalar-container itemization for array/hash elements — its own measured campaign, no closure-capture involvement despite the superficial resemblance to the ADR-0025 cluster. |

## Housekeeping

- **13 tickets resolved since 2026-08-10** (mapped by reading the deleting commit / matching `news/` entry):
  - `pointy-block-custom-param-trait-parse-time-check-fails-for-large-modules` → fixed, `news/2026-08/pointy-block-custom-param-trait-parse-time-check-resolved.md`.
  - `concurrent-http-sessions-share-one-instances-count-attribute` → narrowed 2026-08-10 to a `for`-loop sibling-binding bug, fixed via ADR-0023 the same day, `news/2026-08/for-loop-param-binding-provenance-spawn-capture.md`. **Not** the same as the still-open `shared-store-bare-name-collision-across-unrelated-frames` Icebox item — those are separate root causes; don't reconflate them.
  - `constant-declared-from-a-begin-is-rejected` → fixed, `news/2026-08/constant-begin-initializer-readonly.md`.
  - `metaop-over-range-base-is-unsupported` → fixed, `news/2026-08/metaop-range-base.md`.
  - `multi-arg-type-keys-package-collision` → fixed, `news/2026-08/multi-arg-type-keys-package-collision.md`.
  - `named-capture-absent-from-current-match-leaks-stale-value` → fixed, `news/2026-08/named-capture-absent-from-current-match.md`.
  - `nativecall-surface-gaps` → merged into `nativecall-pointer-short-name.md` (still open, see P3).
  - `parameter-type-not-nominalized-for-user-subsets` → fixed, `news/2026-08/parameter-nominalize-user-subsets-and-typecheck-parameter-object.md`.
  - `promise-supply-coercion-drives-react-on-calling-thread` → fixed, `news/2026-08/promise-supply-coercion-async-drive.md`.
  - `role-our-scoped-attribute-not-rejected` → fixed, `news/2026-08/role-our-scoped-attribute-not-rejected.md`.
  - `role-submethod-array-hash-attr-key-mismatch` → fixed, `news/2026-08/role-submethod-array-hash-attr-key.md`.
  - `slurpy-hash-param-in-start-block-reads-stale-value-across-sequential-calls` → fixed, `news/2026-08/slurpy-param-mask-survives-into-spawned-thread-body.md` (don't confuse with the distinct sibling fix `news/2026-08/hash-slurpy-param-thread-mask.md`).
  - `typed-buf-native-interop-holes` → closed without a fix; item 1 re-verified as non-reproducing, nothing to record.
- **Two previously-P1 Cro tickets likely resolved but NOT re-verified this pass — verify and close, don't leave them stale:**
  - [regex-alternation-ltm-longest-literal-prefix](deep/regex-alternation-ltm-longest-literal-prefix.md) — [ADR-0022](../docs/adr/0022-regex-alternation-ltm-ranking.md) is now "Accepted; all five slices implemented and merged" (2026-08-09), but this ticket file's own text hasn't been updated to say so. Re-run the named Cro repro (`http-router.rakutest` test 61) and the 3 `S05-metasyntax/longest-alternative.t` subtests; if green, retire to `news/`.
  - [pair-namedness-is-a-value-property-not-a-call-site-property](deep/pair-namedness-is-a-value-property-not-a-call-site-property.md) — [ADR-0021](../docs/adr/0021-argument-namedness-is-a-call-site-property.md) is now "Accepted (P1-P3a and P3 shipped; P4 cleanup and P5 measured perf follow-up remain)". This ticket named P1 alone as the fix for the live Cro `headers => [...]` blocker in `Cro::HTTP::Client`; re-run that repro (`tmp/hdr2.p6`-shaped case). If fixed, re-scope the file down to just the P4/P5 cleanup remainder (demote to P3) or retire it.
- **Stale ADR-0019 Phase D design docs (8 files in `deep/`)** — still present, still
  done-stale (D0-D10 all closed; news has the landed entries):
  `adr0019-d2c-attribute-default-chunks`, `adr0019-d2-remainder-attr-plan-lowering`,
  `adr0019-d3-8-method-body-main-pass-compilation`, `adr0019-d4-parent-expr-chunks`,
  `adr0019-d5-plan-driven-how-ops`, `adr0019-d6-d9-legacy-body-removal`,
  `adr0019-d7-d8-role-plan-encoding`, `adr0019-e1-typeid-receiver-owner` — should be
  retired to `news/` **after extracting the unfiled spin-offs they carry**:
  - D3-8's follow-up list: **methods of a class declared inside a sub cannot
    see the sub's lexicals** (real conformance bug, raku 42 / mutsu 0),
    the `record_type_body_captures` double-compile, the
    `class_dispatch.rs:497` per-call recompile, and `augment class` having no
    declaration plan.
  - D7/D8's V2 rider: the once-per-composition memo keying (`pun:`/`mixin:`
    role-global, class path unguarded) still needs its raku case table.
  - D2c-5 (optional): unify the two attribute-default env-setup shapes.
  - E1's V2 (mixin order nondeterminism) is already filed as
    [mixin-role-order-not-tracked](tickets/mixin-role-order-not-tracked.md).
  - A new one this round: `adr0019-e4b-should-bypass-native-fastpath-decomposition.md`
    also exists in `deep/` and reads like a D-phase-style completed-slice design
    doc for the E4b box — not yet surveyed for spin-offs; do that before retiring it.
- Container tickets that are queues, not single fixes:
  [dist-test-suite-failures-batch](tickets/dist-test-suite-failures-batch.md),
  [remaining-language-feature-gaps](tickets/remaining-language-feature-gaps.md),
  [digest-dist-blockers](tickets/digest-dist-blockers.md). Pull one row out,
  fix it as its own PR, and note it in the container file only if the row list
  changes.
- Near-resolved residue files worth a trim/close pass in a future session
  (not done here — this is an index regen, not a cleanup pass):
  `digest-dist-blockers.md` (~90% struck-through FIXED sections),
  `template-mojo-residual-failures.md` (only open item duplicates
  `rule-sigspace-does-not-consume-trailing-whitespace.md`),
  `c6d-interpreter-body-sites-are-mostly-token-bodies.md` (only C6d-2 and a
  Phase-D handoff remain), `nativecall-surface-gaps.md` (already merged, could
  be deleted rather than left as a redirect stub).
