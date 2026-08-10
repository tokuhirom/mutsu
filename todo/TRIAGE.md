# TRIAGE — prioritized snapshot of todo/ (2026-08-10)

A ranked index of every open finding under `todo/tickets/` and `todo/deep/`,
so a session can pick the next ticket without re-reading all of them.

This is a **snapshot, not a ledger**. Resolving a ticket does *not* require
editing this file — that would reintroduce exactly the shared-file merge
conflicts `todo/` exists to avoid. A stale row is fine; the per-ticket files
stay the source of truth. Regenerate the whole file when it has drifted too
far (re-survey every ticket, re-score, rewrite).

Surveyed 2026-08-10: **111 files** (41 `deep/`, 70 `tickets/`). Extreme churn
since the 2026-08-07 survey (77 files): **38 resolved** and moved to `news/`
(most filed-and-fixed within the window — the 16-ticket Cro diagnosis cluster
largely burned down, plus a stream of ADR-0019 Phase D/E byproduct fixes) and
**37 newly filed** (the ADR-0019 D/E design docs, Cro root-cause tickets, and
ADR-0019 spin-off correctness bugs).

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

Three campaigns dominate this tier; most P1 rows are named blockers of one of
them, so progress compounds.

### Campaign: Cro (the web-framework battery slot, PLAN §1 B1)

The 2026-08-08 diagnosis sweep filed 16 tickets and most are already fixed;
these are the survivors, each root-caused with a named failing Cro test.

| Ticket | Axis | Effort | Why here |
|---|---|---|---|
| [slurpy-hash-param-in-start-block-reads-stale-value-across-sequential-calls](tickets/slurpy-hash-param-in-start-block-reads-stale-value-across-sequential-calls.md) | batteries §1 | M | Root cause of both remaining Cro::Core failures (`composer.rakutest` 134, `connection-conditional.rakutest` 23): `*%options` inside a nested `start {}` reads the first call's value because the slurpy mask does not survive `clone_for_thread`. Strongest-researched ticket in the tier — two named hypotheses, a prescribed gdb step, and a fix direction. |
| [promise-supply-coercion-drives-react-on-calling-thread](tickets/promise-supply-coercion-drives-react-on-calling-thread.md) | concurrency §5 | XL | `Promise(supply {…})` runs the react inline instead of returning a Planned promise → deadlock; blocks `http-response-parser.rakutest` 111/120 and likely `http2-request-parser.rakutest` 44. Root cause fully identified; wants a `SupplyDrivePolicy::Promise` caller survey first. |
| [named-parameter-type-constraints-are-not-enforced](tickets/named-parameter-type-constraints-are-not-enforced.md) | correctness §6 | M | Type constraints on *named* params are parsed then ignored (positionals are checked) — Cro router's 400 Bad Request branch is unreachable, bad requests fall through to 404. |
| [parameter-type-not-nominalized-for-user-subsets](tickets/parameter-type-not-nominalized-for-user-subsets.md) | correctness §6 | M | `Parameter.type` reports a user subset itself instead of nominalizing to the base type + constraints (builtin `UInt` already does this); user-subset-typed Cro route params die in the route compiler. Blocker is plumbing, not semantics. |
| [concurrent-http-sessions-share-one-instances-count-attribute](tickets/concurrent-http-sessions-share-one-instances-count-attribute.md) | concurrency §5 | L | Two concurrent sessions with distinct cookies increment one shared attribute — blocks `http-session-inmemory` / `http-session-persistent` subtests 8-9. Investigation-first: the isolated monitor repro does NOT reproduce; needs the real router pipeline. |
| [pair-namedness-is-a-value-property-not-a-call-site-property](deep/pair-namedness-is-a-value-property-not-a-call-site-property.md) | soundness | XL | Now an [ADR-0021](../docs/adr/0021-argument-namedness-is-a-call-site-property.md) campaign with phases P1→P5; the hash-derived-pairs half already landed. **P1 alone is S** (one method-path normalization) and fixes the live Cro blocker: `headers => [...]` still dies in `Cro::HTTP::Client`. |
| [regex-alternation-ltm-longest-literal-prefix](deep/regex-alternation-ltm-longest-literal-prefix.md) | correctness §6 | L | `\|` alternation ranks by actual match length instead of rakudo's literal-prefix (litlen) ranking; blocks Cro `http-router.rakutest` 61 + 3 subtests of `S05-metasyntax/longest-alternative.t`. **Best-prepared open item**: ADR-0022 design is complete with an acceptance matrix — three implementation slices remain. |
| [async-listener-not-freed-when-relistening-in-a-loop](tickets/async-listener-not-freed-when-relistening-in-a-loop.md) | batteries §1 | L | Third+ round of re-binding a port to a Cro server gets empty bodies; blocks the multi-server auth/session/log-file suite family. |
| [for-multi-param-array-hash-shadow-clobbers-outer-container](tickets/for-multi-param-array-hash-shadow-clobbers-outer-container.md) | correctness §6 | L | Broadened 2026-08-08: the scalar fix only covered same-frame slots, so scalar-no-slot AND `@`/`%` cases still silently corrupt an outer lexical. Cheap paths verified dead; the real fix is making the multi-param bind a genuine per-iteration declaration in `build_for_bind_stmts`, which fixes all four variants uniformly. |

### Campaign: vendor the real `Test` module (PLAN §1, batteries policy)

| Ticket | Axis | Effort | Why here |
|---|---|---|---|
| [vendor-real-test-module](tickets/vendor-real-test-module.md) | Test-vendor §1 | XL | The campaign header: regression count driven from 343 down to a handful of remaining files. Read this before any row below. |
| [interpreter-call-path-in-hot-loops](deep/interpreter-call-path-in-hot-loops.md) | perf §4 | L | The one perf axis where mutsu loses to raku, and the real blocker for the flip: real-`Test` inflates heavy roast files past the 30s budget (`state.t` 67× deficit traced here). Attack row B (file-scope sub call) next. |
| [use-inside-a-block-leaks-to-the-enclosing-scope](tickets/use-inside-a-block-leaks-to-the-enclosing-scope.md) | Test-vendor §1 | M | Remaining env half of import scoping; defeats selective imports in real-Test roast files. (The `use fatal` leak sibling was fixed since the last survey.) |
| [cache-on-a-lazy-seq-must-not-answer-seq](deep/cache-on-a-lazy-seq-must-not-answer-seq.md) | soundness | M | Crash-class: real `is-deeply(Seq,Seq)` recurses to a stack-overflow abort because `.cache` still answers `Seq`. |
| [deferred-seq-materialization-destroys-the-original](deep/deferred-seq-materialization-destroys-the-original.md) | correctness §6 | M | Even `.defined` guts a deferred Seq; breaks any `is $fh.lines, <A B C>` under the real module. |

### Campaign: ADR-0019 Phase E — the unified dispatch resolver

Phase D (declaration plans, legacy_body removal) **completed** since the last
survey; the D-phase design docs are stale (see Housekeeping). Phase E is the
live front. Hard gate: neither E4b nor E3 may land while `native_call_unmodeled`
or `resolver_shadow_mismatches` is nonzero — so E2b row coverage is the
critical path for everything downstream.

| Ticket | Axis | Effort | Why here |
|---|---|---|---|
| [adr0019-e2-e4-resolver-core](deep/adr0019-e2-e4-resolver-core.md) | perf §4 | XL | Partially done: E4a shadow parity + four E2b slices landed (`native_call_unmodeled` 37904→4377). Open: remaining E2b rows (mechanical, independently landable), then E4b authoritative switch and the E3 sequence cache. |
| [adr0019-e5-e7-entry-routing](deep/adr0019-e5-e7-entry-routing.md) | correctness §6 | XL | Blocked on E4b/E3, but the E5a traffic measurement and the raku ground-truth pins for its **three recorded dispatch divergences** (dynamic-mut has no native probe, dynamic hyper lacks the user-method gate, `ArrayPush` ignores `augment`) are actionable now. |
| [adr0019-e8-e11-candidate-sequence-semantics](deep/adr0019-e8-e11-candidate-sequence-semantics.md) | correctness §6 | XL | Blocked on E5-E7, but the mandatory E9-pre 13-case raku verification campaign and the `.unwrap`/restore stale-chain leak fix are independent. Also kills the `has_any_wrap_chains()` global prefilter (perf win for any program using `.wrap`). |

### Soundness: blocks the legacy_body-removal architecture goal (ADR-0019 C6e)

| Ticket | Axis | Effort | Why here |
|---|---|---|---|
| [compiled-fns-default-breaks-nested-subs-outside-methods](deep/compiled-fns-default-breaks-nested-subs-outside-methods.md) | soundness | L | ~17 call sites pass an empty `CompiledFns::default()` instead of the real table — nested-sub declarations silently no-op once the `legacy_body` tree-walk fallback is dropped. Confirmed live reproducer; explicitly "do NOT re-attempt dropping legacy_body before this lands." |

### Standalone quick wins (ordinary-code wrong answers / CI health)

| Ticket | Axis | Effort | Why here |
|---|---|---|---|
| [s17-supply-syntax-burns-600-cpu-seconds](tickets/s17-supply-syntax-burns-600-cpu-seconds.md) | perf §4 | M | `S17-supply/syntax.t` burns ~610 CPU-s (11:1 CPU:wall — a busy-wait somewhere in the react runtime) and already produced a jit-stress CI timeout; it is the whole margin of the roast budget. Explicitly not quarantinable; starts with a cheap `perf record`. |
| [metaop-over-range-base-is-unsupported](tickets/metaop-over-range-base-is-unsupported.md) ⚡ | correctness §6 | S | `Z..`/`X..` parse fine but the metaop runtime handler has no Range entry — ordinary Raku silently unusable. |
| [multi-arg-type-keys-package-collision](tickets/multi-arg-type-keys-package-collision.md) ⚡ | soundness | S | Every bare type-object argument shares the literal `"Package"` dispatch-cache key. Fix is one `ValueView` arm; producing the end-to-end repro is the real work. |
| [role-submethod-array-hash-attr-key-mismatch](tickets/role-submethod-array-hash-attr-key-mismatch.md) ⚡ | correctness §6 | S | Role BUILD/TWEAK writes to `@!a`/`%!h` silently no-op on runtime `does`/`but` targets — env keys are seeded scalar-only. Fix sketch is concrete. |
| [constant-declared-from-a-begin-is-rejected](tickets/constant-declared-from-a-begin-is-rejected.md) ⚡ | correctness §6 | S-M | `constant E = BEGIN 5;` dies "Cannot assign to a readonly variable" — the memoized BEGIN store lands as a second write. Two precise sites named. |
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
| [dist-test-suite-failures-batch](tickets/dist-test-suite-failures-batch.md) | batteries §1 | XL | A triage *queue* — several root causes already pulled out into their own deep/ tickets; remainder: Math::Interval, Native::Overflow, App::SudokuHelper, P5tie, Mathematica::Serializer::Encoder, Hash::Restricted, Crypt::RC4, Random::Choice. |
| [same-role-composed-twice-multi-dispatch-picks-one-candidate](tickets/same-role-composed-twice-multi-dispatch-picks-one-candidate.md) | correctness §6 | M-L | `does R[Int] does R[Str]` composes both multi candidates but every call hits one of them (declaration-order tiebreak, not signature match); roast passes only by luck of exercising one arg type. |
| [mixin-role-order-not-tracked](tickets/mixin-role-order-not-tracked.md) | correctness §6 | L | `MixinOverrides` has no application order, so chained `but`/`does` collisions resolve alphabetically instead of later-wins (`(0 but A) but Z` → mutsu `A`, raku `Z`). Also the E1-V2 nondeterminism the Phase E resolver will consume. |
| [wildcard-handles-loses-to-builtin-cool-methods](tickets/wildcard-handles-loses-to-builtin-cool-methods.md) | correctness §6 | M | `handles *` delegation is consulted only after built-in Cool/Any dispatch, so any delegate method colliding with a builtin never forwards. |
| [when-only-block-nonmatch-value-wrong](tickets/when-only-block-nonmatch-value-wrong.md) | correctness §6 | M | A `.map`/`.grep` block whose only statement is a non-matching `when` falls back to the original topic — `.grep({when Int {True}})` filters nothing. Step 1 is pinning raku's actual non-match value (probes disagree); the fallback is load-bearing for rw map/grep. |
| [also-does-role-bracket-args-dropped-in-class-body](tickets/also-does-role-bracket-args-dropped-in-class-body.md) | correctness §6 | L | `also does Role[Args];` drops the bracket args AND skips `compose_role_into_class` entirely (loses attrs, defaults, traits). Substantially larger than the parser one-liner it looks like. |
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
| [module-file-scope-array-and-hash-still-share-the-caller](tickets/module-file-scope-array-and-hash-still-share-the-caller.md) | Test-vendor §1 | L | Sibling of a fixed scalar bug: a module's file-scope `@`/`%` still shares the caller's env key. Costs a whole roast integration file. |
| [local-tests-rely-on-a-lenient-native-is](tickets/local-tests-rely-on-a-lenient-native-is.md) | Test-vendor §1 | M | Six remaining `t/` files in the "raku fails it too" bucket, each an independent triage. |
| [callframe-line-and-file-come-from-different-frames](tickets/callframe-line-and-file-come-from-different-frames.md) | errors §5 | M | Failure locations under the real `Test` point into unrelated frames — affects `throws-like` reporting in at least 4 sweep files. |
| [eval-context-frame-owns-the-return-target](deep/eval-context-frame-owns-the-return-target.md) | Test-vendor §1 | M | Real `throws-like '<code with return>'` reports "did not die"; three coordinated changes, all specified. |
| [sinking-a-try-blocks-discarded-value-escapes-the-try](tickets/sinking-a-try-blocks-discarded-value-escapes-the-try.md) | Test-vendor §1 | L | Aborts `roast/integration/advent2009-day20.t` after 11/21 assertions under real Test; two independent wrongs. |
| [exception-class-hierarchy-is-mostly-unregistered](deep/exception-class-hierarchy-is-mostly-unregistered.md) | correctness §6 | XL | 124 unregistered `X::` classes; mutsu's own compiler emits one of them. Needs the role-vs-prefix parentage design first. |
| [expression-position-my-has-no-scope](tickets/expression-position-my-has-no-scope.md) | correctness §6 | L | Expression-position `my` has no scope at all (silent lexical leak); one roast test currently passes *because* of the bug. |
| [multi-candidates-declaration-order](tickets/multi-candidates-declaration-order.md) | correctness §6 | M | `&foo.candidates` order is hash-bucket-dependent, not declaration order — can dispatch the wrong candidate. Reader-side sort is trivial but may be cheaper after ADR-0019 Phase E lands. |
| [parameter-objects-have-no-stable-identity](tickets/parameter-objects-have-no-stable-identity.md) | correctness §6 | M | `Signature.params` builds a fresh `Parameter` every access; the Cro-blocking case already shipped via a narrower replay mechanism, this is the honest cached-Parameter version. |

## P3 — later

| Ticket | Axis | Effort | Why here |
|---|---|---|---|
| [wasm-start-and-channel-trap](deep/wasm-start-and-channel-trap.md) | batteries §1 | M | Two tutorial-site lessons; small mechanism but the synchronous-`start` semantics need thought. |
| [http-server-tiny-async-serving-remainder](tickets/http-server-tiny-async-serving-remainder.md) | concurrency §5 | L | Humming-Bird is no longer the web target; the general whenever/control-flow bugs it names are tracked in the concurrency family above. |
| [digest-dist-blockers](tickets/digest-dist-blockers.md) | batteries §1 | M | Dist already bundled and ~90% of this file is struck-through "FIXED"; residue is wide-buffer bit accessors and a `with`-modifier placeholder gap. Candidate to trim/archive (see Housekeeping). |
| [nativecall-surface-gaps](tickets/nativecall-surface-gaps.md) ⚡ | batteries §1 | S | Only the `NativeCall::Types::` prefix naming remains open; duplicate of the row below. |
| [nativecall-pointer-short-name](tickets/nativecall-pointer-short-name.md) | batteries §1 | M | Cosmetic `.^name`; must be one deliberate slice with the row above (candidate merge — see Housekeeping). |
| [pointy-block-custom-param-trait-parse-time-check-fails-for-large-modules](deep/pointy-block-custom-param-trait-parse-time-check-fails-for-large-modules.md) | batteries §1 | M | **Watch-only by its own protocol**: the ParseMemo soundness hole found en route is fixed (generation-keyed, 2026-08-09) and four varied rebuilds have held at 64/83; if the 0/83 flip recurs on a post-fix binary, the memo theory is refuted and the CLI-parse-path investigation starts. |
| [procasync-stress-segv](deep/procasync-stress-segv.md) | soundness | L | Real memory unsafety but ~1-in-dozens CI-only, no local repro; *monitor* — the crash reporter now uploads artifacts, wait for the next occurrence. |
| [state-write-through-is-skipped-in-a-jit-compiled-range](tickets/state-write-through-is-skipped-in-a-jit-compiled-range.md) | soundness | M | No deterministic repro today; the `state_vars` rekey half is worth doing on its own merits. |
| [computed-monitor-method-call-in-a-loop-still-leaks-the-topic](tickets/computed-monitor-method-call-in-a-loop-still-leaks-the-topic.md) | correctness §6 | S-M | Only the three-way combination (computed name × monitor × loop) still clobbers `$_`; entry point identified, nothing known blocked (Cro calls monitors by literal name). |
| [closure-capture-shadowed-by-colliding-callee-parameter](deep/closure-capture-shadowed-by-colliding-callee-parameter.md) | correctness §6 | L | Real trap (three ingredients needed) but two narrow fixes already regressed — belongs to the env-layering cluster, do not poke at it narrowly. |
| [stored-regex-loses-its-defining-scope-lexicals](tickets/stored-regex-loses-its-defining-scope-lexicals.md) | correctness §6 | L | Two hard divergences, nothing measured blocked today. |
| [ltm-inline-unbounded-quantifier-vs-array-tie](deep/ltm-inline-unbounded-quantifier-vs-array-tie.md) | correctness §6 | L | On a runtime length tie, rakudo prefers the branch with an inline unbounded quantifier (boundedness-aware NFA); mutsu's two independently-evolved LTM engines both just compare end positions. No known blocked test; fold into the ADR-0022 campaign rather than fixing standalone. |
| [code-var-mention-remakes-the-sub](tickets/code-var-mention-remakes-the-sub.md) | correctness §6 | L | `&f.WHICH` unstable; entangled with `wrap_chains` identity — decide where the canonical Sub lives first (ADR-0019 E10 touches the same store). |
| [role-our-scoped-attribute-not-rejected](tickets/role-our-scoped-attribute-not-rejected.md) ⚡ | errors §5 | S | `our $.attr` in a role should die `X::Declaration::OurScopeInRole` at compile time; the `our_scope_violation` mechanism already exists, needs one more scan arm plus a raku-verified case table first. |
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
| [bare-package-symbolic-deref-and-stash-routines](tickets/bare-package-symbolic-deref-and-stash-routines.md) | roast §3 | M | `pseudo-6e.t` only; needs a semantics decision (SymbolicDeref vs stash-index) first. |
| [typed-buf-native-interop-holes](tickets/typed-buf-native-interop-holes.md) ⚡ | correctness §6 | S | Items 2-4 already fixed; item 1 doesn't currently reproduce — low-value residue, candidate to close (see Housekeeping). |
| [magic-vars-should-be-built-lazily](tickets/magic-vars-should-be-built-lazily.md) | perf §4 | M | Startup metric polish; slice 1 done, profile before designing slice 2. |
| [bench-ctor-construction-parity](tickets/bench-ctor-construction-parity.md) | perf §4 | L | The only bench where mutsu is slower (1.17-1.35×); remaining slices lean on the closure-env-capture-cost Icebox item. |
| [digest-ripemd-start-per-block-overhead](tickets/digest-ripemd-start-per-block-overhead.md) | perf §4 | L | `t/ripemd.t` 295s→119s after major perf work but still exceeds the 120s CI gate margin; profile is now flat, needs one more diminishing-return lever. |
| [yaml-parse-throughput](tickets/yaml-parse-throughput.md) | perf §4 | XL | Correct (81/81) but ~5× raku; next round is structural (ADR-0016 P2/P5), not another call site. |
| [adr0016-p5-match-consumer-inventory](deep/adr0016-p5-match-consumer-inventory.md) | perf §4 | L | The 72-site inventory that gates lazy `Match` (feeds the row above). |
| [c6d-interpreter-body-sites-are-mostly-token-bodies](deep/c6d-interpreter-body-sites-are-mostly-token-bodies.md) | perf §4 | L | Nearly complete: most sub-items landed; remaining scope is grammar token/rule bodies (belongs to ADR-0009/Phase D handoff). |

## Icebox — blocked on a design campaign or an explicit decision

| Ticket | Axis | Blocked on |
|---|---|---|
| [needs-env-sync-blanket-removal](deep/needs-env-sync-blanket-removal.md) | perf §4 | Explicitly a fused campaign (lexical-slot + per-slot precision); a narrow probe deterministically broke four pinned mechanisms. De-prioritized 2026-07. |
| [shared-store-bare-name-collision-across-unrelated-frames](deep/shared-store-bare-name-collision-across-unrelated-frames.md) | concurrency §5 | Its own instruction: every concrete instance has since been fixed elsewhere — **re-measure before starting**, and the per-lineage store redesign needs an ADR. The two downstream tickets (supply-block thread lane, cue-loop residue) are the cheap way to re-establish whether it still drives failures. |
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

- **Stale ADR-0019 Phase D design docs (8 files in `deep/`)** — the phase
  completed since the last survey (D0-D10 all closed; news has the landed
  entries), so `adr0019-d2c-attribute-default-chunks`,
  `adr0019-d2-remainder-attr-plan-lowering`,
  `adr0019-d3-8-method-body-main-pass-compilation`,
  `adr0019-d4-parent-expr-chunks`, `adr0019-d5-plan-driven-how-ops`,
  `adr0019-d6-d9-legacy-body-removal`, `adr0019-d7-d8-role-plan-encoding`,
  and `adr0019-e1-typeid-receiver-owner` are done-stale and should be retired
  to `news/` — **after extracting the unfiled spin-offs they carry**:
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
- Resolved and moved to `news/` since the 2026-08-05/07 surveys — 38 files,
  including almost the whole 2026-08-08 Cro diagnosis cluster
  (`cro-client-cannot-read-a-chunked-response-body`,
  `cro-server-drops-a-quarter-of-in-process-requests`,
  `cro-tcp-connector-real-socket-response-lost`,
  `cro-session-tests-get-an-empty-response-body`,
  `cro-middleware-await-body-text-dies-coercing-any-into-promise`,
  `cro-client-timeout-policy-attribute-still-corrupted`,
  `given-block-binding-is-clobbered-by-a-cro-request`,
  `hpack-module-body-lexical-leaks-into-an-unrelated-frame`,
  `nested-sub-emit-leaks-into-the-outer-supply`,
  `stream-consumer-delivery-not-cross-thread-safe`, …) plus
  `use-fatal-leaks-out-of-a-sub-or-do-block`, `lexical-sub-lost-after-routine-return`,
  `supplier-preserving-backlog-destroyed-by-done-immutable-lane`,
  `sibling-thread-my-array-merges-through-root-atomic-lane`,
  `dynamic-var-leaks-via-start-shared-vars`, and a dozen smaller
  filed-and-fixed tickets.
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
