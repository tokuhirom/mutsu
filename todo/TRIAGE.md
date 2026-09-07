# TRIAGE — prioritized snapshot of todo/ (2026-09-06)

A ranked index of every open finding under `todo/tickets/`, `todo/deep/` and
`todo/perf/`, so a session can pick the next unit of work without re-reading
all of them.

This is a **snapshot, not a ledger**. Resolving a ticket does *not* require
editing this file — that would reintroduce exactly the shared-file merge
conflicts `todo/` exists to avoid. A stale row is fine; the per-ticket files
stay the source of truth. Regenerate the whole file when it has drifted too
far (re-survey every ticket, re-score, rewrite).

## What changed since the 2026-09-04 regen (`cfba3c0d7`)

Surveyed at `884366c95`: **75 files** — 39 `deep/`, 20 `tickets/`, 16 `perf/`
(was 71: 44/15/12).

Two working days replaced the snapshot almost completely.

- **Every row of the previous top is closed.** All four Tier S rows and *all
  fifteen* `todo/tickets/` entries merged (#7296-#7380), plus nine `deep/`
  files. Twenty-four closures in two days. Do not look for the old rows here;
  they are in `news/2026-09/`.
- **`tickets/` refilled from a different source.** The new tickets are not a
  random scatter: most came from the **2026-09-06 full doc-diff re-sweep**
  (`docs/doc-diff-backlog.md` — high-signal 296 → **108**, `mismatch` 184 → 74,
  `crash` 112 → 34), triaged file-by-file into tickets. That backlog is now the
  main feeder of `todo/tickets/`, and its two highest-signal files
  (`Language/operators`, `Language/structures`) have been drained; the next
  untriaged files are `Type/Any`, `Language/experimental`, `Language/objects`.
- **`ADR-0068` was filed and is the only Tier S owner.** The residue of the
  closed `procasync-stress-segv` became
  [ADR-0068](../docs/adr/0068-cross-thread-container-writes-need-a-synchronized-store.md)
  (`Proposed`), which owns *both* remaining Tier S rows, carries a **calibrated
  reproduction harness**, and rejects the obvious remedy on the record. It is
  the highest-value un-owned campaign in the repo.
- **The perf picture inverted.** `bench-fib` went 0.84x raku (2026-09-04) to
  **0.37x** at `87e910a62` (JIT row 0.20x); `hash-access` 0.18x,
  `string-concat` 0.10x. The rows still above 0.5x are `method-call` (0.92x),
  `time-parts` (1.12x) and `debug-guard` (0.76x). Four *new* perf files were
  filed inside that campaign and are its live continuation.
- **The `Test`-provider retirement is one perf slice from done.** The vendored
  `Test` moved `roast/S03-buf/write-int.t` from 48.0s to **29.5s** against a
  hard 30s gate (rakudo 3.49s, native provider 5.04s). Its file states
  completion criterion 2 is **not** met and names its own next targets.

### What was re-verified for this regen

Every one of the 75 files was read and, where it had a runnable repro, run
against `raku` v2026.07 on a fresh `target/debug/mutsu` at `884366c95`. Of the
20 tickets, **19 reproduce as written**. Three `deep/` rows moved and are
called out inline. Four claims were re-run by hand rather than taken from the
survey:

- `deep/module-file-scope-array-and-hash-still-share-the-caller` — its ADR-0039
  §6 acceptance repro is now **byte-identical to raku** (`inner=[3]` /
  `[1 2 9]`). `da8e94252` (the ADR-0055 unvouched-capture cell) appears to have
  closed it. Moved to Icebox pending a re-check of its two named roast rows.
- `deep/call-compiled-closure-lacks-merge-all-...` — its `CALLER`/`OUTER` repro
  also matches raku now. Only structural residue remains.
- The `.WHICH` ticket pair — `===` and `set()` membership already agree with
  raku, so only the `.WHICH` *method string* is wrong. That makes the cluster
  cheap and low-risk, which is why it is recommended below.

**Standing caveat.** A tier is a routing hint. This is one run on one box;
CLAUDE.md's rule still applies — re-verify a ticket's repro on your own build
before acting on it, and read the *tail* of a ticket file, where successive
"Re-verified" sections accumulate (and, as the `$.x` row above shows, can
themselves rot).

## How the ranking works

- **Tier S — Soundness.** Crashes (SEGV/panic/stack overflow), memory
  unsafety, or *silent data loss* — a write that is dropped, or that lands
  somewhere other than where it was aimed, with nothing detecting it. Always
  highest priority regardless of effort.
- **Tier B — Correctness, broad impact.** A wrong answer or missing
  capability in a common construct, or a bug that blocks an entire
  dist/battery. B1 = broad language-construct correctness; B2 =
  batteries/dist-blocking.
- **Tier N — Correctness, narrow impact / diagnostics.** Wrong answer in a
  rare construct with no known blocked test, or a wrong/missing *error
  message*, or mutsu accepting code raku rejects.
- **Perf.** Batched into their own profiling-heavy session; the
  implementation agent for a perf item **runs solo**.
- **Icebox.** Blocked on a design decision or an explicit user call, or a
  pure decision/measurement/cleanup record with no failing repro.

**Effort** (S/M/L/XL) is shown but does not change tier.

---

## Tier S — Soundness (crashes, silent data loss)

| Ticket | Breadth | Effort | Verified 2026-09-06 |
|---|---|---|---|
| [gc-contents-mut-cross-thread-aliased-writes](deep/gc-contents-mut-cross-thread-aliased-writes.md) | 149 `gc_contents_mut` call sites; measured on `.tap`, `Promise.then`, `Thread.start` | XL | **Record, not a repro** — the route audit and the harness behind the row above. Failure shapes observed: `free(): double free detected in tcache 2`, `double free or corruption (out)` with a core dump, SIGILL, and silent lost updates. One `roast/S17-procasync/stress.t` SIGSEGV remains explicitly unexplained. Read this before writing any code against ADR-0068. |
| [baggy-addition-panics-on-a-bigint-bag-weight](tickets/baggy-addition-panics-on-a-bigint-bag-weight.md) | every baggy operator — `(+) (-) (&) (^) (.)` — with a weight past `i64::MAX` | L | **Confirmed**: `say (a => 10**30).Bag (+) (a => 1).Bag` panics at `src/vm/vm_set_arith_ops.rs:60` (`attempt to add with overflow`); raku answers `Bag(a(1000000000000000000000000000001))`. **In release it does not panic — it wraps**, so this is a crash in debug and silent numeric corruption in the shipped build. Root cause: weights are coerced into an `i64`-keyed map *before* the operator body runs, so precision is already gone. Sits in the neighbourhood the 2026-09-06 Mix numeric-tower fix (`771504161`) just corrected — reuse that rule. |

---

## `todo/tickets/` — all 20

Nineteen of the twenty reproduce exactly as written. Three are ranked above
(one Tier S) or in the Icebox; the rest are here.

### Broad correctness (5)

| Ticket | Tier | Note (verified 2026-09-06) |
|---|---|---|
| [smartmatch-against-the-topic-returns-bool-not-match](tickets/smartmatch-against-the-topic-returns-bool-not-match.md) | B1, **M** | **Confirmed**: `for (/a/,) { say ("ab" ~~ $_).raku }` gives `Bool::True`; raku a full `Match`. `exec_smart_match_expr_op` sets `$_` to the LHS *before* running the RHS bytecode range, so an RHS that reads `$_` sees the wrong thing — proved by elimination (`my $r2 = $_` first makes it work). `EXPR ~~ $_` inside `for`/`given` is a very common idiom. The fix needs a design split between `~~`-as-topicalizer and a plain-expression RHS, not a patch. |
| [array-subclass-iterator-override-ignored](tickets/array-subclass-iterator-override-ignored.md) | B1, **M** | **Confirmed**: `class SortedArray is Array {...}; .say for @thing` prints the whole array as one item; raku iterates four. A user `iterator`/`list` override on an `is Array` subclass is not consulted, and `for` decides iterability from its own shape analysis. Adjacent to the `array-subclass-delegation-is-one-decision` work that landed this week — read that news entry first. |
| [array-assignment-eagerly-reifies-a-triangle-reduce](tickets/array-assignment-eagerly-reifies-a-triangle-reduce.md) | B1, **M** | **Confirmed as a hang**: `my @n = [\~] 1..*; say @n[^5]` never terminates (exit 124); raku `(1 12 123 1234 12345)`. The laziness marker is not carried from the `[\op]` producer through `@`-assignment. Sibling of `deep/residual-try-cell-...` (ADR-0058) but a *different* producer — do not assume that ADR covers it. |
| [native-int-candidate-loses-to-int-for-a-literal](tickets/native-int-candidate-loses-to-int-for-a-literal.md) | B1, **M** | **Confirmed**: with `multi d(int $x)` and `multi d(Int $x)`, `d(5)` picks `boxed`; raku `native`. A literal arrives with no declared-type source, so `candidate_type_distance` ranks `Int` closer. Independent of the multi-resolve-cache-key work that landed 2026-09-06 (that fixed *caching* of a resolution, not the ranking). |
| [typed-container-capture-still-loses-to-a-same-named-caller-array](tickets/typed-container-capture-still-loses-to-a-same-named-caller-array.md) | B1, **L** | **Confirmed**: a closure over `my Int @a` reads `1` where raku reads `3` once a callee declares its own `@a`. The **last hole** in the otherwise-complete capture-cell dichotomy (ADR-0055 slice 1b, `da8e94252`) — plain containers were fixed, typed ones were not. The file records two *measured* regressions (BagHash/SetHash roast) from the naive widening, so read it before trying the obvious fix. |

### Battery-blocking (1)

| Ticket | Tier | Note |
|---|---|---|
| [object-hash-key-lost-when-pair-value-is-a-container](tickets/object-hash-key-lost-when-pair-value-is-a-container.md) | B2, **L** | **Confirmed**: a `Pair` whose value came from a hash read loses its key's container on assignment into `my Any:D %t{List:D}` — `Type check failed ... expected List:D but got Str ("1 2")`; raku builds the hash. Blocks 2 of Crane's 15 upstream files, i.e. it is part of the same `Config::TOML` + `Crane` battery slot as `deep/config-toml-battery-core-blockers`. |

### Narrow correctness, diagnostics, permissiveness (11)

| Ticket | Tier | Note |
|---|---|---|
| [subscript-argument-container-producer](tickets/subscript-argument-container-producer.md) | N, **L** | **Confirmed**: `g(@a[0])` writes through for a named sub, but `S.new.take(@a[0])` dies `expects a writable container ... but got '9' (Int) as a value`. It refuses loudly rather than losing the write, hence N. The producer for an `Expr::Index` argument is wired into `CallFunc` only, not `CallMethod`/`CallOnValue`/`CallOnCodeVar`. ADR-0067 residue; pairs with the row below. |
| [immutable-list-element-write-is-silently-dropped](tickets/immutable-list-element-write-is-silently-dropped.md) | N, **L** | **Confirmed**: `$l[0].mut` on `my $l = (1,2)` with a `\S:` raw invocant silently succeeds and changes nothing; raku dies `Cannot modify an immutable Int (1)`. The write is correctly dropped — only the *diagnostic* is missing, because the binder does not consult readonly-ness for a raw invocant. Receiver-side twin of the row above. |
| [list-and-array-which-should-be-object-identity](tickets/list-and-array-which-should-be-object-identity.md) | N, **M** | **Confirmed**: `(1,2).WHICH eq (1,2).WHICH` is `True`; raku `False` (same for `[1,2]` and `{a=>1}`). **Verified separately that `===` and `set()` membership already answer correctly** — only the `.WHICH` method arm in `builtins/methods_0arg/dispatch_core_coerce.rs` computes its own string instead of delegating to `value_which_key`. |
| [pair-which-is-object-identity-not-content](tickets/pair-which-is-object-identity-not-content.md) | N, **S** | **Confirmed**: `(a=>1).WHICH` is `Pair|3` (a per-object counter) so two equal Pairs compare unequal; raku hashes the content. Same `.WHICH` arm, opposite polarity. One fix closes both rows. |
| [itemized-list-element-var-reports-inner-type](tickets/itemized-list-element-var-reports-inner-type.md) | N, **S** | **Confirmed**: `(1,2,3,$(4,5))[3].VAR.^name` is `List`; raku `Scalar`. ADR-0064's descriptor synthesis declines for a *genuinely* itemized element and reports the inner type. Smallest open row in the file. |
| [tolerance-dynamic-variable-is-undefined](tickets/tolerance-dynamic-variable-is-undefined.md) | N, **S** | **Confirmed**: `$*TOLERANCE` is `Nil`; raku `1e-15`, which makes every `≅`/`=~=` comparison answer on the wrong basis. Seed it beside the other `$*`-dynamics. |
| [state-and-our-typed-declaration-hoist](tickets/state-and-our-typed-declaration-hoist.md) | N, **S** | **Confirmed, two independent rows.** `state Int $x` is not in effect before its declaration statement (raku type-checks, mutsu does not); and `our Int $x` compiles and dies at *runtime*, where raku refuses at compile time (`Cannot put a type constraint on an 'our'-scoped variable`). The residue of the typed-declaration-hoist fix that landed as `d61e6c534`. |
| [set-operator-reduction-one-arg-rule-does-not-coerce](tickets/set-operator-reduction-one-arg-rule-does-not-coerce.md) | N, **M** | **Confirmed**: `[(|)] (a => 2).Bag` returns the bare operand `:a(2)`; raku applies the implied unary coercion and answers `("a"=>2).Bag`. The one-argument shortcut skips the coercion; `reduction_identity_opt` already knows the right per-operator zero. |
| [mro-includes-composed-roles](tickets/mro-includes-composed-roles.md) | N, **L** | **Confirmed**: `K.^mro` lists `K,R2,Any,Mu`; raku `K,Any,Mu`. Introspection output is conflated with dispatch order. **L, not S, deliberately** — MRO widening has caused an unrelated dispatch-table regression here before (`trap-mro-widening-...`), so separate the two consumers rather than editing one list. |
| [role-mixin-name-carries-a-spurious-type-parameter](tickets/role-mixin-name-carries-a-spurious-type-parameter.md) | N, **M** | **Confirmed**: `(1 but R(2)).^name` is `Int+{R[Int]}`; raku `Int+{R}`. Naming only — the attribute initialization itself reads back correctly. |
| [zip-chain-with-a-comma-list-left-operand-fails-to-parse](tickets/zip-chain-with-a-comma-list-left-operand-fails-to-parse.md) | N, **M** | **Confirmed, two faces.** The paren spelling refuses to parse (`Confused. Two terms in a row`); the bracket spelling **silently mis-associates** — `[1, 2 Z <a b> Z <c d>]` gives `[(1,"c"), (((2,"a"),).Seq,"d")]` against raku's `[(1,"a","c"), (2,"b","d")]`. The silent-wrong-answer face is S-shaped but only reachable through this rare parse; `lift_meta_ops_in_paren_list` is never reached and `args.rs`'s one-level lift is used instead. |

### Icebox tickets (2)

| Ticket | Why here |
|---|---|
| [miri-leak-check-flakes-on-a-gc-test-fixture](tickets/miri-leak-check-flakes-on-a-gc-test-fixture.md) | CI-only, non-deterministic, and the file already records a **measured disproof of the obvious fix** — the leak did not reproduce in four local runs of the exact CI command. Not reachable through `./target/debug/mutsu` at all. Do not spend a session guessing at it; it needs a `cargo miri` run under the CI's own ordering. |
| [rustdoc-doc-link-warnings](tickets/rustdoc-doc-link-warnings.md) | Pure lint debt with no functional repro and no CI gate. **Its headline number is already stale**: `cargo doc --no-deps --document-private-items` now reports ~248 warnings against the file's recorded 210, so the per-category breakdown is out of date too (the taxonomy is probably still right). Fix the count when you fix the warnings, or add the gate first. |

---

## How to work `todo/deep/` — by ADR cluster

**Do not run `deep/` oldest-first** (filing order is an accident of which
campaign ran last, and `ls -tr` mtimes are corrupted by worktrees). Work it by
ADR cluster: most deep findings wait on a *slice of an ADR that already
exists*, and one landed slice closes several rows. Every `Status` line below
was read on 2026-09-06.

| ADR | Status | Rows it would close |
|---|---|---|
| [ADR-0068](../docs/adr/0068-cross-thread-container-writes-need-a-synchronized-store.md) cross-thread container writes | **`Proposed` (2026-09-05)** — decision written: (C) a global "more than one mutator thread is live" `AtomicBool` gate, then (B) the smallest provable slice, then widen lane by lane. Remedy (A), "lock the primitive", is **rejected on the record** (45 of the 149 sites can re-enter user code while holding it). | **Both Tier S rows.** This is the only ADR that owns a Tier S finding, and it is not started. |
| [ADR-0067](../docs/adr/0067-a-routine-hands-back-the-container-it-was-given.md) a routine hands back its container | **Accepted; every slice implemented** (1, 2, 3a, 3b, 4, 5 across 2026-09-05/06) | Its *residue* is two tickets: `subscript-argument-container-producer` (argument side) and `immutable-list-element-write-is-silently-dropped` (receiver side). Both are the same missing wiring on the non-`CallFunc` call paths. |
| [ADR-0055](../docs/adr/0055-closure-free-vars-resolve-to-their-own-binding.md) closure free vars bind their own | Slices 1 + 1b landed (1b 2026-09-06); slices 2-5 open. §7.6 records that the ADR was wrong about there being two closure-env merges — there are three. | `tickets/typed-container-capture-...` (the last measured hole); `deep/call-compiled-closure-...` (structural only now — its headline repro passes). Slice 1b also appears to have closed ADR-0039's acceptance case, see below. |
| [ADR-0058](../docs/adr/0058-map-grep-produce-a-deferred-seq.md) map/grep produce a deferred Seq | `Proposed`. Slice 1 (un-`todo` the test) done; **slice 2 — wire `SeqSource::MapGrep` into `dispatch_map_method`/`builtin_map`/`grep` — is the concrete next step.** | `residual-try-cell-eager-seq-reification-divergences` (9 of its 11 rows). Implementing it makes mutsu *stricter*, so a full local `make roast` is mandatory. |
| [ADR-0047](../docs/adr/0047-type-identity-is-a-declaration-site-not-a-registry-name.md) type identity | Partially adopted — P1/P2 landed; **P3 and P4 not started** | `subtest-compiled-dispatch-async-middleware-regression` (P4 is the prerequisite for re-landing #6499, *not* itself the fix for the regression). |
| [ADR-0053](../docs/adr/0053-do-whenever-produces-a-tap-on-the-stack.md) `do whenever` produces a Tap | `Proposed`, "implementation not started" — **the header is behind the code**: `.WHAT` already answers `Tap`. | `whenever-expression-position-needs-real-design`, whose symptom has *moved again*: the pre-`close` emission is now dropped too. Reconcile the header before designing. |
| [ADR-0048](../docs/adr/0048-placeholder-scope-is-a-block-invocation-contract.md) placeholder scope | Accepted; P1-P4 landed, P5's scope half landed, its value half deferred | `role-body-placeholder-mu-supply` — and the file argues against doing it: corpus hits are **zero** and rakudo's own behaviour there is an artifact (`$^c.defined` throws in real raku). |
| [ADR-0039](../docs/adr/0039-container-lexicals-resolve-lexically.md) container lexicals resolve lexically | Slice 1 landed 2026-08-20; §8.2 closed 2026-08-22; slice 2 nominally open | `module-file-scope-array-and-hash-still-share-the-caller` — but **its §6 acceptance repro now matches raku exactly** (verified by hand for this regen). Re-check the two roast rows it names (`S15-nfg/concat-stable.t`, `integration/advent2014-day05.t`) and close it rather than resourcing slice 2 blind. |
| [ADR-0065](../docs/adr/0065-language-server-targets-ai-agents.md) LSP targets AI agents | Accepted; header says "S0 and S1 shipped" while its own phasing table marks **S5a done** — the header is behind the table | `lsp-references-needs-a-side-table-not-ast-spans` (amends D6/S5b; blocked on a *measurement*, not a design). |
| [ADR-0029](../docs/adr/0029-exception-class-role-membership.md) exception class role membership | Slices 1-3 + R1-R4 landed; **R5 blocked on `vendor-real-test-module`** | `exception-class-hierarchy-is-mostly-unregistered` — whose title is now badly stale: all 373 rakudo `X::` subtypes match. It is closed-pending-an-unrelated-dependency. |
| [ADR-0059](../docs/adr/0059-is-rw-routines-return-a-container.md) `is rw` routines return a container | Slices 1-2 implemented; slice 3 open, no failing repro attached | Nothing today. |
| [ADR-0051](../docs/adr/0051-type-ancestry-has-one-oracle-and-an-unresolved-method-throws.md) / [ADR-0021](../docs/adr/0021-argument-namedness-is-a-call-site-property.md) / [ADR-0025](../docs/adr/0025-captured-scalar-cells-value-kind-blind.md) | P2+P5 / P5 / slice 3 open respectively | Nothing in `todo/` names them. Open slices without a failing repro — do not resource them ahead of a Tier S row. |
| [ADR-0050](../docs/adr/0050-block-routine-ness-is-a-definition-site-property.md) / [ADR-0043](../docs/adr/0043-scheduled-delivery-hop-belongs-to-the-tapped-supply.md) | Both `Proposed`, implementation not started (ADR-0043's Decision 1 is probe-verified and ready) | Nothing in `todo/` — designs waiting for a consumer. |

### Recommended next campaigns

1. **ADR-0068 slice (B) — the only Tier S cluster, and the design is already
   written.** Two `deep/` rows are one mechanism: the shared-vars lane hands a
   write to a `ContainerRef` cell it *believes* is synchronized and is not, and
   the write goes out through an unlocked `gc_contents_mut`. The ADR has
   already done the expensive parts — it rejects "lock the primitive" with a
   149-site re-entrancy census, and it ships a **calibrated harness** (see the
   methods note below). Start at step 1 (the `AtomicBool` gate), then the
   single-lock repair of the false premise in §2; do not start with a 149-site
   sweep. This is real UB in a shipped configuration and outranks everything
   else in this file.
2. **Finish the `Test`-provider retirement (a PLAN §1 goal item, not perf
   polish).** `deep/vendor-real-test-module` is at 29.5s against a 30s gate,
   and it names its own remaining work. Two of the three named files are now
   measured out: `perf/defaulted-param-forfeits-the-light-call-path` is CLOSED
   (a huge win for ordinary defaulted subs, but `ok` is a multi and `proclaim`
   carries `is copy` + a coercion, so neither becomes eligible and
   `write-int.t`'s resolve count is unchanged), and
   `perf/listop-call-bypasses-every-compiled-call-cache`
   (measured 2026-09-06: the carrier it blames is 1.2%, its own 8.5% finding is
   fixed, and its corrected budget table is now the useful part) and
   `perf/method-dispatch-flattens-the-env-on-every-call` (17% of what is left).
   All three are ordinary implementation work with measured targets — no ADR —
   and retiring a rung-3 native provider is a BATTERIES.md goal. **This is the
   highest-value non-Tier-S work in the repo.**
3. **The ADR-0067 producer residue (two tickets, one mechanism).**
   `subscript-argument-container-producer` and
   `immutable-list-element-write-is-silently-dropped` are the argument side and
   the receiver side of the same gap: the container plumbing a named-sub call
   gets was never extended to `CallMethod`/`CallOnValue`/`CallOnCodeVar`.
   ADR-0067 is Accepted with every slice landed, so this is completing a stated
   rule, not opening a question.
4. **The `.WHICH` identity pair — the cheapest coherent win in the file.**
   `list-and-array-which-should-be-object-identity` and
   `pair-which-is-object-identity-not-content` are one arm of
   `dispatch_core_coerce.rs` computing its own string instead of delegating to
   `value_which_key`. Verified for this regen that `===` and `set()` membership
   **already answer correctly**, so the internal oracle is right and only the
   method disagrees with it — which bounds the blast radius to introspection.
   S+M effort, two rows, one fix.
5. **`perf/locals-frame-is-a-pooled-vec-not-a-register-window` — write the ADR,
   do not start the code.** It is the largest single remaining cluster after the
   September sweep (~5.7% of `bench-fib` spent managing a one-element `Vec`),
   but `self.locals` is touched at 484 sites across 60 files, `mem::take` is
   load-bearing in three call paths, and the JIT emits native code against the
   current layout's offset. A `Proposed` ADR is the deliverable.

**One measured process exception, kept from previous regens:** when a change
alters a *universal property of values* ("what is in every container"), run the
full local `make roast` before pushing (ADR-0040 slice 2 needed 17
counter-current fixes, 9 found only by roast). Campaign 3 above is that shape.
Ordinary parser/operator/dispatch fixes still delegate to CI.

**Methods worth copying.** Five, all earned this cycle:

- **ADR-0068's stress harness — CPU *oversubscription* is the ingredient, not
  concurrency.** At 8-way on 12 cores a racing workload was 0/64 and 0/240 —
  clean. At **24-way on 12 cores** the same binary failed 6/960 within seconds.
  Every earlier "clean" measurement in this area was taken below the threshold
  and means nothing. Negative results kept on the record: `memcheck` finds
  nothing (it serializes threads onto one core), and helgrind was a dead end.
- **The two-breakpoint gdb oracle.** Break on the unsynchronized site and on
  the synchronized lane; `already hit N times` on the first with nothing on the
  second means the workload is *exposed*. Deterministic, one debug run, and it
  settled every route in minutes where the stress harness needs hundreds of
  runs to say the same thing probabilistically.
- **Compare against rakudo on the same workload, not mutsu against mutsu.**
  Three rounds of the YAML campaign profiled mutsu internally and missed a
  40x-over-firing grammar action; instrumenting *both* implementations' action
  call counts found it immediately.
- **`add_constant` must stay flat on a steady-state loop.** Growth with
  iteration count means something is being *compiled* per call. Do not conclude
  "flat profile, no dominant cost" before checking it — that mistake cost the
  bench-ctor campaign three rounds.
- **Do NOT keep bisecting a ~5% perf regression.** A second bisect named a
  commit whose code samples *zero cycles* in the benchmark: pure binary-layout
  noise. Discharge any bisect result by checking whether the named code is even
  sampled, and prefer retired instructions to cycles.

---

## Tier B — Correctness, broad impact

### B1 — broad language-construct correctness

| Ticket | Effort | Why here |
|---|---|---|
| [free-var-lexical-resolution-inside-a-bare-block](deep/free-var-lexical-resolution-inside-a-bare-block.md) | L | **Residue** of the old `free-var-read-in-callee-resolves-through-dynamic-caller-chain` row, whose file-scope half was fixed 2026-09-07 (`news/2026-09/free-var-bind-aliased-caller-lexical.md`; the pin `t/free-var-bind-does-not-alias-caller-lexical.t` is 22/22 green). Free-variable reads and writes were never the problem — a `:=` bind was carrying its cell into an intervening caller's env tier by name. For a compunit/mainline lexical ADR-0024's store supplies the lexical answer and the bind now uses it; for a lexical declared in a **bare block** there is no store, so the same two routes (the bare `env` insert + the call-return merge, and `propagate_bind_to_ancestor_frames`) still reach a shadowing caller. Closing it is the env-model change ADR-0055 §7.5 disclaims, or the smaller step of extending the ADR-0024 capture to block scope. |
| [dot-twigil-dot-assign-metaop-loses-its-rmw-origin](deep/dot-twigil-dot-assign-metaop-loses-its-rmw-origin.md) | S | **Mostly landed 2026-09-07** (`news/2026-09/dot-twigil-accessor-rmw-is-a-noop.md`): every `$.attr` read-modify-write is a silent no-op now, including the `$.x++` forms that were still silently over-mutating. What survives is `$.attr .= meth`, which still refuses -- `.=` loses its RMW origin in the parser, and `$.s .= uc` / `$.s = $.s.uc` lower to the same `AssignExpr` while raku answers them differently. The ticket lists the three carrier routes and their costs; pick one. |
| [immutable-lvalues-that-mutsu-still-lets-you-assign-to](deep/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md) | L | **Partially drained and still worth mining**: the element-store/bind and closure-topic families closed this cycle (both news-linked), and **7 rows survive** — e.g. `my @a = 1,2,3; my $x := @a; $x = 5` mutates `@a` where raku dies. The file records two "obvious fixes" that were tried and *measured* to regress other rows. Mine it for rows; do not dispatch it whole. |
| [definiteness-constrained-type-object-identity-lost](deep/definiteness-constrained-type-object-identity-lost.md) | L | **Confirmed and wider than the title**: `Any:D.^name` → `Any` (raku `Any:D`), `Any:U` likewise, `~~` answers `True` for everything, and `.^base_type` does not exist. `grep -rn DefiniteHOW src/` still finds nothing. Needs its own small representation ADR — none exists. |
| [user-prefix-op-candidate-beats-builtin-typed-candidate](deep/user-prefix-op-candidate-beats-builtin-typed-candidate.md) | XL | **Confirmed with the file's own repro** (`multi prefix:<++>($a) is default {...}; ++$foo` → `0`, raku `2`). Note a plain `sub prefix:<++>` *does* win in raku too, so use the `multi` spelling. The parse-time rewrite bypasses the native op entirely and native operators are not dispatch candidates at all. |
| [residual-try-cell-eager-seq-reification-divergences](deep/residual-try-cell-eager-seq-reification-divergences.md) | L | **Confirmed**, and the file's own self-correction still holds: the cause is not `try`/sink placement — `.map`/`.grep` are simply eager over a finite source. ADR-0058 slice 2 is the fix for 9 of 11 rows; the `fail`+`try` exit-status rows are a separate, uninvestigated bug. |
| [supply-channel-has-no-fanout-to-multiple-taps](deep/supply-channel-has-no-fanout-to-multiple-taps.md) | XL | **Confirmed**: a second `whenever` on `$proc.stdout` gets nothing — one `mpsc` receiver per Supply id, taken exclusively by whoever asks first. A naive buffer-and-replay fan-out would violate raku's live-vs-on-demand distinction, so this needs a design pass, not a patch. |

### B2 — batteries / dist-blocking

| Ticket | Blocks | Effort |
|---|---|---|
| [vendor-real-test-module](deep/vendor-real-test-module.md) | making the vendored upstream `Test` the default (retiring a rung-3 native provider — a PLAN §1 goal) | L — **not XL any more.** `write-int.t` 48.0s → **29.5s** after the multi-resolve-cache fix, against a hard 30s gate (rakudo 3.49s; the native provider 5.04s on the same 93,370 assertions). The file states completion criterion 2 is **not** met and names the three perf rows that close it. All correctness regressions it tracked are closed (`t/` sweep: 0). See campaign 2. |
| [config-toml-battery-core-blockers](deep/config-toml-battery-core-blockers.md) | `Config::TOML` (10/19) + `Crane` (3/15) battery slot | L (cluster). A self-re-measuring record — it flags its own previous numbers as stale each pass. The dominant remaining Crane cluster (`X::OutOfRange`/`CATCH` re-throw descent) is honestly marked "not bisected". `tickets/object-hash-key-lost-when-pair-value-is-a-container` is part of the same slot. **Do not start the vendoring steps.** |
| [template-engines-blocked-on-mutsu](deep/template-engines-blocked-on-mutsu.md) | the template battery runner-ups | XL (cluster) — **re-measured 2026-09-06, trust these numbers**: `Template::Mustache` 13/13 and `Template6` 12/12 are now **done**; `Template::Jinja2` 3/23, `Template::HAML` 39/83, `Template::Mojo` 4/5, `SP6` 10/11, `Template::Nest::Fast` 0/10. The cheapest remaining lever is `Template::Nest::Fast`'s single `with EXPR -> @m` list-wrapping bug. |
| [subtest-compiled-dispatch-async-middleware-regression](deep/subtest-compiled-dispatch-async-middleware-regression.md) | re-landing #6499's `subtest` perf win | L — root cause still unknown; #6499 is still reverted (`subtest_call_block` still routes through `call_sub_value`). Class registration, LEAVE phasers and async transforms are all ruled out by measurement, which narrows it to early/conditional-response middleware meeting compiled-closure frame construction. Bisect from the dispatch end with `rust-gdb` frame diffs, not from Cro. |
| [rakuast-remaining](deep/rakuast-remaining.md) | RakuAST parity | XL — an actively-worked ledger with entries through today. Its own headline count is understated (`t/rakuast*.t` is **113 files**, not the 93 recorded), and **ADR-0011's header is behind it** by five weeks. Zero roast dependents; pick by user impact, not cadence, and read the RakuAST implementation skill first. |
| [unify-statement-expression-control-construct-compilation](deep/unify-statement-expression-control-construct-compilation.md) | nothing directly — architectural debt | XL, and **measurably getting worse**: `helpers_do_expr.rs` is now **669 lines** (476 → 609 → 669), and `ForLoopSpec` is still constructed in two places. It keeps producing paired half-bugs; `do-block-does-not-scope-routine-declarations` (closed this cycle) was the most recent. |

---

## Tier N — narrow correctness / diagnostics

| Ticket | Category | Effort / note |
|---|---|---|
| [chained-index-assign-autoviv-loses-hole-tracking](deep/chained-index-assign-autoviv-loses-hole-tracking.md) | correctness-narrow | **S** — **confirmed**: `@a[0][1] = 5; @a[0][0]:exists` is `True` (raku `False`). The `;` form is already fixed; the file names the exact `IndexAssign` vs `MultiDimIndexAssign` divergence but deliberately leaves the site for the implementer. Smallest open `deep/` row. |
| [end-phasers-install-at-compile-time](deep/end-phasers-install-at-compile-time.md) | correctness-narrow | M — **confirmed**: raku also runs an `END` inside a never-taken `if False` block and in an uncalled sub; mutsu registers only at `PhaserEnd` execution. The source-order half already shipped; this is the residue. |
| [ordered-alternation-eager-candidate-enumeration](deep/ordered-alternation-eager-candidate-enumeration.md) | correctness-narrow | L — **narrowed 2026-09-07 to ADR-0073 Slice 2** (Slices 1+3 shipped; the quantifier face closed to `news/2026-09/regex-atom-candidates-are-demand-driven.md`). Only the `<subrule>` boundary is still collect-then-pick: `regex part { \w* {B} }` under a `regex` caller runs the block 5x against raku's 2, and a `||` inside a non-ratcheted subrule enters both branches. The file now carries the 4-row measured table and says why the `Named` arm resisted (left-recursion seed loop + proto rank-then-match); the ratcheted-caller half (E3) is the cheap slice. |
| [module-toplevel-private-sub-leak-cleanup](deep/module-toplevel-private-sub-leak-cleanup.md) | accepts code raku rejects | L — **confirmed**: a non-exported module `sub helper` stays callable bare after `require`; raku fails at compile time. Needs an exhaustive audit of ambient `GLOBAL::` installers first — a generalisation was tried and reverted. |
| [native-method-accepted-named-declarations](deep/native-method-accepted-named-declarations.md) | accepts code raku rejects | L — **confirmed, with one arm moved**: `"abc".chop(:zzz)` → `abc` and `10.polymod(3,:zzz)` → `(1 3 Inf)` still silently wrong; `3.fmt("%d",:zzz)` now dies with a *positional-arity* error instead of the recorded `X::AdHoc`. Cosmetic drift — note it if you touch the file. Two designs; ADR first. |
| [export-default-package-not-symbolically-navigable](deep/export-default-package-not-symbolically-navigable.md) | missing feature | L — **confirmed**: `::("Test::EXPORT::DEFAULT::&ok")` fails; raku answers `&ok`. Exported symbols are copied by name and `Module::EXPORT::DEFAULT` is never materialized as a real stash. Calls for a module-system design decision that no ADR covers. |
| [is-typename-custom-container-store-protocol-unimplemented](deep/is-typename-custom-container-store-protocol-unimplemented.md) | missing feature | XL — **confirmed**: `my @v is DNA = 1,2` never calls `STORE` and the logger never fires. The file rightly asks you to scope it first (grep the corpus for `method STORE`) before committing. |
| [metamodel-roles-are-not-composable-types](deep/metamodel-roles-are-not-composable-types.md) | missing feature | XL — **renamed and re-scoped 2026-09-07**: the immutable-binding error, `.^add_method`, and the doc's full worked example (`A.x()` prints 42) all agree with raku now. Three framings closed under this file in a row. What survives is the one it recorded as an aside: `Metamodel::Naming`/`Stashing`/`Primitives` are not composable types, so `class WithStashHOW does Metamodel::Naming` dies `X::InvalidType` before the body is read. Two smaller residues (`.name` reports the added name; a plain `sub` as a method does not take the invocant as its first positional) are deferred with corpus evidence in the file. |
| [whenever-expression-position-needs-real-design](deep/whenever-expression-position-needs-real-design.md) | correctness-narrow | M — **moved again**: `.WHAT` answers `Tap`, but the value emitted *before* `.close` is now dropped as well, not just the one after. Reconcile ADR-0053's header with what actually landed before designing the identity slice. |
| [slurpy-hash-named-arg-raku-boolean-shorthand-missing](deep/slurpy-hash-named-arg-raku-boolean-shorthand-missing.md) | rendering | L — **confirmed**: `foo :a1:b2` renders `{:a1(Bool::True), :b2(Bool::True)}`; raku `{:a1, :b2}`. Moved from `tickets/` to `deep/` deliberately: its own recommendation is "do not fix this in isolation" — it wants per-element `Hash` containers (the associative half of element itemization). |
| [typed-shaped-array-rows-lose-element-value-type](deep/typed-shaped-array-rows-lose-element-value-type.md) | self-consistency (**no raku oracle**) | M — **confirmed**, including that raku has no oracle: it dies "Partially dimensioned views of shaped arrays not yet implemented" on the first statement. mutsu's own 1D `:exists` is right and its 2D row `:exists` is not. Thread `value_type` through `make_shaped_array_seeded`'s rows. |
| [begin-time-adverb-value-interpolation](deep/begin-time-adverb-value-interpolation.md) | correctness-narrow | L, **low priority by its own assessment** — confirmed (`$a:foo«$c»` → `Nil`; raku `answer`), but there is no roast coverage and the fix wants a whole-AST name-normalization pass across ~104 scattered `local_map` lookups. |

---

## Perf — batch into one profiling session; implementation agent runs SOLO

The September call-path campaign **landed and inverted the picture**:
`bench-fib` 0.84x → **0.37x** raku (JIT 0.20x), `hash-access` 0.18x,
`string-concat` 0.10x at `87e910a62`. Four files below were filed *inside* that
campaign and are its live continuation; the rest predate it in whole or part.
Numbers marked *debug* have never been confirmed on release.

| Ticket | Status |
|---|---|
| [non-constant-defaults-still-forfeit-the-light-path](perf/non-constant-defaults-still-forfeit-the-light-path.md) | **The residue of `defaulted-param-forfeits-the-light-call-path`, which is CLOSED** (tiers 1+2 landed: constant defaults and bare `?` now fill from a registration-time table, 1001 full resolves over 1000 calls -> 1, and 13.8x fewer instructions on a defaulted-param loop). What is left is tier 3 — compiling default *expressions* into a callee prologue — a compiler change, not a binder one. **Survey before building**: the constant cases were the common ones, and the `Test`-module motivation the original ticket carried is measured stale (`write-int.t` reports the same 22 805 resolves either way). |
| [listop-call-bypasses-every-compiled-call-cache](perf/listop-call-bypasses-every-compiled-call-cache.md) | **MEASURED, and the headline was wrong — read the file before picking it up.** The counter it asked for is in (`execcallpairs:compiled`/`:native`/`:carrier`) and confirms 200/200 assertions take the carrier. But callgrind on release puts the *whole* carrier arm at **1.2%** of the run, and `ok`'s defaulted `$desc` makes it light-ineligible, so a cache hit would land on the same `call_compiled_function_named` the carrier already reaches — gaps (a) and (b) are ~1% slow-path retirements, not speedups. The 8.5% the profile did find (`push_multi_dispatch_frame`'s registry-wide candidate walk) is **fixed** (-7.5% wall clock on the 20k-assertion loop). What is left in the file is the corrected budget table: `bind_function_args_values` 13%, ~47 `format!` per assertion. |
| [method-dispatch-flattens-the-env-on-every-call](perf/method-dispatch-flattens-the-env-on-every-call.md) | **NEXT.** Every full method dispatch that misses the accessor fast path runs `flatten_scoped_env()`, making method cost linear in names-in-scope *and* destroying the O(writes) return-merge — 17% of what remains in the 20k-assertion `ok` loop. Two suspects already eliminated by measurement. Cheaper sub-fix if the full move looks risky: short-circuit `Env::flattened()` to `parent.flattened()` on an empty overlay. Risk shape is a wrong answer, not a crash — read `docs/vm-dual-store.md`. |
| [interpreter-call-path-in-hot-loops](perf/interpreter-call-path-in-hot-loops.md) | **Do NOT start from its "Where to start" — the file says so itself.** The `&`-sigil gate it blamed for a year is measured closed (byte-identical opcode profiles). The live finding is new: under `MUTSU_REAL_TEST=1`, **83.3% of function-call opcodes fall back to the interpreter**, dominated by `nqp::`-prefixed calls that never reach a cached dispatch despite being fixed known names. That is the question. Methodology: "raku will delete your benchmark" — an unread accumulator is optimized away and manufactures a 140-370x fake deficit. |
| [late-august-call-path-slowdown-remainder](perf/late-august-call-path-slowdown-remainder.md) | **ACTIVE — the campaign's central ledger**, items closed inline as they land. Its ADR-0066 item is resolved (Accepted + implemented 2026-09-03). Open item #3: `mutsu_jit_1` got ~50% slower since August with nothing in the interpreter explaining it — dump the generated code for both builds. Its "Do NOT keep bisecting" paragraph is still the most important one in `todo/perf/`. |
| [locals-frame-is-a-pooled-vec-not-a-register-window](perf/locals-frame-is-a-pooled-vec-not-a-register-window.md) | **BLOCKED on an ADR, and it is the largest single remaining cluster** (~5.7% of `bench-fib` managing a one-element `Vec`). 484 `self.locals` sites across 60 files, `mem::take` load-bearing in three call paths, `VmCallFrame::saved_locals` owning a whole `Vec`, and the JIT emitting against the current offset. Write the `Proposed` ADR (representation, migration order, JIT layout) before any code. |
| [hash-access-diffuse-regression-2026-09](perf/hash-access-diffuse-regression-2026-09.md) | **ACTIVE.** Ratio crept 0.17 → 0.19 over 2026-08-27..09-06 with no single culprit; three fixes landed (`hash-access` 266.2M → 253.3M instructions, −4.8%) and the rest is still there. Next: symbol-key the remaining ~155k `Symbol::intern` calls. Methodology: **read the ratio column, not `mutsu_median_s`** — a 26% "step" was runner noise that moved raku's baseline identically. |
| [hash-workload-cost-is-spread-across-gc-alloc-and-key-hashing](perf/hash-workload-cost-is-spread-across-gc-alloc-and-key-hashing.md) | **RECORD**, deliberately not a fix: GC ≈14%, allocation ≈12%, NaN-box decode ≈13%, key hashing+comparison ≈10%. Most tractable lead is `Interpreter::current_package()` (an `RwLock` read + `String` clone, 2.2%, 228 sites, with `current_package_sym()` already beside it) — get a caller breakdown first, most sites are cold. Leads 2 and 3 (SipHash→FxHash; whether a plain-scalar hash element needs a cycle-collected cell) both **need an ADR**, not a drive-by. |
| [adr0019-g3-diffuse-bless-allocation-cost](perf/adr0019-g3-diffuse-bless-allocation-cost.md) | **ACTIVE, and its tooling blocker is gone** — `alloc_scope!` + `--features alloc-stats` replaced the `perf --call-graph` dead end. Next: `bless`'s O(attrs×args) named-argument scan (11 allocations per `bless`) and a reusable scratch `String` in locals-init. Supersedes most of the bench-ctor row below. |
| [bench-ctor-construction-parity](perf/bench-ctor-construction-parity.md) | Round 5 disproved rounds 2-4's "flat profile" conclusion by finding a per-call `.map` **compile** (−12.9%). Still-open lead: custom-`new`→`bless` plumbing (~11.6µs vs raku's ~1.4µs). Largely superseded in substance by the row above; keep it for its methodology note. |
| [yaml-parse-throughput](perf/yaml-parse-throughput.md) | **Round 10 (2026-09-06) closed the gap**: `bench-yaml-parse` 1.14s → 0.138s, now **2.5x faster than rakudo**; a 120-row document is 0.96s vs raku's 0.44s. Remainder is genuinely flat (allocator ~20%, memcpy 6.7%, SipHash on capture maps 8.1%). Carries the "compare against rakudo, not against yourself" lesson. |
| [closure-literal-creation-cost](perf/closure-literal-creation-cost.md) | Parts A and C done (−20%/creation, body shared). **Part B needs an ADR**: narrowing `capture_closure_env`'s kept set trusts an incomplete static analysis — the `roles-6e.t` flake shape CLAUDE.md warns about. Cost the "share the system-name portion through the parent chain" alternative first. Methodology: `bench-startup.raku` is too short to A/B, consecutive runs of one binary differ ~2x. |
| [digest-ripemd-start-per-block-overhead](perf/digest-ripemd-start-per-block-overhead.md) | **Re-measure before anything.** 156.6s against a 120s gate, but its round-6 fix (generation-checked dispatch memos) is the same shape as the campaign's multi-resolution cache, which has since landed generally. The recorded number almost certainly moved. Profile is otherwise flat. |
| [interpreter-new-is-expensive-and-retains-memory](perf/interpreter-new-is-expensive-and-retains-memory.md) | ~9.17ms and **~7.2 KiB retained** per `Interpreter::new()`, linear over 4000 constructions. **Debug numbers.** Nothing is currently blocked (the LSP's S2 avoided needing an `Interpreter`), but the retention is unexplained and nothing read "obviously retains" — chase the *retention*, not the wall clock. |
| [bigint-repeated-addition-performance-gap](perf/bigint-repeated-addition-performance-gap.md) | ~14x raku — **debug, and never profiled**. Its root cause is an untested hypothesis. Step one is the release profile of `builtins/arith.rs` bignum `+` that the file admits never happened. Orthogonal to the call-path campaign. |
| [closure-sequence-evolution-performance-gap](perf/closure-sequence-evolution-performance-gap.md) | ~84x raku — **debug**, pure hypothesis. The actionable signal is that the combined case (48s) far exceeds the sum of its parts (~7.5s). Profile `max :by` and `subst`'s regex-assertion closure crossing on release. |

Numbers that end up in a document must come from the **bench CI**
(`bench-history.tsv` on `bench-data`), never from a profiling session's own
local runs. The `0.37`/`0.20`/`0.18`/`0.10` figures above are read from that
file at `87e910a62`; everything else quoted here is session-local routing
evidence.

---

## Icebox — blocked on a decision, or a pure record

| Ticket | Blocked on / why |
|---|---|
| [module-file-scope-array-and-hash-still-share-the-caller](deep/module-file-scope-array-and-hash-still-share-the-caller.md) | **Probably closeable.** Its ADR-0039 §6 acceptance repro was re-run by hand for this regen and is byte-identical to raku; `da8e94252` (ADR-0055's unvouched-capture cell) appears to have fixed the by-name propagation it names as its blocker. Next step is *verification*, not implementation: re-check `S15-nfg/concat-stable.t` and `integration/advent2014-day05.t`, then move it to `news/` with a pinned regression test. |
| [call-compiled-closure-lacks-merge-all-and-dual-persistence-store](deep/call-compiled-closure-lacks-merge-all-and-dual-persistence-store.md) | Headline defect fixed (ADR-0055 slice 1b) and re-verified here. What remains is structural — no `merge_all` equivalent in `call_compiled_closure`, and two disjoint per-instance state stores — with no measured wrong answer. Both are ADR-0055 slices 3-5, which *retire* the parameter rather than add a knob. |
| [exception-class-hierarchy-is-mostly-unregistered](deep/exception-class-hierarchy-is-mostly-unregistered.md) | Title is badly stale: all **373** rakudo `X::` subtypes now match, and the file regenerates its own numbers from a checked-in script. Only R5 (the real-`Test` sweep) is left and it is blocked entirely on `vendor-real-test-module` (ADR-0029 slice 4). Closed-pending-a-dependency. |
| [lsp-references-needs-a-side-table-not-ast-spans](deep/lsp-references-needs-a-side-table-not-ast-spans.md) | **Measurement before design.** ADR-0065 D6 assumed spans on AST variants; the parser already knows every byte offset, so a thread-local occurrence table gated on an analysis flag is cheaper and touches neither `Expr`'s size nor the bincode cache. The blocker is **backtracking** — a `Var` parsed in a failed alternative is a phantom reference. Build the table behind the flag, run it over `modules/`/`vendor/`/`t/`, measure the phantom rate; that number decides the design. Also needs an explicit ADR decision that `references` is name-based. |
| [role-body-placeholder-mu-supply](deep/role-body-placeholder-mu-supply.md) | **Its own assessment is "don't"**: corpus hits across `roast/`, `modules/`, `vendor/`, `lib/` are **zero**, and the semantics being matched are garbage (rakudo supplies an uninitialized value whose `.defined` throws). ADR-0048 P5's value half; do it only if the deferred-body plumbing is opened for another reason. |
| [adr0019-e2-e4-resolver-core](deep/adr0019-e2-e4-resolver-core.md) | E3/E4 closed; E2 is a non-gating counter cleanup. ADR-0019's four completion gates are all closed. |
| [nativecall-cannot-be-vendored](deep/nativecall-cannot-be-vendored.md) | Measurement record with reopen conditions. Blocker 3 (parser) is gone — re-verified: `is repr<Uninstantiable>` / `is ctype<long>` now parse and run identically to raku. Blockers 1/2/4 (QAST surface, MoarVM dispatch programs, 61 missing `nqp::` ops) stand, so `NativeCall` remains a justified rung-3 provider. |
| [p5tie-stash-bind-key-protocol](deep/p5tie-stash-bind-key-protocol.md) | A corpus-measured deferral, not a bug: `Stash.BIND-KEY`/`CALLER::.BIND-KEY` are unimplemented and cover ~0.5% of sampled dists (`P5tie`, `annotations`). Rung-2 machinery only; ADR-0013 §7's `ContainerRef` is the proposed surface. |

---

## Housekeeping notes

- **Closed since the 2026-09-04 regen** — 24 files, all four Tier S rows and
  every `tickets/` entry: #7296 (`rw-param-does-not-bind-a-proxy-container` +
  `element-bind-fetches-the-proxy-it-should-install`), #7298, #7299, #7300,
  #7302 (`method-rooted-subscript-chain-autoviv-is-dropped`), #7303, #7305,
  #7309, #7310, #7314, #7316, #7317, #7320, #7323, #7324, #7325, #7334, #7336
  (`procasync-stress-segv`), #7355 (`same-named-loop-params-in-one-unit-interfere`),
  #7364, #7367, #7380. Details are in `news/2026-09/`.
- **`todo/` files whose own root-cause or status section is wrong** — this
  project's most common failure mode, so treat it as the default assumption.
  This cycle: `deep/dollar-dot-attr-compound-assign-spurious-ro-error` (its
  2026-09-01 tail is contradicted by today's run, in both directions, and the
  2026-09-07 pass then found a THIRD wrong claim -- `$.x++` was still silently
  over-mutating where the file said silent over-mutation was gone),
  `deep/module-file-scope-...` (headline repro now passes),
  `deep/call-compiled-closure-...` (headline repro now passes),
  `deep/exception-class-hierarchy-...` (title vs body),
  `deep/native-method-accepted-named-declarations` (`fmt`'s failure class
  moved), `deep/whenever-expression-position-...` (symptom moved again),
  `tickets/rustdoc-doc-link-warnings` (210 → ~248), and
  `perf/interpreter-call-path-in-hot-loops` (says so itself, in bold).
  ADR-0041 §6 remains the model for recording this: keep the document, add a
  section naming which premises were measured false and why.
- **Three ADR headers are behind their code**: ADR-0053 says "implementation
  not started" while `.WHAT` already answers `Tap`; ADR-0011 points at
  `rakuast-remaining` as its live inventory but its own timestamp predates five
  weeks of landed slices; ADR-0065's header says "S0 and S1 shipped" while its
  own phasing table marks S5a done.
- **`docs/doc-diff-backlog.md` is now the main feeder of `todo/tickets/`.** Its
  2026-09-06 full re-sweep took high-signal findings 296 → **108** while
  `match` rose 2195 → 2376 (the blocks are being answered, not disappearing).
  Its two highest-signal files are drained into tickets; the next untriaged are
  `Type/Any`, `Language/experimental`, `Language/objects`, `Language/traps`.
  When you close a ticket that came from there, update its **Ticketed** row too
  — a finding tracked in only one of the two places is the one that gets lost.
- **No duplicate rows this cycle** — the previous regen's one known duplicate
  (the two-hop sigilless bind chain) closed with both of its homes.
- Verification for this regen was run ad hoc from `tmp/` (gitignored); each
  ticket's own repro block regenerates it.
