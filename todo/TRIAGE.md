# TRIAGE — prioritized snapshot of todo/ (2026-08-27)

A ranked index of every open finding under `todo/tickets/`, `todo/deep/` and
`todo/perf/`, so a session can pick the next unit of work without re-reading
all of them.

This is a **snapshot, not a ledger**. Resolving a ticket does *not* require
editing this file — that would reintroduce exactly the shared-file merge
conflicts `todo/` exists to avoid. A stale row is fine; the per-ticket files
stay the source of truth. Regenerate the whole file when it has drifted too
far (re-survey every ticket, re-score, rewrite).

## What changed since the 2026-08-19 regen

Surveyed 2026-08-27: **73 files** — 48 `deep/`, 17 `tickets/`, 8 `perf/`.

The previous regen listed 73 rows. **50 of them (68%) name files that no
longer exist**, and **50 files present today were not in it at all.** That is
not a small drift, and it is why this is a rewrite rather than a patch. The
two halves of `todo/` are moving in opposite directions and should be worked
differently:

- **`tickets/` is draining.** 14 → 17 files, but with heavy turnover: the
  oldest-first parallel-agent pipeline in CLAUDE.md works here, and most of
  what remains is genuinely blocked rather than merely unstarted (see the
  blocker taxonomy below).
- **`deep/` is growing.** 66 → 48 files sounds like progress, but **30 of the
  48 are new since 2026-08-19** — i.e. ~18 were closed while ~30 were filed.
  Deep findings are being produced faster than they are consumed, and
  oldest-first is the wrong policy for them. See "How to work `todo/deep/`"
  below.

**Method / caveat for this regen.** Each file's title, status block and
stated blocker were read, and every ADR referenced as a blocker had its real
`Status` line checked. Individual repros were **not** re-run. Treat a tier as
a routing hint, not a verified claim — CLAUDE.md's standing rule applies:
re-verify a ticket's repro on the current build before acting on it, because
stated root causes in this repo are wrong or stale often enough that it is
the default assumption, not the exception.

## How the ranking works

- **Tier S — Soundness.** Crashes (SEGV/panic/stack overflow), memory
  unsafety, or *silent data corruption* (a variable, container, or
  environment write that is wrong and nothing detects it). Always highest
  priority regardless of effort.
- **Tier B — Correctness, broad impact.** A wrong answer or missing
  capability in a common construct, or a bug that blocks an entire
  dist/battery. B1 = broad language-construct correctness (highest leverage,
  not tied to one dist); B2 = batteries/dist-blocking.
- **Tier N — Correctness, narrow impact / diagnostics.** Wrong answer in a
  rare construct with no known blocked test, or a wrong/missing *error
  message*.
- **Perf.** Batched into their own profiling-heavy session; the
  implementation agent for a perf item **runs solo** (parallel perf agents
  produce measurements that never converge).
- **Icebox.** Blocked on a design decision or an explicit user call, or a
  pure decision/measurement record with no action attached.

**Effort** (S/M/L/XL) is shown but does not change tier.

---

## `todo/tickets/` — why each one is or is not startable

The user asked for this explicitly, and it is the most useful thing this
file can say about `tickets/`: of the 17 open, **8 are startable today** and
**9 are not, for four distinct reasons.** "Not startable" almost never means
"not worth doing" — it usually means *the order is wrong*.

### D — Startable today (8)

| Ticket | Tier | Note |
|---|---|---|
| [nativecall-callback-parameter-marshalling](tickets/nativecall-callback-parameter-marshalling.md) | B2 | The **sole remaining blocker** for `Archive::Libarchive::Raw`'s 6th file (1/6 → 5/6 already). Highest-value row here. |
| [anonymous-grammar-is-a-bare-package-with-no-parents](tickets/anonymous-grammar-is-a-bare-package-with-no-parents.md) | B1 | `anon_grammar_expr` emits `Stmt::Package`, which has no `parents` field, so an anonymous grammar never gets the implicit `Grammar` parent. The two statement declarators already do this correctly — a narrow, well-located gap. Filed 2026-08-27 out of the grammar-cursor work. |
| [pod-block-gist-is-not-rakudos-pod-gist-tree](tickets/pod-block-gist-is-not-rakudos-pod-gist-tree.md) | N | `.raku`, the block types and the attribute set already landed; only the *block* gist tree was deliberately left. |
| [thread-clone-interpreter-has-no-source-location](tickets/thread-clone-interpreter-has-no-source-location.md) | N | An exception raised on a worker thread gets an empty backtrace. Diagnostics-class. |
| [reduce-metaop-zero-arg-meaning-returns-nil-not-failure](tickets/reduce-metaop-zero-arg-meaning-returns-nil-not-failure.md) | N | `[x] ()` answers `Nil` where raku answers a `Failure` wrapping `X::NoZeroArgMeaning`. Needs the exception type to exist first, and should be driven by raku's own zero-arg-meaning classification across the infix table rather than special-casing `x`. |
| [procasync-merged-tap-after-start-should-throw](tickets/procasync-merged-tap-after-start-should-throw.md) | N | Missing `X::Proc::Async::TapBeforeSpawn` on the merge. **Not** a copy of the `.stdout` check — raku's is at *tap* time and `whenever $proc` never calls `.Supply`. |
| [procasync-output-chunks-do-not-hold-back-final-grapheme](tickets/procasync-output-chunks-do-not-hold-back-final-grapheme.md) | N | raku emits `["ab","cde","f"]` where mutsu emits `["abc","def"]`: raku holds back each chunk's final grapheme (NFG). Also explains the encoding-error content difference. |
| [repl-routine-unimplemented](tickets/repl-routine-unimplemented.md) | N | Startable but **read the header first** — its own text says it is blocked on EVAL's caller-lexical visibility, so scope that before committing to the whole routine. |

### A — Blocked on another design campaign (5)

These need an ADR slice landed first. Patching around them is exactly the
ad-hoc route CLAUDE.md rules out, and in two cases it was *measured* not to
work.

| Ticket | Blocked on | Status of the blocker |
|---|---|---|
| [immutable-lvalues-that-mutsu-still-lets-you-assign-to](tickets/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md) | [ADR-0036](../docs/adr/0036-element-container-pairs-from-subscripts-and-pairs.md) slices 3-4 | Slices 1-2 landed 2026-08-20. Rows **re-measured 2026-08-27 after ADR-0040 slice 2 and none moved** — *itemization is not container-ness*, so the itemization flag cannot serve as a writability oracle even in principle. |
| [range-assigned-to-named-scalar-not-itemized-as-subscript](tickets/range-assigned-to-named-scalar-not-itemized-as-subscript.md) | [element-itemization-lost-in-scalar-binding](deep/element-itemization-lost-in-scalar-binding.md) | Attribution corrected 2026-08-27: this is a **scalar** store, outside ADR-0040's array/hash element scope. Do not patch the subscript site. |
| [native-method-cannot-return-an-lvalue-container](tickets/native-method-cannot-return-an-lvalue-container.md) | ADR-0001 §2.1 / Track B (universal `ContainerRef` deref), unblocked by [ADR-0013](../docs/adr/0013-container-interior-mutability-cellvalue.md) §7 | `.VAR = `/`.snitch = ` need a native method to hand back the invocant's container. Special-casing `.snitch` is a band-aid over a general gap. |
| [io-listops-bind-colonpair-args-as-positional](tickets/io-listops-bind-colonpair-args-as-positional.md) | ADR-0021 pair-namedness (P1-P3 shipped; P4/P5 remain — the originating `deep/` file has since been closed out) | `say :d, "x"` prints `d => Truex`; raku prints `x`. `Stmt::Say(Vec<Expr>)` has no named/positional distinction, and `(a => 1)` must stay positional. Filed 2026-08-27. |

### B — Deliberate non-divergence records (2)

Not bugs. Both exist so the next reader does not "fix" them back.

| Ticket | Why it stays |
|---|---|
| [multidim-oob-coordinate-nil-vs-empty-list-version-pragma](tickets/multidim-oob-coordinate-nil-vs-empty-list-version-pragma.md) | Matching plain `raku` regresses two whitelisted roast files. roast is authoritative per CLAUDE.md, so the current `Nil` answer is correct for what CI checks. Revisit only if per-language-version multidim branching becomes needed anyway. |
| [backtrace-has-fewer-frames-than-rakudo](tickets/backtrace-has-fewer-frames-than-rakudo.md) | mutsu has no CORE setting written in Raku, so rakudo's `SETTING::` frames have no mutsu equivalent. Synthesizing them would make `.gist`/`.full` *less* useful, not more. |

### C — Prerequisite not met; the ticket itself is mechanical (2)

Both are packaging steps deliberately split from the interpreter fixes they
wait on, so no interpreter fix is scoped to also cover packaging/docs/CI.
Neither is startable until someone else moves the module's pass rate.

| Ticket | Current measurement (2026-08-26) |
|---|---|
| [bundle-xml-battery](tickets/bundle-xml-battery.md) | `XML` v0.3.6: raku 15/15, mutsu **5/15** (was 1/15 at survey, 2/15, then 5/15 after the group-backreference fix). |
| [bundle-config-toml-once-parser-fixed](tickets/bundle-config-toml-once-parser-fixed.md) | `Config::TOML` v0.1.3: raku 19/19, mutsu **0/19**; `Crane` v0.1.2: raku 15/15, mutsu **3/15**. Unchanged since 2026-08-22. |

---

## How to work `todo/deep/` going forward

**Do not run `deep/` oldest-first.** That policy is right for `tickets/` and
wrong here, for a measured reason: 30 of the 48 open deep files were filed in
the last 8 days, so "oldest" selects by *filing accident* rather than by
leverage, and the queue head does not drain. (`ls -tr` is doubly wrong —
worktree checkouts and `git pull` corrupt mtimes; use
`git log --diff-filter=A` if you need true add-order for `tickets/`.)

Work `deep/` by **ADR cluster** instead. Most deep findings are not waiting
for a *diagnosis* — they are waiting for a *slice of an ADR that already
exists*. The bottleneck is ADR implementation, not ADR authoring, and one
landed slice typically closes several deep rows plus one or more `tickets/`
rows at once. The clusters, with their real blocker status:

| ADR | Status | Deep/ticket rows it would close |
|---|---|---|
| [ADR-0040](../docs/adr/0040-array-hash-elements-are-itemized-at-the-store.md) element itemization at the store | **Accepted; slices 0-2 landed** (slice 2 on 2026-08-27), slices 3-5 open | `element-itemization-lost-in-scalar-binding` (partly), row 24 `.VAR` reflection, compensator deletion |
| [ADR-0036](../docs/adr/0036-element-container-pairs-from-subscripts-and-pairs.md) element container cells | Partially implemented; slices 3-4 open | `immutable-lvalues-...`, `for-loop-rw-element-alias-lost-through-deferred-closure`, `is-rw-sub-implicit-return-element-not-mutable`, `return-rw-scalar-and-list-containers` |
| [ADR-0045](../docs/adr/0045-for-loop-parameters-bind-the-element-container.md) for-param binds the element container | **Proposed, not started** | `take-rw-loses-mutable-container-alias`, `for-loop-rw-element-alias-...`, `for-loop-pointy-sigilless-param-write-through-missing` |
| [ADR-0042](../docs/adr/0042-type-constraints-belong-to-the-container-not-to-a-name.md) container-carried type constraints | Partially implemented; slice 1 landed (#6743) | `shadowing-declaration-drops-the-outer-typed-scalar-constraint`, `sigilless-alias-closure-capture-skips-typecheck`, `bare-name-type-constraint-store-is-scope-blind` |
| [ADR-0055](../docs/adr/0055-closure-free-vars-resolve-to-their-own-binding.md) closure free vars bind their own | **Proposed, not started** | `call-compiled-closure-lacks-merge-all-and-dual-persistence-store` (its slice 1 is the entry point) |
| [ADR-0058](../docs/adr/0058-map-grep-produce-a-deferred-seq.md) map/grep produce a deferred Seq | **Proposed** | `residual-try-cell-eager-seq-reification-divergences` |
| [ADR-0051](../docs/adr/0051-type-ancestry-has-one-oracle-and-an-unresolved-method-throws.md) one ancestry oracle | Accepted; P1/P3/P4 landed, **P2/P5 not started** | type-ancestry and unresolved-method rows across several files |

**Recommended selection rule for `deep/`:**

1. Tier S first, always — a soundness bug outranks any amount of leverage.
2. Otherwise pick the **ADR cluster with the most blocked rows whose ADR is
   already `Proposed`/`Partially implemented`**, and land its next slice.
   ADR-0036 and ADR-0045 currently score highest and overlap heavily (both
   are the element-container model seen from different sides) — they are the
   natural next campaign.
3. Only file a *new* ADR when a finding has no cluster. The stated blocker on
   a deep file is frequently an ADR that has since advanced — check the ADR's
   real `Status` line before believing the ticket.
4. A deep item's implementation agent needs a **self-contained prompt with a
   re-verification step**; deep files go stale faster than tickets because
   their blockers move underneath them.

**One measured process exception, from ADR-0040 slice 2 (2026-08-27):** when
a change alters a *universal property of values* ("what is in every
container"), the consumer surface is the whole language and "who consumes the
code I changed" cannot name the sweep. That slice's ~40 lines of hooks needed
17 counter-current fixes; `t/` found 6, deliberate probes 2, and **9 only
against roast, across 8 subsystems and two CI iterations**. For that shape
only, run the full local `make roast` before pushing. This does *not* reopen
the general rule — ordinary parser/operator/dispatch fixes still delegate to
CI.

---

## Tier S — Soundness (crashes, memory unsafety, silent data corruption)

| Ticket | Breadth | Effort | Why here |
|---|---|---|---|
| [bind-propagate-ancestor-frames-clobbers-unrelated-recursive-locals](deep/bind-propagate-ancestor-frames-clobbers-unrelated-recursive-locals.md) | `:=`-bind inside any recursive routine | L | Ancestor-frame propagation clobbers same-named locals across *unrelated* recursive invocations — silent cross-frame corruption. |
| [stale-env-thread-can-resurrect-legacy-atomic-lane-mapping](deep/stale-env-thread-can-resurrect-legacy-atomic-lane-mapping.md) | Shared vars + any spawned thread | L | A thread whose `env` predates a shared-var write resurrects a stale legacy atomic-lane mapping and clobbers a fresher value through the blanket reconcile. |
| [return-rw-scalar-and-list-containers](deep/return-rw-scalar-and-list-containers.md) | `return-rw` of a scalar or list | L | `say $a` gives raku 9, mutsu 1 — the write **silently does nothing**. Shares the element-container model with ADR-0036. |
| [sigilless-alias-closure-capture-skips-typecheck](deep/sigilless-alias-closure-capture-skips-typecheck.md) | `my \x := $typed; ` captured into a closure | M | The write silently skips the type check. ADR-0042 cluster. |
| [grammar-metaclass-parameterize-stack-overflow](deep/grammar-metaclass-parameterize-stack-overflow.md) | `method ^parameterize` + parametric role application | M | Hard stack overflow (crash class). |
| [procasync-stress-segv](deep/procasync-stress-segv.md) | `roast/S17-procasync/stress.t` | — | One CI-only SEGV, not reproduced in 22 local runs. **Not actionable without a fresh crash artifact** — monitor `tmp/crash/<pid>.txt`, do not chase blind. Do NOT quarantine; a crash is a poor quarantine candidate. Explicitly unaffected by the 2026-08-27 merged-supply change. |
| [bare-name-type-constraint-store-is-scope-blind](deep/bare-name-type-constraint-store-is-scope-blind.md) | `@`/`%`-typed lexicals in routines; typed scalars in `if`/`while` bodies | M-L | Silent loss of type enforcement across scope boundaries. Scalars in routines are fixed; containers and branch/loop bodies remain. ADR-0042 cluster. |

## Tier B — Correctness, broad impact

### B1 — broad language-construct correctness

| Ticket | Effort | Why here |
|---|---|---|
| [element-itemization-lost-in-scalar-binding](deep/element-itemization-lost-in-scalar-binding.md) | M/slice | ADR-0040's originating finding. Slices 0-2 landed; the scalar-binding half (and row 24 `.VAR`) remain. |
| [call-compiled-closure-lacks-merge-all-and-dual-persistence-store](deep/call-compiled-closure-lacks-merge-all-and-dual-persistence-store.md) | XL | **Start at [ADR-0055](../docs/adr/0055-closure-free-vars-resolve-to-their-own-binding.md) slice 1.** The old `CapturePriority`/`merge_all` sequencing recorded in the file was explicitly rejected by that ADR §5. |
| [residual-try-cell-eager-seq-reification-divergences](deep/residual-try-cell-eager-seq-reification-divergences.md) | M | `.map` runs its callback eagerly. ADR-0058's target; do not patch ahead of it. |
| [for-loop-rw-element-alias-lost-through-deferred-closure](deep/for-loop-rw-element-alias-lost-through-deferred-closure.md) | XL | Binds a snapshot instead of a live alias. ADR-0036/ADR-0045 cluster. |
| [is-rw-sub-implicit-return-element-not-mutable](deep/is-rw-sub-implicit-return-element-not-mutable.md) | L | `sub ... is rw` returning an element doesn't produce a mutable container. Same cluster. |
| [for-loop-pointy-sigilless-param-write-through-missing](deep/for-loop-pointy-sigilless-param-write-through-missing.md) | L | `for LIST -> \x, $v { }` doesn't write through to the source elements. ADR-0045 cluster. |
| [chained-and-array-element-sigilless-bind-wrongly-readonly](deep/chained-and-array-element-sigilless-bind-wrongly-readonly.md) | M-L | Two-hop sigilless bind chains and binds to typed array elements are wrongly rejected as read-only. |
| [shadowing-declaration-drops-the-outer-typed-scalar-constraint](deep/shadowing-declaration-drops-the-outer-typed-scalar-constraint.md) | M | A shadowing declaration in a branch/loop body **permanently** drops the outer typed scalar's constraint. ADR-0042 §5.2 slice 2 owns it. |
| [dollar-dot-attr-compound-assign-spurious-ro-error](deep/dollar-dot-attr-compound-assign-spurious-ro-error.md) | M-L | `$.attr` is an *itemized accessor read*; mutsu has both halves of the RO rule backwards. Its own text asks for an ADR first, and states it is **not** blocked on ADR-0040. |
| [when-nonmatch-value-outside-map-grep](deep/when-nonmatch-value-outside-map-grep.md) | M | A non-matching `when`-tail block evaluates to `Nil`/`Any` outside the 4 fixed fast paths. A point fix was explicitly rejected as unsafe by its own prior investigation. |
| [whenever-expression-position-needs-real-design](deep/whenever-expression-position-needs-real-design.md) | L/XL | Two independent bugs (parser doesn't accept `whenever` as a term; the one narrow mechanism clobbers the wrong variable and binds no real `Tap`). Needs design across parser/AST/compiler/VM. |
| [supply-channel-has-no-fanout-to-multiple-taps](deep/supply-channel-has-no-fanout-to-multiple-taps.md) | L | A channel-backed Supply can only be tapped once (single `mpsc` receiver), so a second `whenever` gets nothing. Reproduces on `.stdout` too. Filed 2026-08-27. |
| [module-file-scope-array-and-hash-still-share-the-caller](deep/module-file-scope-array-and-hash-still-share-the-caller.md) | XL | Silent corruption of module state on name collision. Blocks vendor-real-Test. Large canonical-slot refactor, deferred twice. |
| [exception-class-hierarchy-is-mostly-unregistered](deep/exception-class-hierarchy-is-mostly-unregistered.md) | XL | 124 `X::` classes aren't registered types. Needs a role-vs-prefix parentage design before mechanical generation. |
| [regex-quantifier-eager-candidate-enumeration-overruns-code-blocks](deep/regex-quantifier-eager-candidate-enumeration-overruns-code-blocks.md) | L | Embedded code blocks run far more often than in raku — observable side effects. Should adopt the ADR-0009 accepted-path discipline already used for LTM. |
| [grammar-action-ordering-vs-inline-code-blocks](deep/grammar-action-ordering-vs-inline-code-blocks.md) | L | A `make`-bearing embedded code block runs at reduce time, not in match order. ADR-0009 part B. |
| [native-method-accepted-named-declarations](deep/native-method-accepted-named-declarations.md) | L | Native methods have no declared accepted-named set, so wrong-arm selection is *silent*. |
| [user-prefix-op-candidate-beats-builtin-typed-candidate](deep/user-prefix-op-candidate-beats-builtin-typed-candidate.md) | M | A user `multi prefix:<++>` wins over the builtin for `Int`/`Bool`/`Num`. |
| [boxed-int-smartmatches-as-a-native-type](deep/boxed-int-smartmatches-as-a-native-type.md) | XL | mutsu cannot tell a boxed `Int` from a native `int`. ADR-scale — `Value`'s layout is pinned by NaN-boxing. |
| [definiteness-constrained-type-object-identity-lost](deep/definiteness-constrained-type-object-identity-lost.md) | M-L | A bare `Type:D`/`Type:U` term loses its definiteness constraint entirely. |
| [resume-does-not-return-to-die-call-site-in-nested-sub](deep/resume-does-not-return-to-die-call-site-in-nested-sub.md) | L | `.resume` doesn't resume at the `die`'s call site in a nested sub; tied to how mutsu unwinds Rust frames. |
| [run-shell-discard-stdout-stderr-by-default](deep/run-shell-discard-stdout-stderr-by-default.md) | M | `run`/`shell` silently discard the child's output instead of inheriting the parent's. |
| [custom-io-handle-write-read-not-dispatched](deep/custom-io-handle-write-read-not-dispatched.md) | L | `IO::Handle` subclasses overriding WRITE/READ/EOF are ignored by print/say/read. |
| [is-typename-custom-container-store-protocol-unimplemented](deep/is-typename-custom-container-store-protocol-unimplemented.md) | L | `my @v is CustomClass = ...` never dispatches the class's `STORE`. |
| [direct-metamodel-classhow-new-type-immutable-error](deep/direct-metamodel-classhow-new-type-immutable-error.md) | M-L | `constant N := Metamodel::ClassHOW.new_type(...)` errors as "immutable". |
| [export-default-package-not-symbolically-navigable](deep/export-default-package-not-symbolically-navigable.md) | M | `EXPORT::DEFAULT` isn't a real symbolically-navigable package. |
| [unify-statement-expression-control-construct-compilation](deep/unify-statement-expression-control-construct-compilation.md) | L | Statement/expression compilation of control constructs is duplicated, not shared — architectural debt that keeps producing paired bugs (two 2026-08-27 fixes, #7048 and #7051, were each half of one). |

### B2 — batteries / dist-blocking

| Ticket | Blocks | Effort |
|---|---|---|
| [vendor-real-test-module](deep/vendor-real-test-module.md) | the vendor-real-Test campaign (its own axis; a campaign log, not one fix) | XL |
| [user-postcircumfix-index-not-dispatched-for-instances](deep/user-postcircumfix-index-not-dispatched-for-instances.md) | `Array::Rounded` | M-L — read path fixed; assignment path and native-term capture remain |
| [p5tie-stash-bind-key-protocol](deep/p5tie-stash-bind-key-protocol.md) | `P5tie` | L — needs a real `Stash.BIND-KEY` protocol; niche (~0.5% of sampled dists) |
| [template-engines-blocked-on-mutsu](deep/template-engines-blocked-on-mutsu.md) | template battery runner-ups | L (cluster) — `Template6` is the natural next pick |
| [subtest-compiled-dispatch-async-middleware-regression](deep/subtest-compiled-dispatch-async-middleware-regression.md) | re-landing #6499's `subtest` perf win (already reverted) | L — bisect from the frame-setup end, not from Cro |

## Tier N — narrow correctness / diagnostics

| Ticket | Category | Effort |
|---|---|---|
| [chained-index-assign-autoviv-loses-hole-tracking](deep/chained-index-assign-autoviv-loses-hole-tracking.md) | correctness-narrow | M |
| [typed-shaped-array-rows-lose-element-value-type](deep/typed-shaped-array-rows-lose-element-value-type.md) | correctness-narrow | M |
| [slurpy-hash-named-arg-raku-boolean-shorthand-missing](deep/slurpy-hash-named-arg-raku-boolean-shorthand-missing.md) | rendering | M |
| [begin-time-adverb-value-interpolation](deep/begin-time-adverb-value-interpolation.md) | correctness-narrow | M — models on ADR-0006 §2.2's `constant` inlining |
| [placeholder-scope-loop-while-block-boundaries](deep/placeholder-scope-loop-while-block-boundaries.md) | correctness-narrow | M — three genuinely *different* raku rules, not one shared fix |

---

## Perf — batch into one profiling session; implementation agent runs SOLO

| Ticket | Status |
|---|---|
| [yaml-parse-throughput](perf/yaml-parse-throughput.md) | Round 9; correct (81/81) but 5-35x raku. Past ADR-0016 territory, chasing a post-P5 drift with no dominant call site identified. |
| [digest-ripemd-start-per-block-overhead](perf/digest-ripemd-start-per-block-overhead.md) | 295s → 119s; needs one more ~20% win to clear the 120s CI gate. Flat profile — needs a fresh one, not a guess. |
| [bench-ctor-construction-parity](perf/bench-ctor-construction-parity.md) | The only bench where mutsu is slower than raku (1.17-1.35x). Pair with the G3 row. |
| [adr0019-g3-diffuse-bless-allocation-cost](perf/adr0019-g3-diffuse-bless-allocation-cost.md) | Blocked on a working call-graph profiler / counting allocator. |
| [interpreter-call-path-in-hot-loops](perf/interpreter-call-path-in-hot-loops.md) | Mostly resolved (13.8x → ~2x). Its redirect target was retired 2026-08-20 — don't re-measure the closed shape. |
| [bigint-repeated-addition-performance-gap](perf/bigint-repeated-addition-performance-gap.md) | ~14x raku on a growing-magnitude Fibonacci loop. New since last regen. |
| [closure-sequence-evolution-performance-gap](perf/closure-sequence-evolution-performance-gap.md) | Closure-generated evolutionary sequences much slower than raku. New. |
| [uniname-sort-performance-gap](perf/uniname-sort-performance-gap.md) | `.sort(*.uniname.chars)` over the full Unicode range ~18x slower (times out). New. |

Numbers that end up in a document must come from the **bench CI**
(`bench-history.tsv` on `bench-data`), never from the profiling session's own
local runs.

## Icebox — blocked on a decision, or a pure record

| Ticket | Blocked on / why |
|---|---|
| [rakuast-remaining](deep/rakuast-remaining.md) | A multi-campaign backlog (ADR-0011). Needs a strategic decision on which gap matters next, not more investigation. |
| [nativecall-cannot-be-vendored](deep/nativecall-cannot-be-vendored.md) | Not actionable — a measurement record with explicit reopen conditions (NQP/QAST, MoarVM dispatch programs, 61 missing `nqp::` ops). Keep as reference; do not re-derive. This is why `NativeCall` remains a justified BATTERIES.md rung-3 provider. |
| [adr0019-e2-e4-resolver-core](deep/adr0019-e2-e4-resolver-core.md) | E3/E4 closed; only E2 remains and it gates nothing (a structural fallback replaced the coverage gate). Demoted out of active work. |

---

## Housekeeping notes

- **50 of the previous regen's 73 rows named files that no longer exist.**
  This regen did not trace each to its resolving PR — assume fixed/merged
  unless a fresh repro says otherwise. `git log --diff-filter=D -- todo/` on
  the filename finds the deleting commit.
- **`deep/` is the growth area, and `vendor-real-test-module` remains its
  single largest source.** Several Tier S/B1 rows were found by that
  campaign's `t/` residue sweep rather than independently; read its tail
  section before picking one, since it tracks which roast files each residual
  gap blocks.
- **Check the ADR, not the ticket, for blocker status.** Several deep files
  name a blocker that has since advanced or shipped. Every ADR referenced in
  this file had its `Status` line verified on 2026-08-27; the per-ticket
  files did not.
