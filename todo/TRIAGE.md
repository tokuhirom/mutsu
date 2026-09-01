# TRIAGE — prioritized snapshot of todo/ (2026-09-01)

A ranked index of every open finding under `todo/tickets/`, `todo/deep/` and
`todo/perf/`, so a session can pick the next unit of work without re-reading
all of them.

This is a **snapshot, not a ledger**. Resolving a ticket does *not* require
editing this file — that would reintroduce exactly the shared-file merge
conflicts `todo/` exists to avoid. A stale row is fine; the per-ticket files
stay the source of truth. Regenerate the whole file when it has drifted too
far (re-survey every ticket, re-score, rewrite).

## What changed since the 2026-08-27 regen

Surveyed 2026-09-01: **67 files** after this regen's own closures — 45
`deep/`, 14 `tickets/`, 8 `perf/`.

- 13 rows of the previous snapshot are closed (PRs #7058 #7063 #7066 #7069
  #7070 #7076 #7113 #7130 #7132 #7146 #7147 #7149 #7151). Between 2026-08-27
  and 2026-09-01, 59 todo files were filed and 67 closed; 42 of the 59 new
  files were closed inside the same window — the fix campaigns file and drain
  their own tickets within a day or two, so a snapshot taken mid-campaign
  overstates the backlog.
- **Unlike the previous regen, every repro was re-run** on a fresh
  `target/debug/mutsu` against `raku` v2026.06 (scripts under
  `tmp/triage-verify/`, gitignored). Verdicts: `tickets/` 19 files → 9 still
  reproduce, **6 fixed**, 4 not verifiable; `deep/` 45 files → 29 still
  reproduce, 5 partially fixed, 2 changed shape, **1 fixed**, 8 not
  verifiable (records, campaign logs, needs a dist or a CI artifact). `perf/`
  was not re-measured (its numbers come from bench CI, not a local run).
- **The 6 fixed tickets are closed by this regen** (3 already carried their
  own "resolved" header and had simply never been moved; 3 were newly stale
  and got a pin each): `init-phaser-...`, `is-copy-param-...`, `take-rw-...`
  → `news/2026-08/`; `eval-declared-my-role-...`, `proxy-at-pos-...`,
  `range-assigned-to-named-scalar-...` → `news/2026-09/`. The one residual
  finding inside `take-rw` was re-filed as
  `tickets/array-seq-view-does-not-carry-element-containers.md`.
- The one fixed `deep/` file (`bare-name-type-constraint-store-is-scope-blind`)
  is *not* closed: it has no failing repro left but still tracks ADR-0042
  slices 2-3 (delete the name-keyed map). It moves to the Icebox as a cleanup
  record, and its file says so now.
- 16 files whose text had drifted from what the build does got a
  "Re-verified 2026-09-01" section (wrong raku expectation, rows that now
  pass, symptoms that moved). **Read the tail of a file before trusting its
  head.**

**Standing caveat.** A tier is a routing hint. The verification above is
one run on one box; CLAUDE.md's rule still applies — re-verify a ticket's
repro on your own build before acting on it.

## How the ranking works

- **Tier S — Soundness.** Crashes (SEGV/panic/stack overflow), memory
  unsafety, or *silent data loss* — a write that is wrong or dropped and
  nothing detects it. Always highest priority regardless of effort.
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

## `todo/tickets/` — why each one is or is not startable

Of the 14 open, **8 are startable today** and **6 are not**, for three
distinct reasons. "Not startable" almost never means "not worth doing" — it
means *the order is wrong*.

### D — Startable today (8)

| Ticket | Tier | Note |
|---|---|---|
| [gather-block-state-is-shared-across-instances](tickets/gather-block-state-is-shared-across-instances.md) | N, **S effort** | `state` inside a `gather` body is one cell for every instance (`a=1 b=2`, raku `a=1 b=1`). map/grep already scope `state` per closure instance (`state_scope_id`); the gather forcing path has no equivalent. Cheapest real fix in the queue; check the lazily-resumed coroutine path too. |
| [for-kv-multi-param-bind-decontainerizes](tickets/for-kv-multi-param-bind-decontainerizes.md) | B1 | ADR-0045 row 16. Needs a raw (non-decontainerizing) bind for an rw scalar multi-parameter in `build_for_bind_stmts`; the shape exists for `@`/`%` params. Then re-add `"kv"` to `ELEMENT_PRODUCERS`. |
| [array-seq-view-does-not-carry-element-containers](tickets/array-seq-view-does-not-carry-element-containers.md) | B1 | `for @a.Seq { $_++ }` writes nothing back. ADR-0045 seen from the `.Seq` producer; probably one more `ELEMENT_PRODUCERS` arm. Filed 2026-09-01. |
| [rakuast-nodes-have-no-stable-identity](tickets/rakuast-nodes-have-no-stable-identity.md) | N | `$p === $p` is `False` for a RakuAST node. The `===` half (identity-backed `WHICH` for `ValueView::RakuAst`) is small and self-contained; `eqv` is a separate structural question. The crash path it guards is dead today but ADR-0059 slice 3 would re-open it. |
| [io-listops-bind-colonpair-args-as-positional](tickets/io-listops-bind-colonpair-args-as-positional.md) | N | `say :d, "x"` prints `d => Truex`; raku prints `x`. `Stmt::Say(Vec<Expr>)` has no named/positional slot. ADR-0021 territory (P5 remains) but no design decision is needed — give the four io statements the `CallArg::Named` representation and have the print ops skip named args. `(a => 1)` must stay positional. |
| [lazy-list-in-scalar-loses-itemization](tickets/lazy-list-in-scalar-loses-itemization.md) | N | Only the inline `(gather ...).List` spelling renders `(1, 2)` instead of `$(1, 2)`; the type half is already fixed. Look at ADR-0038 phase 4's context-flag family before adding a third flag. |
| [procasync-merged-tap-after-start-should-throw](tickets/procasync-merged-tap-after-start-should-throw.md) | N | Missing `X::Proc::Async::TapBeforeSpawn` on the merged Supply. Raku's check is at *tap* time, so the `.stdout` accessor-time check cannot be copied; the Supply value needs the owning proc's `started` flag. |
| [procasync-output-chunks-do-not-hold-back-final-grapheme](tickets/procasync-output-chunks-do-not-hold-back-final-grapheme.md) | N | raku emits `["ab","cde","f"]`, mutsu `["abc","def"]`; on malformed UTF-8 the content differs too (`"ok"` vs `"ok-"`). Generalise the existing `held_cr` hold-back in `feed_utf8_incremental`; confirm rakudo's discard-on-error rule first. |

### A — Blocked on another design campaign (3)

| Ticket | Blocked on | Status of the blocker |
|---|---|---|
| [immutable-lvalues-that-mutsu-still-lets-you-assign-to](tickets/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md) | [ADR-0036](../docs/adr/0036-element-container-pairs-from-subscripts-and-pairs.md) slices 3-4, plus a readonly marking on the closure-call topic binding that no ADR owns yet | 6 of 7 rows still silently succeed (re-verified). `(1..3)[0] = 9` now throws the right class with the wrong rendering. *Itemization is not container-ness* — ADR-0040 landing moved none of the rows, so do not re-attribute it. |
| [free-var-read-in-callee-resolves-through-dynamic-caller-chain](tickets/free-var-read-in-callee-resolves-through-dynamic-caller-chain.md) | Its own ADR: "a routine's env parent is its lexical scope, not its caller" | `f sees 1` where raku says `5`, unchanged by ADR-0055 slice 1 and by a slice-2 prototype; ADR-0055 §7.5 records it as out of scope. Silent wrong *read* in ordinary code — the highest-leverage un-owned finding in `tickets/`. Its cheaper half ("why is `f`'s own `my $var` not visible in `f`'s env tier when a callee reads the name") is worth isolating first. |
| [for-deref-container-source-promotion-breaks-nqp-type-tests](tickets/for-deref-container-source-promotion-breaks-nqp-type-tests.md) | An audit of every `nqp::` op that type-tests a value (`src/runtime/nqp_ops.rs`) | ADR-0045 row 39, implemented and **deliberately backed out** — routing it breaks `CBOR::Simple`'s Capture round-trips in the bundled-library gate. Also: the named `<-> $x` form loses even the *direct* write, which the ticket's "direct case stays correct" claim does not cover. |

### B — Deliberate non-divergence record (1)

| Ticket | Why it stays |
|---|---|
| [multidim-oob-coordinate-nil-vs-empty-list-version-pragma](tickets/multidim-oob-coordinate-nil-vs-empty-list-version-pragma.md) | Behaviour re-confirmed exactly as recorded. Matching plain `raku` regresses two whitelisted roast files; roast is authoritative. Revisit only if per-language-version multidim branching becomes needed anyway. |

### C — Prerequisite not met; the ticket itself is mechanical (2)

Packaging steps split from the interpreter fixes they wait on. Neither dist
was fetched for this regen (network), so both counts are the files' own.

| Ticket | Last measurement (2026-08-31) |
|---|---|
| [bundle-xml-battery](tickets/bundle-xml-battery.md) | `XML` v0.3.6: raku 15/15, mutsu **13/15** (`t/make.rakutest`, `t/namespaces.rakutest` unbisected). The Proxy ticket it blamed is now closed — **re-measure before bisecting**. |
| [bundle-config-toml-once-parser-fixed](tickets/bundle-config-toml-once-parser-fixed.md) | `Config::TOML` v0.1.3: mutsu **0/19**; `Crane` v0.1.2: **3/15**. Three interpreter blockers left (array-path descent, `\UXXXXXXXX` escape, inline-tables timeout). |

---

## How to work `todo/deep/` — by ADR cluster

**Do not run `deep/` oldest-first** (the reasoning from the 2026-08-27 regen
still holds: filing order is an accident of which campaign ran last, and
`ls -tr` mtimes are corrupted by worktrees). Work it by ADR cluster: most
deep findings are waiting for a *slice of an ADR that already exists*, and
one landed slice closes several rows at once. Every ADR below had its
`Status` line read on 2026-09-01.

| ADR | Status (verified 2026-09-01) | Deep/ticket rows it would close |
|---|---|---|
| [ADR-0045](../docs/adr/0045-for-loop-parameters-bind-the-element-container.md) for-param binds the element container | **Slices 0-4 landed 2026-08-27; 5-6 open** | `for-loop-rw-element-alias-...` rows 16/19/28/30/39, tickets `for-kv-multi-param-bind`, `array-seq-view-...`, `for-deref-...` (row 39, needs the nqp audit) |
| [ADR-0036](../docs/adr/0036-element-container-pairs-from-subscripts-and-pairs.md) element container cells | Slices 1-2 landed; slice 3's producer layer landed but **`.pairs` backed out**; slice 4 open | `pairs-element-containers-leak-...`, `immutable-lvalues-...` (6 rows), row 28 of the for-loop matrix (element type constraint — ADR-0036 slice 4 is the natural owner, shared with ADR-0045 slice 5 and ADR-0042) |
| [ADR-0059](../docs/adr/0059-is-rw-routines-return-a-container.md) `is rw` routines return a container | Slices 1-2 landed **except the bare-`is rw`-tail half**; slice 3 open | `is-rw-sub-implicit-return-element-not-mutable` (Tier S — it *is* the bare-tail half), `rakuast-nodes-...`'s latent crash path (slice 3) |
| [ADR-0055](../docs/adr/0055-closure-free-vars-resolve-to-their-own-binding.md) closure free vars bind their own | **Slice 1 landed 2026-08-28; slices 2-5 not started**; slice 2's prerequisite is the `unvouched-capture-cells` ticket | `call-compiled-closure-lacks-merge-all-...` (Gap 1: `OUTER` vs `CALLER`), `unvouched-capture-cells-leak-state-across-cro-client-requests` |
| [ADR-0040](../docs/adr/0040-array-hash-elements-are-itemized-at-the-store.md) element itemization at the store | Slices 0-2 landed; **only row 24 (`.VAR`, slice 3) still diverges**; slices 4-5 = constraint + compensator deletion | `element-itemization-lost-in-scalar-binding` (row 24 only), `slurpy-hash-named-arg-...` (fold in) |
| [ADR-0042](../docs/adr/0042-type-constraints-belong-to-the-container-not-to-a-name.md) container-carried type constraints | Slice 1 landed; slices 2-3 open — **no failing repro remains** | `bare-name-type-constraint-...` (cleanup record only) |
| [ADR-0058](../docs/adr/0058-map-grep-produce-a-deferred-seq.md) map/grep produce a deferred Seq | **Proposed** | `residual-try-cell-eager-seq-reification-divergences` (11 `todo` rows in `t/map-callback-runs-at-consumption.t`) |
| [ADR-0052](../docs/adr/0052-a-when-clause-produces-its-value-on-the-stack.md) `when` value on the stack | Slice 1 landed; 2-4 open | `when-nonmatch-value-outside-map-grep` |
| [ADR-0053](../docs/adr/0053-do-whenever-produces-a-tap-on-the-stack.md) `do whenever` produces a Tap | **Proposed — but stale**: `.WHAT` already answers `Tap`; only the subscription-identity half (`Tap.close` drops a pre-close emit) is open | `whenever-expression-position-needs-real-design` |
| [ADR-0048](../docs/adr/0048-placeholder-scope-is-a-block-invocation-contract.md) placeholder scope | P1-P3 landed; P4-P5 open | `placeholder-scope-loop-while-block-boundaries` (only `while`/`until` remain) |
| [ADR-0041](../docs/adr/0041-sub-hoisting-vs-compile-time-name-visibility.md) sub hoisting vs name visibility | **Proposed, investigation only** | `user-postcircumfix-index-...` item 2 (Tier S: stack overflow) |
| [ADR-0039](../docs/adr/0039-container-lexicals-resolve-lexically.md) container lexicals resolve lexically | Slice 1 landed; slice 2 open | `module-file-scope-array-and-hash-...` (two concrete slice-2 acceptance rows added 2026-09-01) |
| [ADR-0047](../docs/adr/0047-type-identity-is-a-declaration-site-not-a-registry-name.md) type identity | P1-P2 landed; P3-P4 open | `subtest-compiled-dispatch-async-middleware-regression` (P4 re-lands #6499; the regression itself is independent) |

**Recommended next campaign.** The element-container model is still the
cluster with the most rows: **ADR-0045 slices 5-6 together with ADR-0036
slice 4** (the element type constraint on the promoted cell is shared three
ways and ADR-0036 slice 4 is its natural owner) closes rows 19/28/30 of the
for-loop matrix, the `is rw`-over-List bind rejection, and one `immutable-
lvalues` row, and unblocks the two `ELEMENT_PRODUCERS` tickets. Then
**ADR-0059's bare-tail half** (Tier S below), then **ADR-0055 slice 2's
prerequisite** (the Cro-blocking cell-freshness fix).

**One measured process exception, kept from the previous regen:** when a
change alters a *universal property of values* ("what is in every
container"), run the full local `make roast` before pushing (ADR-0040 slice
2 needed 17 counter-current fixes, 9 found only by roast). Ordinary
parser/operator/dispatch fixes still delegate to CI.

---

## Tier S — Soundness (crashes, memory unsafety, silent data loss)

| Ticket | Breadth | Effort | Why here |
|---|---|---|---|
| [user-postcircumfix-index-not-dispatched-for-instances](deep/user-postcircumfix-index-not-dispatched-for-instances.md) | any module aliasing `&postcircumfix:<[ ]>` before overriding it (`Array::Rounded` idiom) | L | **Hard stack overflow** (re-verified): `my constant &old-same = &postcircumfix:<[ ]>` resolves to the module's own hoisted multi. ADR-0041 is `Proposed`. Item 3 (imported constant alias to a type) is wider than recorded: even the bareword `Rounded.new` answers `Array`. |
| [is-rw-sub-implicit-return-element-not-mutable](deep/is-rw-sub-implicit-return-element-not-mutable.md) | every `sub ... is rw { %h<k> }` / `{ @a[i] }` accessor idiom | M-L | `walk(%hash) = "val"` **silently does nothing** (exit 0). It is ADR-0059 slice 2's open bare-tail half: the caller-side tail re-interpretation cannot see a parameter. Compile the bare tail to its container and delete the re-interpretation. |
| [sigilless-alias-closure-capture-skips-typecheck](deep/sigilless-alias-closure-capture-skips-typecheck.md) | any `:=` alias written from a stored closure | M-L | The write is silently lost and the type check silently skipped. **Regressed since filing**: the immediately-invoked mainline block (D4) now loses the alias too. Not an ADR-0055 problem (measured); start at alias identity (`propagate_bind_to_ancestor_frames`). |
| [for-loop-pointy-sigilless-param-write-through-missing](deep/for-loop-pointy-sigilless-param-write-through-missing.md) | `for $a, 1, $b, 2 -> \x, $v { x = $v }` | M | No write-through at all (`a= b=`), so the type check that `Native::Overflow`'s suite relies on never fires. Compare how `Stmt::For` compiles a comma-list of `Expr::Var`s vs a single `ArrayVar`. |
| [procasync-stress-segv](deep/procasync-stress-segv.md) | `roast/S17-procasync/stress.t`, `integration/advent2014-day05.t` | M (diagnostics) | CI-only SEGV, ~130 clean local runs. **Not actionable as a crash hunt**; the actionable slice is §8.2: make a fatal signal on a non-`mutsu-main` thread produce a crash report (three recurrences, zero backtraces). Do NOT quarantine. |

## Tier B — Correctness, broad impact

### B1 — broad language-construct correctness

| Ticket | Effort | Why here |
|---|---|---|
| [call-compiled-closure-lacks-merge-all-and-dual-persistence-store](deep/call-compiled-closure-lacks-merge-all-and-dual-persistence-store.md) | XL | Closure free var resolves to `CALLER` where raku says `OUTER` (re-verified). ADR-0055 slices 2-5; the `merge_all` knob the file proposed is rejected by the ADR. |
| [unvouched-capture-cells-leak-state-across-cro-client-requests](deep/unvouched-capture-cells-leak-state-across-cro-client-requests.md) | M-L | The mechanism that closes ADR-0055 §1.2(b) was built, validated (full roast green) and **removed** because a stale cell leaks request state across `Cro::HTTP::Client.request`'s recursive redirect. Two candidate fixes, both need a cell-freshness design. Gate: the batteries suite, which `make test` does not run. |
| [residual-try-cell-eager-seq-reification-divergences](deep/residual-try-cell-eager-seq-reification-divergences.md) | L | `.map`/`.grep` run their callback eagerly (side effects before `say "before"`). ADR-0058's target; implementing it makes mutsu stricter, so full local `make roast` is mandatory. |
| [for-loop-rw-element-alias-lost-through-deferred-closure](deep/for-loop-rw-element-alias-lost-through-deferred-closure.md) | M | Headline fixed; rows 17/24 now pass too (file updated). Residue: rows 16/19/28/30/39 — ADR-0045 slices 5-6. |
| [element-itemization-lost-in-scalar-binding](deep/element-itemization-lost-in-scalar-binding.md) | M | Nearly closed: only ADR-0040 row 24 (`.VAR` on a `:=`-bound list) diverges. Retire when slice 5 lands. |
| [pairs-element-containers-leak-through-pair-value-consumers](deep/pairs-element-containers-leak-through-pair-value-consumers.md) | L | Latent: `.pairs` is deliberately unrouted because a cell-valued Pair aliases hashes and collapses BagHash weights through 15+ structural consumers. Needs the Pair-value read-boundary decision (ADR-0036 slice 3). Note hash `.pairs[0].value.VAR` is `Int` while array `.pairs` already answers `Scalar`. |
| [dollar-dot-attr-compound-assign-spurious-ro-error](deep/dollar-dot-attr-compound-assign-spurious-ro-error.md) | L | **Symptom moved**: `$.x *= 2` no longer throws — it now *mutates* a non-`rw` attribute (raku: silent no-op), and `$.x = 9` still mutates (raku: throws). Both halves are silent over-mutation now. Needs the "accessor read is an itemized copy" ADR; explicitly not ADR-0040. |
| [when-nonmatch-value-outside-map-grep](deep/when-nonmatch-value-outside-map-grep.md) | L | Non-matching `when`-tail block gives `Nil`/`Any` outside the four fast paths; ADR-0052 slices 2-4 (three disagreeing statement-sequence compilers). |
| [whenever-expression-position-needs-real-design](deep/whenever-expression-position-needs-real-design.md) | M | **Symptom moved**: both legal shapes answer `Tap` now; `Tap.close` retroactively drops the value emitted before it. Reconcile ADR-0053's "not started" header with what landed, then do the identity slice. |
| [supply-channel-has-no-fanout-to-multiple-taps](deep/supply-channel-has-no-fanout-to-multiple-taps.md) | L | Second `whenever` on `$proc.stdout` gets nothing (single `mpsc` receiver). Live-vs-on-demand replay semantics must be respected; unify with the `Supplier` registry. |
| [module-file-scope-array-and-hash-still-share-the-caller](deep/module-file-scope-array-and-hash-still-share-the-caller.md) | L | Module shape fixed (ADR-0039 slice 1); by-name container resolution in inner blocks/closures still corrupts (`[9 3]`/`[]` rows added 2026-09-01). Slice 2. |
| [grammar-action-ordering-vs-inline-code-blocks](deep/grammar-action-ordering-vs-inline-code-blocks.md) | L | A `make`-bearing embedded block runs at reduce time, out of order (byte-identical to filing). Needs a write channel into the live capture accumulator + backtrack undo; lands under full roast + battery coverage or not at all. |
| [regex-quantifier-eager-candidate-enumeration-overruns-code-blocks](deep/regex-quantifier-eager-candidate-enumeration-overruns-code-blocks.md) | L | Embedded blocks fire per *computed* candidate (5 vs 2, 17 vs 3). Quantifier-matching architecture change; ADR-0009's "never execute user code while measuring" discipline is the prior art. |
| [native-method-accepted-named-declarations](deep/native-method-accepted-named-declarations.md) | L | An unknown named silently lands in a positional slot on six measured methods (`"abc".chop(:zzz)` → `abc`). Two designs, ADR first. |
| [user-prefix-op-candidate-beats-builtin-typed-candidate](deep/user-prefix-op-candidate-beats-builtin-typed-candidate.md) | L | Untyped user `prefix:<++>` beats the builtin for Int/Bool/Num; `infix:<+>` user candidate makes `1 + 2` answer `USER`. Native operators are not dispatch candidates at all. |
| [definiteness-constrained-type-object-identity-lost](deep/definiteness-constrained-type-object-identity-lost.md) | L | Bare `Any:D` is indistinguishable from `Any`; `.^base_type` dies. Needs a `DefiniteHOW`-equivalent representation ADR. |
| [resume-does-not-return-to-die-call-site-in-nested-sub](deep/resume-does-not-return-to-die-call-site-in-nested-sub.md) | L/XL | `.resume` after a `die` in a nested sub skips the rest of the block silently. Continuation-shaped; tied to how Rust frames unwind. |
| [custom-io-handle-write-read-not-dispatched](deep/custom-io-handle-write-read-not-dispatched.md) | L | Subclass `WRITE`/`READ`/`EOF` ignored by print/say/read; `$*OUT = $store` writes to the real fd. |
| [is-typename-custom-container-store-protocol-unimplemented](deep/is-typename-custom-container-store-protocol-unimplemented.md) | L | `my @v is DNA = ...` never calls `STORE`. Scope it first (grep the corpus for `method STORE`). |
| [export-default-package-not-symbolically-navigable](deep/export-default-package-not-symbolically-navigable.md) | M-L | `::("Test::EXPORT::DEFAULT::&ok")` fails; `Test.WHO` has no `EXPORT`. Decide how deep to model export tags. |
| [direct-metamodel-classhow-new-type-immutable-error](deep/direct-metamodel-classhow-new-type-immutable-error.md) | M/L | Headline fixed. Remaining: a `.^add_method`-installed method on a `new_type` type object is registered but calling it is a silent no-op; `does Metamodel::Naming/Stashing` is not a valid type. |
| [unify-statement-expression-control-construct-compilation](deep/unify-statement-expression-control-construct-compilation.md) | XL | Architectural debt, still growing (`helpers_do_expr.rs` 609 → 659 lines; two `ForLoopSpec` construction sites). Keeps producing paired half-bugs. |

### B2 — batteries / dist-blocking

| Ticket | Blocks | Effort |
|---|---|---|
| [vendor-real-test-module](deep/vendor-real-test-module.md) | making the vendored upstream `Test` the default (retiring the native provider) | XL as a campaign — but **its remaining blocker is a perf row**: `perf/interpreter-call-path-in-hot-loops.md`'s `&`-sigil parameter gate (below). 1427 files pass under both providers; `S03-buf/{write-int,read-write-bits}.t` time out, `S24-testing/{2-force_todo,6-done_testing}.t` are whitelist rows. |
| [template-engines-blocked-on-mutsu](deep/template-engines-blocked-on-mutsu.md) | the template battery runner-ups | L (cluster) — **re-survey first**: both `Template::Jinja2` blockers are closed in `news/`, so its "0/23" row is unmeasured. `Template6` is still unreduced. |
| [p5tie-stash-bind-key-protocol](deep/p5tie-stash-bind-key-protocol.md) | `P5tie`, `annotations` (~0.5% of sampled dists) | L — `Stash.BIND-KEY` / `CALLER::.BIND-KEY` both missing (re-verified). Rung-2 machinery only. |
| [subtest-compiled-dispatch-async-middleware-regression](deep/subtest-compiled-dispatch-async-middleware-regression.md) | re-landing #6499's `subtest` perf win | M-L — root cause unknown; bisect from the dispatch end with `rust-gdb` frame diffs, not from Cro. ADR-0047 P4 is a prerequisite for re-landing but not a fix. |

## Tier N — narrow correctness / diagnostics

| Ticket | Category | Effort / note |
|---|---|---|
| [chained-index-assign-autoviv-loses-hole-tracking](deep/chained-index-assign-autoviv-loses-hole-tracking.md) | correctness-narrow | S-M — `@a[0][1] = 5; @a[0][0]:exists` is `True`; the `;` form is fixed. Find the chained autoviv site. |
| [typed-shaped-array-rows-lose-element-value-type](deep/typed-shaped-array-rows-lose-element-value-type.md) | self-consistency (no raku oracle) | S-M — thread `value_type` through `make_shaped_array_seeded`'s rows. |
| [chained-and-array-element-sigilless-bind-wrongly-readonly](deep/chained-and-array-element-sigilless-bind-wrongly-readonly.md) | spurious die on valid code | M — message changed to `Cannot modify an immutable Int (5)`, so the `mark_readonly` hypothesis is probably stale; break with gdb first. |
| [end-phasers-install-at-compile-time](deep/end-phasers-install-at-compile-time.md) | correctness-narrow | M-L — an END in a never-run block / uncalled sub never runs (`main loop` vs `main loop uncalled-sub never-run-block`); same-line ordering tie-break. Filed 2026-09-01. |
| [module-toplevel-private-sub-leak-cleanup](deep/module-toplevel-private-sub-leak-cleanup.md) | accepts code raku rejects | M — a non-exported module `sub helper` stays callable bare after `require`. Needs an exhaustive audit of ambient `GLOBAL::` installers first (a generalisation was tried and reverted). |
| [native-method-cannot-return-an-lvalue-container](deep/native-method-cannot-return-an-lvalue-container.md) | missing feature | L — **the `.VAR = 5` row was wrong (raku dies too)**; `.snitch = 666` is the only acceptance case. Needs the container-propagation design campaign. |
| [placeholder-scope-loop-while-block-boundaries](deep/placeholder-scope-loop-while-block-boundaries.md) | diagnostics / arity | M — only `while`/`until` remain (boolified bind, arity leak): ADR-0048 P4-P5. |
| [slurpy-hash-named-arg-raku-boolean-shorthand-missing](deep/slurpy-hash-named-arg-raku-boolean-shorthand-missing.md) | rendering | fold into ADR-0040; do not fix `Hash.raku` in isolation. |
| [begin-time-adverb-value-interpolation](deep/begin-time-adverb-value-interpolation.md) | correctness-narrow | L, **low priority** by its own assessment — no roast coverage; would add a whole-AST name-rewriting pass. |

---

## Perf — batch into one profiling session; implementation agent runs SOLO

| Ticket | Status |
|---|---|
| [interpreter-call-path-in-hot-loops](perf/interpreter-call-path-in-hot-loops.md) | **Highest-value perf row: it is the vendored-`Test` blocker.** An `&`-sigil *parameter* costs 6.75x a `$` one (4.32 vs 0.64 µs/iter) and forces a full by-name callee re-resolve per call even when the callable is never invoked; every real-`Test` assertion declares one. Fix the signature-shape gate, not the call site. Measure with `instructions:u` + core pinning; return the accumulator or raku deletes the benchmark. |
| [bench-ctor-construction-parity](perf/bench-ctor-construction-parity.md) | Round 5 found the "flat profile" conclusion of rounds 2-4 was wrong — a per-call `.map` compile. Lesson: **`MUTSU_VM_STATS` `add_constant` must stay flat on a steady-state loop**; growth = a runtime compile. Three unmeasured leads inside `dispatch_bless` remain. |
| [closure-literal-creation-cost](perf/closure-literal-creation-cost.md) | Parts A/C done (−20%/creation, body shared). Part B (O(kept-env) capture) is ADR territory — narrowing the kept set trusts an incomplete static analysis, the `roles-6e.t` flake shape. Cost the "share the system-name portion through the parent chain" alternative first. |
| [digest-ripemd-start-per-block-overhead](perf/digest-ripemd-start-per-block-overhead.md) | Title is historical (the `start` lever is closed). `t/ripemd.t` ~148-156s vs a hard 120s gate; profile is flat; needs a fresh dominant item, not a guess. |
| [yaml-parse-throughput](perf/yaml-parse-throughput.md) | ~5x raku on real files after nine rounds; open items: candidate enumeration, `invoke_grammar_actions` materializations. Carries the three most valuable methodology notes in `todo/` (CPU-spinner check before any A/B; deep-recursion `perf` children percentages are misattributions; compute from the tag probe you already have). |
| [adr0019-g3-diffuse-bless-allocation-cost](perf/adr0019-g3-diffuse-bless-allocation-cost.md) | Blocked on a working call-graph profiler (`addr2line` stale build-id entries under `/root/.debug`). Pair with the bench-ctor row. |
| [bigint-repeated-addition-performance-gap](perf/bigint-repeated-addition-performance-gap.md) | ~14x raku — **on a debug build**; re-measure on release before ranking. |
| [closure-sequence-evolution-performance-gap](perf/closure-sequence-evolution-performance-gap.md) | ~84x raku — **debug numbers**; the combined case (48s) far exceeds the sum of its parts (~7.5s), which is the actionable signal. |

Numbers that end up in a document must come from the **bench CI**
(`bench-history.tsv` on `bench-data`), never from the profiling session's own
local runs.

## Icebox — blocked on a decision, or a pure record

| Ticket | Blocked on / why |
|---|---|
| [bare-name-type-constraint-store-is-scope-blind](deep/bare-name-type-constraint-store-is-scope-blind.md) | **No failing repro left** (21 rows match raku). Open only as the tracking record for ADR-0042 slices 2-3 (scalar cell carries its `of`; delete `var_type_constraints`). Do not dispatch as a bug. |
| [exception-class-hierarchy-is-mostly-unregistered](deep/exception-class-hierarchy-is-mostly-unregistered.md) | Done except R5 (re-run the real-`Test` sweep), which waits on `vendor-real-test-module`. All 373 rakudo `X::` subtypes match. |
| [rakuast-remaining](deep/rakuast-remaining.md) | ADR-0033 Phase 3 + an undesigned read-gap list (`%h{...}` touches ~225 `Expr::Index` sites). Zero roast dependents; pick by user impact, not cadence. |
| [nativecall-cannot-be-vendored](deep/nativecall-cannot-be-vendored.md) | Measurement record with reopen conditions; blocker 3 (parser) is now gone, 1/2/4 stand. Keeps `NativeCall` a justified rung-3 provider. |
| [adr0019-e2-e4-resolver-core](deep/adr0019-e2-e4-resolver-core.md) | E3/E4 closed; E2 is a non-gating counter cleanup (`native_call_unmodeled`, currently 0 on a trivial run). |

---

## Housekeeping notes

- **Closed since the previous regen**, with the resolving PR, in case a
  reader has the old snapshot: bind-propagate-ancestor-frames #7066,
  grammar-metaclass-parameterize-stack-overflow #7063,
  return-rw-scalar-and-list-containers #7070,
  run-shell-discard-stdout-stderr #7113,
  stale-env-thread-atomic-lane (ADR-0062) #7069, uniname-sort perf #7058,
  anonymous-grammar #7146, backtrace-fewer-frames #7151 (the previous
  snapshot called it a deliberate non-divergence; it was fixed anyway),
  nativecall-callback-marshalling (ADR-0063) #7076, pod-block-gist #7132,
  reduce-metaop-zero-arg #7149, repl-routine #7147, thread-clone-backtrace
  #7130.
- **Verification scripts** for every row live under `tmp/triage-verify/`
  (`tickets/`, `deep/`, `deep/lib/`); `tmp/` is gitignored, so they do not
  survive a fresh checkout — each file's "Re-verified" section names the
  shape, and the repro blocks in the tickets are enough to regenerate them.
- **Two `perf/` files carry debug-build numbers** (bigint, closure-sequence).
  CLAUDE.md's rule is release for wall-clock; do not rank them off those
  figures.
- **Check the ADR, not the ticket, for blocker status** — and now also check
  the ticket's *tail*, where the 2026-09-01 re-verification notes live.
  Three ADR headers are themselves behind their code (ADR-0053 says "not
  started" while `.WHAT` already works; ADR-0059's bare-tail half is this
  regen's Tier S row; ADR-0055 §7.4's slice-2 prerequisite is a `deep/`
  ticket).
