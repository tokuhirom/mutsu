# TRIAGE — prioritized snapshot of todo/ (2026-09-04)

A ranked index of every open finding under `todo/tickets/`, `todo/deep/` and
`todo/perf/`, so a session can pick the next unit of work without re-reading
all of them.

This is a **snapshot, not a ledger**. Resolving a ticket does *not* require
editing this file — that would reintroduce exactly the shared-file merge
conflicts `todo/` exists to avoid. A stale row is fine; the per-ticket files
stay the source of truth. Regenerate the whole file when it has drifted too
far (re-survey every ticket, re-score, rewrite).

## What changed since the 2026-09-01 regen

Surveyed 2026-09-04: **66 files** — 46 `deep/`, 8 `tickets/`, 12 `perf/`.

- **29 todo files closed in three days** (PRs #7190-#7259), and 18 new ones
  were filed. Twelve of the closures were rows the previous snapshot listed
  as open. The `tickets/` queue turned over almost completely: 6 of its 8
  entries did not exist on 2026-09-01.
- **The element-container cluster is finished.** ADR-0036, ADR-0040 and
  ADR-0045 are all **closed with every slice landed** and their §1.3 tables
  re-measured against raku (12/12, 25/25, 45/45). That cluster dominated the
  previous three snapshots' "recommended next campaign" and no longer exists.
  What it left behind are five small `tickets/` residues, not further slices.
- **Tier S went from 5 rows to 4, with three closed and two new.** Closed:
  `is-rw-sub-implicit-return-element-not-mutable` (#7190, ADR-0059's
  bare-`is rw`-tail half), `sigilless-alias-closure-capture-skips-typecheck`
  (#7240), `for-loop-pointy-sigilless-param-write-through-missing` (#7245).
  New: `multidim-assign-to-an-expression-target-is-dropped` and
  `proxy-assigned-into-an-array-is-not-fetched`, both silent wrong writes.
- **A stale diagnosis was found in the top-ranked perf row** — see the perf
  section. `interpreter-call-path-in-hot-loops` says the vendored-`Test`
  blocker is an `&`-sigil *parameter* gate costing 6.75x. **That gate is
  closed** (measured today: identical opcode profile and timing for `sub f(&c)`
  vs `sub f($c)`), while the per-assertion cost it was blamed for is
  **unchanged** at ~40x raku. The symptom is real; the cause named in the file
  is not. Do not start from that file's "Where to start" section.

### What was re-verified for this regen

Every row this snapshot ranks in Tier S or B1, plus all eight `tickets/` and
all six `deep/` files filed after the last regen, was re-run today against
`raku` v2026.06 on a fresh `target/debug/mutsu` (release for the perf rows).
Verdicts are folded into the tables below; the ones that *moved* are called
out explicitly. Rows in Tier N and the Icebox are carried over from the
2026-09-01 verification unless a note says otherwise.

**Standing caveat.** A tier is a routing hint. The verification above is one
run on one box; CLAUDE.md's rule still applies — re-verify a ticket's repro on
your own build before acting on it, and read the *tail* of a ticket file, where
successive "Re-verified" sections accumulate.

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

| Ticket | Breadth | Effort | Verified 2026-09-04 |
|---|---|---|---|
| [user-postcircumfix-index-not-dispatched-for-instances](deep/user-postcircumfix-index-not-dispatched-for-instances.md) | any module aliasing `&postcircumfix:<[ ]>` before overriding it (`Array::Rounded` idiom) | L | **Reconfirmed as a hard crash**: `my constant &old-same = &postcircumfix:<[ ]>` plus two `Baz:D` candidates ends in `thread 'mutsu-main' has overflowed its stack`, core dumped; raku prints `20`/`30`. ADR-0041 is still `Proposed` — **read the ADR first**, it already records the rejected fixes. |
| [multidim-assign-to-an-expression-target-is-dropped](tickets/multidim-assign-to-an-expression-target-is-dropped.md) | every non-variable multi-dim assignment target | M | **Reconfirmed**: `my %o; %o<inner>{1;2} = 5; say %o.raku` gives `{}` (raku: the nested hash), exit 0, nothing reported. `MultiDimIndexAssignGeneric` mutates a popped copy and drops it. The obvious `scalar_bind_autovivify` fix was tried and does not work; needs `fresh_autoviv_container`/`assign_into_nested_container` reachable for an expression target. |
| [proxy-assigned-into-an-array-is-not-fetched](deep/proxy-assigned-into-an-array-is-not-fetched.md) | every `my @a = Proxy.new(...)`; and a `for`-loop landmine in `t/for-loop-element-alias.t` | M-L | **Reconfirmed**: `my @a = Proxy.new(...)` stores `[Proxy]` where raku FETCHes to `[5]`. Worse than a rendering bug — the `for`-loop compensation that hides it **flips on an unrelated same-named lexical anywhere in the unit**, so an `is rw` loop fires the Proxy's `STORE` and corrupts an outer variable. Fix the *store* (ADR-0040's boundary), then delete the loop carve-out and the `$p`/`$q` workaround comment. |
| [procasync-stress-segv](deep/procasync-stress-segv.md) | `roast/S17-procasync/stress.t`, `integration/advent2014-day05.t` | M (diagnostics) | Carried over. CI-only SEGV, ~130 clean local runs. **Not actionable as a crash hunt**; the actionable slice is §8.2 — make a fatal signal on a non-`mutsu-main` thread produce a crash report (three recurrences, zero backtraces). Do NOT quarantine. |

The two new rows are the same shape of defect and both live in the *store*
boundary ADR-0040 established. Doing them together is the coherent campaign.

---

## `todo/tickets/` — all 8, and whether they are startable

Six of these eight were filed after the previous regen; the queue is now
almost entirely the residue of the element-container campaign plus ADR-0065's
first slices.

### Startable today (5)

| Ticket | Tier | Note (verified 2026-09-04) |
|---|---|---|
| [multi-param-read-only-closure-capture-snapshots-the-element](tickets/multi-param-read-only-closure-capture-snapshots-the-element.md) | B1, **M** | `for @a -> $x is rw, $y is rw { $c = -> { $x } }` then `@a[0]=99` reads `1`; raku `99`. The *write* direction is correct, which proves the alias is real — it is the read-only **capture** that snapshots. Exactly CLAUDE.md's by-value-capture hazard: a slot holding a `ContainerRef` must always be captured by cell. Start at `resolve_capture_slot` / `compute_owned_captures`; check the ADR-0027 per-iteration-identity pins before widening. |
| [undefined-typed-scalar-loses-its-constraint-when-aliased](tickets/undefined-typed-scalar-loses-its-constraint-when-aliased.md) | N-B, **M** | `my S $c; my \y := $c; y = 1000` gives `Cannot modify an immutable Package ((S))`; raku type-checks. An *uninitialized* typed scalar hands the bind its type object instead of a container. Has a named consumer (`Native::Overflow`'s `t/01-basic.rakutest`) and the defined-source case already works, so the diff is small. |
| [promoted-element-cell-does-not-know-its-container-name](tickets/promoted-element-cell-does-not-know-its-container-name.md) | N (message quality), **S-M / L** | `Type check failed for an element of @` where raku says `@a`. The *check* is right everywhere; only the name is missing. Option 2 (retag at the naming opcodes) is a cheap interim; option 1 (an owner field beside `value_type`) is the honest fix and belongs in ADR-0042's orbit — it is the *name* half of the same descriptor. Nothing measured depends on the wording, so this is a strict improvement, not a repair. |
| [list-literal-does-not-capture-element-containers](tickets/list-literal-does-not-capture-element-containers.md) | N-B, **M** | `my (\p,\q) := (@a[0],@a[1]); p=9` dies (`Cannot modify an immutable Int (1)`); raku writes `[9 2]`. `compile_call_arg` tags a source container only from a source *name*, and `Expr::Index` has none. Changes what every parenthesised list holds, so it wants its own measurement pass. Paired with `deep/immutable-list-element-bind-is-writable`. |
| [procasync-output-chunks-do-not-hold-back-final-grapheme](tickets/procasync-output-chunks-do-not-hold-back-final-grapheme.md) | N, **M** | Carried over. raku emits `["ab","cde","f"]`, mutsu `["abc","def"]`; on malformed UTF-8 the content differs too. Generalise the existing `held_cr` hold-back in `feed_utf8_incremental`; confirm rakudo's discard-on-error rule first. |

### Rewrite before dispatching (1)

| Ticket | Why |
|---|---|
| [producer-seq-index-read-decontainerizes-the-element-cell](tickets/producer-seq-index-read-decontainerizes-the-element-cell.md) | **Its five headline rows all match raku now** (measured today: `(@a.values)[0].VAR.^name` → `Scalar`, and the three silent-write rows all write through; `.WHAT` correctly stays `(Str)`). The ticket's "Root cause" section — `exec_index_op_with_positional` normalizing a `Seq` through `resolve_array_entry` — no longer describes a failing case. **One residual survives**, the file's own row 67: `my \s = @a.values; s[0] = "x"` leaves `@a` unchanged (raku `[x B]`). Rewrite the file down to that one row before anyone starts from the obsolete blast-radius analysis. |

### Prerequisite not met; the ticket itself is mechanical (1)

| Ticket | Blocked on |
|---|---|
| [analysis-parse-mints-process-unique-registry-names](tickets/analysis-parse-mints-process-unique-registry-names.md) | ADR-0065's analysis parse entry point. Each anonymous declaration leaks one interned registry name **per parse** (measured: 1.00/parse, ~0.5 KiB/parse, linear over 8000 re-parses) — the only unbounded component S0 found. The fix is a unit-local counter mode on the analysis entry point, so it must be done **with** that entry point, not before it. **Do not "fix" it by resetting the global counters** — the file explains why that breaks cross-unit uniqueness. |

The eighth ticket, `multidim-assign-to-an-expression-target-is-dropped`, is
ranked in **Tier S** above rather than here.

---

## How to work `todo/deep/` — by ADR cluster

**Do not run `deep/` oldest-first** (filing order is an accident of which
campaign ran last, and `ls -tr` mtimes are corrupted by worktrees). Work it by
ADR cluster: most deep findings wait on a *slice of an ADR that already
exists*, and one landed slice closes several rows. Every `Status` line below
was read on 2026-09-04.

| ADR | Status (read 2026-09-04) | Rows it would close |
|---|---|---|
| [ADR-0036](../docs/adr/0036-element-container-pairs-from-subscripts-and-pairs.md) element-container Pairs | **Implemented — every slice landed** (slice 5's 69-row sweep completed 2026-09-01) | Nothing. Residues are tickets. |
| [ADR-0040](../docs/adr/0040-array-hash-elements-are-itemized-at-the-store.md) element itemization at the store | **Complete; its one named residue closed the same day** | Nothing directly — but the two new Tier S rows are *store-boundary* defects in exactly this ADR's territory, and it is the natural owner of the Proxy-FETCH rule. |
| [ADR-0045](../docs/adr/0045-for-loop-parameters-bind-the-element-container.md) for-param binds the element container | **Accepted — fully implemented**, §1.3 re-measured 45/45 | Nothing. `tickets/multi-param-read-only-closure-capture` is the read half of rows 11/20 for the *multi*-parameter shapes and is a capture bug, not an ADR-0045 slice. |
| [ADR-0059](../docs/adr/0059-is-rw-routines-return-a-container.md) `is rw` routines return a container | Slices 1-2 implemented (**the bare-tail half landed 2026-09-01**); **slice 3 open** | Previously the Tier S row; now only slice 3 remains, with no failing repro attached to it. |
| [ADR-0048](../docs/adr/0048-placeholder-scope-is-a-block-invocation-contract.md) placeholder scope | **Accepted; P1-P4 landed, P5's scope half landed, its value half deferred** | `role-body-placeholder-mu-supply` (P5's value half — see its own "How much this is worth": corpus hits = zero) |
| [ADR-0055](../docs/adr/0055-closure-free-vars-resolve-to-their-own-binding.md) closure free vars bind their own | Slice 1 landed 2026-08-28; **slices 2-5 not started**; §7 records two prerequisites | `call-compiled-closure-lacks-merge-all-...`, `unvouched-capture-cells-leak-state-across-cro-client-requests` (slice 2's prerequisite) |
| [ADR-0065](../docs/adr/0065-language-server-targets-ai-agents.md) LSP targets AI agents | **Accepted; S0/S1 shipped 2026-09-03, S2 landed** | `lsp-references-needs-a-side-table-not-ast-spans` (amends D6/S5b), `tickets/analysis-parse-mints-...` (needs S1's entry point) |
| [ADR-0042](../docs/adr/0042-type-constraints-belong-to-the-container-not-to-a-name.md) container-carried type constraints | Slice 1 landed; **slices 2-3 open** — no failing repro remains | `bare-name-type-constraint-...` (cleanup record only) — but `tickets/promoted-element-cell-...` is the *name* half of the same descriptor and would ride along with slice 2's design |
| [ADR-0039](../docs/adr/0039-container-lexicals-resolve-lexically.md) container lexicals resolve lexically | Slice 1 landed 2026-08-20; §8.2 and the `our` case closed separately; **slice 2 open** | `module-file-scope-array-and-hash-still-share-the-caller` |
| [ADR-0052](../docs/adr/0052-a-when-clause-produces-its-value-on-the-stack.md) `when` value on the stack | Accepted; slice 1 landed, **2-4 open** | `when-nonmatch-value-outside-map-grep` (reconfirmed: `Any` where raku gives `Bool::False`) |
| [ADR-0053](../docs/adr/0053-do-whenever-produces-a-tap-on-the-stack.md) `do whenever` produces a Tap | **Proposed — and its header is behind the code**: `.WHAT` already answers `Tap`; only the subscription-identity half is open | `whenever-expression-position-needs-real-design` |
| [ADR-0041](../docs/adr/0041-sub-hoisting-vs-compile-time-name-visibility.md) sub hoisting vs name visibility | **Proposed (investigation only; no implementation plan chosen)** | `user-postcircumfix-index-...` item 2 — **the surviving Tier S crash**. This is the highest-value un-owned ADR in the list. |
| [ADR-0058](../docs/adr/0058-map-grep-produce-a-deferred-seq.md) map/grep produce a deferred Seq | **Proposed** | `residual-try-cell-eager-seq-reification-divergences` |
| [ADR-0047](../docs/adr/0047-type-identity-is-a-declaration-site-not-a-registry-name.md) type identity | P1-P2 landed; **P3-P4 open** | `subtest-compiled-dispatch-async-middleware-regression` (P4 is a prerequisite for re-landing #6499, not a fix for the regression) |
| [ADR-0024](../docs/adr/0024-mainline-lexicals-for-named-subs.md) mainline lexicals for named subs | (no slice plan) | `mainline-lexical-sigilless-binding-leaks-into-a-later-redeclaration` — a mainline-lexical entry is keyed by bare **name** and outlives its block; ADR-0032 D2's "slot-addressed, never name-addressed" constraint exists for exactly this failure |

### Recommended next campaigns

1. **The store-boundary pair (Tier S).** `proxy-assigned-into-an-array` and
   `multidim-assign-to-an-expression-target` are both "a write lands in the
   wrong place, silently", both in ADR-0040's territory, and the Proxy one is
   an *active landmine* — `t/for-loop-element-alias.t` had to name its
   parameters `$p`/`$q` instead of `$x`/`$y` to dodge it, so the next person
   to write `$x` in that file gets an unrelated failure.
2. **ADR-0041**, to discharge the surviving Tier S crash. It is `Proposed`
   with no implementation plan; the crash reproduces in three lines.
3. **ADR-0055 slice 2's prerequisite** (`unvouched-capture-cells`), still the
   Cro-blocking cell-freshness design, and still gated by the batteries suite
   that `make test` does not run.

**One measured process exception, kept from previous regens:** when a change
alters a *universal property of values* ("what is in every container"), run the
full local `make roast` before pushing (ADR-0040 slice 2 needed 17
counter-current fixes, 9 found only by roast). Ordinary parser/operator/
dispatch fixes still delegate to CI.

**A method worth copying.** Three consecutive ADR closures (0045 slice 6, 0036
slice 5, 0040 slice 5) each ended with an *instrumented sweep*: instrument the
single function the mechanism stores through, run all of `t/` plus the whole
roast whitelist under it, and diff every row against raku. All three found live
defects their own divergence matrices could not see. Budget the sweep before
declaring any mechanism done.

---

## Tier B — Correctness, broad impact

### B1 — broad language-construct correctness

| Ticket | Effort | Why here (verified 2026-09-04 unless noted) |
|---|---|---|
| [free-var-read-in-callee-resolves-through-dynamic-caller-chain](deep/free-var-read-in-callee-resolves-through-dynamic-caller-chain.md) | XL | **Reconfirmed** (`f sees 1` / `alias 200`; raku `f sees 5`). A callee's env is `Env::scoped_child(caller_env)`, so a free variable walks the *dynamic* chain. Silent wrong read in ordinary code, and still the highest-leverage finding with no ADR. ADR-0055 §7.5 explicitly puts it out of scope. Its cheaper half — why `f`'s own `my $var = 5` is not visible in `f`'s env tier — is worth isolating first. **Note the repro needs the file form**; the `:=` bind in `g` is load-bearing, and a one-liner without it passes. |
| [immutable-list-element-bind-is-writable](deep/immutable-list-element-bind-is-writable.md) | L | **Reconfirmed**: `my @t := (5,6); my $x := @t[0]; $x = 10` prints `(10 6)` — mutsu *mutates an immutable List*; raku refuses. `IndexAutovivifyLazyTerminal` promotes any scalar leaf to a fresh cell. The guard was implemented, measured, and **narrowed to `my \a :=` binds only** because three consumers lean on the promotion (a chunked multi-parameter loop — `S32-str/val.t` loses 1201 subtests — QuantHash `.kv`, and `Pair.kv` in a closure). Closing it means fixing those three, then dropping the flag. The file records prototypes for two of the three. |
| [mainline-lexical-sigilless-binding-leaks-into-a-later-redeclaration](deep/mainline-lexical-sigilless-binding-leaks-into-a-later-redeclaration.md) | L | A named sub closing over `my \x` puts a **name-keyed** mainline-lexical entry that outlives its block, so an unrelated later `my \x := 5` in a sibling block finds the stale `ContainerRef` and a write that must die silently succeeds. ADR-0024 territory; the fix has to give a mainline-lexical entry an identity beyond its name (most likely the declaring slot, per ADR-0032 D2). |
| [call-compiled-closure-lacks-merge-all-and-dual-persistence-store](deep/call-compiled-closure-lacks-merge-all-and-dual-persistence-store.md) | XL | Closure free var resolves to `CALLER` where raku says `OUTER`. ADR-0055 slices 2-5; the `merge_all` knob the file proposes is *rejected* by the ADR — read the ADR, not the file's proposal. |
| [unvouched-capture-cells-leak-state-across-cro-client-requests](deep/unvouched-capture-cells-leak-state-across-cro-client-requests.md) | M-L | The mechanism that closes ADR-0055 §1.2(b) was built, validated (full roast green) and **removed** because a stale cell leaks request state across `Cro::HTTP::Client.request`'s recursive redirect. Two candidate fixes, both need a cell-freshness design. Gate: the batteries suite. |
| [dollar-dot-attr-compound-assign-spurious-ro-error](deep/dollar-dot-attr-compound-assign-spurious-ro-error.md) | L | **Reconfirmed in its moved shape**: `$.x = 9` inside a method now *mutates* a non-`rw` attribute (prints `9`); raku throws `Cannot modify an immutable Int (1)`. Both halves are silent over-mutation. Needs the "accessor read is an itemized copy" ADR; explicitly **not** ADR-0040. |
| [residual-try-cell-eager-seq-reification-divergences](deep/residual-try-cell-eager-seq-reification-divergences.md) | L | `.map`/`.grep` run their callback eagerly. ADR-0058's target; implementing it makes mutsu stricter, so a full local `make roast` is mandatory. |
| [when-nonmatch-value-outside-map-grep](deep/when-nonmatch-value-outside-map-grep.md) | L | **Reconfirmed** (`Any` vs raku's `Bool::False`). ADR-0052 slices 2-4 — three disagreeing statement-sequence compilers. |
| [module-file-scope-array-and-hash-still-share-the-caller](deep/module-file-scope-array-and-hash-still-share-the-caller.md) | L | Module shape fixed (ADR-0039 slice 1); by-name container resolution in inner blocks/closures still corrupts. Slice 2. |
| [supply-channel-has-no-fanout-to-multiple-taps](deep/supply-channel-has-no-fanout-to-multiple-taps.md) | L | Second `whenever` on `$proc.stdout` gets nothing (single `mpsc` receiver). Live-vs-on-demand replay semantics must be respected; unify with the `Supplier` registry. |
| [whenever-expression-position-needs-real-design](deep/whenever-expression-position-needs-real-design.md) | M | Symptom already moved: both legal shapes answer `Tap`; `Tap.close` retroactively drops the value emitted before it. Reconcile ADR-0053's "not started" header with what landed, then do the identity slice. |
| [grammar-action-ordering-vs-inline-code-blocks](deep/grammar-action-ordering-vs-inline-code-blocks.md) | L | A `make`-bearing embedded block runs at reduce time, out of order. Needs a write channel into the live capture accumulator plus backtrack undo; lands under full roast + battery coverage or not at all. |
| [regex-quantifier-eager-candidate-enumeration-overruns-code-blocks](deep/regex-quantifier-eager-candidate-enumeration-overruns-code-blocks.md) | L | Embedded blocks fire per *computed* candidate (5 vs 2, 17 vs 3). Quantifier-matching architecture change; ADR-0009's "never execute user code while measuring" is the prior art. |
| [native-method-accepted-named-declarations](deep/native-method-accepted-named-declarations.md) | L | **Reconfirmed**: `"abc".chop(:zzz)` → `abc` (raku `ab`); an unknown named silently lands in a positional slot on six measured methods. Two designs, ADR first. |
| [user-prefix-op-candidate-beats-builtin-typed-candidate](deep/user-prefix-op-candidate-beats-builtin-typed-candidate.md) | L | **Reconfirmed**: an untyped user `prefix:<++>` makes `++$i` answer `USER` (raku `2`). Native operators are not dispatch candidates at all. |
| [definiteness-constrained-type-object-identity-lost](deep/definiteness-constrained-type-object-identity-lost.md) | L | **Reconfirmed**: `Any:D.^name` → `Any` (raku `Any:D`). Needs a `DefiniteHOW`-equivalent representation ADR. |
| [resume-does-not-return-to-die-call-site-in-nested-sub](deep/resume-does-not-return-to-die-call-site-in-nested-sub.md) | L/XL | **Reconfirmed**: `.resume` after a `die` in a nested sub prints nothing at all (raku: `after-inner` / `after-call`). Continuation-shaped; tied to how Rust frames unwind. |
| [custom-io-handle-write-read-not-dispatched](deep/custom-io-handle-write-read-not-dispatched.md) | L | **Reconfirmed with the file's own repro**: `$*OUT = $store` writes to the real fd and the store stays empty; raku captures. Subclass `WRITE`/`READ`/`EOF` ignored by print/say/read. |
| [is-typename-custom-container-store-protocol-unimplemented](deep/is-typename-custom-container-store-protocol-unimplemented.md) | L | **Reconfirmed**: `my @v is DNA = 1,2` never calls `STORE`. Scope it first (grep the corpus for `method STORE`). |
| [export-default-package-not-symbolically-navigable](deep/export-default-package-not-symbolically-navigable.md) | M-L | **Reconfirmed**: `::("Test::EXPORT::DEFAULT::&ok")` yields a `Failure` (raku `(Sub)`). Decide how deep to model export tags. |
| [unify-statement-expression-control-construct-compilation](deep/unify-statement-expression-control-construct-compilation.md) | XL | Architectural debt, still growing. Keeps producing paired half-bugs. |

### B2 — batteries / dist-blocking

| Ticket | Blocks | Effort |
|---|---|---|
| [vendor-real-test-module](deep/vendor-real-test-module.md) | making the vendored upstream `Test` the default (retiring the native provider) | XL as a campaign. **Its remaining blocker is still perf, but the perf ticket's diagnosis is wrong** — see the perf section. Measured today: `ok` costs 0.241 ms/assertion under real `Test` vs raku 0.006 ms and the native provider 0.0021 ms, essentially unchanged from 2026-08-29. |
| [config-toml-battery-core-blockers](deep/config-toml-battery-core-blockers.md) | `Config::TOML` + `Crane` battery slot | L (cluster). Moved from `tickets/` on 2026-09-02 after re-triage: three independent core campaigns (Crane's array-path semantics, `\UXXXXXXXX` grammar candidate selection, inline-table timeout). `Crane` was 3/15 on 2026-08-31; `t/copy.rakutest` still failed 5 of 6 subtests on 2026-09-02. **Do not start the vendoring steps.** |
| [template-engines-blocked-on-mutsu](deep/template-engines-blocked-on-mutsu.md) | the template battery runner-ups | L (cluster) — **re-survey first**: both `Template::Jinja2` blockers are closed in `news/`, so its "0/23" row is unmeasured. `Template6` is still unreduced. |
| [p5tie-stash-bind-key-protocol](deep/p5tie-stash-bind-key-protocol.md) | `P5tie`, `annotations` (~0.5% of sampled dists) | L — `Stash.BIND-KEY` / `CALLER::.BIND-KEY` both missing. Rung-2 machinery only. |
| [subtest-compiled-dispatch-async-middleware-regression](deep/subtest-compiled-dispatch-async-middleware-regression.md) | re-landing #6499's `subtest` perf win | M-L — root cause unknown; bisect from the dispatch end with `rust-gdb` frame diffs, not from Cro. |

---

## Tier N — narrow correctness / diagnostics

| Ticket | Category | Effort / note |
|---|---|---|
| [end-phasers-install-at-compile-time](deep/end-phasers-install-at-compile-time.md) | correctness-narrow | M-L — **reconfirmed**: mutsu prints `loop`/`main`; raku also runs the END in a never-taken `if False` block and in an uncalled sub. Same-line ordering tie-break included. |
| [chained-index-assign-autoviv-loses-hole-tracking](deep/chained-index-assign-autoviv-loses-hole-tracking.md) | correctness-narrow | S-M — **reconfirmed**: `@a[0][1] = 5; @a[0][0]:exists` is `True` (raku `False`). The `;` form is fixed; find the chained autoviv site. |
| [chained-and-array-element-sigilless-bind-wrongly-readonly](deep/chained-and-array-element-sigilless-bind-wrongly-readonly.md) | spurious die on valid code | M — **reconfirmed, still with the changed message** (`Cannot modify an immutable Int (5)`), so the `mark_readonly` hypothesis in the file's head is probably stale. Break with gdb before fixing. |
| [module-toplevel-private-sub-leak-cleanup](deep/module-toplevel-private-sub-leak-cleanup.md) | accepts code raku rejects | M — **reconfirmed**: a non-exported module `sub helper` stays callable bare after `require` (raku: `Undeclared routine` at compile time). Needs an exhaustive audit of ambient `GLOBAL::` installers first (a generalisation was tried and reverted). |
| [role-body-placeholder-mu-supply](deep/role-body-placeholder-mu-supply.md) | rejects code raku accepts | M-L, **low priority by its own assessment** — ADR-0048 P5's value half. mutsu refuses to compile `role R { $^c }`; raku accepts it but supplies an uninitialized `VMNull` whose `.defined` *throws*, i.e. the semantics being matched are garbage. Corpus scan of `roast/`, `modules/`, `vendor/`, `lib/`: **zero** hits. Pick it up only when the ADR-0019 deferred-body plumbing is open for another reason. |
| [native-method-cannot-return-an-lvalue-container](deep/native-method-cannot-return-an-lvalue-container.md) | missing feature | L — the `.VAR = 5` row is *wrong* (raku dies too); `.snitch = 666` is the only acceptance case. Needs the container-propagation design campaign. |
| [typed-shaped-array-rows-lose-element-value-type](deep/typed-shaped-array-rows-lose-element-value-type.md) | self-consistency (**no raku oracle** — raku says "not yet implemented") | S-M — thread `value_type` through `make_shaped_array_seeded`'s rows. |
| [slurpy-hash-named-arg-raku-boolean-shorthand-missing](deep/slurpy-hash-named-arg-raku-boolean-shorthand-missing.md) | rendering | **Reconfirmed**: `{:a(Bool::True)}` vs raku `{:a}`. Small, self-contained now that ADR-0040 is closed. |
| [direct-metamodel-classhow-new-type-immutable-error](deep/direct-metamodel-classhow-new-type-immutable-error.md) | missing feature | M/L — **narrow the file before starting**: the `.^add_method`-on-`new_type` half appears to work now (a method installed that way is listed and callable). What is confirmed still broken is `does Metamodel::Naming` / `Metamodel::Stashing` — `X::InvalidType: Invalid typename 'Metamodel::Naming'` — which is a much larger slice of the MOP. |
| [begin-time-adverb-value-interpolation](deep/begin-time-adverb-value-interpolation.md) | correctness-narrow | L, **low priority** by its own assessment — no roast coverage; would add a whole-AST name-rewriting pass. |

---

## Perf — batch into one profiling session; implementation agent runs SOLO

The call-path campaign is **live**: six PRs landed 2026-09-03/04 (#7259, #7262,
#7275-#7280) and the ledger for it is
`late-august-call-path-slowdown-remainder.md`. Nothing else in this table
should be started while that campaign is running — a second perf agent makes
both sets of numbers untrustworthy.

| Ticket | Status |
|---|---|
| [late-august-call-path-slowdown-remainder](perf/late-august-call-path-slowdown-remainder.md) | **The live campaign.** ~20% left on `bench-fib` after the ADR-0037 intern fix and the September sweep. Its "Do NOT keep bisecting" section is the most important paragraph in `todo/perf/`: layout noise is ~5%, the remaining regression is several steps of about that size, so any commit a bisect names must be discharged by checking whether its code is even *sampled*. Use the differential profile instead. |
| [locals-frame-is-a-pooled-vec-not-a-register-window](perf/locals-frame-is-a-pooled-vec-not-a-register-window.md) | **The named next target**, filed 2026-09-04: ~5.7% of `bench-fib`'s profile goes to managing a one-element `Vec` (`recycle_locals` 2.36% + `Vec` drop/resize/extend). The fix is the standard register window — one `locals_stack` with a per-frame base. **ADR-class, not a slice**: `self.locals` is touched at 484 sites across 60 files, `mem::take` is load-bearing in three call paths, `VmCallFrame::saved_locals` owns a whole `Vec`, and the JIT emits code against `Interpreter::locals`' offset. Write a `Proposed` ADR before any code. |
| [interpreter-call-path-in-hot-loops](perf/interpreter-call-path-in-hot-loops.md) | **Symptom real, diagnosis STALE — do not start from its "Where to start".** Measured 2026-09-04 (release): `sub f(&c) { 1 }` and `sub f($c) { 1 }` driven 200k times produce a **byte-identical opcode profile** (400 606 opcodes, no `CallOnCodeVar`/`GetCodeVar`/`MakeNamedArg`/`WrapVarRef`) and `function-full-resolve total=1`, i.e. the 6.75x `&`-sigil signature gate the file blames is **closed**. Yet the vendored-`Test` cost is unchanged: 0.241 ms per `ok` vs raku 0.006 ms. New pointer, same run: 2000 real-`Test` assertions do **50 060 full by-name resolves** (`nqp::join` 12 003, `nqp::split` 12 003, `nqp::time` 8 010, `nqp::iseq_i` 6 015, `ok` 6 000, `proclaim` 6 000) and **83.3% of function-call opcodes fall back to the interpreter path**. Start there, and re-write the file's root-cause section first. |
| [hash-workload-cost-is-spread-across-gc-alloc-and-key-hashing](perf/hash-workload-cost-is-spread-across-gc-alloc-and-key-hashing.md) | A profile *record*, deliberately not a fix: GC ≈14%, allocation ≈12%, NaN-box decode ≈13%, key hashing+comparison ≈10%. Three leads, most tractable first: `Interpreter::current_package()` (an `RwLock` read + `String` clone, 2.2%, 228 call sites, with a `current_package_sym()` already beside it); the user-key `HashMap`'s SipHash (**do not just swap the hasher** — iteration order and collision-DoS both need deciding); and whether a hash element store needs a cycle-collected cell at all (ADR discussion, not a tweak). |
| [bench-ctor-construction-parity](perf/bench-ctor-construction-parity.md) | Round 5 found the "flat profile" conclusion of rounds 2-4 was wrong — a per-call `.map` compile. Lesson: **`MUTSU_VM_STATS` `add_constant` must stay flat on a steady-state loop**; growth = a runtime compile. Three unmeasured leads inside `dispatch_bless` remain. |
| [closure-literal-creation-cost](perf/closure-literal-creation-cost.md) | Parts A/C done (−20%/creation, body shared). Part B (O(kept-env) capture) is ADR territory — narrowing the kept set trusts an incomplete static analysis, the `roles-6e.t` flake shape. Cost the "share the system-name portion through the parent chain" alternative first. |
| [interpreter-new-is-expensive-and-retains-memory](perf/interpreter-new-is-expensive-and-retains-memory.md) | ~9 ms and **~7.2 KiB retained** per `Interpreter::new()`, linear over 4000 constructions; `MUTSU_GC=on` changes nothing. **Debug-build numbers** — re-measure in release before designing. Nothing is blocked today (ADR-0065 S2 routed around it), but `new_regex_scratch` constructs one per use and any per-request embedder pays it. Chase the *retention* first, not the wall clock. |
| [digest-ripemd-start-per-block-overhead](perf/digest-ripemd-start-per-block-overhead.md) | Title is historical (the `start` lever is closed). `t/ripemd.t` ~148-156s vs a hard 120s gate; profile is flat; needs a fresh dominant item, not a guess. |
| [yaml-parse-throughput](perf/yaml-parse-throughput.md) | ~5x raku on real files after nine rounds; open items: candidate enumeration, `invoke_grammar_actions` materializations. Carries the three most valuable methodology notes in `todo/` (CPU-spinner check before any A/B; deep-recursion `perf` children percentages are misattributions; compute from the tag probe you already have). |
| [adr0019-g3-diffuse-bless-allocation-cost](perf/adr0019-g3-diffuse-bless-allocation-cost.md) | Blocked on a working call-graph profiler (`addr2line` stale build-id entries under `/root/.debug`). Pair with the bench-ctor row. |
| [bigint-repeated-addition-performance-gap](perf/bigint-repeated-addition-performance-gap.md) | ~14x raku — **on a debug build**; re-measure on release before ranking. |
| [closure-sequence-evolution-performance-gap](perf/closure-sequence-evolution-performance-gap.md) | ~84x raku — **debug numbers**; the combined case (48s) far exceeds the sum of its parts (~7.5s), which is the actionable signal. |

Numbers that end up in a document must come from the **bench CI**
(`bench-history.tsv` on `bench-data`), never from the profiling session's own
local runs. The measurements quoted in this file are session-local routing
evidence, not document figures.

---

## Icebox — blocked on a decision, or a pure record

| Ticket | Blocked on / why |
|---|---|
| [lsp-references-needs-a-side-table-not-ast-spans](deep/lsp-references-needs-a-side-table-not-ast-spans.md) | **Measurement before design.** ADR-0065 D6 assumed spans on AST variants; the parser already knows every byte offset, so a thread-local occurrence table gated on an analysis flag is cheaper and touches neither `Expr`'s size nor the bincode cache. The blocker is **backtracking** — a `Var` parsed in a failed alternative would be recorded as a phantom reference. Step one is to build the table behind the flag, run it over `modules/`/`vendor/`/`t/`, and measure the phantom rate; that number decides the design. Also needs an explicit ADR decision that `references` is name-based, not declaration-based. |
| [bare-name-type-constraint-store-is-scope-blind](deep/bare-name-type-constraint-store-is-scope-blind.md) | **No failing repro left** (21 rows match raku). Open only as the tracking record for ADR-0042 slices 2-3. Do not dispatch as a bug. |
| [exception-class-hierarchy-is-mostly-unregistered](deep/exception-class-hierarchy-is-mostly-unregistered.md) | Done except R5 (re-run the real-`Test` sweep), which waits on `vendor-real-test-module`. All 373 rakudo `X::` subtypes match. |
| [rakuast-remaining](deep/rakuast-remaining.md) | ADR-0033 Phase 3 + an undesigned read-gap list. Zero roast dependents; pick by user impact, not cadence. A RakuAST implementation workflow was added 2026-09-03 — read it first. |
| [nativecall-cannot-be-vendored](deep/nativecall-cannot-be-vendored.md) | Measurement record with reopen conditions; blocker 3 (parser) is gone, 1/2/4 stand. Keeps `NativeCall` a justified rung-3 provider. |
| [adr0019-e2-e4-resolver-core](deep/adr0019-e2-e4-resolver-core.md) | E3/E4 closed; E2 is a non-gating counter cleanup. |
| [immutable-lvalues-that-mutsu-still-lets-you-assign-to](deep/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md) | **A survey, not a unit of work.** Re-run today: 6 of 7 rows still succeed silently, and `(1..3)[0] = 9` still throws the right class with the wrong rendering (`immutable value (1 2 3)` vs `immutable Range (1..3)`) — **no row moved despite ADR-0036, ADR-0040 and ADR-0045 all closing in the interval**, which is itself the finding: these rows are not element-container work. Mine it for rows, do not dispatch it whole. |

---

## Housekeeping notes

- **Closed since the previous regen** (PR that merged the closure): #7190
  is-rw-sub-implicit-return (Tier S), #7196 array-seq-view + for-kv-multi-param,
  #7197 for-loop-rw-element-alias, #7201 pair-new-argument, #7203
  pair-value-assign-immutable, #7205 container-pair-value-increment, #7206
  gather-block-state, #7207 pairs-element-containers, #7210 io-listops-colonpair,
  #7214 quanthash-pairs-value-write, #7218 native-hash-construction, #7220
  lazy-list-in-scalar + quanthash-values-map-write-through, #7222
  element-itemization-lost-in-scalar-binding, #7223 var-on-a-bare-valued-hash,
  #7224 pair-dot-hash-coercion, #7226 var-on-a-real-element (ADR-0064), #7230
  lazy-array-elements-itemized, #7231 hash-multidim-subscript, #7232
  multidim-oob-version-pragma, #7238 bundle-xml-battery, #7240
  sigilless-alias-closure-capture (Tier S) + str-method-falls-back-to-stringy,
  #7244 list-destructuring-sigilless-bind, #7245 for-loop-pointy-sigilless
  (Tier S), #7248 placeholder-scope-loop-while, #7254 rakuast-node-identity,
  #7259 bench-hash-ratio-drift.
  `bundle-config-toml-once-parser-fixed` was **moved**, not closed — it is now
  `deep/config-toml-battery-core-blockers.md`.
- **Two ticket files disagree with the build and should be rewritten before
  they are dispatched**: `tickets/producer-seq-index-...` (five of six rows now
  pass) and `perf/interpreter-call-path-in-hot-loops` (the `&`-sigil gate is
  closed; the cost moved). Both are the "the todo file's own root cause is
  wrong" failure mode, which this project hits often enough to be the default
  assumption.
- **Three ADR headers are behind their code**: ADR-0053 says "not started"
  while `.WHAT` already answers `Tap`; ADR-0041 is `Proposed` while its finding
  is the surviving Tier S crash; ADR-0055 §7.4's slice-2 prerequisite is
  tracked as a `deep/` ticket rather than in the ADR's own phasing.
- **Four `perf/` files carry debug-build numbers** (bigint, closure-sequence,
  interpreter-new, and the anonymous-declaration leak in
  `tickets/analysis-parse-...`). CLAUDE.md's rule is release for wall-clock; do
  not rank them off those figures.
- Verification for this regen was run ad hoc from `tmp/` (gitignored); each
  ticket's own repro block regenerates it.
