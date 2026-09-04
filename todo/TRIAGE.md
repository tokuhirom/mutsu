# TRIAGE — prioritized snapshot of todo/ (2026-09-04, second pass)

A ranked index of every open finding under `todo/tickets/`, `todo/deep/` and
`todo/perf/`, so a session can pick the next unit of work without re-reading
all of them.

This is a **snapshot, not a ledger**. Resolving a ticket does *not* require
editing this file — that would reintroduce exactly the shared-file merge
conflicts `todo/` exists to avoid. A stale row is fine; the per-ticket files
stay the source of truth. Regenerate the whole file when it has drifted too
far (re-survey every ticket, re-score, rewrite).

## What changed since the morning regen (7dce1ff5b, the same day)

Surveyed at `54946f2aa`: **71 files** — 44 `deep/`, 15 `tickets/`, 12 `perf/`.

One working day (PRs #7274-#7294) turned over the entire top of the previous
snapshot:

- **All four Tier S rows moved.** Three are closed — the
  `&postcircumfix:<[ ]>` stack-overflow crash (#7285, and it was *not* the sub
  hoisting ADR-0041 blamed), `proxy-assigned-into-an-array-is-not-fetched`
  (#7290, now ADR-0040 §9), and `multidim-assign-to-an-expression-target`
  (#7286). The fourth, `procasync-stress-segv`, had its **§8.2 diagnostics
  slice landed** (#7291): the reason three CI crashes left no report was that
  the handler *overflowed `std`'s 8 KiB alternate signal stack* on worker
  threads, found with `strace -f -e trace=sigaltstack`, not with a debugger.
- **Eleven new tickets were filed** and four closed, so `tickets/` went 8 → 15
  and is now the queue that matters. They are not scattered: **eight of the
  eleven fall into two coherent clusters** (Proxy containers, and the
  routine registry not being lexically scoped) — see "Recommended next
  campaigns".
- **The perf call-path campaign delivered.** `bench-fib` went from ~1.25-1.32x
  raku on the morning of 2026-09-03 to **0.84x at `54946f2aa`** (JIT row 0.41x)
  across ~10 PRs. No `todo/perf/` file was edited, so every row in that section
  below is now describing a *pre-sweep* world; re-measure before starting one.
- **ADR-0040 grew a §9** that answers the `Proxy`-at-the-store question with
  the same boundary as itemization, and **ADR-0041 grew a §6** recording that
  two of its own premises were wrong. Both are worth reading before touching
  their areas.

### What was re-verified for this regen

All **eleven new tickets** were run today against `raku` v2026.06 on a fresh
`target/debug/mutsu` at `54946f2aa`; every one reproduces as written. So did a
re-run of every Tier S/B1 row and the `immutable-lvalues` probe harness. Three
rows *moved* and are called out inline:

- `promoted-element-cell-does-not-know-its-container-name` — a *direct* store
  (`my Int @a = 1,2; @a[0] = "x"`) now names `@a` correctly; the ticket is about
  the **promotion** sites (`:=` bind, `%h<a>`, `:p`), and all four of those
  still print the bare sigil. Ticket stands, repro unchanged.
- `chained-and-array-element-sigilless-bind-wrongly-readonly` — its **shape 2 is
  fixed** (`my Int @arr = 1,2,3; my \x := @arr[0]; x = 1000` writes through).
  Only shape 1, the two-hop bind chain, survives, and its message changed
  (`Cannot modify an immutable Int (5)`, not `immutable value (x)`), so the
  file's `mark_readonly` hypothesis is stale.
- `when-nonmatch-value-outside-map-grep` — mutsu now answers `Nil`, not `Any`
  (raku: `Bool::False`). Still wrong, different wrongness.

**Standing caveat.** A tier is a routing hint. This is one run on one box;
CLAUDE.md's rule still applies — re-verify a ticket's repro on your own build
before acting on it, and read the *tail* of a ticket file, where successive
"Re-verified" sections accumulate.

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

| Ticket | Breadth | Effort | Verified 2026-09-04 (2nd pass) |
|---|---|---|---|
| [rw-param-does-not-bind-a-proxy-container](tickets/rw-param-does-not-bind-a-proxy-container.md) | every `is rw`/`is raw` parameter and method parameter given a `Proxy` | M-L | **Confirmed**: `sub f($x is rw) { $x = 42 }; f($p)` leaves the backing lexical at `5`; raku `42`. The write is silently dropped — `auto_fetch_proxy_args` FETCHes *before* signature binding, so there is no container left to bind. The gate is keyed on the **callee's name** (a hardcoded `skip_proxy_fetch` list) when it is a property of the **parameter**. |
| [element-bind-fetches-the-proxy-it-should-install](tickets/element-bind-fetches-the-proxy-it-should-install.md) | every `@a[0] := $proxy` | M | **Confirmed**: prints `5` where raku prints `9` — the element stops tracking. Same mechanism seen from the `:=` side: `__mutsu_bind_index_value` is an ordinary `CallFunc` and is the one sibling helper missing from that same name list. ADR-0040 §9 explicitly rules that a `:=` bind installs the Proxy itself, so this is a stated-rule violation, not an open question. |
| [method-rooted-subscript-chain-autoviv-is-dropped](tickets/method-rooted-subscript-chain-autoviv-is-dropped.md) | every lvalue subscript chain rooted at a method call that must autovivify | L | **Confirmed both spellings**: `$o.a[0]<x> = 5` and `$o.a[0]{1;2} = 5` both leave `[]` where raku gives `[{:x(5)},]` / `[{"1" => ${"2" => 5}},]`. Exit 0, nothing reported. It only survives when the element already exists. The fix is **not** to grow `__mutsu_index_assign_method_lvalue_nested` (a `runtime/methods.rs`-era slow path CLAUDE.md forbids extending) — an attribute accessor has to yield a real container reference in lvalue context. |
| [same-named-loop-params-in-one-unit-interfere](tickets/same-named-loop-params-in-one-unit-interfere.md) | any unit with two `for` loops that happen to reuse a parameter name | L | **Confirmed**: an `is rw` loop in one block silently changes what a later, unrelated, non-rw loop's closures capture (`[30 30]` vs raku `[10 30]`). Action at a distance across blocks, with nothing detecting it. Cause: four closure-capture sets (`captured_mutated_locals`, `needs_cell_locals`, `for_loop_param_syms`, `free_var_writes`) are keyed by **name** over the whole `CompiledCode`. **First experiment is cheap**: turn on `MUTSU_SHADOW_SLOTS` and see whether the repro passes — if it does, this is a datapoint for that campaign, not its own work. |
| [procasync-stress-segv](deep/procasync-stress-segv.md) | `roast/S17-procasync/stress.t`, `integration/advent2014-day05.t` | — | **Its actionable slice is done** (§9, #7291): every thread class now writes a crash report, pinned by two `tests/crash_report.rs` cases driven through the production `spawn_user_thread` path. The underlying race is still un-root-caused and still CI-only. **Nothing to do until the next recurrence**, which will finally arrive with a thread name and a backtrace. Do NOT quarantine, do NOT open a speculative crash hunt. |

---

## `todo/tickets/` — all 15

Eleven of these fifteen were filed after the morning regen. Four are ranked in
**Tier S** above; the rest are below.

### Startable today (8)

| Ticket | Tier | Note (verified 2026-09-04, 2nd pass) |
|---|---|---|
| [package-body-proto-multi-not-lexical-to-the-package](tickets/package-body-proto-multi-not-lexical-to-the-package.md) | B1, **M-L** | **Confirmed, and it is two failures.** A class-body `proto`+`multi` family is unreachable from the class's own methods (`Unknown function: foo`; raku `in-class`); add a mainline `foo` and the method silently calls *that* one. A module-body one instead answers `Ambiguous call to foo(Int); these signatures all match: (Int $x), (Int $x)` — the two candidate sets are being **merged** across the package boundary. The single-`sub` baseline is correct in both, which localises it precisely: `resolve_function_with_types` / the multi-candidate gather, not registration. |
| [imported-constant-class-alias-does-not-resolve](tickets/imported-constant-class-alias-does-not-resolve.md) | B1/B2, **M** | **Confirmed both spellings** against the fixture: `Rounded.new(1).^name` and `my @a is Rounded` both answer `Array` where raku answers `RoundedMod::Array::Rounded`. Two resolution paths (`exec_apply_var_trait_op` matches the trait name literally; the bareword path handles a same-file `my constant` but not a cross-module `is export` one). The `Array::Rounded` idiom is common; this is what is left of that dist's blockers now that the postcircumfix half is fixed. |
| [do-block-does-not-scope-routine-declarations](tickets/do-block-does-not-scope-routine-declarations.md) | B1 (narrow breadth), **M** | **Confirmed**: a `sub` declared in a value-position `do { }` leaks out and permanently replaces the outer one (`inner`/`inner`; raku `inner`/`outer`). The statement-form bare block is already correct, and `Compiler::stmts_declare_routines` + the for-loop path are the precedent to copy. Costs a compile-time flag on `OpCode::DoBlockExpr` (mind `opcode_size_guard`) and wants a full roast + batteries run, since correcting it *removes* names that currently leak. |
| [for-loop-sigilless-param-writeback-skips-the-type-check](tickets/for-loop-sigilless-param-writeback-skips-the-type-check.md) | N-B, **M (as a slice: L)** | **Confirmed**: `my SmallInt $a; for $a -> \x { x = 1000 }` silently sets `1000`; raku throws. The `:=` spelling now type-checks, so this is specific to the loop's `store_loop_source_var` writeback, which bypasses the container chokepoint entirely. The file argues for fix (2) — make the loop parameter a real alias and delete the scalar-source writeback — as an ADR-0045 slice, and explicitly asks you not to ship (1) silently. Named consumer: `Native::Overflow`'s `t/01-basic.rakutest`. |
| [list-element-proxy-not-rendered-through-fetch](tickets/list-element-proxy-not-rendered-through-fetch.md) | N (rendering), **M** | **Confirmed**: `say (1, $proxy, 3)` prints `(1 Proxy 3)`; raku `(1 9 3)`. This one is *correct* to keep the Proxy in the List — ADR-0040 §9 rules that a List's elements are not containers — so it is purely a render gap. The `say` half is nearly a one-liner (`resolve_proxies_in_value` exists); every other renderer (`.gist`, `.raku`, interpolation, `~`) goes through pure `Value` methods with no `&mut Interpreter`, which is the real question and belongs as an ADR-0040 amendment. |
| [promoted-element-cell-does-not-know-its-container-name](tickets/promoted-element-cell-does-not-know-its-container-name.md) | N (message quality), **S-M / L** | **Re-confirmed at all four promotion sites** (`:=` bind, `%h<a>`, `:p`, and the cross-routine `@z`-not-`@b` rule): all print `element of @` / `element of %`. The anonymous-container row correctly agrees with raku. Option 2 (retag at the naming opcodes) is a cheap interim; option 1 (an owner field beside `value_type`) is the honest fix and is the *name* half of ADR-0042's descriptor. Nothing measured depends on the wording. |
| [list-literal-does-not-capture-element-containers](tickets/list-literal-does-not-capture-element-containers.md) | N-B, **M** | **Confirmed**: `my (\p,\q) := (@a[0],@a[1]); p = 9` dies with `Cannot modify an immutable Int (1)`; raku writes `[9 2]`. `compile_call_arg` tags a source container only from a source *name*, and `Expr::Index` has none. Changes what every parenthesised list holds, so it wants its own measurement pass. Paired with `deep/immutable-list-element-bind-is-writable`. |
| [analysis-parse-mints-process-unique-registry-names](tickets/analysis-parse-mints-process-unique-registry-names.md) | N (resource leak), **M** | **Its prerequisite is now met** — ADR-0065 S1-S5a shipped and `src/analysis/{mod,symbols}.rs` expose the entry points (`check()`, `symbols()`) the fix hangs off. Each anonymous declaration leaks one interned registry name **per parse** (1.00/parse, ~0.5 KiB/parse, linear over 8000 re-parses); add a unit-local counter mode on those entry points, inherited by nested sub-parses and off for every existing caller. **Do not "fix" it by resetting the global counters** — the file explains why that breaks cross-unit uniqueness. Re-measure first (`MUTSU_S0_ITERATIONS=8000 cargo test --test long_lived_parse`); the numbers in the file are from a debug build. |

### Narrow, permissive, or low priority (3)

| Ticket | Tier | Note |
|---|---|---|
| [associative-multidim-lvalue-edge-divergences](tickets/associative-multidim-lvalue-edge-divergences.md) | N, **S-M** | Three independent rows, all **confirmed**. (1) `%h{1;2} //= 7` writes where raku no-ops, and the named-root and chain-root spellings do not even agree with each other. (2) `%h{*} = 5` yields `{"*" => 5}` — a **silent write to a stringified `Whatever`** where raku throws; that row alone is Tier-S-shaped but only reachable via a `Whatever` in an associative assignment. (3) `:delete` on a multi-dim subscript is accepted where raku refuses to resolve the caller — and its behaviour has *changed since the file was written* (it now deletes the inner key, leaving `{"1" => ${}}`, rather than no-opping), so re-read row 3 before fixing it. |
| [our-proto-redeclaration-across-scopes-is-accepted](tickets/our-proto-redeclaration-across-scopes-is-accepted.md) | N (permissive), **S** | **Confirmed**: nested `our proto sub foo` shadows instead of being refused (`i`/`o`; raku `===SORRY!=== Redeclaration of routine 'foo'`). Direct fallout of the lexical-shadowing exemption added the same day — the exemption should not apply to an `our`-scoped declaration (`__our_scoped` marks it). Re-check all of `t/multi-proto-lexical-scope.t` after, since the `our` and `my` paths share the check. |
| [procasync-output-chunks-do-not-hold-back-final-grapheme](tickets/procasync-output-chunks-do-not-hold-back-final-grapheme.md) | N, **M** | Carried. raku emits `["ab","cde","f"]`, mutsu `["abc","def"]`; on malformed UTF-8 the content differs too. Generalise the existing `held_cr` hold-back in `feed_utf8_incremental`; confirm rakudo's discard-on-error rule first. |

---

## How to work `todo/deep/` — by ADR cluster

**Do not run `deep/` oldest-first** (filing order is an accident of which
campaign ran last, and `ls -tr` mtimes are corrupted by worktrees). Work it by
ADR cluster: most deep findings wait on a *slice of an ADR that already
exists*, and one landed slice closes several rows. Every `Status` line below
was read on 2026-09-04 (2nd pass).

| ADR | Status | Rows it would close |
|---|---|---|
| [ADR-0040](../docs/adr/0040-array-hash-elements-are-itemized-at-the-store.md) elements itemized at the store | Complete, and **grew a §9 today**: `Proxy` is FETCHed at the *same* boundary as itemization, with two deliberate exclusions (a `:=` bind installs the Proxy; a `List`'s elements are not containers). Pinned by `t/proxy-store-boundary.t`, 28 dual-oracled rows. | It is the **owner of the whole Proxy cluster**: two Tier S rows and one Tier N render row are §9's residue, and §9 already states the rule each of them violates. |
| [ADR-0041](../docs/adr/0041-sub-hoisting-vs-compile-time-name-visibility.md) sub hoisting vs name visibility | Still `Proposed`, but **§6 now records that two of its premises were wrong**: the crash was a missing `postcircumfix:<` entry in `resolve_code_var`'s operator fast path, not hoisting; and Option B (emit each `RegisterDecl` at its textual position) is **rejected as specified** — the real discriminator is compile time vs run time. | Its own residue (§6.4): a `&name` inside `constant`/`BEGIN` still sees what only the hoist pass made visible. §6.4 explicitly says this, the proto/multi shadowing gap, and ADR-0039 "should be resourced as one campaign, not three patches". |
| [ADR-0039](../docs/adr/0039-container-lexicals-resolve-lexically.md) container lexicals resolve lexically | Slice 1 landed 2026-08-20; §8.2 and the `our` case closed separately; **slice 2 open** | `module-file-scope-array-and-hash-still-share-the-caller`; and it is the third leg of the registry-scope campaign above. |
| [ADR-0024](../docs/adr/0024-mainline-lexicals-for-named-subs.md) mainline lexicals for named subs | (no slice plan) | `mainline-lexical-sigilless-binding-leaks-into-a-later-redeclaration` — a name-keyed mainline-lexical entry outlives its block. Fourth leg of the same campaign; ADR-0032 D2's "slot-addressed, never name-addressed" is the constraint it violates. |
| [ADR-0045](../docs/adr/0045-for-loop-parameters-bind-the-element-container.md) for-param binds the element container | Accepted, fully implemented; **its last two named residues were closed today** (the multi-parameter read-only capture, and the Proxy landmine that forced `t/for-loop-element-alias.t` to rename its parameters — they are `$x`/`$y` again) | `tickets/for-loop-sigilless-param-writeback-skips-the-type-check` is the *right* home for a new slice: make the scalar-source loop parameter a real alias and delete the writeback. |
| [ADR-0036](../docs/adr/0036-element-container-pairs-from-subscripts-and-pairs.md) element-container Pairs | Implemented; every slice landed, and its last open ticket closed today | Nothing. |
| [ADR-0059](../docs/adr/0059-is-rw-routines-return-a-container.md) `is rw` routines return a container | Slices 1-2 implemented; **slice 3 open**, with no failing repro attached | Nothing today. |
| [ADR-0048](../docs/adr/0048-placeholder-scope-is-a-block-invocation-contract.md) placeholder scope | Accepted; P1-P4 landed, P5's scope half landed, its value half deferred | `role-body-placeholder-mu-supply` (P5's value half — corpus hits: zero) |
| [ADR-0055](../docs/adr/0055-closure-free-vars-resolve-to-their-own-binding.md) closure free vars bind their own | Slice 1 landed 2026-08-28; **slices 2-5 not started**; §7 records two prerequisites | `call-compiled-closure-lacks-merge-all-...`, `unvouched-capture-cells-leak-state-across-cro-client-requests` (slice 2's prerequisite) |
| [ADR-0065](../docs/adr/0065-language-server-targets-ai-agents.md) LSP targets AI agents | **Accepted; S0-S5a shipped** (S1 entry point, S2 unknown-routine reporting, S3 parse-error recovery, S4 symbols + go-to-definition, S5a hover) | `lsp-references-needs-a-side-table-not-ast-spans` (amends D6/S5b); **`tickets/analysis-parse-mints-...` is now unblocked** |
| [ADR-0042](../docs/adr/0042-type-constraints-belong-to-the-container-not-to-a-name.md) container-carried type constraints | Slice 1 landed; **slices 2-3 open** — no failing repro remains | `bare-name-type-constraint-...` (cleanup record only); `tickets/promoted-element-cell-...` is the *name* half of the same descriptor and rides along with slice 2's design |
| [ADR-0052](../docs/adr/0052-a-when-clause-produces-its-value-on-the-stack.md) `when` value on the stack | Accepted; slice 1 landed, **2-4 open** | `when-nonmatch-value-outside-map-grep` (re-measured: now `Nil`, raku `Bool::False`) |
| [ADR-0053](../docs/adr/0053-do-whenever-produces-a-tap-on-the-stack.md) `do whenever` produces a Tap | `Proposed`, **and its header is still behind the code**: `.WHAT` already answers `Tap`; only the subscription-identity half is open | `whenever-expression-position-needs-real-design` |
| [ADR-0058](../docs/adr/0058-map-grep-produce-a-deferred-seq.md) map/grep produce a deferred Seq | `Proposed` | `residual-try-cell-eager-seq-reification-divergences` |
| [ADR-0047](../docs/adr/0047-type-identity-is-a-declaration-site-not-a-registry-name.md) type identity | P1-P2 landed; **P3-P4 open** | `subtest-compiled-dispatch-async-middleware-regression` (P4 is a prerequisite for re-landing #6499, not a fix for the regression) |

### Recommended next campaigns

1. **The Proxy container boundary (two Tier S rows + one Tier N).**
   `rw-param-does-not-bind-a-proxy-container` and
   `element-bind-fetches-the-proxy-it-should-install` are **the same defect from
   two sides**: whether an argument is FETCHed is decided by a hardcoded list of
   *callee names* when it is a property of the *parameter or bind target*.
   Both files say so, and both say "prefer fixing the decision once over
   lengthening the list a fourth time". ADR-0040 §9 already states the rule they
   break, so this is a slice with a written spec, not a design problem.
   `list-element-proxy-not-rendered-through-fetch` is the render-side third and
   can ride along or be split.
2. **The routine registry is not lexically scoped (ADR-0041 §6.4's own
   recommendation).** Four findings are one mechanism:
   `package-body-proto-multi-not-lexical-to-the-package` (candidate sets merged
   across a package boundary), `do-block-does-not-scope-routine-declarations`
   (no snapshot/restore on `DoBlockExpr`),
   `our-proto-redeclaration-across-scopes-is-accepted` (the shadow exemption
   ignores `our`), and ADR-0041 §6.4's BEGIN-time visibility residue — with
   ADR-0039 slice 2 and `mainline-lexical-sigilless-binding-leaks` as the
   container-side analogue. §6.4 says explicitly: **resource these as one
   campaign, not three patches.** This is the highest-value un-owned ADR work.
3. **`same-named-loop-params-in-one-unit-interfere`, starting with the cheap
   experiment.** Turn on `MUTSU_SHADOW_SLOTS` and re-run the two-block repro. If
   it passes, this Tier S row is a datapoint for the shadow-slot campaign
   (`docs/lexical-scope-slot-campaign.md` §1.3/§1.4) and not separate work —
   which would be the strongest argument yet for finishing that campaign.

**One measured process exception, kept from previous regens:** when a change
alters a *universal property of values* ("what is in every container"), run the
full local `make roast` before pushing (ADR-0040 slice 2 needed 17
counter-current fixes, 9 found only by roast). The Proxy campaign above is
exactly that shape. Ordinary parser/operator/dispatch fixes still delegate to CI.

**Two methods worth copying.**

- **The instrumented sweep.** Three consecutive ADR closures each ended by
  instrumenting the single function the mechanism stores through, running all of
  `t/` plus the roast whitelist under it, and diffing every row against raku.
  All three found live defects their own divergence matrices could not see.
- **`strace` for anything signal- or syscall-shaped.** The three-recurrence
  `procasync` diagnostics gap was closed in four lines of
  `strace -f -e trace=sigaltstack` output after a debugger had only established
  that the handler ran. A debugger answers "did we get here"; `strace` answers
  "what did the kernel then do to us".

---

## Tier B — Correctness, broad impact

### B1 — broad language-construct correctness

| Ticket | Effort | Why here |
|---|---|---|
| [free-var-read-in-callee-resolves-through-dynamic-caller-chain](deep/free-var-read-in-callee-resolves-through-dynamic-caller-chain.md) | XL | **Reconfirmed today** (`f sees 1` / `alias 200`; raku `f sees 5`). A callee's env is `Env::scoped_child(caller_env)`, so a free variable walks the *dynamic* chain and a routine loses its own lexical. Silent wrong read in ordinary code, still the highest-leverage finding with no ADR (ADR-0055 §7.5 puts it out of scope). Its cheaper half — why `f`'s own `my $var = 5` is not visible in `f`'s env tier — is worth isolating first. **The repro needs the file form**; the `:=` bind in `g` is load-bearing. |
| [immutable-list-element-bind-is-writable](deep/immutable-list-element-bind-is-writable.md) | L | **Reconfirmed**: `my @t := (5,6); my $x := @t[0]; $x = 10` prints `(10 6)` — mutsu *mutates an immutable List*; raku refuses. The guard exists but is **narrowed to `my \a :=` binds only** because three consumers lean on the promotion (a chunked multi-parameter loop — `S32-str/val.t` loses 1201 subtests — QuantHash `.kv`, `Pair.kv` in a closure). Closing it means fixing those three, then dropping the flag; prototypes for two are in the file. |
| [mainline-lexical-sigilless-binding-leaks-into-a-later-redeclaration](deep/mainline-lexical-sigilless-binding-leaks-into-a-later-redeclaration.md) | L | A named sub closing over `my \x` puts a **name-keyed** mainline-lexical entry that outlives its block, so an unrelated later `my \x := 5` finds the stale `ContainerRef` and a write that must die silently succeeds. Fourth leg of campaign 2 above. |
| [dollar-dot-attr-compound-assign-spurious-ro-error](deep/dollar-dot-attr-compound-assign-spurious-ro-error.md) | L | **Reconfirmed**: `$.x = 9` inside a method *mutates* a non-`rw` attribute (prints `9`); raku throws `Cannot modify an immutable Int (1)`. Both halves are silent over-mutation. Needs the "accessor read is an itemized copy" ADR; explicitly **not** ADR-0040. |
| [call-compiled-closure-lacks-merge-all-and-dual-persistence-store](deep/call-compiled-closure-lacks-merge-all-and-dual-persistence-store.md) | XL | Closure free var resolves to `CALLER` where raku says `OUTER`. ADR-0055 slices 2-5; the `merge_all` knob the file proposes is *rejected* by the ADR — read the ADR, not the file's proposal. |
| [unvouched-capture-cells-leak-state-across-cro-client-requests](deep/unvouched-capture-cells-leak-state-across-cro-client-requests.md) | M-L | The mechanism that closes ADR-0055 §1.2(b) was built, validated (full roast green) and **removed** because a stale cell leaks request state across `Cro::HTTP::Client.request`'s recursive redirect. Two candidate fixes, both need a cell-freshness design. Gate: the batteries suite, which `make test` does not run. |
| [residual-try-cell-eager-seq-reification-divergences](deep/residual-try-cell-eager-seq-reification-divergences.md) | L | `.map`/`.grep` run their callback eagerly. ADR-0058's target; implementing it makes mutsu stricter, so a full local `make roast` is mandatory. |
| [when-nonmatch-value-outside-map-grep](deep/when-nonmatch-value-outside-map-grep.md) | L | **Re-measured**: `{ when 2 {...} }(3)` and `(given 3 { when 2 {...} })` both answer `Nil`; raku `Bool::False`. ADR-0052 slices 2-4 — three disagreeing statement-sequence compilers. |
| [module-file-scope-array-and-hash-still-share-the-caller](deep/module-file-scope-array-and-hash-still-share-the-caller.md) | L | Module shape fixed (ADR-0039 slice 1); by-name container resolution in inner blocks/closures still corrupts. Slice 2, and the container-side leg of campaign 2. |
| [supply-channel-has-no-fanout-to-multiple-taps](deep/supply-channel-has-no-fanout-to-multiple-taps.md) | L | Second `whenever` on `$proc.stdout` gets nothing (single `mpsc` receiver). Live-vs-on-demand replay semantics must be respected; unify with the `Supplier` registry. |
| [whenever-expression-position-needs-real-design](deep/whenever-expression-position-needs-real-design.md) | M | Symptom already moved: both legal shapes answer `Tap`; `Tap.close` retroactively drops the value emitted before it. Reconcile ADR-0053's header with what landed, then do the identity slice. |
| [grammar-action-ordering-vs-inline-code-blocks](deep/grammar-action-ordering-vs-inline-code-blocks.md) | L | A `make`-bearing embedded block runs at reduce time, out of order. Needs a write channel into the live capture accumulator plus backtrack undo; lands under full roast + battery coverage or not at all. |
| [regex-quantifier-eager-candidate-enumeration-overruns-code-blocks](deep/regex-quantifier-eager-candidate-enumeration-overruns-code-blocks.md) | L | Embedded blocks fire per *computed* candidate (5 vs 2, 17 vs 3). Quantifier-matching architecture change; ADR-0009's "never execute user code while measuring" is the prior art. |
| [native-method-accepted-named-declarations](deep/native-method-accepted-named-declarations.md) | L | **Reconfirmed**: `"abc".chop(:zzz)` → `abc` (raku `ab`); an unknown named silently lands in a positional slot on six measured methods. Two designs, ADR first. |
| [user-prefix-op-candidate-beats-builtin-typed-candidate](deep/user-prefix-op-candidate-beats-builtin-typed-candidate.md) | L | **Reconfirmed with the file's own repro** (`multi prefix:<++>($a) is default {...}; ++$foo` → mutsu `0`, raku `2`). Note a plain `sub prefix:<++>` *does* win in raku too — use the file's `multi` repro, not a simplified one. Native operators are not dispatch candidates at all. |
| [definiteness-constrained-type-object-identity-lost](deep/definiteness-constrained-type-object-identity-lost.md) | L | **Reconfirmed**: `Any:D.^name` → `Any` (raku `Any:D`). Needs a `DefiniteHOW`-equivalent representation ADR. |
| [resume-does-not-return-to-die-call-site-in-nested-sub](deep/resume-does-not-return-to-die-call-site-in-nested-sub.md) | L/XL | `.resume` after a `die` in a nested sub prints nothing at all (raku: `after-inner` / `after-call`). Continuation-shaped; tied to how Rust frames unwind. |
| [custom-io-handle-write-read-not-dispatched](deep/custom-io-handle-write-read-not-dispatched.md) | L | `$*OUT = $store` writes to the real fd and the store stays empty; raku captures. Subclass `WRITE`/`READ`/`EOF` ignored by print/say/read. |
| [is-typename-custom-container-store-protocol-unimplemented](deep/is-typename-custom-container-store-protocol-unimplemented.md) | L | `my @v is DNA = 1,2` never calls `STORE`. Scope it first (grep the corpus for `method STORE`). |
| [export-default-package-not-symbolically-navigable](deep/export-default-package-not-symbolically-navigable.md) | M-L | `::("Test::EXPORT::DEFAULT::&ok")` yields a `Failure` (raku `(Sub)`). Decide how deep to model export tags. |
| [unify-statement-expression-control-construct-compilation](deep/unify-statement-expression-control-construct-compilation.md) | XL | Architectural debt, still growing. Keeps producing paired half-bugs — `when-nonmatch` and `do-block-does-not-scope-routine-declarations` are both instances. |

### B2 — batteries / dist-blocking

| Ticket | Blocks | Effort |
|---|---|---|
| [vendor-real-test-module](deep/vendor-real-test-module.md) | making the vendored upstream `Test` the default (retiring the native provider) | XL as a campaign. Its remaining blocker is perf, **and the perf ticket's diagnosis is wrong** — see the perf section. The call-path sweep that landed today (~33% off `bench-fib`) has not been re-measured against this workload; **do that first**, it is the single cheapest thing in this row. |
| [config-toml-battery-core-blockers](deep/config-toml-battery-core-blockers.md) | `Config::TOML` + `Crane` battery slot | L (cluster): three independent core campaigns (Crane's array-path semantics, `\UXXXXXXXX` grammar candidate selection, inline-table timeout). **Do not start the vendoring steps.** |
| [template-engines-blocked-on-mutsu](deep/template-engines-blocked-on-mutsu.md) | the template battery runner-ups | L (cluster) — **re-survey first**: both `Template::Jinja2` blockers are closed in `news/`, so its "0/23" row is unmeasured. `Template6` is still unreduced. |
| [p5tie-stash-bind-key-protocol](deep/p5tie-stash-bind-key-protocol.md) | `P5tie`, `annotations` (~0.5% of sampled dists) | L — `Stash.BIND-KEY` / `CALLER::.BIND-KEY` both missing. Rung-2 machinery only. |
| [subtest-compiled-dispatch-async-middleware-regression](deep/subtest-compiled-dispatch-async-middleware-regression.md) | re-landing #6499's `subtest` perf win | M-L — root cause unknown; bisect from the dispatch end with `rust-gdb` frame diffs, not from Cro. |

---

## Tier N — narrow correctness / diagnostics

| Ticket | Category | Effort / note |
|---|---|---|
| [end-phasers-install-at-compile-time](deep/end-phasers-install-at-compile-time.md) | correctness-narrow | M-L — mutsu prints `loop`/`main`; raku also runs the END in a never-taken `if False` block and in an uncalled sub. Same-line ordering tie-break included. |
| [chained-index-assign-autoviv-loses-hole-tracking](deep/chained-index-assign-autoviv-loses-hole-tracking.md) | correctness-narrow | S-M — **reconfirmed**: `@a[0][1] = 5; @a[0][0]:exists` is `True` (raku `False`). The `;` form is fixed; find the chained autoviv site. |
| [chained-and-array-element-sigilless-bind-wrongly-readonly](deep/chained-and-array-element-sigilless-bind-wrongly-readonly.md) | spurious die on valid code | M — **half fixed, rewrite before dispatching.** Shape 2 (`my \x := @arr[0]; x = 1000`) now writes through. Only shape 1 survives, the two-hop bind chain (`my \y := $a; my \x := y; x = 42` → `Cannot modify an immutable Int (5)`; raku `a=42`), and its message changed, so the file's `mark_readonly` hypothesis is stale. **Note it is a duplicate**: the same two-hop row is also recorded in `tickets/for-loop-sigilless-param-writeback-skips-the-type-check`'s "Also still open" section. Break with gdb before fixing. |
| [module-toplevel-private-sub-leak-cleanup](deep/module-toplevel-private-sub-leak-cleanup.md) | accepts code raku rejects | M — **reconfirmed**: a non-exported module `sub helper` stays callable bare after `require` (raku: compile-time `Undeclared routine`). Needs an exhaustive audit of ambient `GLOBAL::` installers first (a generalisation was tried and reverted). Adjacent to campaign 2, but a *different* mechanism (the `GLOBAL::` key), so do not fold it in blindly. |
| [role-body-placeholder-mu-supply](deep/role-body-placeholder-mu-supply.md) | rejects code raku accepts | M-L, **low priority by its own assessment** — ADR-0048 P5's value half. raku accepts `role R { $^c }` but supplies an uninitialized `VMNull` whose `.defined` *throws*, i.e. the semantics being matched are garbage. Corpus scan of `roast/`, `modules/`, `vendor/`, `lib/`: **zero** hits. |
| [native-method-cannot-return-an-lvalue-container](deep/native-method-cannot-return-an-lvalue-container.md) | missing feature | L — the `.VAR = 5` row is *wrong* (raku dies too); `.snitch = 666` is the only acceptance case. Needs the container-propagation design campaign. |
| [typed-shaped-array-rows-lose-element-value-type](deep/typed-shaped-array-rows-lose-element-value-type.md) | self-consistency (**no raku oracle** — raku says "Partially dimensioned views of shaped arrays not yet implemented") | S-M — thread `value_type` through `make_shaped_array_seeded`'s rows. |
| [slurpy-hash-named-arg-raku-boolean-shorthand-missing](deep/slurpy-hash-named-arg-raku-boolean-shorthand-missing.md) | rendering | **Reconfirmed**: `{:a(Bool::True)}` vs raku `{:a}`. Small and self-contained. |
| [direct-metamodel-classhow-new-type-immutable-error](deep/direct-metamodel-classhow-new-type-immutable-error.md) | missing feature | M/L — **narrow the file before starting**: the `.^add_method`-on-`new_type` half appears to work now. What is confirmed still broken is `does Metamodel::Naming` / `Metamodel::Stashing` (`X::InvalidType`), a much larger slice of the MOP. |
| [begin-time-adverb-value-interpolation](deep/begin-time-adverb-value-interpolation.md) | correctness-narrow | L, **low priority** by its own assessment — no roast coverage; would add a whole-AST name-rewriting pass. |

---

## Perf — batch into one profiling session; implementation agent runs SOLO

**Every file in this section now predates the sweep that landed on 2026-09-03/04.**
Ten PRs (#7259, #7262, #7274-#7280) took `bench-fib` from ~1.25-1.32x raku on
the morning of 2026-09-03 to **0.84x at `54946f2aa`** (JIT row 0.41x), and none
of these files was updated. **Re-measure before starting any row below** — for
several of them the profile they describe no longer exists.

| Ticket | Status |
|---|---|
| [late-august-call-path-slowdown-remainder](perf/late-august-call-path-slowdown-remainder.md) | **The live campaign's ledger, now behind its own campaign.** Re-read it against the bench history before continuing. Its "Do NOT keep bisecting" section is still the most important paragraph in `todo/perf/`: layout noise is ~5%, so any commit a bisect names must be discharged by checking whether its code is even *sampled*. Use the differential profile. |
| [locals-frame-is-a-pooled-vec-not-a-register-window](perf/locals-frame-is-a-pooled-vec-not-a-register-window.md) | **The named next target** — but its ~5.7% figure was measured *before* the sweep; re-profile first. The fix is the standard register window (one `locals_stack` with a per-frame base). **ADR-class, not a slice**: `self.locals` is touched at 484 sites across 60 files, `mem::take` is load-bearing in three call paths, `VmCallFrame::saved_locals` owns a whole `Vec`, and the JIT emits code against `Interpreter::locals`' offset. Write a `Proposed` ADR before any code. |
| [interpreter-call-path-in-hot-loops](perf/interpreter-call-path-in-hot-loops.md) | **Symptom real, root-cause section STALE — do not start from its "Where to start".** The `&`-sigil signature gate it blames was measured closed on 2026-09-04 (byte-identical opcode profiles for `sub f(&c)` vs `sub f($c)`). The morning's replacement pointer — 2000 real-`Test` assertions doing 50 060 full by-name resolves, 83.3% of function-call opcodes falling back to the interpreter path — is also **pre-sweep**. Step one is a fresh measurement of `ok`-per-assertion cost under real `Test`; that number decides whether `vendor-real-test-module` is still perf-blocked at all. |
| [hash-workload-cost-is-spread-across-gc-alloc-and-key-hashing](perf/hash-workload-cost-is-spread-across-gc-alloc-and-key-hashing.md) | A profile *record*, deliberately not a fix: GC ≈14%, allocation ≈12%, NaN-box decode ≈13%, key hashing+comparison ≈10%. Three leads, most tractable first: `Interpreter::current_package()` (an `RwLock` read + `String` clone, 2.2%, 228 call sites, with a `current_package_sym()` already beside it); the user-key `HashMap`'s SipHash (**do not just swap the hasher** — iteration order and collision-DoS both need deciding); and whether a hash element store needs a cycle-collected cell at all (ADR discussion). |
| [bench-ctor-construction-parity](perf/bench-ctor-construction-parity.md) | Round 5 found the "flat profile" conclusion of rounds 2-4 was wrong — a per-call `.map` compile. Lesson: **`MUTSU_VM_STATS` `add_constant` must stay flat on a steady-state loop**; growth = a runtime compile. Three unmeasured leads inside `dispatch_bless` remain. |
| [closure-literal-creation-cost](perf/closure-literal-creation-cost.md) | Parts A/C done (−20%/creation, body shared). Part B (O(kept-env) capture) is ADR territory — narrowing the kept set trusts an incomplete static analysis, the `roles-6e.t` flake shape. Cost the "share the system-name portion through the parent chain" alternative first. |
| [interpreter-new-is-expensive-and-retains-memory](perf/interpreter-new-is-expensive-and-retains-memory.md) | ~9 ms and **~7.2 KiB retained** per `Interpreter::new()`, linear over 4000 constructions. **Debug-build numbers** — re-measure in release before designing. Chase the *retention* first, not the wall clock. Now slightly more relevant: the LSP (ADR-0065 S1-S5a) is a long-lived process that parses repeatedly. |
| [digest-ripemd-start-per-block-overhead](perf/digest-ripemd-start-per-block-overhead.md) | Title is historical (the `start` lever is closed). `t/ripemd.t` ~148-156s vs a hard 120s gate; profile is flat; needs a fresh dominant item, not a guess. |
| [yaml-parse-throughput](perf/yaml-parse-throughput.md) | ~5x raku on real files after nine rounds; open items: candidate enumeration, `invoke_grammar_actions` materializations. Carries the three most valuable methodology notes in `todo/` (CPU-spinner check before any A/B; deep-recursion `perf` children percentages are misattributions; compute from the tag probe you already have). |
| [adr0019-g3-diffuse-bless-allocation-cost](perf/adr0019-g3-diffuse-bless-allocation-cost.md) | Blocked on a working call-graph profiler (`addr2line` stale build-id entries under `/root/.debug`). Pair with the bench-ctor row. |
| [bigint-repeated-addition-performance-gap](perf/bigint-repeated-addition-performance-gap.md) | ~14x raku — **on a debug build**; re-measure on release before ranking. |
| [closure-sequence-evolution-performance-gap](perf/closure-sequence-evolution-performance-gap.md) | ~84x raku — **debug numbers**; the combined case (48s) far exceeds the sum of its parts (~7.5s), which is the actionable signal. |

Numbers that end up in a document must come from the **bench CI**
(`bench-history.tsv` on `bench-data`), never from a profiling session's own
local runs. The `0.84`/`0.41` figures above are read from that file at
`54946f2aa`; everything else quoted here is session-local routing evidence.

---

## Icebox — blocked on a decision, or a pure record

| Ticket | Blocked on / why |
|---|---|
| [lsp-references-needs-a-side-table-not-ast-spans](deep/lsp-references-needs-a-side-table-not-ast-spans.md) | **Measurement before design.** ADR-0065 D6 assumed spans on AST variants; the parser already knows every byte offset, so a thread-local occurrence table gated on an analysis flag is cheaper and touches neither `Expr`'s size nor the bincode cache. The blocker is **backtracking** — a `Var` parsed in a failed alternative would be a phantom reference. Step one is to build the table behind the flag, run it over `modules/`/`vendor/`/`t/`, and measure the phantom rate; that number decides the design. Also needs an explicit ADR decision that `references` is name-based, not declaration-based. |
| [immutable-lvalues-that-mutsu-still-lets-you-assign-to](deep/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md) | **A survey, not a unit of work.** Probe harness re-run today: **6 of 7 rows still succeed silently** and `(1..3)[0] = 9` still throws the right class with the wrong rendering (`immutable value (1 2 3)` vs `immutable Range (1..3)`). No row has moved across four ADR closures, which is itself the finding: these rows are not element-container work. Mine it for rows, do not dispatch it whole. |
| [bare-name-type-constraint-store-is-scope-blind](deep/bare-name-type-constraint-store-is-scope-blind.md) | **No failing repro left** (21 rows match raku). Open only as the tracking record for ADR-0042 slices 2-3. Do not dispatch as a bug. |
| [exception-class-hierarchy-is-mostly-unregistered](deep/exception-class-hierarchy-is-mostly-unregistered.md) | Done except R5 (re-run the real-`Test` sweep), which waits on `vendor-real-test-module`. All 373 rakudo `X::` subtypes match. |
| [rakuast-remaining](deep/rakuast-remaining.md) | ADR-0033 Phase 3 + an undesigned read-gap list. Zero roast dependents; pick by user impact, not cadence. A RakuAST implementation workflow was added 2026-09-03 — read it first. |
| [nativecall-cannot-be-vendored](deep/nativecall-cannot-be-vendored.md) | Measurement record with reopen conditions; blocker 3 (parser) is gone, 1/2/4 stand. Keeps `NativeCall` a justified rung-3 provider. |
| [adr0019-e2-e4-resolver-core](deep/adr0019-e2-e4-resolver-core.md) | E3/E4 closed; E2 is a non-gating counter cleanup. |

---

## Housekeeping notes

- **Closed since the morning regen** (PR that merged the closure): #7285
  `user-postcircumfix-index-not-dispatched-for-instances` (Tier S crash; the
  surviving half was rewritten and moved to `tickets/imported-constant-class-alias-does-not-resolve`),
  #7286 `multidim-assign-to-an-expression-target-is-dropped` (Tier S), #7290
  `proxy-assigned-into-an-array-is-not-fetched` (Tier S, now ADR-0040 §9), #7292
  `multi-param-read-only-closure-capture-snapshots-the-element`, #7293
  `undefined-typed-scalar-loses-its-constraint-when-aliased`, #7294
  `producer-seq-index-read-decontainerizes-the-element-cell`. #7291 closed
  `procasync-stress-segv`'s §8.2 diagnostics slice without closing the file.
- **`todo/` files whose own root-cause section is wrong or half-stale** —
  this project's most common failure mode, so treat it as the default
  assumption: `deep/chained-and-array-element-sigilless-bind-wrongly-readonly`
  (shape 2 fixed, message changed), `perf/interpreter-call-path-in-hot-loops`
  (the gate it blames is closed *and* its replacement pointer predates the
  sweep), `tickets/associative-multidim-lvalue-edge-divergences` row 3
  (`:delete` now deletes rather than no-ops), and every `perf/` file's numbers.
  ADR-0041 §6 is the model for how to record this: keep the ADR, add a section
  saying which premises were measured false and why.
- **One known duplicate**: the two-hop sigilless bind chain
  (`my \y := $a; my \x := y; x = 5`) is recorded both in
  `deep/chained-and-array-element-sigilless-bind-wrongly-readonly` shape 1 and
  in `tickets/for-loop-sigilless-param-writeback-skips-the-type-check`'s "Also
  still open" section. Whoever fixes it should close both.
- **One stale cross-reference**: `tickets/same-named-loop-params-in-one-unit-interfere`
  points at `tickets/closure-for-loop-param-hijacked-by-same-named-captured-outer.md`,
  which was closed by `04d72e09f`
  (`news/2026-09/proxy-at-pos-store-and-shadowed-capture-fixed.md`).
- **Two ADR headers are behind their code**: ADR-0053 says "implementation not
  started" while `.WHAT` already answers `Tap`; ADR-0041 is still `Proposed`
  even though its §6 now records a landed fix and a rejected option.
- Verification for this regen was run ad hoc from `tmp/` (gitignored); each
  ticket's own repro block regenerates it.
