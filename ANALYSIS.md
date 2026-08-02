# mutsu codebase analysis

This document is an **architecture and soundness review** of the mutsu codebase —
"how much of the design is in order, and what debt remains" — not a bug tracker.

First edition: 2026-06-03. Revision history through rev10 (2026-07-19) is in git; each
rev's resolved findings are archived in the news files
([news/2026-07.md](news/2026-07.md) "ANALYSIS.md rev9/rev10 — resolved-item archive").
**rev11: 2026-08-02 — re-verified against HEAD (`c65835e13`) after 1096 commits since
rev10. Three things changed the architecture picture: ADR-0013 landed (the
`gc_contents_mut` provenance UB is gone at the primitive), a bundled-module layer
(`modules/`, 22 vendored dists) and a user-facing MOP (EXPORTHOW::DECLARE) became
first-class subsystems, and ADR-0016 completed the regex capture/`Match` representation
rework. §7 is re-prioritized accordingly, and §8 is a new ADR-ledger review.**

Method:
- subsystem-level close reading, re-verified per claim on the live tree
- every finding carries a `file:line` reference

---

## 0. Summary

mutsu is a Rust implementation of a minimal Raku-compatible interpreter. The roast
whitelist stands at **1435 / 1464 (98.0%)**, up from rev10's 1433/1464 — and that near-flat
number is the point: **roast has been mined out since rev10** (PLAN §3), so a year's worth
of the project's velocity now shows up in places roast does not measure. The 1096 commits
since rev10 went almost entirely into (a) the raku-differential compatibility sweep
(≈100 `news/2026-08/` entries, one general fix each), (b) the batteries campaign — real
upstream modules vendored and run verbatim — and (c) two representation campaigns
(ADR-0015 native-backed storage, ADR-0016 span-based captures).

Overall assessment as of rev11:

- **The execution stack is complete and, since rev10, no longer has a known
  provenance-UB hole.** Single bytecode VM; cycle-collecting GC default-on (ADR-0003);
  8-byte NaN-boxed `Value` (ADR-0005); Cranelift JIT default-on (ADR-0004, closed).
  **ADR-0013 landed**: the payload of every `Gc<T>` now lives in an `UnsafeCell` inside
  `GcBox` (`gc/gc_ptr.rs:166`), so the ~59 deliberate aliased container writes derive a
  `&mut` with valid interior-mutable provenance (`gc_ptr.rs:774`) instead of casting a
  `*const`. What rev10 called the #2 roadmap item is mechanically fixed; what remains is
  *verification* (no Miri gate exists) and the narrow cross-thread race deferred to
  layer 3c (§2.1).
- **Two subsystems that did not exist in rev10 are now load-bearing**: a **bundled-module
  layer** (`modules/`, 22 vendored upstream dists + `vendor/zef`, gated by a release-time
  upstream-test-suite run) and a **user-facing MOP** (`EXPORTHOW::DECLARE` declarator
  registry + HOW-driven class registration, `runtime/metamodel.rs`). Both are described in
  §1.8/§1.9 — and both push load onto the one subsystem that is still tree-walking
  (declaration registration, §1.1).
- **One representation campaign finished, one did not.** ADR-0016 (span-based captures +
  lazy `Match`) landed all five phases in four days at the end of July and corrected five
  real compatibility bugs on the way; it left behind one unenforced invariant (a `view()`
  probe materializes a lazy `Match`). ADR-0015 stopped after P3a: `Buf`/`CArray` are
  native-backed, `array[T]` is not, and the accessor chokepoint that would unify them is
  unbuilt (§1.10).
- **Performance remains a surplus, not a problem.** At HEAD the bench CI ratio vs Rakudo is
  below 1.0 on every benchmark except `bench-ctor` (1.21), `bench-tak` (1.06) and
  interpreter-only `bench-fib` (0.98). Per PLAN's 2026-07-16 priority reset, perf is polish;
  this revision therefore ranks architectural items by **debt shape and dependency**, not by
  profile share.
- **Hygiene trends keep worsening, and faster than rev10's slope**: files >500 lines
  239→**300**, >1000 62→**80**, `runtime/mod.rs` 2470→**2495**, `unwrap/expect/panic!/
  unreachable!` 1789→**1908**, `#[allow(` 170→**178**, `.clone()` 9056→**10192**. Three
  consecutive revisions have flagged `runtime/mod.rs`; it has never been slimmed.

None of the remaining issues is of the "the basic design is broken" kind. The shape of the
debt has changed, though: rev10's top item was *soundness* (raw-pointer writes), and that is
now mechanically closed. rev11's top items are **half-finished migrations** (§1.10) and one
**mechanism that many open correctness bugs share a root in** (§1.3 / §2.4).

Where to look first:
- §1: what architectural work remains (§1.8-§1.10 are new)
- §2: open correctness/soundness issues
- §7: prioritized roadmap — the main deliverable of this revision
- §8: ADR ledger review

---

## 1. Architecture

### 1.1 Remaining tree-walk — declaration registration and dispatch entries only

User-code bodies (subs, methods, blocks) execute exclusively as bytecode. What still walks
the AST (re-verified 2026-08-02):

- **Declaration registration**: `register_class_decl`
  (`runtime/registration_class_decl.rs:419`), `register_sub_decl`
  (`runtime/registration_sub.rs:438`), `register_role_decl`
  (`runtime/registration_role.rs:257`) run off `Register*` opcodes. Class system, MRO and
  role composition are uncompiled — registration, not body execution.
  **This is no longer a static amount of debt.** The MOP campaign (§1.9) hung the user HOW
  protocol off exactly this path: `declare_drive_how_protocol` drives `new_type` /
  `add_method` / `compose` against a user metaobject from inside AST-walking registration,
  and `registration_class_decl.rs` has grown to 2882 lines. Every future MOP feature adds
  to a tree-walk that has been flagged since rev5.
- **Dispatch resolver entries**: multi/submethod and `samewith`/`nextsame` enter through
  `run_instance_method` (`runtime/class_dispatch.rs:52`, plus a `_celled` variant at `:90`);
  bodies are compiled. The sound multi-method resolution cache + `fast_method_cache` still
  amortize the resolver, so what remains is entry-point consolidation (§3.3).
- **Module-sub OTF compile gate** (`def_is_otf_compilable_module_single`,
  `vm/vm_call_func_ops.rs:1991`): unchanged since rev10. The residual exclusions are
  mechanism-level — `state`, sigilless `\x` params, `is encoded(...)`, `start` — each with a
  documented blocker (`todo/tickets/otf-compilation-gate-leftovers.md`).

### 1.2 Closure upvalues — Phase 1 only, unchanged since rev5

`compute_upvalues` (`opcode.rs:3832`) is still the conservative Phase 1: only pure scalar
reads of already-shared `ContainerRef` cells are promoted to indexed `GetUpvalue`; writes,
RW ops and `@`/`%`/`&` sigils are excluded, and the pass bails entirely under
`captures_env_by_name`. Out-of-range indices fall back to live env-by-name reads — always
sound. Phase 2 stays blocked on general captured-lexical cell-ification, and the rev5
soundness walls still stand (value snapshots are unsound because compile-time mutation
analysis is incomplete; blanket cell-ification of read-only captures breaks
ContainerRef-blind paths). This is one of the three consumers that §1.3 must move together.

### 1.3 Lexical-scope slots — precise per-slot sync exists; the blanket is the endgame

Shadow slots have been default-ON since 2026-07-12, the whole-`locals` per-block clones are
gone (rev10 slices 1-3), and `needs_env_sync` is now a genuine **per-slot** `Vec<bool>`
computed by `compute_needs_env_sync` (`opcode.rs:2534`): a slot is marked only if some op
reads or writes that name by name (`opcode.rs:2680-2690`), plus the closure-free-var fold.

What remains is the blanket that overrides all of it:

```
self.captures_env_by_name = ops.any(ForLoop | BlockScope | BlockLocalScope
                                    | MakeGather | WheneverScope)   // opcode.rs:2608
if self.captures_env_by_name { needs_env_sync.fill(true); return }  // opcode.rs:2648
```

One occurrence of any of those five ops in a frame makes **every** local an env-mirror
target. The blanket is not removable one consumer at a time: block-scope restore re-pulls
locals from env by name, the loop mechanism writes the loop var to both slot and env,
`MakeGather`/`WheneverScope` stash a body in `stmt_pool` and run it by name against the live
env, and closure capture reads free vars from the same mirror.
`todo/deep/needs-env-sync-blanket-removal.md` records four independent mechanisms that a
standalone removal deterministically broke.

**The reason this ranks higher in rev11 than in rev10 is not performance.** It is that the
open correctness tickets in `todo/` cluster on this one mechanism — see §2.4.

### 1.4 Optimizer and opcode set — baseline passes exist; the remainder is measurement-gated

ADR-0006's adopted measures all landed (constant folding, constant-pool dedup, `constant`
inlining + constant-condition DCE, declaration-marker fusion, `SetSourceLine` removal).
`OpCode` stays ≤48B (`opcode.rs:1722`), `Value` ≤8B (`value/mod.rs:2186`), and a `CapNode`
leaf is size-guarded too (`runtime/regex_types.rs:309`).

Remaining, all gated on ADR-0006's measurement protocol: the surviving administrative ops
(`SetVarDynamic`, `CheckReadOnly`), inline `Option<String>` payloads → constant-pool
`Option<u32>`, `Jump(i32)` carrying an absolute index, and histogram-driven consolidation of
syntax-shaped specialized ops. The "opcode count ≠ time" lesson (#4489) still governs.

### 1.5 JIT — default on; ADR-0004 closed

Unchanged since rev10: Tier A translates hot opcode chunks into helper-call sequences, Tier B
inlines NaN-box tag-dispatched Int/Num arithmetic. All six J4d slices landed and ADR-0004 is
closed. The structural note from rev10 still holds and matters for §7: **the JIT bails at the
call boundary**, so a loop that calls a sub runs the interpreter call path — no amount of JIT
coverage subsumes §1.3.

### 1.6 Parser and pseudo-slangs — plus a real declarator registry

Hand-written scannerless recursive descent; precedence handling is textbook-clean
(`parser/expr/precedence.rs`); `memo.rs` gives packrat-style backtracking relief. There is
still **no true slang stack**: regex bodies are scanned as raw text at parse time
(`parser/primary/regex/scan.rs`) and structurally parsed at runtime; Pod is skipped by the
parser and rebuilt from raw source at runtime.

New since rev10: the parser carries a **unit-scoped declarator registry**
(`parser/stmt/simple/registry.rs`, `declare_keyword_names()`), fed by a module's
`EXPORTHOW::DECLARE` block, and both the bare form (`class::declare_decl`) and the
scope-prefixed form (`decl::my_decl_dispatch::try_keyword_dispatch`) consult it. That is a
genuine, if narrow, step toward user-extensible syntax — it extends the *declarator* table,
not the grammar. It does not change the slang verdict: user-defined grammar/token/rule slang
switching remains future work, and the registry is deliberately not a general slang
mechanism.

### 1.7 RakuAST model layer — read+write, no second engine

`src/rakuast/` (5 files, ~3960 lines) converts the internal `Stmt`/`Expr` tree to a RakuAST
node tree (`Q[…].AST`) and lowers it back through the **same compiler** (`EVAL($tree)`), so
there is still no second execution engine. ADR-0011's Phase 1-5 work landed broadly; the
remaining gaps are inventoried in `todo/deep/rakuast-remaining.md` (read-direction
representation gaps where the parser desugars before conversion, construction of advanced
parameter forms, a lowering list blocked on representation mismatches, and Phase 6 macros).

**Planning note**: no whitelisted roast file uses RakuAST, and no bundled battery needs it.
It is the one large campaign in this document with no downstream consumer, which is why §7
demotes it relative to rev10.

### 1.8 Bundled-module layer (new since rev10) — the batteries architecture

`modules/` holds **22 vendored upstream dists** (Base64, Crypt-Random, DateTime-Parse,
DBIish, Encode, File-Directory-Tree, File-Temp, HTTP-HPACK, HTTP-Status, HTTP-UserAgent,
IO-Path-ChildSecure, IO-Socket-SSL, MIME-Base64, NativeHelpers-Blob, NativeLibs, OO-Monitors,
OpenSSL, Rakudo-Core, Template-Mustache, Test-Util-ServerPort, URI, YAMLish), plus
`vendor/zef`. Module resolution has a documented precedence chain (`use lib` → `-I` →
`MUTSULIB` → the `mzef` site repo → bundled batteries), and a release-time gate runs each
battery's **upstream** test suite (`scripts/battery-testsuite.sh`, `batteries.lock`).

Architecturally this is a policy decision with teeth (BATTERIES.md): **a module is grown into
by fixing the interpreter (rung 2), never reimplemented natively (rung 3)** — banned by user
decision 2026-08-01. The consequences are visible in the code: the native `Test::Util` /
`Test::Tap` overrides were retired (`news/2026-08/retired-native-test-util-overrides.md`),
`Pod::To::Text` became the real rakudo module, and rakudo's real `Test.rakumod` is now
vendored behind `MUTSU_REAL_TEST=1` pending the flip.

**Why this belongs in an architecture review**: the vendored suites are now the strictest
correctness oracle the project has (stricter than roast, which is mined out), and the
remaining native providers are an explicitly enumerated exception list — currently
`NativeCall` (measured non-vendorable, `todo/deep/nativecall-cannot-be-vendored.md`),
`JSON::Fast`, and `Test` (mid-retirement). That exception list is a first-class piece of the
architecture and should shrink monotonically or be justified in writing.

### 1.9 Metaobject protocol (new since rev10) — user HOWs on top of AST registration

`runtime/metamodel.rs` plus the registration path now implement enough MOP for a real
ecosystem module to install its own declarator and metaobject:
`EXPORTHOW::DECLARE::<keyword>` registration, `Metamodel::ClassHOW` subclassing with
fully-qualified `self.Metamodel::ClassHOW::<meth>` dispatch and `callsame` base candidates,
user `new_type`/`add_method`/`compose`, a user `BUILDALL`/`POPULATE` hook at construction,
and a user `clone` reaching the native attribute-copying clone. `OO::Monitors` runs verbatim
on this (`news/2026-08/exporthow-declare-mop.md`) and the native `monitor` stopgap was
retired.

Two structural observations:

1. **It is bolted onto the tree-walking registration path** (§1.1). The user protocol is
   driven from inside `register_class_decl`, so MOP breadth and §1.1's compilation debt are
   now coupled: each new HOW metamethod is another AST-walking special case.
2. **B2b's "custom HOW inheritance is campaign-sized" verdict is partly obsolete.** PLAN
   §B2b still describes HOW subclassing as unbuilt; the OO::Monitors campaign built a
   meaningful part of it. What remains unbuilt is the NQP/QAST/slang layer Test::Async needs,
   which is a *different* claim. That scouting result now lives in
   `docs/ecosystem-guts-dependency-survey.md` (PLAN's §1 B2b was dropped 2026-08-02 —
   Test::Async is not a bundle candidate), and it is the claim to re-scope.

### 1.10 Representation campaigns — one complete, one half-finished

- **ADR-0016 (regex captures / `Match`) is complete.** All five phases landed between
  2026-07-28 and 2026-07-31: absolute positions, the `CapNode` / `RegexCaptures` split
  (immutable stored node vs. per-run accumulator — a deliberate split, not a dual model), a
  shared `MatchTarget` with span reads, the one-list-per-axis collapse, and a lazy
  `ValueRepr::Match(Gc<MatchNode>)`. Along the way it corrected five real compatibility bugs
  (the four subrule-boundary constructs and search-recovered offsets for repeated text).
  Residue is deliberate and small: `String` capture-name keys, `CodeBlockContext`'s text
  snapshot, and eager `Match` construction in the reduce/failed-replay paths.
  **One standing constraint the whole codebase now inherits**: a `view()`-based
  "is it an X?" probe materializes a lazy `Match`, so variant probes on paths a `Match` can
  reach must be tag probes. That is an invariant with no mechanical enforcement — a plausible
  future regression, and worth a lint or a debug counter.
- **ADR-0015 (native-backed container storage) is half-finished.** P0/P1/P2/P3a landed
  (CStruct bodies, native-backed `Buf`/`Blob`, native-backed `CArray[T]`); **P3b**
  (`array[T]`, which needs the `ArrayData::items` accessor chokepoint and is also the fix for
  roast's shaped-native `array-shapes.t` T36-38) and P3c (reference-element `CArray`) are
  open. So `Buf` and `CArray` have honest `.REPR`/`.WHERE` while `array[T]` does not — one
  concept, two storage models, with the chokepoint refactor that would unify them still
  unbuilt.

A half-migrated representation is the most expensive shape of debt in this codebase: it
doubles the surface every unrelated fix must satisfy and silently invites new code to pick
the old model. That is now a statement about ADR-0015 alone — which is why §7 ranks P3b
first among the representation items and does not schedule further regex work.

---

## 2. Correctness and soundness

### 2.1 GC-era aliased writes — provenance UB fixed (ADR-0013); verification is the gap

**Fixed since rev10.** `GcBox` now stores its payload as `value: UnsafeCell<T>`
(`gc/gc_ptr.rs:166`), and `Gc::as_ptr` projects through it with `UnsafeCell::raw_get`
(`gc_ptr.rs:198`). `gc_contents_mut` (`gc_ptr.rs:774`) therefore derives its `&mut` from a
pointer that carries interior-mutable provenance while shared `&` reads are live — the
Stacked/Tree-Borrows violation rev10 listed as roadmap item #2 is gone, at every call site
at once and with no `Value`-representation churn (ADR-0013 §7).

What is left, in order of tractability:

- **No Miri gate.** ADR-0013 §4 phase 4 defines "done" as a clean `cargo miri test` over the
  container/GC subset, first informational then blocking. There is **no `miri` job in
  `.github/workflows/`**. The soundness claim above is therefore an argument, not a check —
  and the argument is exactly the kind that a borrow-model change can silently invalidate.
- **The call-site contract is still delegated to the caller.** 59 sites across 24 files
  (rev10: ~53/21, rev9: ~53/21 — the count keeps drifting *up* as new mutation paths reuse
  the primitive). Provenance is fixed for all of them, but "no other borrow of this value is
  dereferenced for the lifetime of the returned borrow" is prose per site.
- **The cross-thread race is deferred to layer 3c** (ADR-0013 §3, resolved open question 2).
  Concurrent structural mutation must stay routed through the synchronized shared-store
  lanes; nothing checks that it does.
- **Stale documentation actively misleads.** `value/aliased_mut.rs`'s module header still
  carries a "⚠️ Known unsoundness (tracked, not removed here)" section describing the
  `Arc::as_ptr` provenance violation as live, and still points at Track B as the future fix
  — both untrue since ADR-0013. That file's `arc_contents_mut` is dead code kept for audit.
  A reader looking for the current soundness posture finds the wrong answer first.

### 2.2 `RuntimeError` as a control channel — cheap now, still cohabiting

Unchanged from rev10: `RuntimeError` still carries `return`/`last`/`next`/`take`/`emit`
through `Result::Err`. The size problem is gone (control bools folded into `enum Control`,
cold routing fields boxed, `result_large_err` allows 0). Channel separation remains unstarted
and low priority. Still worth noting: **there is no `size_of` guard test for `RuntimeError`**
(only `Value`, `OpCode` and the `CapNode` leaf are pinned), so a size regression here would
be silent.

### 2.3 Process-level robustness holes

- **Deep recursion** — the interpreter runs on a 256 MB-stack thread (`main.rs:147`) and the
  pure-recursion integration tests pass. Pathologically deep recursion can still overflow; a
  larger fixed stack is a blunt instrument, not heap frames.
- **`Proc::Async` stress segfault** (`todo/deep/procasync-stress-segv.md`) —
  `roast/S17-procasync/stress.t` segfaults rarely, CI-only so far. A segfault is categorically
  worse than a failing assertion and should not sit in a `todo/` file indefinitely.
- **The WASM build traps** on `start` / `Channel` instead of degrading
  (`todo/deep/wasm-start-and-channel-trap.md`).
- **Recursive start/await hang** (deterministic) and **Supply detached-worker panics are
  swallowed** (QUIT propagation unimplemented) — both unchanged since rev10.
- **No thread pool at all**: 20 `spawn_user_thread` sites, each reserving 256 MiB
  (`runtime/builtins_system.rs:9`). `todo/deep/shared-worker-pool-adr.md` measured the
  consequences (50 idle `cue(:every)`
  timers → 52 threads / +16.4 GB VmSize). The decision this needs — what `await` does to a
  pooled worker, given mutsu has no continuations — is still an **unwritten Proposed ADR**.

### 2.4 The env-writeback cluster — many open bugs, one mechanism (new framing in rev11)

rev10 treated §1.3 as a performance item. Re-reading the open `todo/` findings makes a
stronger case: a large share of the *correctness* backlog shares one root — locals are
mirrored into a name-keyed env, and consumers write that mirror back wholesale.

Findings that reduce to it:

- `todo/deep/closure-capture-shadowed-by-colliding-callee-parameter.md` — a caller's closure
  loses its capture to a same-named callee parameter.
- `todo/deep/captured-outer-pair-container-alias.md` — captured outer variables snapshot in
  `Pair` values.
- `todo/deep/closure-env-capture-cost.md` — closure creation materializes the world.
- `todo/tickets/whenever-owned-lexical-outlives-the-react-block.md`,
  `todo/tickets/supply-block-lexical-leaks-through-thread-lane.md`,
  `todo/tickets/schedule-on-whenever-env-loss.md` — `WheneverScope` bodies, which are exactly
  one of the five blanket triggers.
- `todo/tickets/forward-captured-code-var-snapshot.md` — a forward-captured `&`-lexical is
  snapshotted as `Nil`.
- `todo/tickets/inline-start-blocks-clobber-a-later-declared-variable.md` — "a joined `start`
  block writes its stale captured env back over a variable declared after it", where the
  ticket itself already concludes the fix "belongs with the cell-based capture work, not a
  special case at the call sites".

Each has been triaged as "not a small slice" *individually*, which is the signature of a
shared mechanism. Fixing them one at a time is not merely slow — each local fix adds another
consumer of the mirror and makes the eventual campaign harder.

---

## 3. Duplicate implementations

### 3.1 Statement/expression dual compilation of control constructs

Unchanged: `compiler/helpers_do_expr.rs` (476 lines, 6 `compile_do_*` entry points)
duplicates `stmt.rs` logic for do/if/for/while/loop in expression position, including a
21-field `ForLoopSpec` construction maintained twice. Fix remains one value-returning pass.

### 3.2 Sub declaration registered twice

`SubDecl` both registers an AST body (`RegisterSub`) and compiles the body
(`compile_sub_body`). Collapses when §1.1's declaration registration is compiled.

### 3.3 Method dispatch: many entry points, scattered name matching

Entry points remain unconsolidated: `call_method_with_values`
(`runtime/methods_call_dispatch.rs:50`), `dispatch_method_by_name_{1,2,3}`
(`runtime/methods_dispatch_match.rs:14` and siblings), `run_instance_method` /
`run_instance_method_celled` (`runtime/class_dispatch.rs:52,90`), `native_method_{0,1,2}arg`
(`builtins/`). Same-name string matches stay scattered — `"elems"` appears in **33 files**
(rev10: 8+). `runtime/methods_call_dispatch.rs` is now 3875 lines.

Consolidation into a single type×method dispatch table is what would also let §4-1's hand
tables be derived, and what would give §1.9's user HOWs a single place to intercept.

---

## 4. Hardcode / drift risks

No test-specific hardcoded outputs found (re-checked). Two derivation shortcuts remain:

1. **`.^methods`/`.^can`/`.^mro` tables are hand-maintained** — centralized in
   `builtins/builtin_type_methods.rs` (960 lines, rev10: 874) and guarded by structural tests
   plus `t/can-methods-drift.t`. True derivation awaits §3.3. The growth rate matters now
   that §1.9 lets user code introspect through the same surface.
2. **Parser grammar relaxations for roast** (minor, unchanged): `is List` type-ish traits,
   the Test::Assuming colonpair, and the `throws-like` trailing-`)` special form.

---

## 5. Value model, performance, robustness

- **State outside the value (unchanged)**: Failure handled/pending registries are
  `thread_local!` and lose registration across thread boundaries; pending DESTROY queues
  likewise. Seq consumed/cached/lazy state is O(n) linear scans of `OnceLock<Mutex<Vec<Weak>>>`
  statics. Fragile and slow; also the root of
  `todo/deep/cache-on-a-lazy-seq-must-not-answer-seq.md` and
  `todo/deep/deferred-seq-materialization-destroys-the-original.md`.
- **Env**: COW `Arc<FxHashMap<Symbol,Value>>` with a scoped parent-overlay chain capped at
  `MAX_OVERLAY_DEPTH=16` (`env.rs:318`). The structural remainder is §1.3's blanket.
- **`.clone()` ≈ 10192** (rev10: 9056): each is an 8-byte NaN-box copy plus a refcount for
  container tags, so the unit cost is low, but the growth is a code-shape signal, not just a
  perf one.
- **`unwrap`/`expect`/`panic!`/`unreachable!` ≈ 1908 (+119)** and **`#[allow(` 178 (+8)**.
  PLAN §6's "mutsu must never Rust-panic on any input" goal is in tension with a metric
  that has risen every revision.
- Allocation-failure aborts on user-sized allocations remain guarded via `try_reserve`.

---

## 6. Repository hygiene

- **500-line rule**: **80 files >1000 lines, 300 files >500** (rev10: 62 / 239; rev9:
  57 / 210). Total `src/` is ~427k lines. Largest: `opcode.rs` 4823,
  `vm/vm_exec_dispatch.rs` 4646, `runtime/methods_call_dispatch.rs` 3875,
  `compiler/stmt.rs` 3775, `runtime/regex_parse_core.rs` 3698,
  `vm/vm_var_assign_index_named.rs` 3542, `parser/expr/postfix/loop_.rs` 3260,
  `runtime/registration_class_decl.rs` 2882. Giant dispatch matches remain intentional
  exceptions; the other seven are not. `runtime/mod.rs` is **2495** lines (rev7 1932 → rev8
  2118 → rev9 2309 → rev10 2470) — flagged for four consecutive revisions, never actioned.
  The rule as written ("split immediately") is not being followed, so either the rule or the
  practice should change deliberately rather than by drift.
- **Stale docs that assert the opposite of the code**: `value/aliased_mut.rs`'s unsoundness
  header (§2.1), and the comment references to the retired `MUTSU_SHADOW_SLOTS` opt-in gate
  (now the opt-out `MUTSU_NO_SHADOW_SLOTS`).

---

## 7. Recommended roadmap (priority order)

rev10's #1 (lexical-slot endgame) is still open; rev10's #2 (`gc_contents_mut` UB) is
mechanically **done** and drops to a verification task. The ordering below is derived from
debt shape and dependency, not from profile share — per PLAN's 2026-07-16 priority reset,
performance is polish and is not used as a ranking criterion here.

The ranking rule, stated so it can be argued with:

1. **First, mechanisms that many open bugs share a root in** — one campaign closes a
   backlog, whereas N local fixes grow it *and* make the campaign harder, because each fix
   adds a consumer of the mechanism being replaced.
2. **Then finish half-migrated representations** — they double the surface every unrelated
   fix must satisfy and silently recruit new code into the old model.
3. **Then the subsystem whose deferral cost is rising**, i.e. where new feature work keeps
   landing on uncompiled/unconsolidated code (declaration registration and dispatch, now
   carrying the MOP).
4. **Then close verification gaps on already-fixed soundness** — cheap, and the alternative
   is an unverified argument that a refactor can silently invalidate.
5. Long-tail cleanups last — but stop the *trend* items from compounding while doing 1-4.

| # | Item | Kind | Why here |
|---|------|------|----------|
| 1 | **The env-writeback / lexical-slot fused campaign** — `captures_env_by_name` blanket → precise per-slot sync for its five consumers → `BlockScope` restore → closure capture cells (§1.3, §1.2, §2.4) | **correctness** + perf | Re-ranked from rev10's "top perf lever" to "the largest correctness cluster": at least seven open `todo/` findings reduce to this one mechanism, each individually triaged as "not a small slice". Fixing them separately is not merely slow — each local fix adds another consumer of the mirror. Needs a Proposed ADR first (five mechanisms are known to break on a standalone change). |
| 2 | **Finish ADR-0015 P3b** (`array[T]` behind the `ArrayData::items` accessor chokepoint) (§1.10) | representation | The one genuinely half-migrated representation left: `Buf`/`CArray` are native-backed with honest `.REPR`/`.WHERE`, `array[T]` is not. The chokepoint refactor is the shared prerequisite, and P3b is simultaneously the fix for roast's shaped-native `array-shapes.t` T36-38 — the last roast item with real leverage. |
| 3 | **Declaration registration → bytecode, and dispatch-entry consolidation into one type×method table** (§1.1, §3.3; retires §4-1's hand tables) | design | Was a standing "medium" item; the MOP campaign (§1.9) turned it into a *growth surface* — the user HOW protocol now runs inside AST-walking registration (`registration_class_decl.rs` 2882 lines), and `"elems"`-style scattered name matching spread from 8 files to 33. Every further batteries/MOP feature pays interest here, so the cost of deferring is rising rather than flat. |
| 4 | **Exception-class hierarchy registration** (124 core `X::` classes unregistered, `todo/deep/exception-class-hierarchy-is-mostly-unregistered.md`) | design | A registry/data-model job, not a pile of small fixes, and the prerequisite for PLAN §6 error/exception parity — the QA axis that replaced roast as the compatibility signal. |
| 5 | **Close ADR-0013**: add the Miri job (informational → blocking), correct the stale unsoundness docs in `value/aliased_mut.rs`, then take the layer-3c cross-thread-race decision explicitly (§2.1) | soundness verification | The mechanism is fixed; the *check* that keeps it fixed does not exist. What shipped is an argument about borrow provenance, and that is exactly the kind of thing a later refactor invalidates with no observable failure. Cheap relative to that risk. |
| 6 | **Concurrency substrate**: write the shared worker-pool Proposed ADR (still unwritten), then the `Proc::Async` stress segfault, Supply panic → QUIT, and WASM `start`/`Channel` degradation (§2.3) | robustness | 20 spawn sites × 256 MiB is a structural resource decision currently being made by default. Within this row the segfault outranks the rest — a crash is categorically worse than a wrong answer. |
| 7 | **Guard the ADR-0016 invariant** (§1.10): a `view()`-based variant probe materializes a lazy `Match`. Add a debug counter or lint rather than leaving it as prose | correctness (regression prevention) | Small, but it protects a campaign that just finished; unguarded invariants of this shape are how completed migrations quietly un-finish. |
| 8 | **Statement/expression dual compilation** (§3.1) and the measurement-gated opcode remainder (§1.4) | design / perf | Genuine duplication, but bounded and stable — it is not growing the way #3 is. |
| 9 | **RakuAST completion** (`todo/deep/rakuast-remaining.md`, ADR-0011 Phase 6) | feature | Demoted from rev10's #3: no roast file and no bundled battery consumes it, so it is the one large campaign with no downstream dependency. Pick node classes by user impact, as the ticket itself says. |
| 10 | **Hygiene, treated as a trend not a chore**: `runtime/mod.rs` re-slim (2495, flagged 4 revisions running), the 300/80 file-size population, the `unwrap`/`clone` slopes, the stale-doc corrections (§5, §6) | hygiene | Every metric here has worsened monotonically for three revisions, which means the current practice is not the stated rule. The actionable form is to fix the trend on files that #1-#4 touch anyway, and to decide deliberately whether the 500-line rule still stands. |

Explicitly **not** ranked as architecture work in rev11: roast whitelist chasing (mined out,
PLAN §3), and perf levers with no goal-item consumer (PLAN §4 header). Both remain available
as opportunistic work when an item above happens to unblock them.

---

## 8. ADR ledger review (new in rev11)

Reviewed all 17 ADRs against the tree. The decisions themselves hold up — no ADR was found
to be *wrong*. The systematic problem is that **an ADR's recorded status drifts from what
shipped**, because implementation progress is reported in `news/` and PLAN.md but not folded
back into the ADR that owns the decision. That defeats the ADR's stated purpose (preserving
the judgment context) for anyone who reads the ADR first.

Actions taken in this revision (each ADR updated in place with a progress/outcome record):

| ADR | Drift found | Action |
|---|---|---|
| 0001 GC strategy & phasing | Status still listed §4.2 (trigger) and §4.3 (A' scope) as open; §4.2 was decided by ADR-0003, and layers 3a/3b/4 have all shipped. Its standing guidance "do not start Track B standalone; it is fused with GC" was superseded by ADR-0013 §7, which fixed the same sites at the primitive. | Added §7 "Outcome (2026-08-02)"; status line updated to point at it. |
| 0007 trail matcher | Recorded its own implementation outcome, but its explicitly deferred "per-subrule ceremony" became ADR-0016 with no forward pointer. | Added the successor link and marked the P2-P3 phasing superseded. |
| 0011 RakuAST | Status read "Phase 1 implemented (PR #4679); Phases 2-6 pending" while Phases 2-5 had substantially landed across ~37 slices. | Status corrected; pointer to `todo/deep/rakuast-remaining.md` as the live gap list. |
| 0013 container interior mutability | Status was accurate about the *decision*, but §4's phasing did not record that the primitive change shipped, and the README index still listed it `Proposed`. The undone phase (Miri) was indistinguishable from the done ones. | Added §8 "Implementation status"; index corrected. |
| 0015 native-backed storage | Phase markers were inline ("landed"/"open") but the status line gave no summary, so the campaign's state required reading the whole ADR. | Status line now carries the P0-P3c state. |
| 0016 span-based captures | Status `Proposed` while **all five phases** had shipped to `main` — a fully-implemented ADR still labelled as a proposal, which is the most misleading drift found in this review (it invites a reader to re-litigate a decision that is already executed). | Promoted to `Accepted`; the phase state, the deliberate residue, and P5's standing `view()`-probe constraint summarized at the top. |
| 0003, 0004, 0005, 0006, 0009, 0010, 0012, 0014, 0017 | Accurate; 0004 and 0009 already carry closing addenda. | No change. |
| 0002 | Historical gate record; still accurate. | No change. |

Two **missing** ADRs are worth writing, and are listed here rather than drafted unilaterally:

1. **A shared worker pool** — `todo/deep/shared-worker-pool-adr.md` has specified its content
   in detail for over two weeks
   ("the central question is not pool sizing — it is what `await` does to a pooled worker"),
   and the decision is being made by default in the meantime (20 spawn sites, 256 MiB each).
2. **The batteries adoption policy** — "grow the interpreter until the real upstream module
   runs verbatim (rung 2); native provision (rung 3) is banned" is a load-bearing,
   costly-to-reverse decision recorded only in `BATTERIES.md` and CLAUDE.md as a user
   decision. Its rejected alternative (native reimplementation) and its named exceptions
   (NativeCall, JSON::Fast, Test) are exactly the "why, and what we rejected" an ADR exists
   to preserve. The companion measurement — "do not build an `nqp::` op layer" — belongs in
   the same record.

A third, **the env-writeback campaign** (§7-3), needs a Proposed ADR *before* implementation
starts, per the repository's own rule for high-blast-radius mechanism changes.

---

*Based on static close reading plus live verification against HEAD.*
*rev11 (2026-08-02): re-verified against `c65835e13` after 1096 commits; ADR-0013 recorded as
landed, the bundled-module (§1.8) and MOP (§1.9) subsystems added, the representation
campaigns (§1.10) and the env-writeback correctness cluster (§2.4) named, §7 re-prioritized
around the latter, and §8 ADR-ledger review added.*
