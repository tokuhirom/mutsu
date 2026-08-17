# mutsu codebase analysis

This document is an **architecture and soundness review** of the mutsu codebase —
"how much of the design is in order, and what debt remains" — not a bug tracker.

First edition: 2026-06-03. Revision history through rev10 (2026-07-19) is in git; each
rev's resolved findings are archived in the news files
([news/2026-07.md](news/2026-07.md) "ANALYSIS.md rev9/rev10 — resolved-item archive").
**rev12: 2026-08-04 — re-verified against HEAD (`435de2d3e`), 302 commits after rev11.
ADR-0013 is now closed with a required Miri gate and a call-site audit; ADR-0016's lazy
`Match` invariant has an observable counter; and ADR-0019 has turned rev11's top design
item into an explicit 51-slice migration, with 14 slices merged. §7 removes completed
work and reorders only the current tasks by dependency and actionability.**

Method:
- subsystem-level close reading, re-verified per claim on the live tree
- every finding carries a `file:line` reference

---

## 0. Summary

mutsu is a Rust implementation of a minimal Raku-compatible interpreter. The roast
whitelist stands at **1435 / 1464 (98.0%)**, up from rev10's 1433/1464 — and that near-flat
number is the point: **roast has been mined out since rev10** (PLAN §4), so a year's worth
of the project's velocity now shows up in places roast does not measure. Work since rev10
went almost entirely into (a) the raku-differential compatibility sweep
(≈100 `news/2026-08/` entries, one general fix each), (b) the batteries campaign — real
upstream modules vendored and run verbatim — and (c) two representation campaigns
(ADR-0015 native-backed storage, ADR-0016 span-based captures).

Overall assessment as of rev12:

- **The execution stack is complete and, since rev10, no longer has a known
  provenance-UB hole.** Single bytecode VM; cycle-collecting GC default-on (ADR-0003);
  8-byte NaN-boxed `Value` (ADR-0005); Cranelift JIT default-on (ADR-0004, closed).
  **ADR-0013 landed**: the payload of every `Gc<T>` now lives in an `UnsafeCell` inside
  `GcBox` (`gc/gc_ptr.rs:166`), so the ~59 deliberate aliased container writes derive a
  `&mut` with valid interior-mutable provenance (`gc_ptr.rs:774`) instead of casting a
  `*const`. The required Miri gate now covers the GC primitives and real VM call shapes,
  and the 62-site aliasing audit found no live violation. ADR-0013 is closed; the narrow
  cross-thread race remains explicitly deferred to frozen layer 3c (§2.1).
- **Two subsystems that did not exist in rev10 are now load-bearing**: a **bundled-module
  layer** (`modules/`, 22 vendored upstream dists + `vendor/zef`, gated by a release-time
  upstream-test-suite run) and a **user-facing MOP** (`EXPORTHOW::DECLARE` declarator
  registry + HOW-driven class registration, `runtime/metamodel.rs`). Both are described in
  §1.8/§1.9 — and both push load onto the one subsystem that is still tree-walking
  (declaration registration, §1.1).
- **The two representation campaigns are landed, with only optional or deliberate residue.**
  ADR-0016 (span-based captures + lazy `Match`) landed all five phases in four days at the
  end of July and corrected five real compatibility bugs on the way; its `view()`-forcing
  invariant is now observable as `match_materializations` under `MUTSU_VM_STATS=1`.
  ADR-0015 P3b has landed (`array[T]` is native-backed behind the `ArrayData::items`
  chokepoint, with honest
  VMArray `.REPR`/`.WHERE` at the native boundary); only optional P3c remains (§1.10).
- **Performance remains a surplus, not a problem.** At HEAD the bench CI ratio vs Rakudo is
  below 1.0 on every benchmark except `bench-ctor` (1.21), `bench-tak` (1.06) and
  interpreter-only `bench-fib` (0.98). Per PLAN's 2026-07-16 priority reset, perf is polish;
  this revision therefore ranks architectural items by **debt shape and dependency**, not by
  profile share.
- **Hygiene trends keep worsening**: files >500 lines 239→300→**302**, >1000
  62→80→**83**, `runtime/mod.rs` 2470→2495→**2700**, `unwrap/expect/panic!/
  unreachable!` 1789→1908→**1920**, and `.clone()` 9056→10192→**10283**. The
  `#[allow(` count improved slightly, 178→**176**, but it does not change the overall slope.

None of the remaining issues is of the "the basic design is broken" kind. The shape of the
debt has changed, though: the soundness and representation campaigns are closed, and as of
2026-08-17 **ADR-0019 (compiled declarations and unified method dispatch) is Accepted/
Implemented** — declaration registration compiles to typed plans, and one TypeId/MRO resolver
with a generation-checked O(1) cache serves every dispatch entry (§1.1, §3.3). Its own
completion-gate investigation (G3) also found and fixed two real perf regressions, unrelated to
the ADR's own mechanisms (§1.1). What is left of that campaign is deliberately non-gating
residue, now tracked as independent tickets rather than inside the ADR. The active center
moves to the exception type model and two missing policy/concurrency decisions.

Where to look first:
- §1: what architectural work remains (§1.8-§1.10 are new)
- §2: open correctness/soundness issues
- §7: prioritized roadmap — the main deliverable of this revision
- §8: ADR ledger review

---

## 1. Architecture

### 1.1 Declaration registration and dispatch entries — ADR-0019 closed (2026-08-17)

User-code bodies (subs, methods, blocks) execute exclusively as bytecode. **ADR-0019 is now
Accepted/Implemented**: declarations compile to immutable typed plans (`RegisterDecl`), user
and native methods share one registry-owned `MethodEntry` write side and one generation
invalidation boundary, and a single TypeId/MRO resolver (`resolve_sequence`,
`resolved_seq_cache`) serves every dispatch read entry with generation-checked O(1) cache
hits — see the ADR's completion-gate section for the full closure record (G1-G4).

What is left is deliberately non-gating residue, each tracked as its own ticket rather than
inside the ADR:

- **`legacy_body` adapters and the class/role registration walker**
  (`runtime/registration_class_decl.rs`, 2927 lines) still tree-walk MOP protocol calls
  (`declare_drive_how_protocol` driving `new_type`/`add_method`/`compose`) — declaration
  *registration*, not body execution, which is fully compiled. This is the largest remaining
  tree-walk surface and the one most likely to keep growing with future MOP features (§1.9).
- **Native/builtin method introspection fidelity** (F1/F2's remaining slice): user-method
  `.^methods`/`.^can`/method MRO views are now derived from the canonical table (no longer
  hand-maintained, closing most of §4 item 1 below); native method metadata (`.package` on
  multi dispatchers, exact per-method `.signature`, the `.^lookup` Sub-vs-Method-Instance
  representation mismatch) is not yet at full parity. Tracked in
  `todo/deep/adr0019-f1-f2-introspection-canonical-source.md`.
- **E2's exact-handler-ID catalog** (giving every native entry a static type×method row) is
  open cleanup, no longer gating dispatch correctness — `native_call_unmodeled` is a
  monitoring signal, not a precondition. Tracked in `todo/deep/adr0019-e2-e4-resolver-core.md`.
- **D2c-5** (collapsing three near-duplicated default-evaluation env-setup shapes) is optional
  de-duplication with no correctness impact. Tracked in
  `todo/tickets/adr0019-d2c5-collapse-default-eval-env-setup.md`.
- **Module-sub OTF compile gate** (`def_is_otf_compilable_module_single`,
  `vm/vm_call_func_ops.rs:1991`): unchanged since rev10, outside ADR-0019's scope. The
  residual exclusions are mechanism-level — `state`, sigilless `\x` params,
  `is encoded(...)`, `start` — each with a documented blocker (PLAN §3).

### 1.2 Closure upvalues — indexed reads plus capture cells landed

`compute_upvalues` (`opcode.rs:3832`) promotes proven immutable scalar reads to indexed
`GetUpvalue`; escaping mutable lexicals use shared `ContainerRef` cells addressed by creator
slot. The env-writeback campaign removed the blanket that previously disabled this analysis
and removed whole-env closure fallback for ordinary lexicals. Reflective and dynamic names
still use the explicit env boundary; uncertain mutation captures a cell rather than a
snapshot.

### 1.3 Lexical-scope slots — precise per-slot synchronization complete

Shadow slots have been default-ON since 2026-07-12, the whole-`locals` per-block clones are
gone (rev10 slices 1-3), and `needs_env_sync` is now a genuine **per-slot** `Vec<bool>`
computed by `compute_needs_env_sync` (`opcode.rs:2534`): a slot is marked only if some op
reads or writes that name by name (`opcode.rs:2680-2690`), plus the closure-free-var fold.

`EnvConsumerSlots` now records the exact slots for `ForLoop`, `BlockScope`,
`BlockLocalScope`, `MakeGather`, and `WheneverScope`; their union drives synchronization
without filling the whole local vector. Block restore is slot-authoritative, and closure
capture cells preserve identity across calls and thread descendants. The former blanket and
`captures_env_by_name` metadata are gone from production paths. Name-keyed env access remains
only where reflection or dynamic lookup genuinely asks for names.

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
   which is a *different* claim. PLAN §B2b should be re-scoped, not simply left deferred.

### 1.10 Representation campaigns — complete, with optional residue

- **ADR-0016 (regex captures / `Match`) is complete.** All five phases landed between
  2026-07-28 and 2026-07-31: absolute positions, the `CapNode` / `RegexCaptures` split
  (immutable stored node vs. per-run accumulator — a deliberate split, not a dual model), a
  shared `MatchTarget` with span reads, the one-list-per-axis collapse, and a lazy
  `ValueRepr::Match(Gc<MatchNode>)`. Along the way it corrected five real compatibility bugs
  (the four subrule-boundary constructs and search-recovered offsets for repeated text).
  Residue is deliberate and small: `CodeBlockContext`'s text snapshot and eager `Match`
  construction in the reduce/failed-replay paths. Capture-name keys are interned `Symbol`s.
  **One standing constraint the whole codebase now inherits**: a `view()`-based
  "is it an X?" probe materializes a lazy `Match`, so variant probes on paths a `Match` can
  reach must be tag probes. `MUTSU_VM_STATS=1` now reports `match_materializations`, so an
  accidental forcing path is observable in regex/grammar diagnostics rather than prose-only.
- **ADR-0015 (native-backed container storage) is complete through P3b.** P0/P1/P2/P3a
  landed (CStruct bodies, native-backed `Buf`/`Blob`, native-backed `CArray[T]`), and P3b
  is now merged: all `ArrayData` element access goes through the accessor chokepoint and
  numeric `array[T]` storage is synchronized with the native payload at the VMArray/native
  boundary. Targeted array, CArray, shaped-array, and native-storage regressions pass, as
  do the full CI test, GC-stress, JIT-stress, Miri, and WASM jobs. Only optional P3c
  (reference-element `CArray`) remains.

Neither campaign is current roadmap work. ADR-0015 P3c is optional and should start only when
a real NativeCall consumer needs reference-element `CArray`; ADR-0016's remaining eager paths
are explicitly deferred, small-count compatibility carriers rather than a dual representation.

---

## 2. Correctness and soundness

### 2.1 GC-era aliased writes — ADR-0013 closed and gated

**Fixed since rev10.** `GcBox` now stores its payload as `value: UnsafeCell<T>`
(`gc/gc_ptr.rs:166`), and `Gc::as_ptr` projects through it with `UnsafeCell::raw_get`
(`gc_ptr.rs:198`). `gc_contents_mut` (`gc_ptr.rs:774`) therefore derives its `&mut` from a
pointer that carries interior-mutable provenance while shared `&` reads are live — the
Stacked/Tree-Borrows violation rev10 listed as roadmap item #2 is gone, at every call site
at once and with no `Value`-representation churn (ADR-0013 §7).

The verification gap is closed. The required `miri` CI job runs both primitive GC/container
tests and five interpreter-level soundness shapes; `src/gc/borrow_shapes.rs` pins the handle,
raw-pointer, and shared-read orderings that callers actually use. The audit covered 62 sites:
60 are ordinary in-process writes and the two NativeCall buffer paths are represented by a
Miri-testable equivalent shape. The stale `value/aliased_mut.rs` warning and the last
`arc_contents_mut` path are gone.

One limit remains **deferred by decision, not as an ADR-0013 task**: genuinely concurrent
structural mutation of the same node must stay behind synchronized shared-store lanes. That is
ADR-0001 layer 3c territory, frozen until a measured trigger; it does not keep ADR-0013 open.

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
  (`runtime/builtins_system.rs:9`). PLAN §6 measured the consequences (50 idle `cue(:every)`
  timers → 52 threads / +16.4 GB VmSize). The decision this needs — what `await` does to a
  pooled worker, given mutsu has no continuations — is still an **unwritten Proposed ADR**.

### 2.4 The env-writeback cluster — resolved by the fused campaign

The correctness cluster identified in rev11 was resolved as one mechanism change: locals are
now synchronized by slot and escaping captures share cells instead of being copied through a
name-keyed env. PR #5759 and its stacked fixes are merged with green CI.

The campaign closed the findings that reduced to it, including:

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
- PLAN §6's "a joined `start` block writes its stale captured env back over a variable
  declared after it", where PLAN itself already concludes the fix "belongs with the
  cell-based capture work, not a special case at the call sites".

The remaining `todo/` items in this area are follow-up observations or broader concurrency
work, not blockers for the slot-addressed design.

---

## 3. Duplicate implementations

### 3.1 Statement/expression dual compilation of control constructs

Unchanged: `compiler/helpers_do_expr.rs` (476 lines, 6 `compile_do_*` entry points)
duplicates `stmt.rs` logic for do/if/for/while/loop in expression position, including a
21-field `ForLoopSpec` construction maintained twice. Fix remains one value-returning pass.

### 3.2 Sub declaration registered twice

`SubDecl` both registers an AST body (`RegisterSub`) and compiles the body
(`compile_sub_body`). Collapses when §1.1's declaration registration is compiled.

### 3.3 Method dispatch: canonical write side and TypeId/MRO resolver both landed (ADR-0019 closed)

ADR-0019 Phase B gives built-in and user candidates one registry-owned `MethodEntry` write
side and one generation invalidation boundary. Phase E added the read side: `resolve_sequence`
builds the shape-independent ordered candidate universe (user candidates, accessor
arbitration, native catalog rows, proto slot) from the same TypeId/MRO chain calls use, and
`resolved_seq_cache` (keyed `(TypeId, Symbol, CallShape)`) makes a cache hit
generation-checked O(1) — bench-CI parity confirmed on cutover (2026-08-14). Entry points
still call into this resolver from multiple call sites rather than one funnel
(`call_method_with_values` in `runtime/methods_call_dispatch.rs:50`,
`dispatch_method_by_name_{1,2,3}` in `runtime/methods_dispatch_match.rs:14` and siblings,
`run_instance_method`/`run_instance_method_celled` in `runtime/class_dispatch.rs:52,90`,
`native_method_{0,1,2}arg` in `builtins/`), but all of them resolve through the same
candidate-sequence logic rather than independently re-walking the MRO. Same-name string
matches stay scattered — `"elems"` appears in **33 files** (rev10: 8+) — a cosmetic/lookup
surface issue, not a second dispatch mechanism. `runtime/methods_call_dispatch.rs` is now
3875 lines.

What remains is the E2 exact-handler-ID catalog (giving every native entry a static row
instead of the arity-cascade fallback) and deriving the last mile of introspection fidelity
(§4 item 1) — both open cleanup tracked as independent tickets
(`todo/deep/adr0019-e2-e4-resolver-core.md`,
`todo/deep/adr0019-f1-f2-introspection-canonical-source.md`), no longer gating dispatch
correctness or performance.

---

## 4. Hardcode / drift risks

No test-specific hardcoded outputs found (re-checked). Two derivation shortcuts remain:

1. **User-method `.^methods`/`.^can`/method MRO views are now derived** from the canonical
   `Registry::method_entries[(owner, name)].user_candidates` table (ADR-0019 F1/F2, closed for
   this half 2026-08-14) — no longer hand-maintained for user-declared types. **Native/builtin
   method metadata stays a hand-maintained shortcut**: `builtins/builtin_type_methods.rs` (960
   lines, rev10: 874) centralizes the native candidate-name universe, guarded by structural
   tests plus `t/can-methods-drift.t`; per-method fidelity gaps (native `.package` on multi
   dispatchers, synthesized-not-exact `.signature`, the `.^lookup` Sub-vs-Method-Instance
   mismatch) remain open in
   `todo/deep/adr0019-f1-f2-introspection-canonical-source.md`. The growth rate matters now
   that §1.9 lets user code introspect through the same surface.
2. **Parser grammar relaxations for roast** (minor, unchanged): `is List` type-ish traits,
   the Test::Assuming colonpair, and the `throws-like` trailing-`)` special form.

---

## 5. Value model, performance, robustness

- **State outside the value (unchanged)**: Failure handled/pending registries are
  `thread_local!` and lose registration across thread boundaries; pending DESTROY queues
  likewise. Seq consumed/cached/lazy state is O(n) linear scans of `OnceLock<Mutex<Vec<Weak>>>`
  statics. Fragile and slow; also the root of
  `todo/deep/deferred-seq-materialization-destroys-the-original.md`.
- **Env**: COW `Arc<FxHashMap<Symbol,Value>>` with a scoped parent-overlay chain capped at
  `MAX_OVERLAY_DEPTH=16` (`env.rs:318`). The structural remainder is §1.3's blanket.
- **`.clone()` ≈ 10283** (rev11: 10192, rev10: 9056): each is an 8-byte NaN-box copy plus a refcount for
  container tags, so the unit cost is low, but the growth is a code-shape signal, not just a
  perf one.
- **`unwrap`/`expect`/`panic!`/`unreachable!` ≈ 1920** and **`#[allow(` 176**.
  PLAN §8.3's "mutsu must never Rust-panic on any input" goal is in tension with a metric
  that has risen every revision.
- Allocation-failure aborts on user-sized allocations remain guarded via `try_reserve`.

---

## 6. Repository hygiene

- **500-line rule**: **83 files >1000 lines, 302 files >500** (rev11: 80 / 300;
  rev10: 62 / 239). Total `src/` is ~434k lines. Largest: `opcode.rs` 5349,
  `vm/vm_exec_dispatch.rs` 4710, `runtime/methods_call_dispatch.rs` 3886,
  `compiler/stmt.rs` 3848, `runtime/regex_parse_core.rs` 3698,
  `vm/vm_var_assign_index_named.rs` 3570, `parser/expr/postfix/loop_.rs` 3297,
  `runtime/registration_class_decl.rs` 2927. Giant dispatch matches remain intentional
  exceptions; the other seven are not. `runtime/mod.rs` is **2700** lines (rev7 1932 → rev8
  2118 → rev9 2309 → rev10 2470 → rev11 2495) — still growing after five reviews.
  The rule as written ("split immediately") is not being followed, so either the rule or the
  practice should change deliberately rather than by drift.
- The stale `value/aliased_mut.rs` unsoundness header was corrected with ADR-0013's closure.
  Comment references to the retired `MUTSU_SHADOW_SLOTS` opt-in gate remain cleanup targets.

---

## 7. Recommended roadmap (priority order)

Completed campaigns are omitted rather than retained as numbered rows. The ordering below is
derived from dependency, deferral cost, severity, and actionability — per PLAN's 2026-07-16
priority reset, performance is polish and is not used as a ranking criterion here.

The ranking rule, stated so it can be argued with:

1. **Design prerequisites before data sweeps or concurrency implementation.** Exception roles
   need a type-metadata decision; a worker pool needs an `await` decision; batteries policy
   needs a durable adoption boundary.
2. **Severity can override the queue only when the task is actionable.** A new crash artifact
   makes the Proc::Async SIGSEGV P0; without one, blind local loops have already been measured
   as unproductive.
3. **Make hygiene a completion gate on the subsystem being replaced.** File splitting without
   deleting a model is churn; the ADR-0019 registration walker
   (`runtime/registration_class_decl.rs`, §1.1) is the next concrete case once a future MOP
   feature forces it open.
4. Feature breadth with no downstream consumer remains last.

| # | Item | Kind | Why here |
|---|------|------|----------|
| 1 | **Exception role/type registration and error parity** (`todo/deep/exception-class-hierarchy-is-mostly-unregistered.md`) | correctness / type model | 124 core `X::` names cannot be constructed as types, and prefix-as-parent is semantically wrong because Rakudo expresses most membership through roles. Collect the role/MRO data now; align implementation with ADR-0019's TypeId/MRO resolver model rather than creating a second registry. |
| 2 | **Write the batteries adoption-policy ADR, then follow the Cro/mzef compatibility frontier** (§1.8) | policy / product architecture | The project's main goal depends on the costly-to-reverse rule “vendor upstream verbatim; grow mutsu; no new native providers,” but the decision and exceptions live only in `BATTERIES.md`/`CLAUDE.md`. Preserve that boundary first; then let real downstream failures choose interpreter work. |
| 3 | **Write the shared-worker-pool Proposed ADR** (`todo/deep/shared-worker-pool-adr.md`) | concurrency design | Thread-per-task at 19 sites reserves 256 MiB each. A bounded pool deadlocks nested blocking `await` without continuations, and idle workers must cooperate with GC stop-the-world. The next deliverable is the decision, not implementation. |
| 4 | **Crash and panic-zero response lane** (§2.3, PLAN §6) | conditional P0 robustness | A fresh Proc::Async crash report preempts the roadmap immediately; the single historical CI SIGSEGV is otherwise evidence-starved and did not reproduce in 22 local runs. Supply panic propagation, deterministic hangs, and parser panic-zero work remain actionable correctness slices, not part of the worker-pool design. |
| 5 | **Unify statement/expression compilation of control constructs** (§3.1) | design cleanup | The duplicated `do`/`if`/loop compilation is real but bounded and stable. Opcode leftovers remain measurement-gated, not bundled into this task. |
| 6 | **Pay hygiene debt through the work above** (§5, §6) | completion discipline | `runtime/mod.rs` reached 2700 lines and the >500/>1000 populations reached 302/83. `registration_class_decl.rs` (2927 lines, §1.1) is the next walker due for deletion once a MOP feature forces it open; touched oversized files should be split when ownership boundaries become clear. A standalone line-moving campaign is not the priority. |
| 7 | **RakuAST completion** (`todo/deep/rakuast-remaining.md`, ADR-0011 Phase 6) | demand-driven feature | No whitelisted roast file or bundled battery consumes the remaining forms or macros. Pick a slice only when a real downstream use case supplies acceptance tests. |

Explicitly **not** ranked as current architecture work: completed ADR-0013/0015-P3b/0016/0018
campaigns; optional ADR-0015 P3c; ADR-0016's deliberately deferred eager replay carriers;
roast whitelist chasing; and perf levers with no goal-item consumer. They become candidates
only when new evidence or a real downstream dependency changes that premise.

---

## 8. ADR ledger review (new in rev11; refreshed in rev12)

Reviewed all 19 ADRs against the tree. The decisions themselves hold up — no ADR was found
to be *wrong*. The systematic problem is that **an ADR's recorded status drifts from what
shipped**, because implementation progress is reported in `news/` and PLAN.md but not folded
back into the ADR that owns the decision. That defeats the ADR's stated purpose (preserving
the judgment context) for anyone who reads the ADR first.

The rev11 corrections remain valid; rev12 adds the closure/progress deltas below:

| ADR | Drift found | Action |
|---|---|---|
| 0001 GC strategy & phasing | Status still listed §4.2 (trigger) and §4.3 (A' scope) as open; §4.2 was decided by ADR-0003, and layers 3a/3b/4 have all shipped. Its standing guidance "do not start Track B standalone; it is fused with GC" was superseded by ADR-0013 §7, which fixed the same sites at the primitive. | Added §7 "Outcome (2026-08-02)"; status line updated to point at it. |
| 0007 trail matcher | Recorded its own implementation outcome, but its explicitly deferred "per-subrule ceremony" became ADR-0016 with no forward pointer. | Added the successor link and marked the P2-P3 phasing superseded. |
| 0011 RakuAST | Status read "Phase 1 implemented (PR #4679); Phases 2-6 pending" while Phases 2-5 had substantially landed across ~37 slices. | Status corrected; pointer to `todo/deep/rakuast-remaining.md` as the live gap list. |
| 0013 container interior mutability | Rev11 correctly recorded the primitive but still listed verification and documentation as open. | Required Miri gate, VM reach, 62-site aliasing audit, Mixin migration, and documentation correction all landed on 2026-08-03; ADR closed. |
| 0015 native-backed storage | The prior entry stopped at P3a and left the P3b completion unrecorded. | Recorded P3b (PR #5785) as landed, including the `ArrayData::items` chokepoint and native-boundary behavior; P3c remains optional. |
| 0016 span-based captures | Status and all five phases remain accurate; the standing `view()` invariant was prose-only in rev11. | `match_materializations` now exposes each first lazy-node force under `MUTSU_VM_STATS=1`; capture-name keys are also interned. Deliberate replay/snapshot residue remains deferred. |
| 0018 slot-addressed lexical capture | Added after rev11 to own the env-writeback/lexical-slot fused campaign. | Accepted and implemented; it replaces rev11's completed roadmap rows rather than remaining a current task. |
| 0019 compiled declarations and unified dispatch | Added after rev11 to own §1.1/§3.3/§4-1 as one phased migration. | **Accepted/Implemented (2026-08-17).** All four completion gates (G1-G4) closed; the ADR's own execution checklist remains the historical record. Deliberately non-gating residue (native-method introspection fidelity, the E2 handler-ID catalog, D2c-5) now lives in independent tickets, not the ADR. |
| 0003, 0004, 0005, 0006, 0009, 0010, 0012, 0014, 0017 | Accurate; 0004 and 0009 already carry closing addenda. | No change. |
| 0002 | Historical gate record; still accurate. | No change. |

Two **missing** ADRs are worth writing, and are listed here rather than drafted unilaterally:

1. **A shared worker pool** — PLAN §6 has specified its content in detail for over two weeks
   ("the central question is not pool sizing — it is what `await` does to a pooled worker"),
   and the decision is being made by default in the meantime (19 spawn sites, 256 MiB each).
2. **The batteries adoption policy** — "grow the interpreter until the real upstream module
   runs verbatim (rung 2); native provision (rung 3) is banned" is a load-bearing,
   costly-to-reverse decision recorded only in `BATTERIES.md` and CLAUDE.md as a user
   decision. Its rejected alternative (native reimplementation) and its named exceptions
   (NativeCall, JSON::Fast, Test) are exactly the "why, and what we rejected" an ADR exists
   to preserve. The companion measurement — "do not build an `nqp::` op layer" — belongs in
   the same record.

---

*Based on static close reading plus live verification against HEAD.*
*rev12 (2026-08-04): re-verified against `435de2d3e`; ADR-0013 closure, ADR-0016's
materialization counter, ADR-0018 completion, and ADR-0019's 14/51 progress recorded;
live hygiene metrics refreshed; §7 reduced to current work and reordered by dependency,
severity, and actionability.*
*2026-08-17 addendum (not a full rev13 re-verification): ADR-0019 reached Accepted/Implemented
(all four completion gates closed); §1.1, §3.3, §4 item 1, the §7 roadmap, and the §8 ADR-ledger
row were updated in place to describe the closed architecture and point at the tickets tracking
its non-gating residue. Other findings/metrics in this document were not re-verified as part of
this addendum.*
