# mutsu codebase analysis

This document is an **architecture and soundness review** of the mutsu codebase —
"how much of the design is in order, and what debt remains" — not a bug tracker.

First edition: 2026-06-03. Revision history through rev8 (2026-07-06) is in git; each
rev's resolved findings are archived in the news files
([news/2026-07.md](news/2026-07.md) "ANALYSIS.md rev9/rev10 — resolved-item archive"
holds both the pre-rev8 archive and everything closed since).
**rev10: 2026-07-19 — re-verified against HEAD (#4811) after 259 commits since rev9;
the `integration/` frontier is fully cleared, deep-recursion is fixed, a RakuAST model
layer (§1.7) landed, and hygiene metrics were re-measured. Resolved items moved to news.**

Method:
- subsystem-level close reading by survey agents
- per-claim verification on the live tree

Every finding carries a `file:line` reference. Reproducible defects were actually run.

---

## 0. Summary

mutsu is a Rust implementation of a minimal Raku-compatible interpreter. The roast
whitelist stands at **1433 / 1464 (97.9%)**, up from rev9's 1391/1463. The former
frontier — `integration/` (real Raku programs) — is **fully whitelisted (0 remaining)
as of 2026-07-17**; roast is no longer the productive axis (BLOCKERS.md: "No cluster
remains"). Active work has moved to the RakuAST model layer (§1.7) and PLAN.md §1/§5/§6.
No test-specific hardcoded outputs were found.

Overall assessment as of rev10:

- **The execution stack is complete in outline**: a single bytecode VM (the tree-walk
  interpreter, dual variable store, and tree-walk method bodies are all long gone), a
  **cycle-collecting GC, default-on** (ADR-0003), an **8-byte NaN-boxed `Value`**
  (layer 3b, #4469), and a **Cranelift JIT, default-on** (ADR-0004 J5, #4482). The
  remaining tree-walk is declaration registration and dispatch-resolver entries, not
  body execution (§1.1). A **RakuAST model layer** (ADR-0011, §1.7) landed on top of
  this stack since rev9: `Q[…].AST` reads out a RakuAST tree and `EVAL($tree)` lowers it
  back through the *same* compiler — no second execution engine.
- **Performance vs raku flipped from deficit to surplus.** Bench CI (2026-07-14, main
  `827fdb0e`, vs Rakudo v2022.12 on the same runner): fib 0.59x (JIT 0.48x),
  method-call 0.89x, bench-class 0.84x, bench-fib 1.15x (JIT 0.92x), bench-tak+jit
  1.02x; most other benches ≤0.5x. Every rev8 gap (bench-fib 3.2x, method-call 2.7x,
  bench-class 2.3x) is closed. Caveat: the runner's Rakudo is v2022.12.
- **Quality gates are thick**: CI = `test` (clippy -D warnings, cargo test, t/ TAP,
  roast) + blocking `gc-stress` + blocking `jit-stress` + `wasm-e2e`; bench CI records
  history on every main push. Size guards pin `Value` ≤8B and `OpCode` ≤48B; drift
  tests (`t/can-methods-drift.t`) catch structural regressions.
- **Soundness debt is localized**: raw-pointer aliased writes survive behind
  `gc::gc_contents_mut` / `Gc::{get,make}_mut` (~59 call sites across 25 files, up from
  rev9's ~53/21, §2.1); the GC's buffered-clone/uniqueness invariant is still mostly
  prose (§2.1). The rev9 "deep recursion aborts the process" hole is **fixed**: the
  interpreter now runs on a 256 MB-stack thread (`main.rs:99`) and the three pure-recursion
  integration tests pass; only the memory-bound `deep-recursion-initing-native-array.t`
  still aborts, and that is a typed-shaped-array coercion issue, not Rust-stack recursion
  (§2.3).
- **Negative hygiene trends continue, faster than rev9's slope** (GC/NaN-box/JIT/RakuAST
  churn): `unwrap/expect/panic!/unreachable!` 1759→**1789**, `#[allow(` 165→**170**,
  files >500 lines 210→**239** (>1000: 57→**62**), `runtime/mod.rs` 2309→**2470** lines
  (the rev9 "needs another facade slim-down" flag is now overdue). `.clone()` 8947→**9056**;
  each is an 8-byte NaN-box copy, but useless-clone pruning (3b-2) remains open (§5).

None of the remaining issues is of the "the basic design is broken" kind; they are
design/soundness/maintainability debt.

Where to look first:
- §1: what architectural work remains
- §2: open correctness/soundness issues
- §4: hardcode and drift risks
- §7: prioritized roadmap

---

## 1. Architecture

### 1.1 Remaining tree-walk — declaration registration and dispatch entries only

User-code bodies (subs, methods, blocks) execute exclusively as bytecode. What still
walks the AST (re-verified 2026-07-15):

- **Declaration registration**: `register_class_decl`
  (`runtime/registration_class_decl.rs:210`), `register_sub_decl`
  (`runtime/registration_sub.rs:407`), `register_role_decl`
  (`runtime/registration_role.rs:210`) run off `Register*` opcodes
  (`opcode.rs:1328-1336`, dispatched at `vm_exec_dispatch.rs:4137-4209`). Class system,
  MRO, and role composition are uncompiled — but this is registration, not body
  execution.
- **Dispatch resolver entries**: multi/submethod and `samewith`/`nextsame` enter
  through `run_instance_method` (`runtime/class_dispatch.rs:11`); bodies are compiled.
  A **sound multi-method resolution cache + `fast_method_cache`**
  (`vm/vm_call_method_compiled_cache.rs:111-154`, invalidated on new declarations) now
  amortizes the resolver, so what remains is entry-point consolidation (§3.3), not
  caching.
- **Module-sub OTF compile gate** (`def_is_otf_compilable_module_single`,
  `vm/vm_call_func_ops.rs:1852`): after the relaxation campaign
  (#4427→#4429→#4431→#4437) the residual exclusions are mechanism-level, each with a
  known blocker: `state` (would sever the shared state cell, `:1879-1883`), sigilless
  `\x` params (caller-alias writeback across EVAL needs a #4091-style compile-time
  caller slot, `:1793-1800`), `is encoded(...)` (NativeCall marshalling, `:1878`), and
  `start` (recursive-capture clobber; an AST predicate cannot see recursion,
  `:1944-1962`).

### 1.2 Closure upvalues — Phase 1 only, unchanged since rev5

`compute_upvalues` is still the conservative Phase 1 (`opcode.rs:3021-3068`): only
pure scalar reads of already-shared `ContainerRef` cells are promoted to indexed
`GetUpvalue` (`opcode.rs:259`); writes, RW ops, `@`/`%`/`&` sigils are excluded, and
the pass bails entirely under `captures_env_by_name` (`opcode.rs:3039`). Out-of-range
indices fall back to live env-by-name reads (`vm_closure_dispatch.rs:508`) — always
sound. Phase 2 (value-izing read-only constant captures, write paths, env-copy
removal) stays blocked on general captured-lexical cell-ification; the rev5 soundness
walls still stand: value snapshots are unsound (compile-time mutation analysis misses
role/class-method writes and rw-arg sinks — the `S12-construction/roles-6e.t` flake),
and blanket cell-ification of read-only captures breaks ContainerRef-blind paths and
deadlocks per-iteration × cross-thread (#2749).

### 1.3 Lexical-scope slots — shadow slots default ON; the clone endgame remains

Big status change since rev8: the §1.4 campaign
([docs/lexical-scope-slot-campaign.md](docs/lexical-scope-slot-campaign.md)) landed
all slot bakes S1–S17, and **shadow slots are default ON since 2026-07-12** — a full
toggle-ON whitelist survey (1379 files) found zero genuine regressions.
`shadow_slots_active()` is opt-out via `MUTSU_NO_SHADOW_SLOTS`
(`compiler/mod.rs:27-31`); `declare_local` mints fresh/ancestor-shadow slots by
default and keeps the legacy shared-slot branch for the escape hatch
(`compiler/mod.rs:545-601`).

What remains is the endgame that motivated the campaign:

- **By-name slot resolution is still load-bearing**: `resolve_local_slot` prefers the
  baked slot but falls back to `find_local_slot(code, name)`
  (`vm_env_helpers.rs:1200-1214`), and several writeback callers still pass `None`
  (e.g. `vm_for_loop_dispatch.rs:307-310`, `vm_loop_writeback.rs:23-24`).
- **`BlockScope` still clones/restores the whole `locals` vec**
  (`vm_misc_scope.rs:178,184,376`) — the cost the campaign exists to delete. A
  clone-removal probe was attempted and reverted on 2026-07-12
  (lexical-scope-slot-campaign.md:396).
- **The `needs_env_sync` blanket**: `captures_env_by_name` returns true if the frame
  contains any `ForLoop`/`BlockScope`/`MakeGather`/`WheneverScope`, mirroring *every*
  local to env on each store. Removing it precisely requires un-name-keying block
  restore, loop shadow save/restore, and closure capture **simultaneously** — one
  fused campaign with this section and §1.2 (PLAN §5 item 0; single-mechanism attempts
  are known to break five writeback mechanisms).

### 1.4 Optimizer and opcode set — baseline passes exist now; remainder is measured

rev8's "no constant folding / DCE / peephole" is obsolete: ADR-0006 landed literal
constant folding (#4485), constant-pool dedup (#4486), `constant` inlining +
constant-condition DCE (#4487), declaration-marker fusion (#4488), and replaced the
`SetSourceLine` opcode with a static ip→line table (#4489). The instruction set keeps
its 2026-07-06 audit verdict "shape is sound"
([docs/opcode-design-review.md](docs/opcode-design-review.md)); `OpCode` stays ≤48B
(`opcode.rs:1507-1508`).

Remaining, all measurement-gated:

- The **"opcode count ≠ time" lesson** (#4489): `SetSourceLine` was 21% of executed
  opcodes on fib but the cheapest possible op — deleting it bought -3.4% instructions,
  and one refresh-everywhere variant was a +7.8% regression. The surviving
  administrative ops (`SetVarDynamic` ~500k, `CheckReadOnly` ~100k) must clear a
  retired-instructions (`instructions:u`, core-pinned) gate before anyone touches them
  (ADR-0006 measurement protocol).
- Inline `Option<String>` payloads (`Last`/`Next`/`Redo`/loop labels,
  `SmartMatchExpr.lhs_var`) → constant-pool `Option<u32>`; per-instruction constant
  costs (`current_code` raw-pointer store, `trace_log!` check); `Jump(i32)` carrying
  an absolute index; histogram-driven consolidation of syntax-shaped specialized ops
  (`ContainerEq`×4, `IndexAssign*`×6).

### 1.5 JIT (new since rev8) — default on; Tier B variable ops are the open slice

A Cranelift JIT (ADR-0004) landed as J1–J5 and is **default-on since 2026-07-13**
(#4482; `src/vm/vm_jit*.rs`, 7 files ≈2000 lines; blocking `jit-stress` CI job).
Tier A translates hot opcode chunks into helper-call sequences; Tier B inlines
NaN-box tag-dispatched Int/Num arithmetic (including div/mod, #4503) into native code.

Two open points:

- **J4d (Tier B variable-op inlining)** — GetLocal/SetLocal fast paths, JIT→JIT
  per-call bind inlining, args-Vec allocation removal. This is the body of the
  "int loops 5-10x" ADR-0004 gate.
- **The profile says the JIT is not yet the bottleneck's owner**: on fib (release,
  JIT on) native code runs only ~5.7% of cycles; allocation (~11.8%), hashing/env
  table cloning (~12%), and the call path (~10.9%) dominate
  (PLAN §5 table, 2026-07-13). The next perf lever is therefore §1.3's env work, not
  more JIT coverage.

### 1.6 Parser (good) and pseudo-slangs — unchanged

Hand-written scannerless recursive descent; precedence handling is textbook-clean
(`parser/expr/precedence.rs`); `memo.rs` gives packrat-style backtracking relief.
There is no true slang stack: Regex bodies are scanned as raw text at parse time
(`parser/primary/regex/scan.rs`) and structurally parsed at runtime; Pod is skipped
by the parser and rebuilt from raw source at runtime (`runtime/io_pod_blocks.rs:4`).
Real user-defined grammar/token/rule slang switching remains future work.

### 1.7 RakuAST model layer (new since rev9) — read+write, no second engine

A RakuAST model layer (ADR-0011) landed as ~37 slices across Phases 4–5 (PRs
#4729–#4804) and is the single largest new subsystem since rev9 (`src/rakuast/`,
13 files ≈3200 lines). It has two directions:

- **Read** (`Q[…].AST`): the internal `Stmt`/`Expr` tree is converted to a RakuAST node
  tree whose `.gist` is byte-identical to Rakudo's where applicable.
- **Write** (`EVAL($rakuast)`): `lower(RakuAstNode) -> Stmt/Expr` re-enters the **existing
  compiler**, so there is no separate interpreter — the model layer is a translation
  surface, not a new execution engine.

Coverage is broad: literals (interpolation, array/hash literals, pairs, `*`, bareword
types), all common operators + the ternary, the full control-flow set
(`if`/`elsif`/`unless`/`while`/`until`/`repeat`/`for`/C-style `loop`/`given`-`when`/`do`/
`try`/`gather`), sub declarations with typed/default/named/slurpy parameters, control-flow
calls (`return`/`last`/`next`/`die`/`fail`/`take`), and first-class closures. Each slice is
pinned by a dual-oracle test (`t/rakuast-*.t`) that passes under both mutsu and raku.
Remaining: Phase 6 (macros / `quasi`), the type registry beyond the covered node set, and
the two read-only nodes whose raw round-trip diverges from raku (hash literals, the
Phase-2 read cluster). This work is orthogonal to the roast frontier (0 whitelisted roast
files use RakuAST) and drives PLAN §RakuAST, not roast.

---

## 2. Correctness and soundness

### 2.1 GC-era raw-pointer writes — reduced, not eliminated

The GC itself (layer 3a) is done and default-on; the STW handshake races and worker
TLS teardown race found in gc-stress were root-caused and fixed (#4410), and the
2026-07-11 audit sweep (#4414) closed rev8's auditability concerns (stale "default
off" headers, module-wide `#![allow(dead_code)]`, undocumented `make_mut` ordering).
What remains:

- **The aliased-write mechanism survives.** `gc::gc_contents_mut`
  (`gc/gc_ptr.rs:660-663`) and `Gc::get_mut`/`make_mut` (`gc_ptr.rs:319-336`/
  `456-502`) still write through `Arc::as_ptr as *mut` — a provenance violation plus
  a data race whenever the target is genuinely shared. Track B T4/T5/T6
  (#4416/#4417/#4418) cut the biggest cluster (`vm_var_assign_index_named.rs` 18→6)
  but **~59 production call sites remain across 25 files** (rev9: ~53/21 — the count has
  drifted *up*, not down, as new mutation paths reuse the primitive). Full removal awaits
  the deferred `CellValue`
  work ([docs/gc-post-3a-roadmap.md](docs/gc-post-3a-roadmap.md) §2). The audited old
  primitive `arc_contents_mut` is kept dead (`value/aliased_mut.rs:69`), along with a
  dead duplicate `gc_contents_mut` shim (`aliased_mut.rs:84`).
- **The uniqueness-vs-buffered-clone invariant is still mostly prose.**
  `get_mut`/`make_mut` treat `header.strong == 1` as unique while the GC candidate
  buffer holds uncounted clones (now `Weak`, #4420); correctness rests on buffered
  entries only being dereferenced at collect safepoints and `Gc::drop` early-returning
  during collection. #4414 added debug asserts (`gc_ptr.rs:458-461,492`) and a
  20-line soundness argument for the all-`Relaxed` strong-count re-link in `make_mut`
  (`gc_ptr.rs:468-483`: the collector reads counts only under the STW SeqCst
  handshake), but machine checking beyond that exists only under `MUTSU_GC_VERIFY`.

### 2.2 `RuntimeError` as a control channel — cheap now, still cohabiting

`RuntimeError` (`value/error.rs:108`) still carries `return`/`last`/`next`/`take`/
`emit` through `Result::Err`. The size problem is gone (control bools folded into
`enum Control`; cold routing fields boxed behind `cold: Option<Box<RuntimeErrorCold>>`,
`error.rs:85,131`; `result_large_err` allows 0), and NaN-boxing shrank the dominant
`return_value: Option<Value>` field to 8B. Channel separation itself remains unstarted
and low priority — note there is no `size_of` guard test for `RuntimeError` (only
`Value` and `OpCode` are pinned), so a regression here would be silent.

### 2.3 Process-level robustness holes

- **Deep Raku recursion — mostly fixed (rev10).** The rev9 "Rust stack overflow aborts
  the process" hole is closed: the interpreter runs on a dedicated **256 MB-stack thread**
  (`main.rs:99`), and the three pure-recursion integration tests (`99problems-41-to-50.t`,
  `99problems-51-to-60.t`, `man-or-boy.t`) now pass — the whole `integration/` frontier is
  whitelisted (§0). The one residual abort is `deep-recursion-initing-native-array.t`
  (a 100M-element `my int @a[N;N]`), which core-dumps on **memory**, not Rust-stack depth;
  it is the shaped-native typed-array coercion issue tracked in BLOCKERS (S02 array-shapes
  T36-38), not a recursion-mechanism gap. A larger fixed stack is a blunt instrument, not
  true heap frames / stack growth, so pathologically deep recursion can still overflow —
  but it is no longer a frontier blocker.
- **Recursive start/await hang** (2026-07-11, deterministic): after one recursive
  `start`-chain sub completes, a second two-branch recursive `start` sub hangs —
  suspected thread-pool worker not released (PLAN §6).
- **Supply detached-worker panics are swallowed**; QUIT propagation is unimplemented.

---

## 3. Duplicate implementations

### 3.1 Statement/expression dual compilation of control constructs

Each control construct still has separate stmt-form and expr-form compilers:
`compile_do_block_expr`/`_scoped`, `compile_do_if_expr_bound`, `compile_do_for_expr`,
`compile_do_while_expr`, `compile_do_loop_expr` (`compiler/helpers_do_expr.rs:4,63,
103,160,323,350`) duplicate `stmt.rs` logic with subtle differences — e.g. the
21-field `ForLoopSpec` construction is maintained twice. Fix: one value-returning
pass.

### 3.2 Sub declaration registered twice

`SubDecl` both registers an AST body (`RegisterSub`) **and** compiles the body
(`compile_sub_body`) — the registration side is load-bearing for §1.1's declaration
registration. Collapses when declaration registration is compiled.

### 3.3 Method dispatch: many entry points, scattered name matching

Entry points remain unconsolidated: `call_method_with_values`
(`runtime/methods_call_dispatch.rs:15`), `dispatch_method_by_name_{1,2,3}`
(`runtime/methods_dispatch_match.rs:14`, `match2.rs:8`, `match3.rs:9`),
`run_instance_method` (`runtime/class_dispatch.rs:11`), `native_method_{0,1,2}arg`
(`builtins/`). Same-name string matches stay scattered (`"elems"` in 8+ files).
Resolution *caching* landed (§1.1); the consolidation into a single type×method
dispatch table has not, and it is also what would let §4-1's method tables be derived
instead of maintained.

---

## 4. Hardcode / drift risks

No test-specific hardcoded outputs found (re-checked at rev8; nothing new since).
Two known derivation shortcuts remain:

1. **`.^methods`/`.^can`/`.^mro` tables are hand-maintained** — but centralized in the
   single canonical module `builtins/builtin_type_methods.rs` (874 lines), guarded by
   structural tests and `t/can-methods-drift.t`. True derivation from dispatch awaits
   §3.3's unified table.
2. **Parser grammar relaxations for roast** (minor, present):
   `parser/stmt/decl/helpers.rs:30` (`is List` type-ish traits),
   `parser/primary/misc/colonpair.rs:73` (Test::Assuming),
   `parser/stmt/stmtlist.rs:337,394` + `parser/stmt/control.rs:72` (`throws-like`
   trailing-`)` special form).

---

## 5. Value model, performance, robustness

- **State outside the value (unchanged)**: Failure handled/pending registries are
  `thread_local!` (`value/mod.rs:361,398`) and lose registration across thread
  boundaries; pending DESTROY queues likewise (fire-trigger is GC `Trace::finalize`,
  double-fire guarded). Seq consumed/cached/lazy state is O(n) linear scans of
  `OnceLock<Mutex<Vec<Weak>>>` statics (`value/mod.rs:18,25,40`) plus
  `CONSUMED_LAZYLISTS` (`value/mod.rs:76`). Fragile and slow.
- **Env**: COW `Arc<FxHashMap<Symbol,Value>>` with a scoped parent-overlay chain
  capped at `MAX_OVERLAY_DEPTH=16` (`env.rs:303`). The 2026-07-12/14 perf slices
  (#4463, #4492–#4495, #4506–#4508) removed the worst interning/SipHash/COW churn on
  the declaration path; the structural remainder is §1.3's `needs_env_sync` blanket
  and `BlockScope` clone.
- **`.clone()` ≈ 9056** (rev9: 8947): each is now an 8-byte NaN-box copy
  (+ refcount for container tags), so the unit cost collapsed, but useless-clone
  pruning (3b-2 "traffic pruning", gc-post-3a-roadmap §3.3) is still open,
  profile-driven.
- **`unwrap`/`expect`/`panic!`/`unreachable!` ≈ 1789 (+30)** and
  **`#[allow(` 170 (+5)**: mostly invariant asserts/tests, but the upward trend
  continues and deserves a periodic sweep. (Module-wide `#![allow]`s are gone.)
- Allocation-failure aborts on user-sized allocations remain guarded via
  `try_reserve`; remaining aborts are true OOM.

---

## 6. Repository hygiene

- **500-line rule**: **62 files >1000 lines, 239 files >500** (rev9: 57 / 210).
  Largest: `vm/vm_exec_dispatch.rs` 4490, `opcode.rs` 4237,
  `runtime/methods_call_dispatch.rs` 3569, `compiler/stmt.rs` 3424,
  `vm/vm_var_assign_index_named.rs` 3124 (a new top-5 entry, displacing
  `runtime/regex_parse_core.rs`). Giant dispatch matches stay intentional exceptions, but
  `runtime/mod.rs` re-grew to **2470** lines (rev7: 1932 → rev8: 2118 → rev9: 2309) — the
  facade slim-down flagged three revs running is now overdue.
- Stale comment references to the old `MUTSU_SHADOW_SLOTS` opt-in gate survive in
  `opcode.rs:1441,1671,1692` and `vm_register_ops.rs:81,547` — the gate is now the
  opt-out `MUTSU_NO_SHADOW_SLOTS` (§1.3); worth a doc sweep.

---

## 7. Recommended roadmap (priority order)

rev9's #1 — the deep-recursion process abort — is done (256 MB-stack thread; the
`integration/` frontier cleared, §2.3). Everything that topped rev8's table (Track B tail
T4-T6, the GC auditability sweep, NaN-boxing) was already done. Remaining, aligned with
PLAN.md:

| # | Item | Kind | Impact |
|---|------|------|--------|
| 1 | **Lexical-slot endgame as one fused campaign**: `needs_env_sync` blanket removal → by-name fallback retirement → `BlockScope` full-clone removal (§1.3; also the top remaining perf lever per the fib profile) | correctness + perf | large |
| 2 | **JIT J4d** — Tier B variable-op inlining (the ADR-0004 "int loops 5-10x" gate, §1.5) | perf | medium-large |
| 3 | **`gc_contents_mut` residue inventory** (~59 sites, trending *up*; route through cells or `make_mut`-style COW, §2.1) + buffered-clone invariant hardening | soundness (UB) | medium |
| 4 | **RakuAST completion** (Phase 6 macros/`quasi`, type-registry breadth beyond the covered node set, §1.7) — the largest active campaign | feature | medium |
| 5 | Scouted perf slices: `compiled_fns` SipHash removal, callsite-line marker removal (PLAN §5 items 1-2) | perf | medium |
| 6 | Declaration registration → bytecode; dispatch entry consolidation into a type×method table (also retires §4-1's hand tables) (§1.1/§3.3) | design | medium |
| 7 | Opcode remainder, measurement-gated (§1.4); recursive start/await hang, shaped-native array coercion (§2.3) | perf / robustness | low-medium |
| 8 | Control-channel separation (§2.2); Supply panic → QUIT (§2.3) | design / robustness | low |
| 9 | Hygiene: `runtime/mod.rs` re-slim (2470 lines, overdue), file splits (239 >500), clone/unwrap trend sweep, stale gate-name comments (§5/§6) | hygiene | low-medium |

---

*Based on static close reading plus live reproduction.*
*rev10 (2026-07-19): re-verified against HEAD (#4811) after 259 commits; integration
frontier cleared, deep-recursion fixed, RakuAST layer (§1.7) added, hygiene re-measured;
resolved items archived to [news/2026-07.md](news/2026-07.md).*
