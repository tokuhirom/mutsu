# Performance Tracking

## Benchmark Suite

Benchmarks live in `benchmarks/`. Run all with:

```bash
cargo build --release
./benchmarks/run-all.sh
```

## Current Status (bench CI, main commit `c8955d2e`, 2026-07-13)

> Source of truth: the **bench CI history** (`bench-history.tsv` on the
> `bench-data` branch; median of 7 runs per main push, raku measured on the
> same runner in the same job so the ratio normalizes runner speed). The
> `+jit` column is the JIT-on series (recorded since #4480; the **default**
> configuration since J5 — the plain series pins `MUTSU_JIT=off`). Local
> numbers drift with thermals/binary layout — use them only for in-flight
> A/B decisions, and check for stray `mutsu` processes before measuring.

| Benchmark | ratio (interp) | ratio (JIT on) | notes |
|-----------|----------------|----------------|-------|
| fib(25) | 0.82x | **0.65x** | recursive function calls (was 4.8x before the ?LINE/overlay fix) |
| bench-fib | 1.78x | **1.39x** | fib with type constraint (`Int $n --> Int`); was 10.5x pre-July |
| int-arith | **0.47x** | **0.43x** | `for ^100000 { $sum += $_ * 3 + 1 }`; interpreter alone gained −27% in J4c |
| string-concat | 0.14x | 0.13x | `$s ~= 'x'` × 10000 |
| hash-access | 0.16x | 0.16x | 10K hash inserts + value iteration |
| method-call | 1.19x | 1.16x | Point.distance-to × 10000 (was 2.7x on 2026-05) |
| array-ops | 0.37x | 0.37x | grep+map on 1000-elem array × 100 |
| bench-array | 0.19x | 0.19x | push+map+grep+sort+reverse on 10K |
| bench-hash | 0.16x | 0.16x | 10K insert+lookup+delete+keys+values |
| bench-class | 1.02x | 1.00x | class instantiation + method calls + inheritance |
| bench-startup | **0.06x** | **0.06x** | JIT on does NOT hurt startup (hot-only compilation) |
| bench-string | 0.35x | 0.35x | string operations |

### Summary

- **Faster than raku on 10/12** with the interpreter alone; JIT on pushes the
  call-heavy pair further (fib 0.65x, bench-fib 1.39x) and regresses nothing —
  benchmarks without JIT-eligible hot code measure identical on/off.
- **Above parity (2/12)**: bench-fib 1.39x (JIT on; per-call binding/dispatch
  is the residual — J4d territory) and method-call ~1.16x (near-parity).
- CI runners are slower and noisier than the local box, so ratios are the
  comparable quantity, not absolute times.

### ✔ fib / bench-fib absolute regression — RESOLVED (2026-07-12)

The ~2.3x absolute slowdown vs 2026-05 (fib 0.37s → 0.85s) was **not GC**:
`MUTSU_GC=off` measured identical to GC-on (the #4420 buffered-flag fast path
already reduced per-drop cost to one relaxed load; fib does 1 collection, 86µs
total pause). A `perf record` profile showed >40% of fib wall time in
`Env::flattened` HashMap deep clones (+ ~28% dropping the clones): every
recursive call stacked a scoped-overlay tier, and each call past
`MAX_OVERLAY_DEPTH` (16) re-flattened the whole chain. The overlays were only
non-empty because the per-statement `SetSourceLine` opcode inserted `?LINE`
into the env (728k inserts on fib(25), each potentially forking the CoW map).

Fix (two parts): (1) `?LINE` moved from the env to an `Interpreter` field
(`cur_source_line`), saved/restored at frame boundaries (`VmCallFrame` /
`CallFrameEntry`); (2) `Env::scoped_child` reuses the parent chain instead of
stacking when the parent tier's overlay is empty (no writes, no tombstones), so
pure recursion runs at constant chain depth and never flattens. Container-`Gc`
drop churn on fib(25) went from 17.5M to 14; fib beat the 2026-05 record by
2.4x. Pin: `t/source-line-deep-recursion.t`,
`env::tests::empty_tiers_are_reused_not_stacked`.

## Architecture Overview

```
Source → Parser → AST → Compiler → Bytecode → VM (run() → opcode dispatch)
```

The tree-walking interpreter has been eliminated (CP-1/CP-2/CP-3, #3075–#3104).
There is now a **single struct `Interpreter`** which *is* the bytecode VM — the
former `struct VM` was melted into it. All execution flows through compiled
bytecode.

### Bytecode execution (the only engine)
- Compiled functions/methods execute via `run()` → opcode dispatch
- Local variables use indexed slots (`self.locals[i]`), not HashMap
- Parameter binding via `call_compiled_function_positional_light` (no env clone)

### Remaining tree-walk carriers
- `eval_block_value()` still exists as a method, but only as a *carrier* for
  cases that re-enter source evaluation (e.g. `EVAL`, embedded `{...}` blocks in
  regexes), delegating to the same native implementations — it is no longer a
  separate execution engine.

### Single store (`locals` authoritative, `env` derived)

- The VM keeps indexed `locals` slots as the **single authoritative store**; the
  `env: HashMap<Symbol, Value>` is a *derived view* needed for closure captures,
  dynamic variables (`$*VAR`), and package-scoped resolution. Coherence is
  maintained by **cell-boxing** (shared `ContainerRef` cells for
  captured-and-mutated lexicals), made permanent in #3450.
- The old `env_dirty` / reverse-`sync_locals_from_env` dual-store machinery was
  **physically removed in #3455** — it is no longer a source of cost.

## Remaining Bottleneck: Method Dispatch

> **Update 2026-06-28 (re-measured, machine-quiet, release build).** The cost
> model below replaces the stale "env deep clone ~9μs/call" analysis: that
> bottleneck no longer exists. With the single-store campaign done, a
> `MUTSU_VM_STATS=1` run of the method-call benchmark shows
> `env_deep_copies=1` and `clone_env=10000` (O(1) Arc bumps) over the *entire*
> 10000-iteration run — i.e. **there is no per-call env deep clone**. The
> `vm_call_fast` path's `saved_env.ptr_eq(self.env())` check already skips the
> merge when the body did not mutate env, so `Arc::make_mut` essentially never
> fires.

**Actual per-call costs (release perf profile, method-call / bench-class):**

| Cost | Share | Notes |
|------|-------|-------|
| ~~per-call body fingerprint~~ | ~~6–15%~~ | **fixed in #3853** — `push_method_dispatch_frame` `format!("{:?}", body)`-hashed the whole method-body AST on every call; now short-circuited for the single-candidate case + allocation-free fingerprint. |
| `Symbol::intern` + `memcmp` | ~14% | class/method names are `resolve()`d to `String` then re-`intern()`ed per call (a `Symbol → String → Symbol` round-trip); each intern takes a global `RwLock` read + `FxHashMap` lookup. **Next target.** |
| ~~`Value::clone` / `drop_in_place::<Value>` / `Vec::clone`~~ | ~~~50% on bench-class~~ | **fixed 2026-07-12** — a 2026-07-12 perf profile showed the bench-class share was NOT dispatch/attribute churn but the for-loop topic writeback: `loop_var_unchanged` had no `Instance` arm, so a read-only `for @instances { .method }` rebuilt (cloned) the whole backing array every iteration (O(n²)). Adding Instance/Package/Enum/Rat/Nil arms cut bench-class 1.06s → 0.38s (2.8x, now 0.58x vs raku). Residual `Value` clone/drop in dispatch is still addressed long-term by NaN-boxing (Lever 2 / ADR-0001 layer 3b). |

**Why env clone is no longer the issue**: the single-store work made `locals`
authoritative and `env` a cheap derived view; method calls no longer snapshot
the whole env.

### Approaches investigated and rejected (historical — pre single-store)

1. **Two-level Env (base + delta)**: Branch overhead on every `get()` offset clone savings. Net regression on non-method benchmarks.
2. **Fresh env (bypass clone)**: Broke closure captures, dynamic variable visibility, package-scoped sub resolution.
3. **Scope chain (`parent: Option<Arc<EnvParent>>`)**: Extra indirection in every get()/insert() caused 10-24% regression.
4. **Env compaction (remove locals-only entries before clone)**: Broke `let`/`temp` restoration, BlockScope, line tracking.

## Improvement Plan

### Method-dispatch hot-path cleanups — target: method-call < 2x

> The previously-listed "closure captures as indexed slots / eliminate env
> clone" item is **obsolete**: the single-store campaign already removed the
> per-call env deep clone (see the bottleneck section above; `env_deep_copies ≈ 0`).
> The remaining method-call cost is in dispatch bookkeeping, not env cloning.

Remaining, in priority order (measured 2026-06-28):
- **Done (#3853)**: stop Debug-formatting the method-body AST per call for
  candidate identity (single-candidate fast path + allocation-free fingerprint).
- **Next — symbol round-trips**: dispatch resolves `Value::Package(Symbol)` /
  `Value::Instance { class_name: Symbol }` to a `String` (`resolve()`, allocates)
  and then re-`intern()`s it to a `Symbol` for cache keys and re-built invocant
  values. Reuse the original `Symbol` and migrate the `&str`-based dispatch APIs
  toward `Symbol` to drop both the allocation and the `FxHashMap` intern lookup.
- **Longer-term — `Value` size**: `Value::clone`/`drop` dominate class benchmarks;
  NaN-boxing (Lever 2, ADR-0001 layer 3b) shrinks the common variants but is
  sequenced *after* GC.

### Phase 4a: Value representation (NaN-boxing) — DONE (2026-07-12)

`Value` is one NaN-boxed 8-byte word (ADR-0001 layer 3b, #4467/#4469; was 48
bytes). All benchmarks gained 5-9%; the flip also unblocked the JIT's Tier B
tag-dispatched inline arithmetic (below).

### Phase 4b: Threaded dispatch — FROZEN (ADR-0004 §2.5 J0)

Rejected in favor of the JIT: Tier A subroutine threading removes the same
dispatch-loop cost with a larger ceiling. Revisit only if the JIT stalls.

### Phase 4c: JIT compilation (Cranelift) — DEFAULT ON since J5 (2026-07-13)

Method JIT per ADR-0004: hot `CompiledCode` chunks (and, since J4b, hot
compound-loop body sub-ranges via the `run_range` hook) compile to native
code. **On by default** since J5; `MUTSU_JIT=off` is the interpreter-only
opt-out.

- **J1 (#4471)**: skeleton — call counting, Tier A helper-call bodies, fib native.
- **J2 (#4474)**: Tier A opcode coverage, all 6 call-path entry hooks, jit-stress CI.
- **J3 (#4476)**: call convention — CallMethod shims, `to_map()` removal,
  type-check fast accept, `const_sym` memoization.
- **J4 (#4478/#4479/#4480)**: Tier B inline arithmetic (NaN-box tag-dispatched
  Int/Num fast paths in CLIF, zero refcount/GC traffic), hot-loop entry
  (int-arith-class top-level loops finally JIT), and variable-op helper
  hot-path fixes (which also sped the interpreter itself: int-arith −34%).
- **J5 (default on, 2026-07-13)**: gates verified on bench CI main `f19946ad`
  — every `+jit` series at or ahead of the interpreter (fib 0.59x vs 0.77x,
  bench-fib 1.26x vs 1.64x, no series slower) and bench-startup unchanged
  (0.0086s vs 0.0087s: cold code never compiles, so the startup budget is
  untouched); gc-stress × jit-stress matrix green since J2.
- Remaining: **J4d** — Tier B variable-op inlining (GetLocal/SetLocal),
  JIT→JIT call inlining, args-Vec alloc removal (this is where the original
  "5-10x for tight numeric loops" expectation now lives: after J4c the
  interpreter is fast enough that the residual JIT gap IS the variable-op
  helper calls).

### Function calls with type constraints

bench-fib (`sub fib(Int $n --> Int)`) is at ~1.7-1.9x of raku (was 3.2x): the
J3 `type_matches_value` tag/class fast accept recovered most of the per-call
type-check cost. The residual is per-call binding/dispatch overhead (J4d
territory).

## Measurement Notes

- **Numbers in documents come from the bench CI** (`bench-history.tsv` on the
  `bench-data` branch; appended per main push, median of 7 runs plus a
  same-runner raku ratio). The `<bench>+jit` rows are the JIT-on series
  (recorded since #4480; the default configuration since J5 — plain rows pin
  `MUTSU_JIT=off`). Cite the commit hash a number belongs to.
- Local `perf stat -r5` under `taskset` is fine for in-flight A/B decisions
  and PR descriptions, but drifts with thermals and binary layout (±5%);
  check for stray `mutsu` processes before measuring.
- All benchmarks run with the `--release` build; raku times include ~120-170ms
  startup overhead, mutsu startup is ~4-8ms.
