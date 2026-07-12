# Performance Tracking

## Benchmark Suite

Benchmarks live in `benchmarks/`. Run all with:

```bash
cargo build --release
./benchmarks/run-all.sh
```

## Current Status (benchmarks: 2026-07-12)

> Measured 2026-07-12 with `perf stat -r5` (or `-r3` for the sub-0.1s ones) under
> `taskset -c 0-3` on a quiet machine, release build (`opt + debuginfo`), rakudo
> v2022.12. Includes the July perf slices #4447 (for-loop topic writeback O(n^2)
> fix), #4451 (native-ctor plan cache + is-default gate), and the
> `method_local_keys` predicate. **Caution: verify machine quietness before
> benchmarking** — a single stray `mutsu` process inflated earlier same-day
> measurements by 2-3x (both mutsu's and raku's), which briefly put wrong
> ratios (bench-class "0.58x") into PR #4447/#4451 text. The table below is the
> corrected record.

| Benchmark | mutsu | raku | ratio | notes |
|-----------|-------|------|-------|-------|
| fib(25) | 0.85s | 0.18s | 4.8x | recursive function calls — **regressed ~2.3x vs 2026-05 (0.37s), see below** |
| bench-fib | 2.51s | 0.24s | 10.5x | fib with type constraint (`Int $n --> Int`) — **regressed ~2.3x vs 2026-05 (1.09s)** |
| int-arith | 0.12s | 0.15s | **0.85x** | `for ^100000 { $sum += $_ * 3 + 1 }` |
| string-concat | 0.03s | 0.17s | **0.18x** | `$s ~= 'x'` × 10000 |
| hash-access | 0.03s | 0.18s | **0.16x** | 10K hash inserts + value iteration |
| method-call | 0.22s | 0.17s | 1.31x | Point.distance-to × 10000 (was 2.7x on 2026-05) |
| array-ops | 0.10s | 0.23s | **0.44x** | grep+map on 1000-elem array × 100 |
| bench-array | 0.05s | 0.23s | **0.22x** | push+map+grep+sort+reverse on 10K |
| bench-hash | 0.04s | 0.25s | **0.14x** | 10K insert+lookup+delete+keys+values |
| bench-class | 0.23s | 0.20s | 1.15x | class instantiation + method calls + inheritance (was 2.3x on 2026-05; 1.06s → 0.23s across the July slices) |
| bench-startup | 0.004s | 0.12s | **0.03x** | startup overhead |
| bench-string | 0.08s | 0.27s | **0.28x** | string operations |

Note: raku times include ~120ms startup overhead. mutsu startup is ~4ms.

### Summary

- **Faster than raku (9/12)**: startup, string-concat, bench-string, int-arith, array-ops, hash-access, bench-hash, bench-array
- **Near parity, target `<1.5x` met (2/12)**: bench-class (1.15x), method-call (1.31x)
- **Regressed / far from target (2/12)**: fib (4.8x), bench-fib (10.5x)

### ⚠ fib / bench-fib absolute regression (2026-07-12 finding)

`fib` (0.37s → 0.85s) and `bench-fib` (1.09s → 2.51s) both slowed ~2.3x in
absolute terms since the 2026-05-24 measurement, on the same benchmark files.
The May-era table predates GC-default-on (2026-07-05, ADR-0003) and the layer-3a
Track B churn, which are the prime suspects — but this is unbisected. Needs a
dedicated investigation (bisect over the GC-enable range, `MUTSU_VM_STATS`
counter diff) before NaN-boxing (layer 3b) work banks its expected fib gains on
top of a regressed baseline.

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

### Phase 4a: Value representation (NaN-boxing)

`Value` enum is currently 48 bytes (down from 72 after boxing the oversized
variants — Capture, BigRat, CustomTypeInstance, Uni, etc.; guarded by the
`value_size_guard` test in `src/value/mod.rs`). NaN-boxing would encode common
types (Int, Num, Bool, Nil) in 8 bytes using NaN payload bits. Expected: 2x
improvement on int-arith, ~30% on fib.

### Phase 4b: Threaded dispatch (direct threading)

Replace the `match` opcode dispatch with a function pointer table. Expected: 10-30% on instruction-bound benchmarks (fib, int-arith).

### Phase 4c: JIT compilation (Cranelift)

Compile hot bytecode sequences to native code. Expected: 5-10x for tight numeric loops.

### Function calls with type constraints (3.2x slower)

bench-fib uses `sub fib(Int $n --> Int)` which adds type constraint checking per call. Potential: trust type constraints after first check in tight loops.

## Measurement Notes

- All benchmarks run with `--release` build
- raku times include ~170ms startup overhead; mutsu startup is ~4ms
- Run 3x and take median for precise measurements
