# Performance Tracking

## Benchmark Suite

Benchmarks live in `benchmarks/`. Run all with:

```bash
cargo build --release
./benchmarks/run-all.sh
```

## Current Status (benchmarks: 2026-05-24)

> The benchmark table below was measured on 2026-05-24 and has not been re-run
> since (Value layout shrank to 48 bytes and the VM was unified into a single
> struct in the meantime). Re-measure with `./benchmarks/run-all.sh` before
> relying on these exact numbers.

| Benchmark | mutsu | raku | ratio | notes |
|-----------|-------|------|-------|-------|
| fib(25) | 0.37s | 0.36s | 1.0x | recursive function calls |
| bench-fib | 1.09s | 0.34s | 3.2x | fib with type constraint (`Int $n --> Int`) |
| int-arith | 0.16s | 0.22s | **0.7x** | `for ^100000 { $sum += $_ * 3 + 1 }` |
| string-concat | 0.02s | 0.21s | **0.09x** | `$s ~= 'x'` × 10000 |
| hash-access | 0.04s | 0.23s | **0.17x** | 10K hash inserts + value iteration |
| method-call | 0.71s | 0.27s | 2.7x | Point.distance-to × 10000 |
| array-ops | 0.16s | 0.26s | **0.6x** | grep+map on 1000-elem array × 100 |
| bench-array | 0.03s | 0.29s | **0.1x** | push+map+grep+sort+reverse on 10K |
| bench-hash | 0.03s | 0.29s | **0.1x** | 10K insert+lookup+delete+keys+values |
| bench-class | 0.79s | 0.34s | 2.3x | class instantiation + method calls + inheritance |
| bench-startup | 0.005s | 0.14s | **0.04x** | startup overhead |
| bench-string | 0.08s | 0.33s | **0.2x** | string operations |

Note: raku times include ~170ms startup overhead. mutsu startup is ~4ms.

### Summary

- **Faster than raku (9/12)**: startup, string-concat, bench-string, int-arith, array-ops, hash-access, fib, bench-hash, bench-array
- **~2-3x slower (3/12)**: bench-fib (3.2x), bench-class (2.3x), method-call (2.7x)

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

### `env_dirty` dual store
- The VM keeps both indexed `locals` slots and an `env: HashMap<Symbol, Value>`.
  The `env` is needed for closure captures, dynamic variables (`$*VAR`), and
  package-scoped resolution. Keeping the two in sync (reverse-sync, guarded by
  `env_dirty` / `sync_locals_from_env`) is the main internal optimization debt
  still being paid down (Slice F campaign).

## Remaining Bottleneck: Method Dispatch (~2.5x slower)

**Current cost**: ~31μs per method call (fast path). raku achieves ~5μs.

| Step | Cost | Description |
|------|------|-------------|
| `exec_call_method_op` entry | ~2μs | Arg processing, stack ops |
| Method resolution | ~1μs | FastMethodCacheEntry hit (pre-computed dispatch) |
| `push_call_frame` | ~0.1μs | Arc bump for env |
| **env deep clone** | **~9μs** | **Arc::make_mut on HashMap<Symbol, Value>** |
| Direct locals init | ~2μs | Populate from source data |
| **Bytecode execution** | **~1μs** | The actual method body |
| Cleanup | ~3μs | Writeback attrs, restore env |
| **Total** | **~31μs** | |

**Root cause**: `Arc::make_mut` deep clones the entire ~100-entry HashMap when shared (refcount > 1). Symbol keys reduced clone cost ~30% vs String keys, but the clone itself is still the dominant cost.

**Why env clone can't be eliminated**: Inner method calls inherit the current env as their outer scope. Closures capture outer-scope variables from env. Dynamic variables (`$*VAR`) must be visible through the env chain.

### Approaches investigated and rejected

1. **Two-level Env (base + delta)**: Branch overhead on every `get()` offset clone savings. Net regression on non-method benchmarks.
2. **Fresh env (bypass clone)**: Broke closure captures, dynamic variable visibility, package-scoped sub resolution.
3. **Scope chain (`parent: Option<Arc<EnvParent>>`)**: Extra indirection in every get()/insert() caused 10-24% regression.
4. **Env compaction (remove locals-only entries before clone)**: Broke `let`/`temp` restoration, BlockScope, line tracking.

## Improvement Plan

### Phase 3b: Closure captures as indexed slots — target: method-call < 2x

Currently closures capture the entire env HashMap. Replace with indexed capture slots:
- At compile time, analyze which variables the closure reads/writes
- Store only those values in a `Vec<Value>` (indexed by slot)
- Closure execution reads/writes slots directly, no HashMap

This eliminates env clone for closure creation (currently clones ~100 entries).

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
