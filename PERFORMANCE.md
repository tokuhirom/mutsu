# Performance Tracking

## Benchmark Suite

Benchmarks live in `benchmarks/`. Run all with:

```bash
cargo build --release
./benchmarks/run-all.sh
```

## Current Status (2026-05-23)

| Benchmark | mutsu | raku | ratio | notes |
|-----------|-------|------|-------|-------|
| fib(25) | 0.37s | 0.36s | 1.0x | recursive function calls (bench-fib: 1.13s / 0.31s = 3.6x with type constraint) |
| int-arith | 0.16s | 0.22s | **0.7x** | `for ^100000 { $sum += $_ * 3 + 1 }` |
| string-concat | 0.02s | 0.21s | **0.09x** | `$s ~= 'x'` × 10000 |
| hash-access | 0.04s | 0.23s | **0.17x** | 10K hash inserts + value iteration |
| method-call | 0.85s | 0.29s | 2.9x | Point.distance-to × 10000 |
| array-ops | 0.16s | 0.26s | **0.6x** | grep+map on 1000-elem array × 100 |
| bench-array | 0.04s | 0.30s | **0.1x** | push+map+grep+sort+reverse on 10K |
| bench-hash | 0.03s | 0.30s | **0.1x** | 10K insert+lookup+delete+keys+values |
| bench-class | 1.03s | 0.37s | 2.8x | class instantiation + method calls + inheritance |
| bench-startup | 0.004s | 0.17s | **0.02x** | startup overhead |
| bench-string | 0.08s | 0.33s | **0.2x** | string operations |

Note: raku times include ~170ms startup overhead. mutsu startup is ~4ms.

### Summary

- **Faster than raku (9/11)**: startup, string-concat, bench-string, int-arith, array-ops, hash-access, fib, bench-hash, bench-array
- **~3x slower (2/11)**: bench-class (2.8x), method-call (2.9x)

## Architecture Overview: Execution Paths

Understanding the execution paths is critical for optimization:

```
Source → Parser → AST → Compiler → Bytecode → VM (fast path)
                    ↓
              Interpreter (slow path: eval_block_value, etc.)
```

### VM path (fast)
- Compiled functions/methods execute via `vm.run()` → opcode dispatch
- Local variables use indexed slots (`self.locals[i]`), not HashMap
- Parameter binding via `call_compiled_function_positional_light` (no env clone)
- ~10-100ns per instruction

### Interpreter path (slow)
- `eval_block_value()` walks the AST directly
- Variables stored in `env: HashMap<String, Value>` — **full clone on every scope boundary**
- Method dispatch goes through MRO walk + type checking per call
- ~1-10μs per expression (100-1000x slower than VM)

### The Fallback Problem

Many operations that should run in the VM fall back to the interpreter:

| Operation | Current path | Impact |
|-----------|-------------|--------|
| `sort { $^b <=> $^a }` | ~~Interpreter~~ **Inlined** (2026-05-22) | Fixed |
| `sort { .key }` (mapper) | ~~Interpreter~~ **Schwartzian transform** (2026-05-22) | Fixed |
| `map { ... }` | VM compiles block, but interpreter dispatches for some closures | Mixed |
| `grep { ... }` | Similar to map | Mixed |
| Class `.new()` | Interpreter: `dispatch_new` | Fast (0.03ms/call) |
| **Method call** | **`call_compiled_method`: env deep clone + batched setup** | **#1 bottleneck (~40μs/call)** |
| Hash element access | ~~interpreter for complex~~ **VM fast path** (2026-05-22) | Fixed |
| `@arr.push(val)` | ~~CallMethodMut dispatch~~ **ArrayPush opcode** (2026-05-22) | Fixed |
| `for` loop body | VM compiled | Fast |
| `EVAL` body | Interpreter only | Inherently slow |

## Bottleneck Analysis

### Hash operations — RESOLVED (now 10x faster than raku)

Both hash insert and delete fast paths now handle `Arc::strong_count == 2` by temporarily dropping the locals reference before `Arc::make_mut`. bench-hash: 22.7s → 0.03s.

### Array push — RESOLVED (now 10x faster than raku)

`ArrayPush` opcode emitted at compile time for `@arr.push(val)` on local arrays, bypassing the full CallMethodMut dispatch chain. bench-array: 0.59s → 0.04s.

### Method dispatch (~3x slower) — THE REMAINING BOTTLENECK

**Current cost**: ~40μs per method call (fast path). raku achieves ~5μs.

**What the fast path (`call_compiled_method_fast`) saves** (2026-05-22):
- Batched env_mut(): single `&mut HashMap` hold for all inserts (~3μs saved)
- Direct locals initialization: skip env→locals copy loop (~5μs saved)
- Bypass `bind_function_args_values`: direct param mapping (~3μs saved)
- Simplified cleanup for read-only methods (~5μs saved)
- Compiler emits GetLocal("self") for `$.attr` / bare `self` access

**Remaining cost breakdown** (fast path, after 2026-05-23 optimizations):

| Step | Cost | Description |
|------|------|-------------|
| `exec_call_method_op` entry | ~3μs | Arg processing, stack ops (no String alloc — Cow borrows from const pool) |
| Method resolution | ~2μs | Cache hit: Symbol pair lookup in HashMap |
| `push_call_frame` | ~0.1μs | Arc bump for env (O(1)) |
| **env deep clone + reduced inserts** | **~13μs** | **Arc::make_mut + ~6 inserts (skip !, callable_id, attrs for can_skip_merge)** |
| Direct locals init | ~2μs | Populate from source data (attrs, params, special vars); ?CLASS/?ROLE via GetLocal |
| **Bytecode execution** | **~1μs** | The actual method body |
| Cleanup (can_skip_merge) | ~3μs | Writeback attrs via pre-computed slot indices, restore env |
| Cleanup (non-can_skip_merge) | ~10μs | Sync locals→env, merge, writeback |
| **Total** | **~35μs** | **Down from ~54μs (35% faster from start)** |

**Root cause**: The env deep clone (~12-15μs) remains the dominant cost. The `Env` is `Arc<HashMap<String, Value>>` and `Arc::make_mut` deep clones the entire ~100-entry HashMap when shared.

**Why the env clone can't be eliminated**: Although `$!attr` now uses `GetLocal` (2026-05-23), the env clone is still needed because: (1) inner method calls inherit the current env as their outer scope, (2) closure captures of outer-scope variables need the full env, (3) dynamic variables (`$*VAR`) must be visible through the env chain, (4) package-scoped subs (`&foo`) are resolved from env.

### Approaches investigated and rejected

**Two-level Env (base + delta HashMap)**: Tested 2026-05-22. Made `clone()` O(delta) instead of O(n), but added a branch to every `get()` call. Since `get()` is called ~300 times per method call (across all paths), the branch overhead (~2ns × 300 = 0.6μs) offset the clone savings for non-method-call code paths. Net result: method-call improved 22%, but bench-fib and bench-class regressed 15-20%.

**Skip attribute binding for non-attribute methods**: Tested 2026-05-22. Skipping the attribute loop when `cc.locals` doesn't reference `!name`/`.name` broke chained method calls (`$obj.set-to(20).add(5)`) because the attribute writeback path depends on attributes being in env even when the current method doesn't read them. The dependency chain between attribute binding, `self` construction, and exit-time writeback makes partial skipping unsafe.

**Batched env_mut() calls**: Tested 2026-05-22. Reducing 7 separate `env_mut()` calls to 1 (by holding the `&mut HashMap` reference across all inserts) saved ~8% on bench-class. Now adopted in the fast path.

**Fresh env (bypass clone entirely)**: Tested 2026-05-22 and 2026-05-23. Build a small HashMap with only method-specific entries instead of cloning the ~100-entry outer env. Achieved 23-27% improvement but broke: (1) closure captures of outer-scope variables, (2) dynamic variable (`$*VAR`) visibility through method chains, (3) package-scoped sub resolution (`&foo` not found in fresh env). The 2026-05-23 attempt added `may_capture_outer_vars` detection and selective copying of `$*`, `&`, `?` entries, but inner method calls that inherit the calling method's env still lose outer-scope variables needed by sub-calls. **Root issue**: env serves as the scope chain — a fresh env breaks the chain for all transitive callees, not just the current method. Requires a proper scope chain data structure to fix.

### Function calls with type constraints (3.6x slower)

bench-fib uses `sub fib(Int $n --> Int)` which adds:
- Type constraint checking per call (~150ns)
- Return type validation
- The light-call path can't be used with constraints

Potential improvements:
- Trust type constraints after first check in tight loops
- Specialize compiled code for known-Int arguments

## Optimization History

### 2026-05-24: Fast method dispatch cache
- **Change**: Add `FastMethodCacheEntry` cache that stores pre-computed dispatch info (compiled code, `can_skip_merge`, positional count, default presence) for non-multi compiled methods. On cache hit, skip wrap chain check, `compiled_code` extraction, and `param_defs` eligibility scans — call `call_compiled_method_fast` directly. Also avoid extra `target.clone()` on fast path by moving target as base value. Use `Symbol` for cached `owner_class`.
- **Effect**: method-call ~4% faster, bench-class ~2% faster
- **Value**: Eliminates ~2-3μs of dispatch overhead per method call. Savings are modest because env deep clone (~13μs) dominates.

### 2026-05-23: Reduce env inserts and compile ?CLASS/?ROLE to GetLocal
- **Change**: Two optimizations: (1) Pre-allocate local slots for `?CLASS` and `?ROLE` in method bodies (both compile-time via `compile_sub_body` and runtime via `compile_methods_for_map`). `$?CLASS` and `$?ROLE` now compile to `GetLocal` instead of `GetGlobal`, avoiding env HashMap lookups. (2) For `can_skip_merge` methods with no closures, skip inserting `!`, `__mutsu_callable_id`, and array/hash attribute entries into env. Only insert self, __ANON_STATE__, ?CLASS, ?ROLE, _, and param values (~6 inserts instead of ~15+).
- **Effect**: method-call ~11% faster, bench-class ~10% faster
- **Value**: Reduces per-method-call env insert count and eliminates two GetGlobal→env lookups for ?CLASS/?ROLE. Foundation for further env optimization.
- **Limitation**: env_mut() still called (triggering Arc::make_mut deep clone). Full env_mut() skip was attempted but breaks `when` smartmatch which reads topic `_` from env, and creates stale ?CLASS values across different class method calls.

### 2026-05-23: Pre-compute attribute slot indices for method exit writeback
- **Change**: Add `AttrSlots` struct to `CompiledCode` that pre-computes local slot indices for each attribute (private, public, array, hash variants) at compile time. `writeback_attributes_from_locals` uses these pre-computed indices instead of 6 × N_attrs linear searches with `format!()` allocations per method exit. Also eliminate env round-trip in `call_compiled_method_fast`'s `can_skip_merge` path — attribute writeback goes directly from locals to attributes.
- **Effect**: method-call ~4% faster, bench-class ~3% faster
- **Value**: Eliminates O(6 × N_attrs × N_locals) string comparisons and 6 × N_attrs `format!()` allocations per method exit. For a class with 3 attributes and 10 locals, this saves ~18 heap allocations and ~90 string comparisons per method call.

### 2026-05-23: Avoid String allocation and skip wrap chain check
- **Change**: Two optimizations: (1) Use `Cow<str>` in `rewrite_method_name` for `exec_call_method_op` — avoids a String allocation on every method call when no modifier (`^`/`!`) is present (the common case). The method name stays as a borrowed `&str` from the constant pool. (2) Add `has_any_wrap_chains()` early return in `check_method_wrap_chain` — skip the expensive `find_method_candidate_index` (which computes AST fingerprints) and `get_method_wrap_chain` when no wrap chains exist.
- **Effect**: method-call ~10% faster, bench-class ~8% faster, bench-fib ~5% faster
- **Value**: Eliminates the last String allocation per method call in the common path, and avoids O(n) AST fingerprint computation on every method call when wrapping is unused.

### 2026-05-23: Reduce method dispatch entry overhead
- **Change**: Three optimizations: (1) Wrap MethodDef in Arc in method resolve cache — cache hit clone is now O(1) instead of deep-cloning Vec<String>/Vec<ParamDef>. (2) Avoid String allocations for method_raw and modifier — use `&str` from constant pool, only allocate when modifier requires `^`/`!` prefix (rare). (3) Skip Slip flattening when no Slip args present — check discriminants first, reuse raw_args Vec directly.
- **Effect**: Incremental (~1-3μs per call, hard to measure on noisy 1-core system)
- **Value**: Reduces allocation count per method call from 3-4 Strings to 1, eliminates MethodDef deep clone on cache hit, and avoids Vec copy for non-Slip args.

### 2026-05-23: Monomorphic inline cache for method resolution
- **Change**: Add `last_method_resolve` single-entry cache to VM. Before HashMap lookup, compare (class_sym, method_sym) with last result using pointer equality. Populate cache on HashMap hit or full resolution.
- **Effect**: bench-class ~2-4% improvement
- **Value**: Eliminates HashMap lookup for monomorphic call sites (same class+method in tight loops). Low overhead for polymorphic sites (one extra comparison before fallthrough).

### 2026-05-23: Compile $!attr to GetLocal + skip env inserts
- **Change**: Allocate local slots for `!attr` names across all compiler paths (Var, Assign, PostIncrement, PreIncrement, PostDecrement, PreDecrement). Skip `!attr`/`.attr` env inserts in fast method dispatch — locals handle all reads/writes. `sync_locals_from_env` skips `!attr` to prevent stale overwrites. Inc/dec fast paths skip Proxy-bound attributes, falling back to env-based path for `:=` bindings.
- **Effect**: Foundation for env clone reduction. Eliminates ~6 env HashMap inserts per method call for scalar attributes.
- **Value**: `$!attr` access is now O(1) array index instead of HashMap lookup. Prerequisite for future env-free method execution.

### 2026-05-22: Fast method dispatch path (batched env + direct locals init)
- **Change**: Add `call_compiled_method_fast()` that batches all env inserts through a single `env_mut()` hold, directly initializes locals from source data (skipping env→locals copy), and bypasses `bind_function_args_values` for simple positional params. Pre-allocate "self" and "__ANON_STATE__" as locals in method body compilation so `$.attr` and bare `self` use `GetLocal` instead of `GetGlobal`. Add `may_capture_outer_vars` flag on `CompiledCode`.
- **Effect**: bench-class 1.34s → 1.03s (**23% faster**, ratio 3.6x → 2.8x); method-call 1.04s → 0.85s (**18% faster**, ratio 3.6x → 2.9x)
- **Value**: Reduces per-call overhead from ~54μs to ~40μs by eliminating redundant env→locals copy, parameter binding overhead, and simplified cleanup. Compiler change makes `self` resolution O(1) via local slot instead of env HashMap lookup.
- **Limitations**: Falls back to slow path for: `is rw` params, invocant type constraints, coercion types, attribute aliases, role param bindings, complex params (slurpy/named/where), arg count mismatches.

### 2026-05-22: ArrayPush opcode for @arr.push(val)
- **Change**: Detect `@arr.push(single_expr)` at compile time, emit specialized `ArrayPush` opcode that directly appends to the array Arc. Uses same locals-drop technique as hash fast paths. Falls back to interpreter for shared arrays, typed arrays, shaped arrays, and non-local (captured) variables.
- **Effect**: push 10K elements: 0.51s → 0.008s (**64x speedup**); bench-array: 0.59s → 0.04s (**16x**, now 10x faster than raku)
- **Value**: Demonstrates the power of compile-time pattern recognition + specialized opcodes. The full CallMethodMut dispatch chain (54μs/call) is bypassed entirely for the common case.

### 2026-05-22: Fix hash fast path for shared Arc (locals + env)
- **Change**: When `Arc::strong_count == 2` (locals + env share the Arc), temporarily drop the locals reference before `Arc::make_mut`, enabling O(1) in-place mutation instead of O(n) clone per insert. Sync the mutated Arc back to locals afterward.
- **Effect**: hash-access 12.8s → 0.035s (**365x speedup**, now 6x faster than raku); bench-hash 22.7s → 1.79s (**12.7x speedup**, ratio 17.9x → 6.6x); bench-array 3.0s → 0.59s (**5x speedup**, now faster than raku)
- **Value**: Eliminates the #1 bottleneck. Hash insert in for loops was doing full HashMap clone per iteration due to shared Arc preventing in-place mutation.

### 2026-05-22: Fast path for hash delete (shared Arc)
- **Change**: Add `try_fast_hash_delete` that handles `Arc::strong_count == 2` like the insert fast path — drop locals ref, mutate in-place, sync back.
- **Effect**: bench-hash 1.79s → 0.035s (**51x speedup**, now 7.7x faster than raku); hash delete: 1.7s → 0.031s (**55x speedup**)
- **Value**: Combined with insert fast path, all hash operations now fast-pathed. bench-hash went from 22.7s to 0.035s total (**649x improvement**).

### 2026-05-22: Inline sort mapper patterns (Schwartzian transform)
- **Change**: Detect `{ .method }` and `{ $^a.method <=> $^b.method }` sort blocks. Use Schwartzian transform (pre-compute keys once, sort by keys) instead of calling mapper N*log(N) times.
- **Effect**: sort with mapper on 1000 elements × 10 iterations: faster than raku (0.10s vs 0.22s)
- **Value**: Reduces mapper calls from N*log(N) to N, eliminating interpreter overhead per comparison.

### 2026-05-22: Inline sort comparator for simple patterns
- **Change**: Detect `{ $^a <=> $^b }`, `{ $^b <=> $^a }`, `{ $^a cmp $^b }`, `{ $^b cmp $^a }` at sort time via AST pattern matching; execute as direct Rust comparison instead of eval_block_value
- **Effect**: bench-array 60s → 3.0s (**20x speedup**, ratio 37.5x → 2.4x vs raku); sort of 10K elements: 104s → 0.012s (**8700x speedup**)
- **Value**: Eliminates ~130K interpreter invocations + env clones for a 10K-element sort. Demonstrates the power of recognizing and short-circuiting interpreter fallbacks.

### 2026-05-22: Partial env save/restore for sort comparators
- **Change**: For non-inlinable sort blocks, save/restore only the keys the block touches instead of cloning the entire env HashMap
- **Effect**: ~20% improvement for complex sort blocks (still slow, but better)
- **Value**: Incremental improvement for sort blocks that can't be inlined

### 2026-05-20: Skip env merge for read-only compiled methods
- **Change**: Pre-compute `has_env_writes` flag on CompiledCode during emission; when a compiled method has no env-writing opcodes and no `is rw` params, skip the expensive locals→env sync, method_local_keys HashSet construction, merge_method_env deep clone, and writeback_attributes on method exit
- **Effect**: method-call 1.01s → 0.78s (**-23%**, ratio 3.5x → 2.7x vs raku)
- **Value**: Eliminates one HashMap deep clone, one HashSet construction (~25 entries), and one full env iteration per call for pure/read-only methods

### 2026-05-19: Fast path for hash element assignment
- **Change**: Add `try_fast_hash_element_assign()` that skips 16+ edge-case HashMap lookups when no constraints/mixins/bindings apply
- **Effect**: hash-access benchmark ~2.4s → ~0.044s (**55x speedup**, now 5x faster than raku)
- **Value**: Common hash assignment pattern is now fast-pathed

### 2026-05-19: Int arithmetic fast paths + attribute accessor fast path
- **Change**: Add inline Int+Int, Int-Int, Int*Int, Int<Int fast paths with checked overflow; add 0-arg Instance attribute accessor fast path
- **Effect**: int-arith 0.55s → 0.11s (**5x**, faster than raku); fib 1.05s → 0.21s (**5x**, faster than raku); method-call 1.30s → 1.26s (-3%)
- **Value**: Common integer operations are now as fast as direct Rust arithmetic + enum matching; attribute access skips full method dispatch

### 2026-05-19: Lazy method dispatch resolution + Num fast paths
- **Change**: Defer callsame/nextsame candidate resolution until actually called; add Num+/-/*/< fast paths
- **Effect**: method-call 1.16s → 1.01s (**-13%**); simple method 100K: 5.12s → 4.05s (**-21%**)
- **Value**: Eliminates 2 MRO walks + fingerprinting per method call for methods that don't use callsame

### 2026-05-19: Skip NFC normalization for ASCII strings
- **Change**: Add `is_ascii()` fast path before NFC normalization in string concat (~, interpolation) and repetition (x) operators
- **Effect**: string-concat benchmark 0.69s → 0.015s (**46x speedup**, now 14x faster than raku); hash-access -6% from key construction
- **Value**: Eliminates O(n²) NFC normalization in ASCII string append loops

### 2026-05-19: Guard ensure_env_synced + restructure native method bypass
- **Change**: Skip ensure_env_synced call in method/function dispatch when locals_dirty is false; restructure try_native_method bypass checks to short-circuit by target type (avoid MRO walks for non-Instance targets)
- **Effect**: method-call benchmark 1.46s → 1.30s (**-12%**)
- **Value**: Avoids expensive type_matches_value/has_user_method calls for non-Instance method dispatch

### 2026-05-19: Reduce env mutations in light call path
- **Change**: Skip env HashMap writes during parameter binding in positional light calls; defer env writes in SetLocal when shared_vars is inactive
- **Effect**: 40% fewer env_mut calls (1.21M → 0.73M for fib(25))
- **Benchmark impact**: < 1% on fib (HashMap::insert on non-shared Arc is O(1))
- **Value**: Reduces unnecessary work; will matter more with larger env HashMaps and DESTROY timing

## Improvement Plan

**Goal: world-class performance — faster than raku on all benchmarks, competitive with V8/LuaJIT on numeric workloads.**

The remaining bottleneck is method dispatch (~2.5x slower than raku). Everything else is at parity or faster. Achieving world-class performance requires structural changes to the runtime — incremental optimizations have diminishing returns. The plan below is ordered by impact. Phases 3 and 4 are the critical path; Phase 5 is a fallback only if Phase 3 proves architecturally impossible.

### Phase 1: Reduce method call overhead — target: method-call < 2x

**Step 1: Direct locals initialization — DONE (2026-05-22)**

Implemented `call_compiled_method_fast()` with batched env inserts, direct locals init, and simplified cleanup. Also pre-allocated "self" in method body compilation for GetLocal access. Achieved 18-23% improvement.

**Remaining bottleneck**: The env deep clone (~12-15μs) can't be eliminated because inner method calls inherit the env as their scope chain, closure captures need the full env, and dynamic variables must be visible through the env.

**Step 1b: Compile $!attr to GetLocal — DONE (2026-05-23)**

`$!attr` now compiles to `GetLocal` instead of `GetGlobal`. Attribute locals are allocated for all `!attr` references (Var, Assign, PostIncrement, PreIncrement, PostDecrement, PreDecrement). The fast method dispatch path skips `!attr`/`.attr` env inserts since locals handle all reads/writes. `sync_locals_from_env` skips `!attr` locals to prevent stale overwrites. Inc/dec fast paths skip Proxy-bound attributes (`:=` bindings) and fall back to the env-based path.

**Step 2: Eliminate `exec_call_method_op` overhead — PARTIAL (2026-05-23)**

Done:
- Avoid String allocation for `method_raw` and `modifier` — use `&str` from constant pool
- Skip Slip flattening when no Slip args present
- Arc<MethodDef> in resolve cache to avoid deep clone on cache hit
- `Cow<str>` for `rewrite_method_name` — eliminates the last String allocation in the common path (no modifier)
- Skip `check_method_wrap_chain` when no wrap chains exist — avoids expensive AST fingerprinting

Remaining:
- Skip junction check when target is `Value::Instance` (minimal impact — pattern match already fast-fails)

**Step 3: Lightweight call frame — PARTIAL (2026-05-23)**

Added `push_light_call_frame()` that skips cloning `readonly_vars: HashSet<String>`. The fast method dispatch path uses this since simple methods don't use `:=` binding. `VmCallFrame.saved_readonly` is now `Option<HashSet<String>>` — `pop_call_frame` only restores when `Some`.

Remaining: `locals_dirty_slots` and `local_bind_pairs` are moved via `std::mem::take` (O(1)), so skipping them would not improve performance.

### Phase 2: Compile-time method resolution — target: method-call < 1.5x

**2a. Monomorphic inline cache — DONE (2026-05-23)**

Added `last_method_resolve: Option<(Symbol, Symbol, String, MethodDef)>` to VM. Before the HashMap lookup, checks if the last resolution matches (class_sym, method_sym) — two Symbol comparisons (pointer equality). On hit: returns cached result with 0 HashMap lookups. On miss: falls through to `method_resolve_cache`, then updates inline cache. Effect: ~2-4% improvement on bench-class.

**2b. Fast method dispatch cache — DONE (2026-05-24)**

Added `FastMethodCacheEntry` that pre-computes and caches compiled code, `can_skip_merge`, positional count, and default presence for non-multi compiled methods. On cache hit (no wrap chains, arg count matches), skips wrap chain check, `compiled_code` extraction, and 5-6 `param_defs` eligibility scans — calls `call_compiled_method_fast` directly. Also avoids an extra `target.clone()` on the fast path by moving target directly as base value. Uses `Symbol` for cached `owner_class` to make clone O(1). Effect: method-call ~4% faster, bench-class ~2% faster.

**Step 1c: Reduce env inserts for read-only methods — DONE (2026-05-23)**

For `can_skip_merge` methods with no closures, skip inserting `!`, `__mutsu_callable_id`, and array/hash attribute entries into env. `$?CLASS` and `$?ROLE` now compile to GetLocal. Reduces inserts from ~15+ to ~6 per call. Effect: method-call ~11% faster, bench-class ~10% faster.

**Remaining env deep clone cost**: env_mut() is still called, triggering Arc::make_mut deep clone (~12-15μs). Full env_mut() skip was attempted but breaks `when` smartmatch (reads topic `_` from env) and causes stale `?CLASS` across different class method calls.

### Phase 3: Scope chain architecture — target: all benchmarks < 1.5x

The env deep clone (~12-15μs) is the dominant remaining cost. Current `Env` is `Arc<HashMap<String, Value>>` (defined in `src/env.rs` as a struct wrapping `Arc<HashMap>`, with `Deref`/`DerefMut` impls that call `Arc::make_mut` on mutation). The clone is triggered by `push_light_call_frame` bumping the Arc refcount, then `env_mut()` seeing refcount > 1.

**3a. Scope chain data structure — PARTIALLY COMPLETED (2026-05-24)**

**Completed**: API refactoring of `Env` — removed `Deref`/`DerefMut` impls and added explicit methods (`get`, `insert`, `remove`, `contains_key`, `get_mut`, `retain`, `iter`, `keys`, `values`, `values_mut`, `flatten`, `entry_or_insert`, `entry_or_insert_with`). Zero performance regression. This decouples the Env API from the internal representation, allowing future scope chain changes without touching callers.

**Attempted and rejected**: Adding a `parent: Option<Arc<EnvParent>>` field to the Env struct for scope chain lookups. Benchmarked with three approaches:
1. `Arc<EnvInner { local: HashMap, parent: Option<Arc<EnvInner>> }>` — 16-24% regression on bench-class/method-call due to extra indirection in every get()/insert() call
2. `Arc<HashMap> + Option<Arc<EnvParent>>` — 10-20% regression from extra struct size (16 bytes vs 8 bytes) and parent-check branch in get()
3. Fresh env for can_skip_merge methods — broke `test-util-test-iter-opt.t` because `overwrite_instance_bindings_by_identity` needs instance references in the env for attribute propagation

**Root cause**: Env is in the hottest possible path (every variable read/write). Even a single extra field or branch in `get()` causes measurable regression across all benchmarks. The estimated ~0.3μs overhead per method call (from PERFORMANCE.md risk analysis) was correct for individual calls, but the aggregate impact across ALL env operations (not just method dispatch) was 3-5x higher than estimated.

**Alternative approach needed**: Instead of changing the Env data structure, optimize the deep clone by reducing the env SIZE before cloning. The `skip_env_setup` optimization already reduces inserts from ~15+ to ~6. Further reducing the env size (by moving more variables to locals-only) would shrink the clone cost proportionally. See Phase 3b for the complementary approach of indexed closure captures.

**3b. Closure captures as indexed slots**

Currently, closures capture the entire env HashMap. Replace with indexed capture slots:
- At compile time, analyze which variables the closure reads/writes
- Store only those values in a `Vec<Value>` (indexed by slot)
- Closure execution reads/writes slots directly, no HashMap

This eliminates env clone for closure creation (currently clones ~100 entries).

**Implementation plan**:
1. Extend `CompiledCode` with `captured_vars: Vec<String>` — list of outer-scope variables referenced
2. At closure creation time, copy only those variables from env into a `Vec<Value>`
3. At closure execution time, bind captured values to local slots

**Prerequisite**: Phase 3a (scope chain) makes this easier because captures become "grab N values from parent scope" instead of "clone entire HashMap".

### Phase 4: Advanced optimizations — target: faster than raku

**4a. Value representation (NaN-boxing)**

`Value` enum is currently 72 bytes (discriminant + largest variant). NaN-boxing encodes common types (Int, Num, Bool, Nil) in 8 bytes using NaN payload bits.

**Implementation plan** (can be parallelized across 2-3 agents):

1. **Agent 1: Core NanBoxedValue type** (new file `src/nanbox.rs`, ~300 LOC)
   - Define `NanBoxedValue` as a `u64` with encoding:
     - Quiet NaN pattern: `0x7FF8_xxxx_xxxx_xxxx`
     - Double (f64): any non-NaN bit pattern → extract directly
     - Int (i48): `0x7FF9_xxxx_xxxx_xxxx` — 48-bit signed int in payload
     - Bool: `0x7FFA_0000_0000_000{0,1}`
     - Nil: `0x7FFA_0000_0000_0002`
     - Pointer (heap types): `0x7FFB_xxxx_xxxx_xxxx` — 48-bit pointer to `Box<HeapValue>`
   - `HeapValue` enum: Str, Array, Hash, Instance, Sub, Pair, Range, etc.
   - Implement `From<i64>`, `From<f64>`, `From<bool>`, arithmetic methods, comparison methods
   - Implement `Display`, `Debug`, `Clone` (pointer variants bump refcount), `Drop` (pointer variants decrement)

2. **Agent 2: Bridge layer** (modify `src/value.rs`)
   - Add `impl From<NanBoxedValue> for Value` and vice versa for gradual migration
   - OR: replace `Value` enum entirely (more aggressive, bigger diff)
   - The gradual approach is safer — use NanBoxedValue internally in hot paths (VM locals, stack) while keeping Value at API boundaries

3. **Agent 3: VM integration** (modify `src/vm/vm_arith_ops.rs`, `src/vm/vm_comparison_ops.rs`)
   - Replace `Value::Int(a) + Value::Int(b)` with NanBoxedValue arithmetic
   - The fast paths for Int+Int, Int*Int, etc. become direct u64 operations
   - Stack and locals become `Vec<NanBoxedValue>` instead of `Vec<Value>`

**Expected impact**: 2x improvement on int-arith, ~30% on fib (due to smaller stack/locals, better cache utilization). Memory reduction: 9x for Int/Num/Bool values.

**Risk**: Pointer types still require heap allocation. String operations won't benefit. Complex to implement correctly (especially GC/refcounting for heap types).

**4b. Threaded dispatch (direct threading)**

Replace the `match` opcode dispatch in `exec_one()` with direct threading:

```rust
// Current: indirect dispatch (2 branches per instruction)
fn exec_one(&mut self, code: &CompiledCode, ip: &mut usize) {
    match code.ops[*ip] {
        OpCode::Add => { ... }
        OpCode::Sub => { ... }
        // ~100 arms
    }
}

// Threaded: direct dispatch (1 branch per instruction)
// Each handler ends with: goto handlers[code.ops[ip + 1]]
```

**Implementation plan**:
1. Convert `OpCode` handlers from match arms to individual functions
2. Build a dispatch table: `[fn(&mut VM, &CompiledCode, &mut usize); NUM_OPCODES]`
3. Each handler calls the next handler directly via the dispatch table
4. Use `#[inline(never)]` to prevent LLVM from re-merging into a match

**Note**: Rust doesn't support computed goto. The closest is a function pointer table + tail calls. LLVM may or may not optimize this well. Benchmark before committing.

**Expected impact**: 10-30% on instruction-bound benchmarks (fib, int-arith). Minimal impact on method-call (dominated by env overhead, not instruction dispatch).

**Alternative**: Use Rust's `unsafe` + inline assembly for computed goto on x86-64. This is fragile and non-portable. Only consider if the function pointer approach doesn't help.

**4c. JIT compilation (Cranelift)**

Compile hot bytecode sequences to native code using Cranelift:

**Implementation plan** (multi-session project):

1. **Profiling infrastructure**: Add execution counters to loop headers and function entries. When a counter exceeds threshold (e.g., 100 iterations), trigger JIT compilation.

2. **IR lowering**: Translate `OpCode` sequences to Cranelift IR. Start with a subset:
   - Arithmetic: Add, Sub, Mul, Div, Mod, Pow
   - Comparison: Lt, Le, Gt, Ge, Eq, Ne
   - Control flow: Jump, JumpIfFalse, ForLoop
   - Variable access: GetLocal, SetLocal, LoadConst
   - Type guards: check Value discriminant, deoptimize on unexpected type

3. **Type specialization**: Most hot loops operate on Int or Num values. Generate specialized code paths for known types, with deoptimization traps for type mismatches.

4. **Integration**: Replace `exec_one()` loop with JIT-compiled function for hot code. Fallback to interpreter for cold code and deoptimization.

**Dependencies**: Stable bytecode format (Phase 1-2 done), NaN-boxing (Phase 4a) for efficient type guards.

**Expected impact**: 5-10x for tight numeric loops. Method dispatch overhead becomes the bottleneck once loop bodies are JIT-compiled.

### Phase 5: Env key optimization — target: reduce clone cost

If Phase 3a (scope chain) proves too risky, this is an alternative approach to reduce env clone cost without changing the data structure.

**5a. Symbol keys for env**

Replace env key type from `String` to `Symbol`:
```rust
pub struct Env {
    inner: Arc<HashMap<Symbol, Value>>,  // Was: HashMap<String, Value>
}
```

- Key clone becomes O(1) (pointer copy) instead of O(n) (heap alloc + memcpy)
- Key comparison becomes O(1) (pointer equality) instead of O(n) (string compare)
- For a 100-entry env, saves ~40ns × 100 = ~4μs on deep clone

**Implementation plan**: Grep all `env.insert(String, ...)` and `env.get(&str)` calls, convert to Symbol. Moderate-sized refactor (~50 files).

**5b. Env compaction — PARTIALLY COMPLETED (2026-05-24)**

**Completed**: Infrastructure — `needs_env_sync: Vec<bool>` bitmap on `CompiledCode` computed at compile time. For each local slot, `true` if the variable is referenced by GetGlobal/SetGlobal/etc. in this code (meaning it needs env visibility). If the code has closures, all locals are conservatively marked `true`. Computed by `compute_needs_env_sync()` for all compiled code (subs, methods, closures, top-level).

**Attempted and rejected**: Three approaches to active compaction:

1. **Skip env sync in `ensure_env_synced`** — Prevent locals-only variables from being synced to env. Broke `let`/`temp` (BlockScope restoration reads vars from env), CATCH blocks, and dynamic variable resolution. Root cause: env serves as the communication channel between `ensure_env_synced` (writes) and `sync_locals_from_env` (reads). Skipping writes causes stale reads.

2. **Filtered `sync_locals_from_env`** — Skip reading env→locals for locals-only variables. Broke `let`/`temp` restoration (which restores values VIA env) and BlockScope exit (which uses env to propagate changes across block boundaries).

3. **Remove entries before method call** (`compact_env_for_method_call`) — Remove env entries matching locals-only variables before `push_light_call_frame`. Broke test-assertion line tracking and other mechanisms that read variables from env in called functions. The env removal triggers `Arc::make_mut` (the same deep clone we're trying to avoid), negating any benefit.

**Root cause**: The env serves multiple purposes beyond variable lookup: (1) scope chain for BlockScope save/restore, (2) `let`/`temp` value preservation, (3) caller-callee communication, (4) line number tracking, (5) closure capture at creation time. Removing entries breaks any of these mechanisms.

**Alternative path**: Phase 5a (Symbol keys) is more promising — reduces clone cost without removing entries. Or reduce env SIZE by preventing `ensure_env_synced` from adding entries in the first place (requires architectural change to BlockScope restoration to use locals directly instead of env round-trip).

## Measurement Notes

- All benchmarks run on the same machine with `--release` build
- raku times include ~170ms startup overhead; mutsu startup is ~3ms
- For fair comparison on compute benchmarks, subtract startup from both
- `time` output parsing can be noisy; run 3x and take median for precise measurements
- bench-* files test more comprehensive scenarios; non-prefixed files test specific operations
