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

**Remaining cost breakdown** (fast path):

| Step | Cost | Description |
|------|------|-------------|
| `exec_call_method_op` entry | ~5μs | Method name String alloc, arg processing, stack ops |
| Method resolution | ~2μs | Cache hit: Symbol pair lookup in HashMap |
| `push_call_frame` | ~0.1μs | Arc bump for env (O(1)) |
| **env deep clone + batched inserts** | **~15μs** | **Arc::make_mut + all special vars + attrs in one hold** |
| Direct locals init | ~2μs | Populate from source data (attrs, params, special vars) |
| **Bytecode execution** | **~1μs** | The actual method body |
| Cleanup (can_skip_merge) | ~5μs | Writeback attrs from locals, restore env |
| Cleanup (non-can_skip_merge) | ~10μs | Sync locals→env, merge, writeback |
| **Total** | **~40μs** | **Down from ~54μs (26% faster)** |

**Root cause**: The env deep clone (~12-15μs) remains the dominant cost. The `Env` is `Arc<HashMap<String, Value>>` and `Arc::make_mut` deep clones the entire ~100-entry HashMap when shared.

**Why the env clone can't be eliminated**: `$!attr` reads use `GetGlobal` which reads from env. PostIncrement/PreIncrement on attributes also read/write env. Closure captures of outer-scope variables need the full env. Dynamic variables (`$*VAR`) must be visible from the caller's env.

### Approaches investigated and rejected

**Two-level Env (base + delta HashMap)**: Tested 2026-05-22. Made `clone()` O(delta) instead of O(n), but added a branch to every `get()` call. Since `get()` is called ~300 times per method call (across all paths), the branch overhead (~2ns × 300 = 0.6μs) offset the clone savings for non-method-call code paths. Net result: method-call improved 22%, but bench-fib and bench-class regressed 15-20%.

**Skip attribute binding for non-attribute methods**: Tested 2026-05-22. Skipping the attribute loop when `cc.locals` doesn't reference `!name`/`.name` broke chained method calls (`$obj.set-to(20).add(5)`) because the attribute writeback path depends on attributes being in env even when the current method doesn't read them. The dependency chain between attribute binding, `self` construction, and exit-time writeback makes partial skipping unsafe.

**Batched env_mut() calls**: Tested 2026-05-22. Reducing 7 separate `env_mut()` calls to 1 (by holding the `&mut HashMap` reference across all inserts) saved ~8% on bench-class. Now adopted in the fast path.

**Fresh env (bypass clone entirely)**: Tested 2026-05-22. Build a small HashMap with only method-specific entries instead of cloning the ~100-entry outer env. Achieved 23-27% improvement but broke: (1) closure captures of outer-scope variables, (2) dynamic variable (`$*VAR`) visibility, (3) package-scoped sub resolution. Adding `may_capture_outer_vars` detection helped but too many edge cases remained (coercion types, `is hidden` classes, caller env changes). Reverted to clone-based approach with batched inserts + direct locals init.

### Function calls with type constraints (3.6x slower)

bench-fib uses `sub fib(Int $n --> Int)` which adds:
- Type constraint checking per call (~150ns)
- Return type validation
- The light-call path can't be used with constraints

Potential improvements:
- Trust type constraints after first check in tight loops
- Specialize compiled code for known-Int arguments

## Optimization History

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

The remaining bottleneck is method dispatch (~3x slower than raku). Everything else is at parity or faster. The plan below is ordered by impact and feasibility.

### Phase 1: Reduce method call overhead — target: method-call < 2x

**Step 1: Direct locals initialization — DONE (2026-05-22)**

Implemented `call_compiled_method_fast()` with batched env inserts, direct locals init, and simplified cleanup. Also pre-allocated "self" in method body compilation for GetLocal access. Achieved 18-23% improvement.

**Remaining bottleneck**: The env deep clone (~12-15μs) can't be eliminated because `$!attr` uses GetGlobal (reads env), PostIncrement/PreIncrement on attrs use env, and closure captures need the full env.

**Step 1b: Compile $!attr to GetLocal (NOT YET DONE)**

Currently `$!attr` compiles to `GetGlobal("!attr")` which reads from env. If the compiler allocated "!attr" as a local and used `GetLocal`, the env clone could potentially be eliminated for methods without closure captures. However, `$!attr++` and `$!attr = val` also need to work via locals, requiring changes to PostIncrement/PreIncrement/AssignExpr opcodes to check locals first. This is a larger change that could enable the full "bypass env clone" optimization.

**Step 2: Eliminate `exec_call_method_op` overhead**

The VM entry point `exec_call_method_op` does:
- `Self::const_str(code, name_idx).to_string()` — String allocation per call
- `Self::rewrite_method_name()` — modifier processing
- `Self::append_flattened_call_arg()` — Slip flattening check per arg
- Junction autothread check

For the common case (no modifier, no junction, no slip), most of this is wasted:
- Use `Symbol` (interned string) for method names instead of `String`
- Skip junction check when target is `Value::Instance`
- Skip slip flattening when args have no Slip values (checkable via flag)

**Step 3: Lightweight call frame for simple methods**

`push_call_frame` saves: env, readonly_vars, locals, stack_depth, env_dirty, locals_dirty, locals_dirty_slots, local_bind_pairs. For simple methods, most of these are unnecessary:
- `readonly_vars`: only needed if method uses `:=` binding
- `local_bind_pairs`: only needed if method uses `:=` binding
- `locals_dirty_slots`: Vec allocation per call

Create a `push_light_call_frame` that only saves env + locals + stack_depth.

### Phase 2: Compile-time method resolution — target: method-call < 1.5x

**2a. Monomorphic inline cache**

Method resolution is already cached in `method_resolve_cache: HashMap<(Symbol, Symbol), ...>`, but each call still does a HashMap lookup. For monomorphic call sites (>95% of all calls), the target class never changes:

- Add a `last_class: Symbol` + `last_result: (String, MethodDef)` pair to each `CallMethod` opcode
- On dispatch: compare target class with `last_class` (1 pointer comparison)
- On hit: use cached result directly (0 HashMap lookups)
- On miss: fall through to `method_resolve_cache`, then update inline cache

Expected: ~2μs savings per call (HashMap lookup elimination).

**2b. Direct compiled function pointer in method cache**

Currently, even after resolution, the call path goes through:
```
try_compiled_method_or_interpret → check_method_wrap_chain → call_compiled_method
```

For the common case (compiled method, no wrap chain), store a direct function pointer in the cache:
```
CallMethod { cache: Option<(Symbol, Arc<CompiledCode>)> }
```

Skip all intermediate dispatch and call `vm.run()` with the cached compiled code.

### Phase 3: Remove env dependency from VM — target: all benchmarks < 1.5x

**3a. Self-contained VM execution**

Currently, compiled method execution reads/writes `self.interpreter.env` for:
- Variable lookup (when not in locals)
- `$*DYNAMIC` variable resolution
- `EVAL` body execution
- `callsame`/`nextsame` dispatch

Goal: VM execution never touches env except for:
- `EVAL` (inherently needs AST interpreter)
- Dynamic variable lookup (small dedicated chain)
- Interpreter fallback calls (decreasing as coverage improves)

**3b. Closure captures as indexed slots**

Currently, closures capture the entire env HashMap. Replace with indexed capture slots:
- At compile time, analyze which variables the closure reads/writes
- Store only those values in a `Vec<Value>` (indexed by slot)
- Closure execution reads/writes slots directly, no HashMap

This eliminates env clone for closure creation (currently clones ~100 entries).

### Phase 4: Advanced optimizations — target: faster than raku

**4a. Value representation (NaN-boxing)**
- `Value` enum is currently 72 bytes (discriminant + largest variant)
- NaN-boxing: 8 bytes for Int/Num/Bool/Nil (common cases)
- Heap-allocated Box for large variants (String, Array, Hash, Instance)
- Expected: 2x improvement on arithmetic benchmarks, significant memory reduction

**4b. Threaded dispatch**
- Replace `match` on ~100 OpCode variants with computed goto
- Each opcode handler jumps directly to the next (1 indirect branch vs 2)
- Expected: 10-30% improvement on instruction-bound benchmarks

**4c. JIT compilation**
- Cranelift backend for hot loops
- Trace-based: detect hot bytecode sequences, compile to native
- Depends on stable bytecode format and value representation

## Measurement Notes

- All benchmarks run on the same machine with `--release` build
- raku times include ~170ms startup overhead; mutsu startup is ~3ms
- For fair comparison on compute benchmarks, subtract startup from both
- `time` output parsing can be noisy; run 3x and take median for precise measurements
- bench-* files test more comprehensive scenarios; non-prefixed files test specific operations
