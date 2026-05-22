# Performance Tracking

## Benchmark Suite

Benchmarks live in `benchmarks/`. Run all with:

```bash
cargo build --release
./benchmarks/run-all.sh
```

## Current Status (2026-05-22)

| Benchmark | mutsu | raku | ratio | notes |
|-----------|-------|------|-------|-------|
| fib(25) | 0.37s | 0.36s | 1.0x | recursive function calls (bench-fib: 1.13s / 0.31s = 3.6x with type constraint) |
| int-arith | 0.16s | 0.22s | **0.7x** | `for ^100000 { $sum += $_ * 3 + 1 }` |
| string-concat | 0.02s | 0.21s | **0.09x** | `$s ~= 'x'` × 10000 |
| hash-access | 0.04s | 0.23s | **0.17x** | 10K hash inserts + value iteration |
| method-call | 1.21s | 0.29s | 4.2x | Point.distance-to × 10000 |
| array-ops | 0.16s | 0.26s | **0.6x** | grep+map on 1000-elem array × 100 |
| bench-array | 0.59s | 0.30s | 2.0x | push+map+grep+sort+reverse on 10K |
| bench-hash | 0.03s | 0.30s | **0.1x** | 10K insert+lookup+delete+keys+values |
| bench-class | 1.49s | 0.37s | 4.0x | class instantiation + method calls + inheritance |
| bench-startup | 0.004s | 0.17s | **0.02x** | startup overhead |
| bench-string | 0.08s | 0.33s | **0.2x** | string operations |

Note: raku times include ~170ms startup overhead. mutsu startup is ~4ms.

### Summary

- **Faster than raku (8/11)**: startup, string-concat, bench-string, int-arith, array-ops, hash-access, fib, bench-hash
- **2x slower (1/11)**: bench-array
- **4x slower (2/11)**: bench-class, method-call

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
| `sort { $^b <=> $^a }` | ~~Interpreter~~ **Inlined** (2026-05-22) | ~~1300x slower~~ Fixed |
| `sort { .key }` (mapper) | Interpreter: env clone per comparison | Very slow for large arrays |
| `map { ... }` | VM compiles block, but interpreter dispatches for some closures | Mixed |
| `grep { ... }` | Similar to map | Mixed |
| Class `.new()` | Interpreter: `dispatch_new` | 4-5x overhead per instantiation |
| Method call with types | Interpreter: type checking + candidate resolution | Per-call overhead |
| Hash element access | VM fast path for simple cases, interpreter for complex | 13x on bench-hash |
| `for` loop body | VM compiled | Fast |
| `EVAL` body | Interpreter only | Inherently slow |

## Bottleneck Analysis

### Hash operations — RESOLVED (now 7.7x faster than raku)

Both hash insert and delete fast paths now handle `Arc::strong_count == 2` by temporarily dropping the locals reference before `Arc::make_mut`. bench-hash: 22.7s → 0.035s.

### Method dispatch (4-5x slower)

Every compiled method call (via `call_compiled_method`) does:
1. Method resolution (cached via `method_resolve_cache` for non-multi)
2. COW env clone (`push_call_frame` → `clone_env()` is O(1) Arc bump, but first `env_mut()` triggers full HashMap clone)
3. 7+ `env_mut().insert()` calls for `?CLASS`, `self`, `__ANON_STATE__`, `$_`, `$!`, `__mutsu_callable_id`, `?ROLE`
4. Attribute binding: iterate all instance attributes, insert `$!name`, `$.name`, `@!name`, etc. into env
5. Parameter binding via interpreter
6. Initialize locals from env
7. On exit (if `can_skip_merge`): skip env merge, just pop frame

**Root cause**: The env HashMap is cloned on every method call because `env_mut()` (Arc::make_mut) triggers COW. With ~100 entries in env, this is ~100 key-value clones per call. For a simple method like `bark() { "Woof!" }`, the setup is 30+ HashMap operations while the method body is 1 opcode.

**.new() is fast**: 5000 `Dog.new()` calls take only 0.14s. The bottleneck is method calls (5000 iterations × 3 methods = 15000 calls taking 1.4s).

Potential improvements:
- **Layered scope for methods**: Instead of cloning env, use a scope overlay that gets checked first. This would avoid the COW clone entirely for method calls.
- **Inline cache (PIC)**: Cache resolved method + skip resolution for monomorphic call sites.
- **Skip attribute env binding**: For methods that don't access `$!attr` (detectable from locals list), skip the attribute iteration loop.

### Function calls with type constraints (4.4x slower)

bench-fib uses `sub fib(Int $n --> Int)` which adds:
- Type constraint checking per call (~150ns)
- Return type validation
- The light-call path can't be used with constraints

Potential improvements:
- Trust type constraints after first check in tight loops
- Specialize compiled code for known-Int arguments

## Optimization History

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

### Phase 1: Interpreter Bypass (Q2 2026) — target: all benchmarks < 5x

Low-risk, high-impact changes that reduce interpreter fallbacks without architectural changes.

**1a. Inline common higher-order function patterns** ✅ (sort done)
- [x] `sort { $^a <=> $^b }` / `{ $^b <=> $^a }` — AST pattern match → Rust comparison
- [x] `sort { .key }` (1-arity mapper) — Schwartzian transform with direct method dispatch
- [x] `sort { $^a.method <=> $^b.method }` — Schwartzian transform with method key extraction
- [ ] `grep { ... }` with simple conditions — inline truthiness check
- [ ] `map { ... }` with simple expressions — inline evaluation

**1b. Hash operation fast paths**
- [ ] VM opcode for `:delete` on hash elements
- [ ] Compiled `.values` / `.keys` iteration (avoid intermediate array materialization)
- [ ] String key interning for repeated hash access patterns (e.g., `"key-$_"`)

**1c. Object instantiation fast path**
- [ ] Compile `Class.new(named => args)` to a VM opcode sequence
- [ ] Skip full `dispatch_new` interpreter path for simple classes (no BUILD/TWEAK)
- [ ] Pre-compute attribute slot layout at class registration time

### Phase 2: Method Dispatch Optimization (Q3 2026) — target: all benchmarks < 3x

Reduce per-call overhead for method dispatch.

**2a. Inline caching (Polymorphic Inline Cache)**
- Cache resolved method + type check result at each call site
- Monomorphic sites (>95% of calls): 1 type check + direct jump
- Megamorphic fallback: current MRO walk
- Requires: call site IDs in bytecode, cache invalidation on class mutation

**2b. Type specialization for compiled methods**
- When all callers pass Int, compile an Int-specialized variant
- Skip runtime type checks for specialized calls
- Deoptimize to generic version on type mismatch

**2c. Eliminate env HashMap for compiled code**
- Currently: VM locals[] ↔ interpreter env HashMap sync on every fallback
- Goal: compiled methods never touch env HashMap
- Requires: all built-in methods callable without interpreter env
- Blocker: ~35 runtime/ submodules still read from `self.env`

### Phase 3: Interpreter Elimination (Q4 2026) — target: all benchmarks < 2x

Systematic removal of interpreter fallbacks.

**3a. Compile all block callbacks to bytecode**
- `sort`, `map`, `grep`, `first`, `reduce` callbacks compiled at parse time
- VM invokes compiled closures directly via `call_compiled_closure`
- No env clone needed — closure captures are indexed slots

**3b. Full VM method dispatch**
- Method resolution at compile time where possible (sealed classes)
- Runtime dispatch via vtable for non-sealed classes
- Remove `methods.rs` / `methods_mut.rs` interpreter dispatch

**3c. Remove env HashMap entirely**
- All variables in indexed local/closure slots
- Dynamic variables (`$*FOO`) via dedicated dynamic scope chain
- `EVAL` gets its own mini-interpreter (only case that needs AST walk)

### Phase 4: Advanced Optimizations (2027+)

**4a. VM instruction dispatch**
- Computed goto / threaded code for opcode dispatch
- Currently: `match` on ~100 OpCode variants (indirect branch prediction)
- Threaded code: direct jump to next handler (1 branch vs 2)
- Expected: 10-30% improvement on CPU-bound benchmarks

**4b. Value representation**
- NaN-boxing or tagged pointers for small integers (avoid heap allocation)
- Currently: `Value::Int(i64)` is 16 bytes enum + 8 bytes payload
- NaN-boxed: 8 bytes total for Int/Num/Bool/Nil
- Expected: 2x improvement on arithmetic-heavy benchmarks

**4c. JIT compilation**
- Cranelift or LLVM backend for hot loops
- Trace-based: detect hot bytecode sequences, compile to native
- Most ambitious optimization; depends on stable bytecode format

## Measurement Notes

- All benchmarks run on the same machine with `--release` build
- raku times include ~170ms startup overhead; mutsu startup is ~3ms
- For fair comparison on compute benchmarks, subtract startup from both
- `time` output parsing can be noisy; run 3x and take median for precise measurements
- bench-* files test more comprehensive scenarios; non-prefixed files test specific operations
