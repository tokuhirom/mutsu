# Performance Tracking

## Benchmark Suite

Benchmarks live in `benchmarks/`. Run all with:

```bash
cargo build --release
./benchmarks/run-all.sh
```

## Current Status (2026-05-19)

| Benchmark | mutsu | raku | ratio | notes |
|-----------|-------|------|-------|-------|
| fib(25) | 0.21s | 0.27s | 0.8x | recursive function calls (**faster than raku**) |
| int-arith | 0.11s | 0.25s | 0.4x | `for ^100000 { $sum += $_ * 3 + 1 }` (**faster than raku**) |
| string-concat | 0.015s | 0.21s | 0.07x | `$s ~= 'x'` × 10000 (**faster than raku**) |
| hash-access | 0.044s | 0.24s | 0.18x | 10K hash inserts + value iteration (**faster than raku**) |
| method-call | 1.01s | 0.29s | 3.5x | Point.distance-to × 10000 |
| array-ops | 0.14s | 0.30s | 0.5x | grep+map on 1000-elem array × 100 |

Note: raku times include ~170ms startup overhead. mutsu startup is ~3ms.

## Bottleneck Analysis

### Function calls (fib)
- Positional light-call path skips push_call_frame (no env save/restore)
- Main cost: VM instruction dispatch overhead (~15 ops per fib call × 242K calls)
- env HashMap mutations are cheap (Arc not shared → no deep clone)

### Hash operations
- hash-access is 10x slower — likely HashMap construction/iteration overhead
- Hash interpolation in strings (`"key-$_"`) adds parsing cost

### Method dispatch
- 5x for simple method calls with typed params
- Candidate resolution + type checking on each call

## Optimization History

### 2026-05-19: Reduce env mutations in light call path
- **Change**: Skip env HashMap writes during parameter binding in positional light calls; defer env writes in SetLocal when shared_vars is inactive
- **Effect**: 40% fewer env_mut calls (1.21M → 0.73M for fib(25))
- **Benchmark impact**: < 1% on fib (HashMap::insert on non-shared Arc is O(1))
- **Value**: Reduces unnecessary work; will matter more with larger env HashMaps and DESTROY timing

### 2026-05-19: Guard ensure_env_synced + restructure native method bypass
- **Change**: Skip ensure_env_synced call in method/function dispatch when locals_dirty is false; restructure try_native_method bypass checks to short-circuit by target type (avoid MRO walks for non-Instance targets)
- **Effect**: method-call benchmark 1.46s → 1.30s (**-12%**)
- **Value**: Avoids expensive type_matches_value/has_user_method calls for non-Instance method dispatch

### 2026-05-19: Skip NFC normalization for ASCII strings
- **Change**: Add `is_ascii()` fast path before NFC normalization in string concat (~, interpolation) and repetition (x) operators
- **Effect**: string-concat benchmark 0.69s → 0.015s (**46x speedup**, now 14x faster than raku); hash-access -6% from key construction
- **Value**: Eliminates O(n²) NFC normalization in ASCII string append loops

### 2026-05-19: Int arithmetic fast paths + attribute accessor fast path
- **Change**: Add inline Int+Int, Int-Int, Int*Int, Int<Int fast paths with checked overflow; add 0-arg Instance attribute accessor fast path
- **Effect**: int-arith 0.55s → 0.11s (**5x**, faster than raku); fib 1.05s → 0.21s (**5x**, faster than raku); method-call 1.30s → 1.26s (-3%)
- **Value**: Common integer operations are now as fast as direct Rust arithmetic + enum matching; attribute access skips full method dispatch

### 2026-05-19: Lazy method dispatch resolution + Num fast paths
- **Change**: Defer callsame/nextsame candidate resolution until actually called; add Num+/-/*/< fast paths
- **Effect**: method-call 1.16s → 1.01s (**-13%**); simple method 100K: 5.12s → 4.05s (**-21%**)
- **Value**: Eliminates 2 MRO walks + fingerprinting per method call for methods that don't use callsame

### 2026-05-19: Fast path for hash element assignment
- **Change**: Add `try_fast_hash_element_assign()` that skips 16+ edge-case HashMap lookups when no constraints/mixins/bindings apply
- **Effect**: hash-access benchmark ~2.4s → ~0.044s (**55x speedup**, now 5x faster than raku)
- **Value**: Common hash assignment pattern is now fast-pathed

## Next Steps

- [ ] VM instruction dispatch optimization (computed goto / threaded code)
- [ ] Value representation: inline small integers, avoid Box for common types
- [ ] Specialized arithmetic opcodes (Int+Int, Int<Int)
- [ ] Hash operation optimization (avoid string formatting in key construction)
- [ ] Method dispatch caching for monomorphic call sites
