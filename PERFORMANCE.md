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
| fib(25) | 1.12s | 0.27s | 4.1x | recursive function calls |
| int-arith | 0.55s | 0.25s | 2.2x | `for ^100000 { $sum += $_ * 3 + 1 }` |
| string-concat | 0.69s | 0.21s | 3.3x | `$s ~= 'x'` × 10000 |
| hash-access | 2.51s | 0.24s | 10.5x | 10K hash inserts + value iteration |
| method-call | 1.46s | 0.29s | 5.1x | Point.distance-to × 10000 |
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
- **Benchmark impact**: < 1% (HashMap::insert on non-shared Arc is O(1))
- **Value**: Reduces unnecessary work; will matter more with larger env HashMaps and DESTROY timing

## Next Steps

- [ ] VM instruction dispatch optimization (computed goto / threaded code)
- [ ] Value representation: inline small integers, avoid Box for common types
- [ ] Specialized arithmetic opcodes (Int+Int, Int<Int)
- [ ] Hash operation optimization (avoid string formatting in key construction)
- [ ] Method dispatch caching for monomorphic call sites
