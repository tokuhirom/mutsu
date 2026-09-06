# `hash-access` / `bench-hash` are ~12% slower than their 2026-08-31 baseline

The bench CI shows `hash-access` and `bench-hash` (both series, JIT on and off)
sitting at a raku ratio of 0.19 from 2026-09-01 onward, against 0.17 through
late August. That is a real ~12% regression, and unlike the `bench-string`
regression fixed alongside this note it does **not** have a single hotspot.

## Measurement

Callgrind, `MUTSU_JIT=off`, `benchmarks/hash-access.raku`:

| build | Ir |
| --- | --- |
| a0932728 (2026-08-31) | 237,598,221 |
| beb00070 (2026-09-06) | 265,876,144 |

That is 1.119x, matching the bench CI ratio almost exactly, so it reproduces
deterministically and is not runner noise. `bench-hash` moves the same way
(287.5M -> 313.3M, 1.09x).

The profile shape, however, is unchanged. Every frame grew by roughly the same
proportion:

| frame | pre | post |
| --- | --- | --- |
| `exec_for_loop_int_range` | 118.9M (50.1%) | 134.9M (50.7%) |
| `exec_for_loop_body` | 86.4M (36.4%) | 96.6M (36.3%) |
| `exec_index_assign_expr_named_op` | 73.0M (30.7%) | 83.4M (31.4%) |
| `exec_index_assign_expr_named_op_seeded_inner` | 49.9M (21.0%) | 59.7M (22.4%) |
| `__rust_dealloc` | 24.1M (10.2%) | 27.0M (10.2%) |

`exec_index_assign_expr_named_op_seeded_inner` grew the most in relative terms
(+19.5%), which is where to start, but nothing here is a smoking gun the way
`reset_capture_env_vars` was for `bench-string`.

## Why this is a perf ticket and not a quick fix

A diffuse +12% across the whole hash store/read path most likely means either a
small cost was added to something every iteration touches (an extra check in
the named-index assign path, a Value/container shape change that costs one more
indirection), or several unrelated small costs landed in the same week. Either
way it needs a bisect over the 2026-08-31..2026-09-01 range with callgrind Ir
as the signal — cheap and deterministic, but a few release builds' worth of
wall clock, which is why it is not folded into the `bench-string` fix.

## Reproduce

```
cargo build --release
MUTSU_JIT=off valgrind --tool=callgrind --callgrind-out-file=/dev/null \
    --cache-sim=no --branch-sim=no ./target/release/mutsu benchmarks/hash-access.raku
```

Ir is stable to within a few thousand instructions across runs, so a bisect can
compare single runs rather than medians. Per `todo/README.md` this is a
`todo/perf/` item: mutsu is correct here, just slower, and the next step is
profiling.
