# Five more per-call helpers stopped costing a call to answer "nothing to do"

A second pass in the spirit of `news/2026-09/hot-path-noop-guards-are-now-inlined.md`.
`perf` on `bench-fib` showed a cluster of small out-of-line symbols on the
positional-light dispatch path, each of which answers "no" for essentially every
call and costs more to *reach* than to compute:

| helper | self time | what it decides |
| --- | ---: | --- |
| `record_cf_deprecation` | 1.32% | is this routine deprecated? (almost never) |
| `unwrap_varref_value` | 1.44% | is this argument a varref capture? |
| `arg_is_container_value` | 0.90% | is this argument an `@`/`%` variable? |
| `decode_arg_slip_positions` | 0.72% | does this call site have an argument-source table? |
| `enter_routine_package` | 0.72% | is this routine's package non-`GLOBAL`? |

Each now keeps only its cheap test at the call site and outlines the rest:

- `record_cf_deprecation` inlines the `is_some` test; the env lookup and
  `String` build move into a `#[cold] #[inline(never)]` half, so they neither
  cost a call nor reserve stack slots in
  `call_compiled_function_positional_light_at`'s already-large frame.
- `enter_routine_package` inlines only the emptiness test and the 6-byte
  `"GLOBAL"` compare; the `"::&"` substring search and the two `String` clones
  live in the outlined half.
- `decode_arg_slip_positions` inlines down to the `Option<u32>` test; the
  constant-pool scan that builds the position vector stays outlined.
- `unwrap_varref_value` / `unwrap_var_ref_value` / `arg_is_container_value` are
  a single tag test each, so they are simply `#[inline]`.

Measured cross-build against `main`, release, pinned to one core (retired
instructions -- the layout-insensitive oracle, since inlining cannot be toggled
by an env switch the way the other changes in this series were):

| benchmark | retired instructions |
| --- | ---: |
| `bench-fib` | **-2.37%** |
| `bench-tak` | **-1.93%** |
| `poly-call` | -0.14% |
| `bench-ctor` | -0.05% |
| `bench-mandelbrot` / `bench-string` | -0.01..0.00% |
| `bench-class` | +0.10% |
| `method-call` | +0.32% |

`method-call` is the one row that moves the wrong way; the method path takes a
different dispatch route, so the inlined argument helpers grow its code without
removing calls from its hot loop. At +0.3% against -2.4% on the call-heavy
benchmarks it is a clear net win, but it is real, not noise -- worth knowing if
a later change makes the method path the bottleneck.
