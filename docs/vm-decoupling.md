# VM decoupling progress

Goal: make the bytecode VM execute everything natively and remove its remaining
dependence on the tree-walking `Interpreter` (see `ANALYSIS.md` section 1).

We take a **strangler-fig** approach: keep the interpreter fallback paths working,
but measure how often they are hit and shrink that number one change at a time.
Each step should be small, shippable, and move the metric in the right direction.

## Measuring fallback

Run any program with `MUTSU_VM_STATS=1`; a one-line summary is printed to stderr
at the end of the run:

```
$ MUTSU_VM_STATS=1 ./target/debug/mutsu roast/S02-types/array.t 2>&1 | grep vm-stats
[mutsu vm-stats] method-call opcodes=9 interpreter_fallbacks=2 (22.2% of opcodes)
```

- `method-call opcodes` — explicit `.foo(...)` calls executed via the
  `CallMethod*` opcode family (`exec_call_method_op` and its mut/dynamic variants).
- `interpreter_fallbacks` — method executions delegated to the tree-walking
  interpreter (`Interpreter::call_method_with_values` /
  `call_method_mut_with_values`) from the VM.

The two counters are measured at different layers, so `fallback` can exceed
`total` for code dominated by implicit coercions (`.Str`/`.Numeric`/`.Bool`)
that reach the interpreter without going through a method-call opcode. The
counters are process-global atomics, so worker threads (Promise / Proc::Async /
hyper) are included.

The instrumentation is gated behind a single cached boolean and is a no-op when
`MUTSU_VM_STATS` is unset.

## Baseline (2026-06, PR #1 of the series)

| program | method-call opcodes | interpreter fallbacks |
|---|---|---|
| `roast/S02-types/array.t` | 9 | 2 |
| `roast/S32-str/sprintf.t` | 5 | 2 |
| `await (^4).map: { start { (1..100).map(*.Str).join.chars } }` | 413 | 5 |

### What the baseline already tells us

The explicit method-call **execution** fallback rate is lower than expected:
most heavy collection work (`push`, `map`, `grep`, `elems`, …) compiles to
dedicated native opcodes (e.g. `ArrayPush`) that run in the VM and never reach
the `CallMethod` path. So the dominant VM↔Interpreter coupling is **not** method
execution delegation — it is the shared runtime *state* the VM borrows from the
interpreter (~480 `env()`/`env_mut()` calls and the package/type/readonly state;
see `ANALYSIS.md` §1.1–1.2).

## Next steps (candidate strangler targets)

1. Extend instrumentation to **function** dispatch (`call_function` /
   `call_function_fallback`) — likely a larger fallback surface than methods.
2. Instrument the dedicated native opcodes' delegation into `builtins`/`runtime`
   to see where execution actually lands.
3. Begin the state-ownership work (collapse the `locals`↔`env` dual store,
   `ANALYSIS.md` §1.2) so the VM stops borrowing the interpreter's env.

Record each step's before/after numbers in this file so the trend is visible.
