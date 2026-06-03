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

Two lines are printed — one for method dispatch, one for function dispatch:

```
[mutsu vm-stats] method-call opcodes=5 interpreter_fallbacks=2 (40.0% of opcodes)
[mutsu vm-stats] function-call opcodes=198 interpreter_fallbacks=190 (96.0% of opcodes)
```

- `*-call opcodes` — explicit `.foo(...)` / `foo(...)` calls executed via the
  `CallMethod*` opcode family (`exec_call_method_op` and its mut/dynamic
  variants) and the `CallFunc*` family (`exec_call_func_op`,
  `exec_call_func_slip_op`, `exec_call_on_value_op`, `exec_call_on_code_var_op`).
- `interpreter_fallbacks` — executions delegated from the VM into the
  interpreter: for methods, `Interpreter::call_method_with_values` /
  `call_method_mut_with_values`; for functions, `Interpreter::call_function` /
  `call_function_fallback`.

Note on functions: `call_function` dispatches both user subs (tree-walked /
on-the-fly compiled) *and* native-Rust builtins implemented in the interpreter
crate. So a function "fallback" means "left the VM's compiled/native dispatch and
entered the interpreter", which is exactly the coupling we want to remove, but it
is not always a tree-walk.

The two counters are measured at different layers, so `fallback` can exceed
`total` for code dominated by implicit coercions (`.Str`/`.Numeric`/`.Bool`)
that reach the interpreter without going through a method-call opcode. The
counters are process-global atomics, so worker threads (Promise / Proc::Async /
hyper) are included.

The instrumentation is gated behind a single cached boolean and is a no-op when
`MUTSU_VM_STATS` is unset.

## Baseline (2026-06)

method dispatch (PR #2571, strangler step 1):

| program | method-call opcodes | interpreter fallbacks |
|---|---|---|
| `roast/S02-types/array.t` | 9 | 2 (22%) |
| `roast/6.d/S32-str/sprintf.t` | 5 | 2 (40%) |
| `await (^4).map: { start { (1..100).map(*.Str).join.chars } }` | 413 | 5 (1%) |

function dispatch (strangler step 2):

| program | function-call opcodes | interpreter fallbacks |
|---|---|---|
| `roast/S02-types/array.t` | 12 | 10 (83%) |
| `roast/6.d/S32-str/sprintf.t` | 198 | 190 (96%) |
| `roast/S04-statements/if.t` | 49 | 17 (35%) |
| `fib(20)` (recursive user sub) | 21891 | 0 (0%) |

### What the baseline tells us

- **Method** execution fallback is *low*: most heavy collection work (`push`,
  `map`, `grep`, `elems`, …) compiles to dedicated native opcodes (e.g.
  `ArrayPush`) that run in the VM and never reach the `CallMethod` path.
- **Function** execution fallback is *high* (often 80–96%): builtins and `Test`
  functions (`is`/`ok`/`plan`/`sprintf`/`join`/…) are not in the VM's native
  function table, so they route through `Interpreter::call_function`. Recursive
  user subs, by contrast, compile cleanly (`fib(20)` → 0% fallback).

So the two biggest decoupling levers are:
1. the shared runtime **state** the VM borrows from the interpreter (~480
   `env()`/`env_mut()` calls; `ANALYSIS.md` §1.1–1.2), and
2. **function** dispatch — moving hot builtins/`Test` functions into the VM's
   native dispatch so they stop routing through `call_function`.

## Next steps (candidate strangler targets)

1. Move the hottest builtins / `Test` functions into the VM's
   `try_native_function` table so they execute without `call_function`.
2. Begin the state-ownership work (collapse the `locals`↔`env` dual store,
   `ANALYSIS.md` §1.2) so the VM stops borrowing the interpreter's env.

Record each step's before/after numbers in this file so the trend is visible.
