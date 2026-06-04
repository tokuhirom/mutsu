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

## Per-name fallback histogram (2026-06)

`MUTSU_VM_STATS=1` now also prints the **top function names that fell back**, so
decoupling work can target the highest-count builtin instead of guessing:

```
[mutsu vm-stats] function-fallback by name (top N): sprintf=183 EVAL=1 plan=1 ...
```

This immediately showed that `sprintf` alone was **183 of sprintf.t's 190**
function fallbacks (96%). The rest were `Test` functions (`is`/`plan`/`subtest`/
`is-deeply`) and `EVAL` — which genuinely need interpreter state.

## Step 3 — nativize `sprintf` / `zprintf` (2026-06)

`Interpreter::builtin_sprintf` was already pure (it only calls the free
functions in `runtime::sprintf`; it never touches `&self`), yet `sprintf` lived
in the interpreter's `call_function` match, so every call fell back. Moved it
into the VM's `native_function` table (`builtins::functions::native_sprintf`),
mirroring the interpreter exactly (single-array-arg flattening, directive/type
validation, error propagation). The only case that still falls back is a
**Junction format argument** (`.Str`-per-eigenvalue threading needs interpreter
rendering) and object args (already excluded by `try_native_function`).

| program | function fallback before | after |
|---|---|---|
| `roast/6.d/S32-str/sprintf.t` | 190 / 198 (96.0%) | 7 / 198 (**3.5%**) |
| realistic scripting probe (`tmp/probe-script.raku`: map/join/split/sprintf/printf/sort/sum) | — | **0 / 2000 (0.0%)** |

So for non-test scripting code, function dispatch is now effectively fully
decoupled; the residual roast-test function fallback is dominated by `Test`
functions (TAP state) and `EVAL`, which are a separate, harder effort.

## Method per-name histogram + step 4 — accessor reads on the mut opcode (2026-06)

`MUTSU_VM_STATS=1` now also prints a per-name **method**-fallback histogram. It
showed `method-call.raku`'s 60% method fallback was `new=10001 x=10000 y=10000`
— i.e. the auto-generated attribute **accessor reads** `.x`/`.y`, not just the
constructor.

Root cause: a method call on a *variable* (`$obj.x`) compiles to `CallMethodMut`
(for potential invocant write-back), but the 0-arg attribute-accessor fast path
existed only in the non-mut `CallMethod` handler. So every accessor read on a
lexical fell back to the interpreter. Extracted that fast path into a shared
`VM::try_fast_accessor_read` and called it from both opcodes. It is a pure read
(no invocant write-back), gated on `has_public_accessor` so a private attribute
(`has $!secret`, stored under the `secret!` key) is **not** leaked — it falls
through and the interpreter denies it (this also fixes a latent private-leak in
the old non-mut fast path).

| program | method fallback before | after |
|---|---|---|
| `method-call.raku` | 30001 / 50002 (60.0%) | 10001 / 50002 (**20.0%**) |
| `bench-class` | 9000 / 65000 (13.8%) | unchanged (no accessor reads in its hot loop) |

The remaining method fallback in both is `.new` (the default constructor),
which is a larger nativization (BUILD/TWEAK/attribute init/type coercion) and is
the clear next target.

Pre-existing, out of scope: a typed `@.items` / `%.foo` accessor read throws
`No such method` on `main` too (the interpreter accessor path does not handle
typed-container accessors); `try_fast_accessor_read` returns the value when the
attribute is present, so it does not regress this — it falls through identically.

## Step 5 — native default construction (`Foo.new(...)`) (2026-06)

`.new` was the dominant remaining method fallback (`new=10001` in
`method-call.raku`, `new=9000` in `bench-class`). The interpreter already had a
**fast path** for default-constructing a simple user class (no
BUILD/TWEAK/BUILDALL/custom-new, only public `$`-sigiled attributes with no
required/type/where constraints, no native methods) — but it lived *inside*
`dispatch_new`, reachable only after `Foo.new` had already routed through the
VM→`call_method_with_values`→generic-dispatch→`dispatch_new` chain (method
dispatch frame push/pop, samewith context, parametric-role/array/hash/enum
guards). So every such `.new` counted as an interpreter fallback.

Extracted that fast path into `Interpreter::try_native_default_construct`
(eligibility = `is_native_default_constructible`, construction =
`build_native_default_instance`) and call it **directly** from the VM's
`try_compiled_method_or_interpret` / `_mut_` variants before recording a
fallback. `dispatch_new` now calls the same helper, so behavior is identical —
this is a relocation of the dispatch decision into the VM, not a new code path.

Eligibility was also generalized from "parents are only Any/Mu/Cool" to "every
user class in the MRO is simple" so a plain inheritance chain (`Dog is Animal`)
qualifies too. Construction stays pure data (named args → attributes, evaluate
attribute defaults); anything needing user code (BUILD/TWEAK, typed/required
attrs, custom `new`) is ineligible and falls through unchanged.

| program | method fallback before | after |
|---|---|---|
| `method-call.raku` | 10001 / 50002 (20.0%) | 0 / 50002 (**0.0%**) |
| `bench-class` | 9000 / 65000 (13.8%) | 0 / 65000 (**0.0%**) |

Both OO benchmarks now have **zero** method-call interpreter fallback. The
residual `.new` fallback that remains in the wild is for classes with custom
constructors / BUILD / TWEAK / typed attributes, whose construction genuinely
runs user code — the next, larger step is moving the class registry + BUILD/
TWEAK execution to VM-owned data (`ANALYSIS.md` §1.1).

## Next steps (candidate strangler targets)

1. `Test` functions: would need the TAP `TestState` reachable from the VM rather
   than only the interpreter — large, defer.
2. Begin the state-ownership work (collapse the `locals`↔`env` dual store,
   `ANALYSIS.md` §1.2) so the VM stops borrowing the interpreter's env.

Record each step's before/after numbers in this file so the trend is visible.
