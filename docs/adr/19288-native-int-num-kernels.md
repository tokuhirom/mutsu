# ADR-19288: Native kernels — closed int/num regions lowered to typed native code, rerun from their start on the generic path when they cannot continue

- **Status**: Accepted (maintainer decision 2026-11-28; proposed 2026-11-28)
- **Issue**: [#19288](https://github.com/tokuhirom/mutsu/issues/19288) (carrier); stages
  [#19290](https://github.com/tokuhirom/mutsu/issues/19290) (sub kernels),
  [#19292](https://github.com/tokuhirom/mutsu/issues/19292) (loop kernels),
  [#19293](https://github.com/tokuhirom/mutsu/issues/19293) (method rows, not kernels)
- **Related**:
  - [ADR-0004](0004-jit-strategy.md): the Cranelift method JIT. Its "no guards, no deopt, no
    OSR exit" rule (§2.3) is kept: a kernel never resumes the interpreter in the middle of
    native code (D4).
  - [ADR-0110](0110-typed-resolved-ir-for-statically-typed-routines.md),
    [ADR-0112](0112-trir-completion-plan-for-beating-rakudo.md),
    [ADR-0116](0116-trir-native-lowering-measured-before-building.md): TRIR, the typed IR for
    routines whose types are *declared*. A kernel covers routines whose types are not declared
    but are observed at entry. §5 says why this is not TRIR's native lowering again.
  - [ADR-18244](18244-wide-raku-code-floor-staged-structural-plan.md),
    [ADR-18393](18393-slot-frame-without-env-overlay.md), #18267: the general call frame and
    plan. They make *every* call cheaper; a kernel removes the frame from the calls it admits.
    Neither replaces the other.

## 1. Context

### 1.1 The rows

Bench CI median since the reference moved to rakupp v5.9.0 (`+jit` series, 3 commits), and
whole-process instructions measured with callgrind on `main` `80ebc4c1`:

| bench | vs rakupp | vs rakudo | mutsu Ir | rakupp Ir | per unit |
|---|---:|---:|---:|---:|---|
| bench-fib | 12.0x | 0.81x | | | |
| bench-tak | 8.9x | 0.60x | 1,613.9M | 77.3M | |
| fib | 6.9x | 0.34x | 790.8M | 41.3M | ~3.2k Ir per call; rakupp ~130 |
| time-parts | 5.9x | 0.26x | 975.7M | 75.8M | ~9.7k Ir per iteration; rakupp ~660 |
| bench-mandelbrot | 4.8x | 0.19x | 493.3M | 37.8M | |
| int-arith | 3.3x | 0.11x | 197.3M | 15.6M | ~1.9k Ir per iteration; rakupp ~63 |
| num-arith | 2.4x | 0.14x | 270.9M | 35.1M | ~1.3k Ir per iteration |

mutsu is already ahead of rakudo on every row. The gap to rakupp is 7-20x in instructions.

### 1.2 What rakupp does

rakupp v5.9.0 is an AST tree-walker. Its machine-code backends (`--jit`, `--cnp`, `--exe`) are
opt-in and were not used by the bench. These rows are fast because of two tiers added between
v5.2 and v5.6 (`src/IntKernel.cpp`; CHANGELOG v5.2.0: "`fib` −97% (392 → 14 ms)"):

- **Integer kernels.** A sub with at most 6 `$` parameters that are untyped, `Int` or `int`,
  return type none or `Int`, and a body of closed integer arithmetic is compiled on its first
  call into a tree of int64 nodes, each a pointer to a template-specialised C++ function. Frames
  are a stack array, a recursive call is a direct C call, `return` is a flag. The checks that can
  change (wrapped subs, user-shadowed operators, every argument a plain Int) are made once per
  entry. Overflow, a zero divisor, the depth limit or the stack limit sets a bail flag, and the
  whole call is rerun on the generic path. After 8 bails in a row the sub stops trying.
- **Loop kernels.** `for` over an integer Range, `while`/`until` and C-style `loop`. The loop's
  scalars are copied into int64 slots (a Num as its bits), typed by what they hold at entry and
  re-checked on each entry, and written back when the loop ends.

Neither tier has inline caches, inlining or constant folding. rakupp's `Value` is a 32-byte
tagged struct. The speed comes entirely from running admitted code on machine words with no
per-operation dispatch, no per-call frame and no boxing.

### 1.3 What mutsu does instead

`sub fib($n) { if $n < 2 { return $n }; return fib($n - 1) + fib($n - 2) }` compiles to 18 ops,
and the JIT compiles the body. But each `CallFunc` calls the `call_func` helper, which runs the
whole sub-call machinery: `exec_call_func_site` (source line, `use fatal` scan, wraps sync, the
name caches), then `call_compiled_function_positional_light_impl` (native-stack guard, GC poll,
mark-context guard, compilation-unit switch, bind, slot window, `try_enter`, finalisation).
That is ~3.2k Ir per call. TRIR declines all three call rows: `$n` is untyped, and a call's
result is boxed.

For loops, `MUTSU_VM_STATS` on `int-arith` shows `jit entries=99901`: the interpreter's
`ForLoop` driver binds each element and enters the native body once per iteration. `$_` is read
by name (`GetGlobal("_")`, because mainline has no `$_` slot). Each `my` in a loop body runs
`SetVarDynamic` (500,001 executions in `time-parts`). `while` enters the condition and body
ranges separately (#18362).

### 1.4 Why the general frame work cannot close this

ADR-18244 stage D and #18267's goal is a sub call at ≤ 2k Ir. That would leave fib at ~10x
rakupp's instructions. The frame is not wrong; it is general. It has to serve a callee that
declares dynamics, observes `CALLER::`, returns a container, is wrapped later, and binds any
signature. A call between two closed-arithmetic subs needs none of that. What it needs is a
machine call with machine arguments. The same goes for a loop body that only adds integers.

## 2. Decision

**A native kernel is a typed Cranelift lowering of a bytecode region whose every value is a
machine `int` (and, from stage 3, a machine `num`).** It is admitted by a closed op list (D1),
entered only under per-entry guards (D2), and keeps its values in registers. It calls other
kernels directly (D3). When it meets a condition it cannot express, it **bails**, and **the
region is rerun from its start on the generic path** (D4). An admitted region has no effect
outside its own slots, so the rerun cannot be observed.

Two region kinds, sharing one lowering:

- a **sub kernel**: the whole body of a sub, with its parameters as i64 arguments and its
  result as an i64;
- a **loop kernel** (stage 2): a counted `for` over an int `Range` or a `while`, with the
  loop's scalars as typed slots that are loaded at entry and written back at exit.

### D1. Admission is a closed op list over the compiled bytecode

Admission is decided from the `CompiledCode` and the routine's signature, never from an
analysis that could miss a case. An op not on the list declines the region, so a new op can only
make kernels rarer.

**Sub kernel signature.** 1-6 parameters, all positional, mandatory, `$`-sigil, scalar, not
`is rw`/`is raw`/`is copy`, no default, no `where`, no sub-signature. Each one is untyped or
nominally `Int`. Return spec none or `Int`. Not a multi, not a method, not `is cached`, no
phasers, no `CALLER`/dynamic/`state`/closure use (the `CompiledCode` flags already record these).

**Op list (stage 1, int only).** Each op's typing is over an abstract stack of i64 / bool
entries, checked by a single forward pass that rejects a join whose depths or kinds disagree.

| op | admitted when | lowered to |
|---|---|---|
| `GetLocal(s)` | `s` is a parameter or a kernel local | the SSA variable |
| `LoadConst(k)` | `k` is an `Int` in i64 range | `iconst` |
| `Add`, `Sub`, `Mul` | both operands i64 | checked arithmetic; overflow bails |
| `IntDiv`, `IntMod` | both operands i64 | floored `div`/`mod`; a zero divisor bails |
| `NumLt`/`NumLe`/`NumGt`/`NumGe`/`NumEq`/`NumNe` | both operands i64 | `icmp` → bool |
| `JumpIfFalse`, `JumpIfTrue`, `Jump` | a bool (or i64, tested ≠ 0) on top | native branch |
| `Return`, falling off the end | an i64 on top | return |
| `ContainerizePair` | the operand is i64 | nothing (it is the identity on a non-Pair) |
| `SetVarDynamic` + `SetLocalDecl` | non-dynamic, `Fresh`, slot-only, no type follows | slot store |
| `CallFunc` | positional args only, no `arg_sources`, the callee resolves (D3) to an admitted sub kernel | direct call |

Arithmetic on two i64s is the same operation the generic path does on two `Int`s that do not
overflow, so the kernel's answer equals the generic answer whenever it does not bail. The floored
`div`/`mod` call the one implementation in `src/builtins/arith` (ADR-0118); they are not
re-derived in CLIF.

### D2. Entry guards, once per entry

A kernel is entered from the light call path only when:

1. every argument is a plain `Int` value that fits in i64 (not a `Bool`, not an allomorph, not
   a container, not a big integer). A `Bool` argument is refused because `fib(True)` returns
   `True` on the generic path;
2. `USER_INFIX_DECLS` is zero: no user infix operator exists anywhere in the process (the latch
   Tier B already relies on);
3. the process-wide function-resolution generation equals the one the kernel was compiled
   against (D3). Wraps, new subs, `EXPORT` overrides and re-registration all move it;
4. the profiler is off (a sampled kernel frame would have no line).

A guard that fails runs today's path. None of these can change while a kernel runs, because a
kernel runs no code that could change them, so they are not re-checked on the calls a kernel
makes to other kernels.

### D3. Calls between kernels resolve once, at kernel compile time

A `CallFunc` inside a kernel is resolved when the kernel is compiled, through the same
name-keyed resolution cache the light call path fills (`pos_light_call_cache`), keyed by the
callee name and the kernel's package, at the current function-resolution generation. If the
cache has no entry, or the entry's routine is not itself admitted, the region declines and is
retried at a later call (the generic calls in between fill the cache). A self-call is a direct
`call` to the function being built; a call to another kernel loads that kernel's entry pointer
and calls it indirectly. The kernel records the generation it resolved under; D2.3 checks it.

The native ABI is `extern "C" fn(ctx: *mut KernelCtx, a0..an: i64) -> i64`. `KernelCtx` holds
the bail flag, the call depth and a poll countdown. After each call the caller tests the bail
flag and, if set, returns at once.

### D4. Bail and rerun, never resume

A kernel bails, by setting the flag and unwinding its native frames, on:

- an overflowing `+`, `-`, `*`;
- a zero divisor in `div` or `mod`;
- a call depth past a fixed bound (well under the interpreter's own recursion limit), so deep
  recursion is handled, and reported, by the generic path;
- a GC or STW request. Every kernel call decrements the poll countdown; at zero, a cold helper
  runs the ordinary call-boundary poll (`vm_poll::poll`). A kernel frame holds no `Value`, so
  it can park and a collection can run under it; the poll only bails if the safepoint asks the
  thread to unwind.

On a bail the entry point discards the kernel's result and runs the same call on the generic
path, with the same arguments, which are still on the value stack. The generic path then does
whatever the kernel could not: promote to a big integer, throw the division-by-zero exception,
raise the recursion error. Because an admitted region reads only its arguments and constants and
writes only its own slots, the rerun sees exactly the state the kernel saw.

A routine that bails on 8 consecutive entries is marked and never entered again, the way rakupp
does it, so a routine that always overflows (factorial of a large argument) pays for the attempt a
bounded number of times.

This is not a deopt. Nothing is reconstructed from the middle of native code: the generic path
starts the region from its beginning, as if the kernel had never run.

### D5. Loop kernels (stage 2)

A counted `for` over a `Range` whose endpoints are `Int`s, and a `while`, whose body and
condition are in the op list extended with: reads and writes of the enclosing frame's slots, the
loop parameter or `$_` as an i64 slot, and `num` arithmetic once stage 3 lands. The kernel is
invoked by the loop driver with the current iteration. At entry it reads each slot it uses; a
slot whose value is not a plain `Int` (or `Num`, stage 3) refuses the kernel for this execution of
the loop. At exit it writes the slots back as `Int`/`Num` values.

Rerun for a loop is per iteration. The kernel keeps the slots' values at the start of the
iteration it is executing. On a bail it writes those back and returns the iteration number to the
driver, which continues generically from that iteration. The iteration that bailed is the one the
generic path reruns from its start, so again nothing resumes mid-region.

A loop kernel is entered at most once per execution of the loop, so its body runs with no
interpreter dispatch at all, which also retires #18362's double entry for the loops it admits.

### D6. Where kernels live

The kernel entry point and its bail counter are one more field of the JIT state the
`CompiledCode` already has (`code.jit`). Compilation is triggered by the same call counter as
the bytecode JIT. With `MUTSU_JIT=off`, or without the `jit` feature, kernels are off and every
call takes today's path. `MUTSU_KERNEL=off` disables kernels alone, so a test can compare the two
paths.

### Amendment A1 (proposed 2026-11-28): array elements, C-style loops, bit ops

Campaign B (#19294) asked loop kernels to serve the index benchmarks, whose loops read
`@a[$i +& 15]` and store `@a[$i] = ...` inside C-style `loop (...; ...; $i++)` statements. The
amendment:

- **Element reads and stores are calls of campaign B's helpers** (`vm_kernel_elem`). A helper
  checks the container on every access and declines when it no longer holds a plain `Array` of
  the expected kind. A decline bails the iteration like an overflow does. The checks that only
  code can change (thread clone, user `postcircumfix:<[ ]>`, readonly, `is default`, constraints,
  `:=` pairs) are made once at entry. A read array's element kind is the kind of its first element
  at entry and is part of the variant key.
- **A store is the one outside effect a region may have, and D4's rerun argument is extended
  rather than broken.** A store either writes or declines with no mutation. The region never
  reads an array it stores into: a slot both read and stored declines at analysis time, and two
  names for the same array decline at entry by node identity. So an iteration that bails after a
  store and is rerun recomputes every stored index and value from the same snapshot of the carried
  slots. It writes the same elements again, an append included, because the rerun's index is then
  in range.
- **C-style loops** run as a `while` whose body is the body followed by the step. `++`/`--` on a
  carried `Int` slot are admitted, with their readonly check made at entry. `+&`, `+|`, `+^` and
  `%` are admitted on two `Int`s.

## 3. Consequences

- The rows of §1.1 should end up within a small factor of rakupp, or ahead of it: a kernel call
  is a few dozen machine instructions against rakupp's ~130.
- The bytecode is the only input. A kernel needs no AST, so precompiled modules get kernels too.
- `MUTSU_JIT=off` (the bench's plain series) does not get kernels. That series stays the
  interpreter baseline.
- **Testing.** Every kernel test runs its program three ways: kernels on from the first call
  (`MUTSU_JIT_THRESHOLD=1`), kernels off (`MUTSU_KERNEL=off`), and `MUTSU_JIT=off`, and requires
  identical output. The bail cases each get a test: overflow to a big integer, division by zero
  (the exception and its message), deep recursion, a `Bool`, `IntStr` and big-integer argument, a
  user infix operator declared after the kernel was built, a sub wrapped after the kernel was built,
  and a callee redefined by `EVAL`.
- A kernel is invisible to a backtrace. That is sound because nothing inside a kernel can throw:
  every exceptional condition is a bail, and the exception is raised by the rerun.

## 4. Risks

| risk | handling |
|---|---|
| A semantic the op list does not see (a user `infix:<+>`, a wrapped callee, an `EXPORT`-installed override) | D2's guards are the same latches Tier B and the light call caches already trust. Anything that changes resolution moves the generation |
| Overflow semantics | i64 checked arithmetic; overflow bails, and the generic path promotes |
| Native-stack exhaustion | the depth bound bails long before the native stack is at risk; the generic path has its own guard (ADR-0100) |
| GC starvation in a long kernel | the poll countdown runs the ordinary safepoint |
| A kernel that keeps bailing | 8 consecutive bails disable it for that routine |
| Compile cost | one Cranelift function per admitted routine, compiled once it is hot, like the bytecode JIT |

## 5. Alternatives rejected

- **Make the general sub call cheaper and stop there** (#18267's ≤ 2k Ir). Necessary for code
  that is not admitted, but still ~10x rakupp on these rows (§1.4).
- **TRIR native lowering (ADR-0116).** TRIR admits only routines whose types are declared, so
  none of these rows; and ADR-0116 measured its op-by-op lowering as slower than the TRIR switch
  loop because every op that was not inline paid a shim call. A kernel admits only what it can
  emit inline, and declines everything else, so it has no shim calls at all.
- **Speculation with deoptimization** (guards inside the region, OSR exit to the interpreter
  mid-region). Rejected by ADR-0004 and still rejected: it needs interpreter-state reconstruction.
  Rerun from the region's start needs none, because an admitted region has no outside effects.
- **A rakupp-style tree of node functions.** Cranelift already gives machine code, register
  allocation and direct calls, so a node tree would only be slower.

## 6. Open questions

- Whether stage 2's per-iteration snapshot is cheaper as copies in registers or as a write log.
  Measured in stage 2.
- Whether kernels should admit a `Str` slot (rakupp's loop kernels have string slots). Not needed
  by any row of §1.1.
