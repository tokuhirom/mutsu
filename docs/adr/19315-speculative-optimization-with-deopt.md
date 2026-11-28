# ADR-19315: Speculative optimization with deoptimization is not needed for the remaining rows; build it only when a measured workload requires it

- **Status**: Proposed (2026-11-28)
- **Issue**: [#19315](https://github.com/tokuhirom/mutsu/issues/19315); campaign A carrier
  [#19288](https://github.com/tokuhirom/mutsu/issues/19288)
- **Related**:
  - [ADR-0004](0004-jit-strategy.md) §2.3: no guards, no deopt, no OSR exit. This ADR keeps that
    rule for now.
  - [ADR-19288](19288-native-int-num-kernels.md): native kernels, the Phase 1 that landed instead
    of TRIR lowering. They rerun a region from its start instead of deoptimizing.
  - [ADR-18393](18393-slot-frame-without-env-overlay.md), #18267: the O2 lean call frame and call
    plan.

## 1. Context

On 2026-11-28 the maintainer staged campaign A in phases:

- **Phase 1**: unboxed native code for the call and arithmetic rows.
- **Phase 2**: general speculative optimization with deoptimization. This means type feedback,
  guards with frame-state maps, invalidation, inlining with multi-frame reconstruction and a
  deopt-stress CI mode, estimated at 10-20k lines.

The maintainer asked that Phase 2 not be started. Instead, this ADR should use the Phase 1 results
to state four things:

- the remaining gap per row;
- which rows only speculation can reach;
- its cost and risks, above all silent wrong answers after a deopt;
- how it would be tested.

### 1.1 What Phase 1 became

TRIR → Cranelift lowering was the staged Phase 1, but it could not reach these rows:

- TRIR admits none of the call rows: their parameters are untyped, and call results are boxed.
- ADR-0116 measured op-by-op TRIR lowering as slower than the TRIR switch loop.

Phase 1 became ADR-19288's native kernels instead. A kernel is a closed-op-list region lowered to
machine words. On a bail it is rerun from its start on the generic path. No interpreter state is
ever rebuilt from inside native code. Landed:

| PR | slice |
|---|---|
| #19303 | sub kernels (fib, tak) |
| #19307 | loop kernels (`for` over an int range, `while`) |
| #19309 | nested loops and `.Num` in loop kernels |
| #19313 | loop kernels call sub kernels |
| #19312 (in flight) | Rat fast path in the arithmetic opcodes |

### 1.2 The rows now

Local measurements, minimum of 5 runs, release build on the 4-core container. mutsu is the build of
`perf/19312-plain-rat-fast-path`; debug-guard is from `perf/19292-loop-calls-sub-kernels` (#19313).
The bench CI median replaces this table once those rows exist.

| bench | mutsu | rakupp v5.9.0 | rakudo 2026.11 | vs rakupp | vs rakudo | before Phase 1, vs rakupp (CI) |
|---|---:|---:|---:|---:|---:|---:|
| bench-fib | 12.6 ms | 14.6 | 263 | 0.86 | 0.05 | 12.0 |
| bench-tak | 12.9 | 14.6 | 277 | 0.88 | 0.05 | 8.9 |
| fib | 11.2 | 8.3 | 232 | 1.35 | 0.05 | 6.9 |
| time-parts | 9.1 | 11.9 | 289 | 0.76 | 0.03 | 5.9 |
| bench-mandelbrot | 8.3 | 7.5 | 280 | 1.11 | 0.03 | 4.8 |
| int-arith | 7.2 | 6.0 | 219 | 1.20 | 0.03 | 3.3 |
| num-arith | 7.8 | 7.9 | 183 | 0.99 | 0.04 | 2.4 |
| debug-guard | 10.0 | 51.1 | 265 | 0.20 | 0.04 | 1.4 |
| poly-call | 37.5 | 16.9 | 326 | 2.22 | 0.11 | 2.0 |
| method-call | 63.8 | 34.9 | 261 | 1.83 | 0.24 | 1.5 |
| bench-class | 82.9 | 45.0 | 340 | 1.84 | 0.24 | 1.5 |
| bench-ctor | 182 | 89.3 | 516 | 2.04 | 0.35 | 2.2 |

How to read this table:

- The fib, int-arith and mandelbrot rows are around 8-11 ms per process. Startup and kernel
  compilation are most of that.
- The table was measured with a gate running beside it, so the small rows read high. On a quiet
  run fib was 9.7 ms against 8.7 ms.
- The arithmetic and call rows are at or near rakupp. Every row is far ahead of rakudo.
- **What is left is the four object-oriented rows, at 1.8-2.2x rakupp.**

### 1.3 What the OO rows are made of

poly-call, before #19312: 322.7M Ir.

- Rat arithmetic: `exec_add_op` was 23.6% of the run. #19312's fast path takes the run to 238.9M
  Ir (−26%) and the wall time from 49.2 ms to 35.5 ms.
- The rest is ~9.6k Ir per `.area` call through `call_compiled_method_lean_in_unit`, with
  `$!r`/`$!w` reads in the body.

method-call: 469.3M Ir for 10,000 iterations, ~46k Ir per iteration.

- `distance-to(Point $other)` costs ~23k Ir per call, inclusive (`call_compiled_method_fast_in_unit`).
  That includes:
  - the nominal type check of `Point $other`, ~3.5k (`type_matching`);
  - the general fast binder;
  - two accessor calls;
  - `**` and `.sqrt`.
- `Point.new` (`try_native_default_construct`) is ~2.8k.
- Each iteration's loop and frame bookkeeping is the remainder.

rakupp has no inline caches, no inlining and no speculation. Its attributes live in a hash table
(`AttrTable` wraps a `ValueHash`). Its method lookup walks the class chain by name on every call
(`ClassInfo::findMethodForCall`). Its OO rows are faster than mutsu's only because its generic
call is cheap. **The gap is the cost of mutsu's general method call (call-site layers plus frame,
ADR-18393 §1.1: ~3.8k + ~4.8k Ir for `$o.m0`), not the absence of speculation.**

## 2. Decision (proposed)

**Do not build general speculation with deoptimization now.** None of campaign A's rows needs it
to reach rakupp, and every row is already ahead of rakudo. Close the OO rows without deopt:

1. **The O2 lean call frame and call plan** (#18267, ADR-18393 stages 2-4), driven by #19293's
   targets. ADR-18393 D4 puts `$o.m0` at ≤ 5.5k Ir (from 8.7k) with today's call-site layers, and
   ≤ 3k with O2's. On method-call and bench-class that removes 35-65% of each call.
2. **Method kernels: an ADR-19288 extension, not a deopt.** A method whose body is closed
   arithmetic over its parameters and its receiver's scalar attributes is lowered like a sub
   kernel. It is entered under one more per-entry guard:
   - the receiver's class is the class the kernel was compiled for;
   - no augment, MOP change or wrap since then (the registry generation and the existing latches).

   The kernel reads attributes through the instance cell at entry and returns a machine value. A
   bail reruns the call generically, exactly as for subs. This covers poly-call's `area` and
   method-call's `distance-to` minus its typed parameter. rakupp's v5.3 kernels take "pure
   methods" in the same way.
3. **Nominal parameter checks on the fast binder** (`Point $other` at ~3.5k Ir per call) become a
   memoized per-(site, class) answer. This belongs with O2's call plan.
4. **Rat values in kernels**, if a row still needs them after #19312.

Revisit Phase 2 only when a measured workload needs it, and write a new ADR then. Such a workload
is a row or an ecosystem distribution whose remaining cost is code that none of the above can
admit, for example:

- megamorphic call sites whose bodies must be inlined across their receivers;
- closures captured and called inside hot loops with mutation;
- long-running dynamically typed code where rakudo's spesh, not rakupp, is the reference that
  mutsu loses to.

## 3. If Phase 2 were built: cost, risks, tests

This section records the estimate the decision weighs, so a later ADR can start from it.

**Cost.** 10-20k lines across these pieces:

- type feedback (per-site observed receiver classes and operand kinds);
- a speculative compiler tier over the bytecode with guards;
- frame-state maps at every guard, enough to rebuild one or more interpreter frames from native
  registers;
- deopt entry into the middle of the interpreter loop (`run_range_from` at an arbitrary ip with a
  rebuilt value stack, locals and pending call state);
- invalidation of compiled code on sub redefinition, multi candidate addition, `wrap`, `augment`,
  MOP `add_method`/`^compose`, `EVAL` and module load, including code running on other threads
  (safepoint-based patching or epoch checks);
- inlining with multi-frame reconstruction, so backtraces, `callframe`, `CALLER::` and `&?ROUTINE`
  see the inlined frames after a deopt;
- a deopt-stress CI mode.

The kernels' rerun-from-start approach needs only one of these: the guards, checked once per
entry.

**Risks, worst first.**

1. **Silent wrong answers after a deopt.** A frame-state map that omits a live value, records a
   stale one, or rebuilds a container where the interpreter had a value produces a program that
   continues with wrong data and no error. Unlike a crash, nothing fails. Every escaping effect
   between guards (a store, a call, an `is rw` write-back) must be exactly once.
2. **Incomplete invalidation.** One missed source of change (an `EXPORT` override, a `require` at
   run time, a MOP call from a module, a `.wrap` on a method) leaves optimized code calling the old
   target. mutsu's history with name-resolution caches (#8746, #9073) shows how many sources there
   are.
3. **Threads.** Invalidating code another thread is running needs that thread to reach a
   safepoint and deopt there. That is a new interaction with the cooperative STW, and with
   ADR-12814's memory-safety rule.
4. **Maintenance.** Every new opcode needs a frame-state description. A wrong one is risk 1.
   Debuggability drops: a bug appears only on the deopt path.
5. **Compile cost.** Cranelift compilation is already 12% of poly-call's run (38.8M Ir). A
   speculative tier compiles more, and recompiles after invalidation.

**Test strategy, if it is built.**

- **A deopt-stress mode**: every guard fails on its first check, and every compiled region deopts
  at every guard once. Run `t/` and the whitelisted roast under it in CI, like `jit-stress`.
- **Three-way differential runs** on every fixture: interpreter, kernels, speculative tier.
  Require byte-identical output.
- **An invalidation matrix test**: each source of change (redefinition, wrap, augment, multi
  addition, `EVAL`, module load, MOP) applied mid-loop to a hot optimized call, single- and
  multi-threaded.
- **A debug-build frame-state verifier**: at each deopt, rebuild the frame and also run the region
  to that point in the interpreter, then compare the two states.
- **Ecosystem ledger parity** before each stage lands.

## 4. Consequences

- ADR-0004's no-deopt rule stands. ADR-19288's rerun-from-start contract is the way native code
  leaves its fast path.
- The OO rows are worked through O2 (#18267) and method kernels. Method kernels need their own
  short ADR or an ADR-19288 amendment, because they add a receiver-class guard.
- If those land and a row still trails, this ADR's §3 is the starting point for a Phase 2 ADR,
  with that row's measurement as its context.

## 5. Alternatives considered

- **Start Phase 2 now.** Rejected for the reasons above. None of the rows needs it. Risk 1 is
  permanent, and the cost is 10-20k lines that O2 and method kernels make unnecessary for these
  rows.
- **A limited speculation tier: guarded inline caches with deopt only at call boundaries.** It
  still needs frame-state maps at every call site that is inlined, which is most of the risk. Method
  kernels get the same effect for the bodies they admit with no frame reconstruction at all.
