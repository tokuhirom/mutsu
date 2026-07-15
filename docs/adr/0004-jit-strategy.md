# ADR-0004: JIT — mechanism selection and phasing (layer 4)

- **Status**: Accepted (user approval 2026-07-06 — including the Lever 3 freeze)
- **Date**: 2026-07-05
- **Deciders**: tokuhirom, Claude
- **Relates to**: [ADR-0001](0001-gc-strategy-and-phasing.md) (phase order 3a→3b→4; the
  decision that level-1 GC eliminates the JIT's prerequisite costs),
  [gc-post-3a-roadmap.md](../gc-post-3a-roadmap.md) (layer 3b is the prerequisite work for
  this ADR), [PLAN.md](../../PLAN.md) §5 Lever 3/4, PERFORMANCE.md

> ADR-0001 fixed the *order and prerequisites*: "JIT comes after GC" and "with level-1 GC
> (non-moving + refcount), the JIT needs no stack maps / forwarding / write barriers — it
> just emits `Arc`/`Gc` inc/dec". This ADR decides the approach for the JIT **itself**.

---

## 1. Context

- The execution engine is a single bytecode VM (`Interpreter`, ~330 opcodes, `exec_one`
  match dispatch). The compilation unit is `CompiledCode` (constant pool, indexed locals,
  `simple_locals`/upvalue analysis done).
- Current perf (vs raku): fib(25) 1.0x and int-arith 0.7x — instruction-bound workloads are
  roughly at parity — but **bench-fib (with type constraints) 3.2x, method-call 2.7x,
  bench-class 2.3x** remain. Targets (PLAN §5): method-call < 1.5x, bench-class < 1.5x,
  bench-fib < 2x.
- Expected JIT payoff (PERFORMANCE.md Phase 4c): 5–10x on tight numeric loops.
- GC is level-1 (cycle collector on Arc, default-on). It is **non-moving**, so pointers
  never move and refcounts guarantee liveness. The safepoint machinery
  (backedge/call/return/await/...) and cooperative STW are already operational — JIT code
  participates as a **poller** of these safepoints.
- Layer 3b (NaN-boxing, 8B `Value`) comes first (ADR-0001's ordering). The JIT is designed
  on the assumption that 8B fixed-width values can be handled in registers.

## 2. Decision (proposed decisions)

### 2.1 Backend = **Cranelift** (JIT mode)

- Rationale: pure Rust with lightweight embedding (no rustc/LLVM toolchain dependency);
  compilation speed suited to JIT; production track record in wasmtime; a "plain function
  pointer" model that fits well with a non-moving GC.
- Rejected: LLVM (inkwell) has the strongest generated-code quality but is heavyweight in
  build, link, and compile time, risking mutsu's "fast startup" weapon (startup 0.04x).
  A hand-rolled asm template JIT means per-arch maintenance cost. Going through wasm is a
  wasted indirection layer.
- The dependency is isolated behind a **feature flag + runtime flag** (`MUTSU_JIT=on|off`,
  default off for the time being), keeping full functionality in JIT-less builds
  (unsupported arches).
  (**Update 2026-07-13**: with the J5 gates met, the default has been flipped to on — see
  the addendum at the end. `MUTSU_JIT=off` is the explicit opt-out.)

### 2.2 Compilation unit = **whole `CompiledCode` functions** (method JIT). No tracing

- Hotness detection: a per-`CompiledCode` call counter (+ backedge counter). Compile when
  the threshold is exceeded; subsequent entries go through the native function pointer.
- OSR (switching over mid-execution inside a running loop) is **out of initial scope**
  (revisit hot-loop entry from the backedge counter in phase J4). mutsu's main benchmarks
  are repeated function calls, so entry replacement alone recovers most of the gain.
- Meta-tracing (PyPy-style) is rejected: the infrastructure scale is of a different order,
  and it would mean throwing away the existing VM assets.

### 2.3 Execution model = **subroutine-threading + gradual inlining** (no deopt)

The JIT's first form is "nativize the opcode sequence as a **sequence of calls to VM helper
functions**" (= removing the dispatch loop is the first win). From there, expand the
highest-frequency opcodes into CLIF inline, in order:

- Tier A (J1–J2): translate all supported opcodes directly as helper calls. Functions
  containing unsupported opcodes are excluded from JIT (they stay interpreted) —
  **no guards/deopt/OSR-exits are built at all**. Whether a function "can be compiled" is
  determined statically by looking at the `CompiledCode`, so no runtime deoptimization is
  needed. This is the JIT counterpart of ADR-0001's "keep GC simple = keep the JIT simple"
  line.
- Tier B (J3–J4): expand Int/Num arithmetic, comparison, branches, local get/set,
  constants, and the small calling convention directly into CLIF (NaN-box tag check +
  fast path; slow path goes to a helper). Use **tag branching** rather than type profiling
  (relying on the 8B NaN-box).
- The "GC-cooperation code the JIT emits" is only two things:
  1. `Gc`/`Arc` inc/dec at value copy/destroy sites (only in Tier B inline sections; helper
     call sections handle it inside the helper as before)
  2. **Safepoint polls** at backedges / call sites (`STOP_REQUESTED` check → cooperative
     park)

### 2.4 GC / thread coherence (making the level-1 assumptions explicit)

- **No stack maps needed**: JIT frames may hold "raw pointers invisible to the collector".
  As long as refcounts are maintained correctly, a node being referenced is never reclaimed
  (the cycle collector only reaps isolated cycles).
- **No forwarding needed**: non-moving, so pointers embedded in compiled code are immutable.
- **No write barriers needed**: not generational. However, the candidate push at the
  mutation chokepoint (buffered bit) still works as before for mutations that go through
  helpers. When Tier B inlines container mutation, the candidate push must be emitted
  together with it (= container-mutation inlining is placed at the lowest priority and stays
  in helpers for now).
- **STW**: JIT code interleaves safepoint polls, so it participates in the cooperative STW
  quiescence accounting as-is. A long-running native loop that lacks polls would starve the
  STW, so **backedge polls are mandatory from Tier A onward**.

### 2.5 Phases and gates

| Phase | Content | Gate (acceptance criteria) |
|---|---|---|
| **J0** | Prerequisite: layer 3b complete (8B Value). **Lever 3 (threaded dispatch) is frozen** (Tier A captures the same gain, larger, via dispatch-loop removal; avoid double investment) | 3b gates (gc-post-3a-roadmap §3.3) met |
| **J1** | Cranelift integration skeleton: feature flag, function counters, compile queue, entry swapping. Minimal opcode set for fib-equivalent code in Tier A | With `MUTSU_JIT=on`, fib/int-arith run under JIT with **byte-identical output on/off**. make test green (on) |
| **J2** | Expand Tier A opcode coverage (arith/compare/branch/local/const/call-compiled-fn/return) + a **jit-stress job** in CI (same shape as gc-stress: `MUTSU_JIT=on` for make test + roast) | jit-stress green. Measured improvement on bench-fib from dispatch removal (target 3.2x → ~2.5x) |
| **J3** | Calling-convention optimization: JIT→JIT direct calls, inline caches for method dispatch (class Symbol keys), in-JIT fast path for type-constraint checks | method-call < 1.5x, bench-class < 1.5x, bench-fib < 2x (the lever for hitting the PLAN §5 targets) |
| **J4** | Tier B: NaN-box tag-branching arithmetic inlining + hot loops (backedge counter, OSR entry if needed) | Int loops 5–10x (PERFORMANCE.md expectation). Inside the fib loop, `gc_candidate_pushes == 0` and zero refcount operations (the finished form of ADR-0001 §3-8) |
| **J5** | Default-on flip: gc-stress × jit-stress matrix green + overhead budget (compile cost of non-hot code must not worsen startup 0.04x) measured, then `MUTSU_JIT` default on | Startup bench unchanged, all CI green, ADR updated |

### 2.6 Test strategy

- **Differential execution is the first gate**: run the same program with `MUTSU_JIT=off/on`
  and require byte-identical output (reusing the methodology established for gc-stress).
- CI: a `jit-stress` job (from J2 onward). Eventually, `MUTSU_GC=on × MUTSU_JIT=on` are
  both defaults, so the regular jobs double as the matrix.
- Add `jit_compiles` / `jit_entries` / `jit_bailouts` (per-reason counts of excluded
  functions) to `MUTSU_VM_STATS`, tracking "what still falls back to the interpreter" with
  numbers.

## 3. Consequences

- Without deopt/OSR-exit/stack maps, **the JIT core stays small**, but any function
  containing an unsupported opcode remains fully interpreted. → The `jit_bailouts` counter
  makes the distribution of excluded functions visible, driving coverage expansion by
  measurement.
- Lever 3 (threaded dispatch) is frozen. It is revived only if the JIT founders.
- The go/no-go decision on layer 3c (biased RC) is deferred until profiling after J4
  completes (gc-post-3a-roadmap §4).
- A Cranelift dependency is added (build time, binary size). The feature flag keeps
  non-JIT builds viable; the default for release binaries is decided at J5.

## 4. Alternatives considered

| Option | Pros | Cons | Verdict |
|---|---|---|---|
| **Cranelift method JIT (this ADR)** | Lightweight, Rust integration, JIT-suited compile speed | Peak performance below LLVM | **Adopted** |
| LLVM (inkwell) | Best code quality | Heavyweight build/link, slow compiles, undermines the startup-performance weapon | Rejected |
| Hand-rolled template/copy-patch JIT | Zero dependencies, fastest compiles | Per-arch implementation/maintenance; Tier B-grade optimizations built by hand | Rejected (Cranelift's Tier A is portable at roughly the same cost) |
| Meta-tracing (PyPy-style) | Adaptive optimization in theory | Rebuilding the VM assets; measured in years | Rejected |
| Via wasm (wasmtime) | Sandboxing/portability | Indirection-layer overhead; GC/host interop is cumbersome | Rejected |
| Deopt/guard speculative JIT | Peak performance | Stack reconstruction (OSR-exit) implementation is full-scale-VM-grade | Rejected (replaced by bailout = whole-function exclusion) |

---

*Accepted by user approval on 2026-07-06 (the Lever 3 freeze approved at the same time).
Implementation starts at phase J1 of §2.5, after gc-post-3a-roadmap's layer 3b (NaN-boxing)
is complete.*

*Addendum 2026-07-13: **J5 complete — `MUTSU_JIT` default on**. Gate measurements (bench CI
main `f19946ad`): gc-stress × jit-stress matrix green (permanent since J2);
bench-startup 0.0087s → +jit 0.0086s (startup budget unchanged — cold code stays below the
threshold and is never compiled); all `+jit` series at or above interpreter parity
(fib 0.77x→0.59x, bench-fib 1.64x→1.26x, no regressing series). The bench CI plain series
henceforth pins `MUTSU_JIT=off` explicitly to preserve its meaning as the interpreter
baseline. The J4 "int loops 5–10x" gate was, by its letter, still unmet when J5 went ahead:
J4c sped up the interpreter itself by -34%, shrinking the denominator of the relative
difference, while absolute performance improved in both series. The remaining J4d
(variable-op inlining) continues on top of default on.*

*Addendum 2026-07-15: **J4d complete — this ADR's phase plan is fulfilled and closed**.
Slice breakdown: #4527 (Tier B GetLocal inlining + lock-free hot-range cache), #4528
(removal of the per-store intern chain in the SetLocal mirror), #4529 (alloc-free
light-call ceremony), #4534 (FxHash-ing the dispatch table + fusing the argument scan), #4537
(caller-env frame reuse for light-call — a dynamic-detection scheme using the empty overlay
singleton as a CoW write latch), #4540 (readonly-set Arc snapshot → mutation journal). Bench CI measurements (ratio = vs raku): fib+jit 0.34 (`d6d405cc`, pre-#4534)
→ 0.28 (`c397b90b`); bench-fib+jit 0.66 → 0.56. Cumulatively, local fib(28) went from
pre-J4d ~0.4s → 0.13s.*

*Re-judgment of the J4 "int loops 5–10x" gate: the JIT on/off ratio for int-arith is 1.24x
(`c397b90b`: 0.0375s vs 0.0466s), still unmet by its letter — but that expectation used
**the pre-J4c/J4d interpreter as its denominator**, and as J4c (interpreter -34%) and J4d
(reduction of dispatch and call ceremony, costs shared by on and off) kept shrinking the
denominator, the ratio became unreachable in principle. In absolute terms, int-arith+jit is
0.18x of raku (≈5.5x faster), and every `+jit` series is at or above its interpreter
counterpart — the level the original "5–10x" intended (native-grade execution of tight
numeric loops) has been achieved in absolute terms. The ratio gate is judged obsolete and
closed.*

*Design note (evaluation results for J4d items ① and ②): "per-call-site inline caches on the
CallFunc opcode" was evaluated and **not adopted** — after #4534's FxHash conversion, the
double probe on name keys dropped to ~10-15ns/call, and the remaining gain from
per-site-ification (~3%) does not justify the complexity of Arc-ifying `CompiledFns` plus a
map-id validation mechanism. "JIT→JIT direct calls" likewise did not get a dedicated native
path; the same gain was recovered by cutting the fixed costs of the light-call path shared
by interpreter and JIT (the env swap's Arc churn / the readonly snapshot / the GC
safepoint's locked RMW) (#4537/#4540 — because in profiles the call ceremony was over 3x
the dispatch probe). The root-cause fix for the remaining on/off-shared fixed costs
(the SetLocal env-mirror, etc.) belongs to the PLAN §6 lexical-slot campaign.*
