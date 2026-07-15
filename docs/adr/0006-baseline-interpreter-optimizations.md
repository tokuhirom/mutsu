# ADR-0006: Baseline (classical) interpreter optimizations — adoption decisions and priorities

- **Status**: Accepted (approved by tokuhirom 2026-07-13)
- **Date**: 2026-07-13
- **Deciders**: tokuhirom, Claude
- **Related**: [ADR-0004](0004-jit-strategy.md) (JIT; the decision to freeze threaded dispatch),
  [ADR-0005](0005-nanbox-representation-encoding.md) (8B Value),
  [docs/opcode-design-review.md](../opcode-design-review.md) (remaining opcode items §2/§5/§6),
  PLAN.md §5, PERFORMANCE.md

> While ADR-0004 got us all the way to a JIT (Cranelift, default-on since J5, 2026-07-13),
> an audit (2026-07-13) found that the classical bytecode optimizations that CPython /
> Ruby (YARV) / PHP (opcache) have had **since before their JITs** are almost entirely
> unimplemented. This ADR records that audit and decides the adoption, priority, and
> Raku-specific safety conditions for each measure.

---

## 1. Context

- Execution pipeline: Parser → Compiler (`src/compiler/`) → bytecode (~340 opcodes) → VM.
  The compiler lowers the AST to the opcode stream **one-to-one**; the only post-emit
  optimization passes are `compute_upvalues` / `compute_needs_env_sync` (variable-access
  related).
- Status in the major language implementations (organized for comparison):
  - **CPython**: AST-level constant folding, peephole (jump optimization), the 3.11
    adaptive specializing interpreter (quickening + inline caches).
  - **Ruby (YARV)**: peephole, specialized instructions (`opt_plus` etc.),
    operand/instruction unification, call-site inline caches.
  - **PHP (opcache)**: a suite of SSA-based optimization passes — constant folding,
    constant propagation, DCE, jump optimization, opcode specialization via type inference.
- Equivalents mutsu **already has**: superinstructions (compound ops like `WhileLoop`/`ForLoop`,
  `StringConcat(n)`, `JunctionAnyN`, etc.), a global method cache
  (`method_resolve_cache` + the monomorphic 1-entry `last_method_resolve`),
  indexed locals, COW env, Symbol interning, NaN-box small-Int (±2^47 inline),
  static folding of grammar tokens (#4460).
- In other words, the "variable access / calls / data representation" optimizations are well
  advanced, but the entire class of optimizations that **shorten the opcode stream itself**
  is missing.

### 1.1 Audit results (2026-07-13; authoritative record of implementation status)

| Measure | CPython | YARV | opcache | Current state in mutsu | Verdict |
|---|---|---|---|---|---|
| Constant folding | ✅ | ✅ | ✅ | ❌ `compile_expr_binary` always emits the arithmetic op even for literal-literal pairs (`expr_binary.rs`) | **Adopt** (§2.1) |
| Inlining of `constant` reads | ✅ (co_consts) | ✅ | ✅ | ❌ evaluated at BEGIN time, but reads go through `GetLocal`/`GetBareWord`/`GetGlobal` (`stmt.rs` constant declaration) | **Adopt** (§2.2) |
| Constant-condition branch elimination / DCE | ✅ | ✅ | ✅ | ❌ even `if False {...}` emits the condition evaluation + `JumpIfFalse` | **Adopt** (§2.2) |
| Peephole (redundant-op removal/fusion) | ✅ | ✅ | ✅ | ❌ no post-emit rewrite pass (only the variable-related `compute_*`) | **Adopt** (§2.3) |
| Constant-pool deduplication | ✅ | ✅ | ✅ | ❌ `add_constant` unconditionally does `constants.push` (`opcode.rs:2927`) | **Adopt** (§2.4) |
| Per-call-site inline cache (PIC) | ✅ (3.11) | ✅ | partial | Partial: only the global cache + monomorphic 1-entry; no cache embedded in the opcode | **Deferred** (§3.1) |
| Quickening / adaptive specialization | ✅ (3.11) | partial | ✅ | ❌ | **Rejected** (§3.2) |
| Threaded dispatch | ✅ (computed goto) | ✅ | ✅ | ❌ | **Already rejected** (frozen in ADR-0004 §2.5 J0 — not relitigated) |
| Small-integer cache | ✅ | ✅ (Fixnum) | — | **Effectively achieved**: NaN-box small-Int is heap-allocation-free (layer 3b) | No action needed |
| String interning (values) | partial (fstring) | ✅ (frozen str) | ✅ | ❌ `Value::Str` is a fresh `Arc<String>` each time (Symbols are interned separately) | **Deferred** (§3.3) |

### 1.2 Empirical basis (newly added benchmarks; local release, min of 3 runs, 2026-07-13)

To back the verdicts with mutsu measurements rather than generalities, four natural workloads
sensitive to the targeted optimizations were newly added under `benchmarks/` and measured:

| Benchmark | interp | JIT on | raku | Ratio (interp) | Primary sensitivity |
|---|---|---|---|---|---|
| time-parts (timestamp decomposition) | 0.73s | 0.75s | 0.21s | **3.5x** | constant folding, peephole |
| debug-guard (hot function with a constant guard) | 0.42s | 0.43s | 0.19s | **2.2x** | constant inlining, constant-condition DCE |
| poly-call (area aggregation over 3 mixed classes) | 0.10s | 0.10s | 0.21s | 0.48x | PIC (baseline for the deferral) |
| word-count (word frequency counting) | 0.10s | 0.10s | 0.22s | 0.45x | string interning (baseline for the deferral) |

- **time-parts at 3.5x / debug-guard at 2.2x exceed bench-fib's 1.78x, the worst in the current
  suite**. The existing suite skews toward "function calls / OOP / collections" and had missed
  the weakness on code with a high density of plain expressions and statements.
- **The JIT does not help either** (same time as interp) — the slowness of these two comes not
  from dispatch but from "simply executing too many opcodes", which is evidence that this is an
  orthogonal lever independent of the JIT.
- Breakdown of the opcode histogram (`MUTSU_VM_STATS=1`):
  - time-parts: out of 7.6M ops/run, `LoadConst`=1.7M and `Mul`=1.0M (of which recomputation
    of literal constant expressions like `60*60*24` is Mul×6 per iteration = 600k), plus the
    administrative ops `SetSourceLine`=600k and `Mark*`/`SetVarDynamic`=1.5M.
  - debug-guard: `if DEBUG` executes as `GetBareWord`+`JumpIfFalse` every time (200k each).
    With constant inlining + constant-condition DCE the whole block disappears.
- poly-call / word-count are **already ahead of raku** (0.48x / 0.45x). This is the basis for
  the verdict that there is no urgency to implement PIC or string interning merely "because
  other languages have them".

## 2. Decision — adopted measures (in priority order)

All are **compile-time transformations (at emit time / post-emit)** with no runtime adaptive
machinery. Same shape as ADR-0004's "no deopt" policy: transform only when statically provable
safe; otherwise do nothing (the fallback is "no transformation", so it cannot become flaky).

### 2.1 Constant folding

Evaluate pure operations between literals (arithmetic, comparison, string concatenation,
logical) at emit time and replace them with a single `LoadConst`. Implemented bottom-up at the
entry of `compile_expr_binary` by checking "are both operands already-folded constants" (the
AST is not modified).

**Raku-specific safety conditions** (folding is allowed only when all of the following hold):

1. **No operator override present**: if the same compilation unit contains a
   `multi sub infix:<op>` declaration for the operator (including `sub infix:<op>`),
   disable folding for the entire file (no precise declaration-scope analysis —
   being too conservative and losing folding opportunities is better than mis-folding).
   JIT Tier B solves the same problem with an infix-override guard
   (pin: `t/jit-tier-b-infix-override.t`) — share the same detection result.
2. **Same operation implementation as at runtime**: folding evaluates by calling the VM's
   native operations (`builtins/arith.rs` etc.) directly. Do not write a separate
   "compile-time arithmetic" (Int's BigInt promotion, Rat-ification of `1/3`, and type
   promotion rules automatically match runtime behavior).
3. **Do not fold expressions that can throw**: if evaluation yields `Err` (`0 div 0` etc.),
   leave the expression untransformed (preserving throw-at-runtime semantics; no promotion
   to a compile-time error).
4. **Fold results must be value types only**: Int/Num/Str/Bool/Rat. Expressions producing
   containers or Instances are out of scope (identity is observable).

### 2.2 Inlining of `constant` reads + constant-condition DCE

- `constant NAME = ...` is, per the language spec, fixed at compile time (BEGIN) and cannot
  be reassigned. Currently the BEGIN-evaluated value is put into a slot/env and read at
  runtime (the code around `src/compiler/mod.rs:180` deliberately avoids "turning it into
  GetLocal" — but that is for *use-before-declaration detection*, not a rejection of inlining
  itself). Replace reads of `constant`s whose value is a value type (Int/Num/Str/Bool/Rat/Nil)
  with `LoadConst`. Container-valued constants have observable identity and are out of scope
  (as before).
- When inlining turns the condition of an `if`/`unless`/`while` into a literal constant,
  resolve the branch entirely: `if False {...}` emits no block (for elsif/else chains, the
  remainder is compiled normally). `constant DEBUG = False; if DEBUG { note ... }` — guarded
  logging — is a frequent pattern in real code, and the debug-guard benchmark (2.2x)
  measures it.
- **Safety**: DCE removes only "the unreachable side of a branch whose value is determined".
  No side-effect analysis is needed (it only fires when the condition expression itself is a
  literal constant). Handling of `BEGIN`/declarations inside the block follows Rakudo
  semantics (they still get compiled): start from the conservative rule that **blocks
  containing declarations are not removed but fall back to normal compilation**.

### 2.3 Peephole — reduction/fusion of administrative opcodes

Of time-parts' 64 ops per iteration, about 27% are administrative
(`SetSourceLine`, `MarkVarDeclContext`, `MarkExplicitInitializerContext`,
`SetVarDynamic`, `CheckReadOnly`). Statically eliminating/fusing this mutsu-specific
administrative sequence yields more, empirically, than classical push/pop removal:

- Turn the 3-4-op declaration sequence of `my $x = <expr>` into a compound op or a static
  flag (`SetLocalDecl`, #4488).
- **`SetSourceLine` is not "deduplicated" but abolished as an opcode entirely (implemented)**:
  the line number is **static data** per instruction, and no dispatched instruction is needed
  to carry it. Give `CompiledCode` a static ip→line table (`op_lines`, a `Vec<u32>` parallel
  to `ops`); `Stmt::SetLine` emits no opcode and merely advances the emit cursor
  (`set_emit_line`). `cur_source_line` is pulled via `sync_source_line(code, ip)` **only by
  instructions that can observe the line** (the arms for calls / re-entry into user code /
  frame pushes / declaration registration / raise sites, and the JIT's call shim that runs
  natively). Pure hot ops — arithmetic, locals, branches — pay nothing at all.
  - **Refreshing on every instruction is a loser** (measured): a version doing a table lookup
    + store for every instruction in the `exec_one` prologue costs more than the dispatch it
    saved — **+7.8% instructions** on fib. Restricting to "observation points only" gives
    **-3.4%** on fib (interpreter path).
  - The executed opcode count itself drops by -21% on fib (1.91M of 8.90M were
    `SetSourceLine`) and -11% on time-parts, but **the reduction in opcode count does not
    match the reduction in time** (`SetSourceLine` was the cheapest op — a single store —
    while mutsu's average op is an order of magnitude heavier). On the JIT path (the default
    configuration) it is ±0%. Bytecode memory shrinks by ~15%.
  - Line numbers actually become **more accurate** (previously the line stayed stale after
    executing a block/loop body, which required defensively re-emitting `SetSourceLine`
    before method calls via `emit_source_line_if_known` — also deleted).
    Pin: `t/source-line-table.t`.
- Jump-to-jump barely occurs today thanks to the compound loop ops, so it is not prioritized.
- Target selection is **histogram-driven** (`MUTSU_VM_STATS=1` opcode_histogram); no
  rewriting for aesthetics — the existing policy of
  [opcode-design-review.md](../opcode-design-review.md) §6 applies to peephole as-is.

### 2.4 Constant-pool deduplication (dedup)

Add an FxHashMap reverse index (value→index, hashable value types only) to `add_constant` so
equal constants are shared. The direct effect on execution speed is small, but the current
state — every compiler site pushing name strings once per use site — wastes `CompiledCode`
memory and cache locality, and this also helps mzef's large-scale populate (compiling tens of
thousands of dists). Minimal to implement (1 function), so it suits the first slice.

### Implementation order

By dependency and cost-effectiveness: **(1) §2.4 dedup → (2) §2.1 folding →
(3) §2.2 constant inlining + DCE → (4) §2.3 peephole**.
(2) and (3) share the same "emit-time constness tracking" foundation.
Gate: for each slice, confirm the time-parts / debug-guard improvement in the bench CI, and
that the existing 12 benchmarks show no regression.

### Measurement protocol for implementation slices (★added 2026-07-13; lessons from #4489)

**The reduction in opcode count does not match the reduction in time.** The
`MUTSU_VM_STATS` opcode histogram only answers "which ops are frequent", not "whether that op
is eating time". Indeed §2.3-b (abolishing `SetSourceLine`) cut executed opcodes by 21% on
fib, but the time reduction was an order of magnitude smaller (it was the cheapest op — a
single store). Moreover, **an implementation that trades the reduction for a small cost added
to every instruction easily goes into the red** (the per-instruction line-refresh version =
+7.8% instructions).

Therefore, every administrative-op-reduction slice must be **verified by instruction counts**
before starting and after implementing:

1. **Use the histogram only for candidate generation** (`MUTSU_VM_STATS=1`; a debug build is
   fine; independent of optimization level).
2. **Judge by perf's retired instructions** (wall-clock jitters by ±2–6% at this scale and
   cannot decide). On hybrid CPUs (P/E cores) measurements swing by 8%, so
   **user-space only + core pinning** is mandatory:
   ```
   sudo perf stat -r 5 -x, -e instructions:u,task-clock -- taskset -c 2 <bin> <bench>
   ```
   (Measuring with `instructions` (kernel included) mixes in page faults etc. and does not
   reproduce. With `:u` + `taskset`, re-running the same binary reproduces within 0.1%.)
3. **Prefer designs that add not even one instruction to hot ops** (#4489 only went into the
   black once shaped as "only ops that can observe the line consult the table").
4. The authoritative final benchmark numbers are the bench CI (`bench-data` branch) — local
   perf is for in-flight design decisions.

**Re-aiming the next target**: fib's profile top entries are
`call_compiled_function_positional_light` / `malloc`+`free` / `Env::get_sym`, not
administrative opcodes. Before starting on the remaining `SetVarDynamic` (time-parts 500k)
and `CheckReadOnly`, confirm via the protocol above that they are "eating time". If they are
not, **allocation churn and the call path (= the lexical-slot campaign) are the real target**.

## 3. Deferred / rejected measures (and their re-activation triggers)

### 3.1 Per-call-site inline cache (PIC) — deferred

- With the current monomorphic 1-entry (`last_method_resolve`) + global cache, poly-call is
  **already ahead of raku at 0.48x**. The per-miss cost of the 1-entry cache at megamorphic
  sites is a single FxHashMap lookup, which is not the bottleneck at present.
- Re-activation trigger: `method_resolve_cache` lookups showing up high in a profile, or JIT
  J4d (call inlining) needing a per-call-site cache structure anyway. In that case, put it
  **on the JIT's CallMethod shim side (ADR-0004 J3), not on the opcode side** — do not build
  duplicate cache machinery in the interpreter and the JIT.

### 3.2 Quickening / adaptive specialization — rejected

CPython 3.11's main weapon, but in mutsu **JIT Tier B (inline arithmetic on NaN-box tag
branches) plays the same role with a higher ceiling**. Machinery that rewrites the opcode
stream at runtime interacts in complex ways with resume/re-entry and concurrent execution;
apply the same "double investment alongside the JIT" judgment as threaded dispatch
(ADR-0004 §2.5). Reconsider only if the JIT fails, together with threaded dispatch.

### 3.3 Value string interning — deferred

word-count (hash aggregation over string keys) is **already ahead at 0.45x**. Hash keys
already flow mostly through the `Symbol`-based path, and `Value::Str`'s Arc sharing makes
copies O(1). Re-activation trigger: string alloc/hash showing up high in a profile. Even
then, consider NaN-box small-string inlining (SSO) first rather than whole-program interning
(consistent with the layer-3b design).

## 4. Consequences

- Four new benchmarks (time-parts / debug-guard / poly-call / word-count) added under
  `benchmarks/` — automatically picked up by the `bench-ci.sh` glob, so the effect of each
  adopted-measure slice and the baselines for the deferred measures remain in the bench CI
  history.
- All adopted measures are "only when statically safe; fallback is no transformation", so
  semantic risk to roast is low. The override guard (§2.1-1) is the only semantic boundary;
  include its pin test in the implementation slice.
- Folding and inlining **shorten the JIT's input (the opcode stream)**, which also benefits
  Tier B coverage and compile time (independent, but same direction).
