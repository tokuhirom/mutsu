# ADR-0111: Finishing ADR-0110 for the JSON::Fast goal — whole-unit linkage, the string path in TRIR, typed container ops, then native lowering with inlining

- Status: Accepted (2026-09-23, approved by tokuhirom; Step 1 landed — see "Implementation status")
- Date: 2026-09-23
- Deciders: tokuhirom, Claude
- Tracked by: [#8673](https://github.com/tokuhirom/mutsu/issues/8673) (goal: `bench-json-fast-spdx@section+jit` below 1.0, i.e. faster than rakudo)
- Related:
  [ADR-0110](0110-typed-resolved-ir-for-statically-typed-routines.md) (the typed, resolved IR this completes; its decision stands, its Stage 3-4 plan is replaced here),
  [ADR-0004](0004-jit-strategy.md) (Cranelift method JIT, no deopt — the backend step 4 uses),
  [ADR-0001](0001-gc-strategy-and-phasing.md) §7 (level-2 VM redesign stays rejected; nothing here needs it),
  [#8831](https://github.com/tokuhirom/mutsu/issues/8831) (native-typed `nqp::` ops on unboxed registers — the bytecode-side twin of step 3/4),
  [#8900](https://github.com/tokuhirom/mutsu/issues/8900) (`nqp::` op bodies and the general binder)

## 1. Context

### 1.1 Why a new ADR, and not ADR-0110's Stage 3

ADR-0110 Stage 2 concluded that Stage 3 (lowering TRIR chunks to Cranelift) could not reach its gate, because "those chunks are 96% callouts". That measurement was of a TRIR that did not run: inside a module every statically linked call bailed, and each routine was re-run on the untyped path. #9072 (PR #9088) fixed that; see the correction note in ADR-0110's implementation status. The Stage 2 evidence behind the plan for Stages 3-4 is therefore void, and the plan has to be re-derived from measurements of a TRIR that completes. This ADR does that. It keeps ADR-0110's decision (a typed, resolved IR on the existing stack machine) and replaces only its Stage 3-4 plan.

### 1.2 Where the decode stands (main at `9ddb77c1`)

Release build, 4-core container, `from-json` of the 727-record SPDX-shaped document (`benchmarks/bench-json-fast-spdx.raku`):

| | mutsu | rakudo | ratio |
|---|---:|---:|---:|
| whole decode | 1.67 s | 0.058 s | 29x |
| same document, every string rewritten to word characters | 0.334 s | 0.032 s | 10x |

`MUTSU_VM_STATS` (100 records): `trir: entries=1949 completed=1949 bails=0`. So the typed IR runs, and the gap has two parts of very different size.

**The slow string path is ~1.35 s, about 80% of the decode.** Strings that are not all `CCLASS_WORD` characters go through `parse-string-slow` → `nqp::strtocodes` → `unjsonify-string`. Neither routine is TRIR: `parse-string-slow` declines on `$end + 1` (a `:=`-bound boxed value), and `unjsonify-string` is never attempted because it declares an inner `my sub fetch-codepoint`. The document has 4,363 such strings, 197,669 characters in all.

| slow-path string | mutsu | rakudo |
|---|---:|---:|
| fixed cost per string | ~123 µs | ~6-11 µs |
| per character | ~4.9 µs | ~75-120 ns |

**The rest (0.33 s) is TRIR-resident, but slow at its edges.** Cost per element, measured with a 4,000-element array of one kind:

| element | mutsu | rakudo |
|---|---:|---:|
| `"abcdefgh"` | 23.5 µs | 3.0 µs |
| `true` / `false` | 25.4 / 25.9 µs | 1.6 / 1.5 µs |
| `534` | 69.5 µs | 3.4 µs |
| `{}` | 26.5 µs | 1.7 µs |
| `[]` | 40.0 µs | 1.1 µs |

### 1.3 The cost model: what one operation costs, TRIR against rakudo

Microbenchmarks inside a module, direct calls, release build (`tmp` sources are reproduced in §6):

| operation, per iteration | TRIR | untyped (`MUTSU_TRIR=off`) | rakudo |
|---|---:|---:|---:|
| loop body: `islt_i` + `add_i` + store | 26 ns | 723 ns | 11 ns |
| call to an **earlier**-declared routine (`CallTr`) with an `int is rw` argument | 172 ns | 1,879 ns | 73 ns |
| call to a **later**-declared routine (`CallGen` → by-name dispatch → TRIR again) | **8,857 ns** | 1,896 ns | 80 ns |
| `unjsonify-string`'s per-codepoint loop, per character | 556 ns (a TRIR-admissible rewrite) | 3,700 ns | ~100 ns |

Three facts follow.

1. **Code that TRIR runs end to end is within ~2.5x of rakudo.** That covers the loop body and the resolved call. The typed IR works: the remaining factor is the cost of interpreting typed ops, which native lowering exists to remove.
2. **The edges of TRIR are where the multiples are.** A forward call leaves TRIR, is boxed, containerized and resolved by name, and enters TRIR again. That round trip costs 110x rakudo, and 5x the untyped path it was meant to beat. JSON::Fast makes one on every value, because `parse-obj` and `parse-array` call `parse-thing`, which is declared after them. Mutual recursion means one direction is always a forward reference.
3. **A routine that is not TRIR is ~7-40x rakudo per operation**, whatever TRIR does elsewhere. The string path is exactly such a routine, and it is 80% of the time.

## 2. Decision

Four steps, in order. Each has a gate that can falsify the plan, measured on the bench CI rows `bench-json-fast-spdx@section+jit` / `bench-json-fast-spdx+jit` (instructions) and on the §1.3 microbenchmarks.

### Step 1 — Whole-unit static linkage

Link every call to a routine of the same lexical scope, whatever its declaration order: forward references, self recursion and mutual recursion. Raku hoists a lexical `sub` to its whole scope, so resolving at the end of the enclosing scope is the language's own semantics, not an approximation. `record_trir_routine`'s shadowing rules keep applying.

Mechanism: a call site to a not-yet-compiled routine records a pending link, which the enclosing scope patches when it finishes compiling. Recursion makes `Arc<TrChunk>` links cyclic, so a link becomes an index into a per-unit chunk table owned by the compilation unit (`TrUnitTable`), rather than the `Arc` of #9088. That also gives step 4 a natural unit of compilation.

Gate: the forward-call microbenchmark within 1.2x of the backward one, and JSON::Fast's `trir: entries` from outside the typed IR at ~1 per `from-json`.

### Step 2 — The string path in TRIR

Admit the constructs that keep `parse-string-slow`, `unjsonify-string`, `fetch-codepoint`, `parse-numeric`, `parse-true` and `parse-false` out:

- **a sigilless positional parameter** (`\codes`) as a boxed slot. It binds without a container, which is exactly what a boxed TRIR slot already is;
- **an inner `my sub`** as a nested chunk with access to its enclosing frame's slots (a static link), so `fetch-codepoint` can read `codes` and `$pos` without a closure being registered anywhere. #9073 stopped the per-call registry churn on the untyped path; this removes the frame altogether;
- **`uint32` / the other sized native ints** as the `int` bank, with the narrowing the general binder applies;
- **boxed-plus-native arithmetic** where the boxed operand is `nqp::`-sourced through a `:=` binding (`my $end := nqp::index(...)`), extending the existing `nqp_sourced` rule to bound slots;
- **a definite return type** (`--> True`, `--> False`) as "discard, return this constant", as #9074 did for the light path;
- **a method call on a native result** (`.Numeric` on an `nqp::substr`) as a `CallGen`-style method callout, so `parse-numeric` is admitted with its one cold edge boxed.

Gate: all six routines accepted, and the full decode at or below the word-only decode's 0.334 s.

### Step 3 — Typed container ops inside TRIR

`nqp::shift_i`, `push_i`, `elems`, `atpos_i`, `bindpos_i`, `push`, `bindkey` and `p6scalarwithvalue` on the representations TRIR has proved (`list_i`, `Uni`, `IterationBuffer`, the `Hash` storage map) operate on the Rust structure directly, with no `dispatch_nqp_op_by_id` and no boxed round trip. The same goes for the `NewHash` / `NewArray` construction that makes `{}` cost 26.5 µs and `[]` 40 µs. This is #8900's "op bodies" half and #8831's direction, scoped to the ops TRIR emits.

Gate: the per-character loop at or below 100 ns (rakudo's), and `{}` / `[]` elements at or below 5 µs.

### Step 4 — Native lowering of TRIR chunks, with inlining

Lower a `TrUnitTable` to Cranelift through ADR-0004's backend. Native `int`/`num` values live in SSA values across the whole chunk, and are boxed only at `CallGen`, at return, and at a bail. Inline linked callees under a small op budget that are not recursive, which is `nom-ws` inlined into its callers, as rakudo's spesh does. TRIR has already proved every operand kind, so no speculative guard or deoptimization is needed. The one exit is the existing bail, which is admitted only where re-running the routine from the start is equivalent. ADR-0004's "no deopt" constraint therefore holds unchanged.

Gate: **`bench-json-fast-spdx@section+jit` below 1.0**, which is #8673's close condition.

## 3. The arithmetic (CLAUDE.md / perf-tuning §5a)

The goal needs 29x from today. Estimated remaining time after each step, derived from §1.2-§1.3 (estimates, to be replaced by each step's measurement):

| after | decode (est.) | ratio (est.) | where the estimate comes from |
|---|---:|---:|---|
| today | 1.67 s | 29x | measured |
| step 1 | ~1.60 s | ~28x | ~8,000 forward calls × ~8.7 µs |
| step 2 | ~0.45 s | ~8x | per-char cost 4.9 µs → ~0.56 µs; fixed per-string cost to a few µs |
| step 3 | ~0.25 s | ~4x | per-char to ~0.1 µs; containers to ≤ 5 µs |
| step 4 | < 0.058 s | < 1x | TRIR-resident code is ~2.5x rakudo when interpreted; native code with inlining must remove that factor and the per-call frame cost together |

Steps 1-3 cannot close the gap on their own, and are not presented as doing so. They remove the multiples that come from not being in TRIR at all, which step 4 cannot compile away. Step 4 alone would lower a TRIR that sees only ~20% of the time. The order is the point: each step makes the next one's target the dominant cost.

## 4. Consequences

- The work comes in slices with a gate each. Every step is independently useful: steps 1-3 help any `nqp::`-style module (`CBOR::Simple`, `JSON::Tiny`'s actions, the YAML batteries), not only JSON::Fast.
- TRIR's admitted surface grows. The differential gate (`t/vm/codegen/adr0110-trir-differential.t`, made non-vacuous by #9088) and the module-shaped fixture (`t/modules/adr0110-trir-module-linkage.t`) are extended with every construct steps 1-3 admit. A TRIR bug gives a right first answer and wrong later ones (ADR-0110 Stage 2's note), so every pin calls twice.
- The `trir:` stats line becomes a gate input: a step that raises `bails` or outside `entries` has regressed, whatever the wall clock says.

## 5. Rejected alternatives

- **Continuing profile-rank micro-slices.** Over twenty landed at 1-9% each and moved ~77x to ~60x (#8673). §1.3 shows the multiples sit at the edges of TRIR, not in any profile entry.
- **A native JSON implementation.** A rung-3 substitution under the module's name, banned by ADR-0096 §D3 whatever it measures.
- **Native lowering first (ADR-0110's original order).** It would compile the 20% of the time that is already TRIR, and leave the 80% on the untyped path.
- **A level-2 VM redesign or a tracing JIT.** Rejected by ADR-0001 §7 and ADR-0004. Nothing in §1.3 points at a limit of the level-1 design: the TRIR-resident numbers are already within ~2.5x.

## 6. Reproduction

- The decode: `benchmarks/bench-json-fast-spdx.raku` (`BENCH_DET=1` for 100 records), with `MUTSU_VM_STATS=1` for the `trir:` line and `MUTSU_TRIR_WHY=1 MUTSU_TRIR_DUMP=1` for acceptance and decline reasons.
- The word-only variant: the same document with every string passed through `.trans([':', '/', '.', '-', ' ', ','] => ['c', 's', 'd', 'h', '_', 'm'])` before the timed call.
- The call cost model: a module declaring `early` (before), `via-back` / `via-fwd` (callers, each looping `$n` times on `nqp::while(nqp::islt_i($p, $n), early/late($t, $p))`), and `late` (after), where `early`/`late` are `my sub (str $t, int $p is rw) { $p = nqp::add_i($p, 1); $p }`. Call each caller directly from an `our sub` in the same module. Calling through a `&f` variable enters the untyped path and measures something else.
- The per-character loop: `unjsonify-string`'s `nqp::while(nqp::elems($codes), ...)` body in a routine taking `$codes`, fed `nqp::strtocodes(..., NORMALIZE_NFD, nqp::create(NFD))` of a 1,008-character URL string.

## Implementation status

### Step 1 — landed 2026-09-23, by a different mechanism than §2 describes

§2 proposed patching pending links when the enclosing scope finishes compiling, with links as indices into a per-unit chunk table. Reading the compiler showed what that costs. The TRIR compile of every routine would have to be deferred to the end of its scope, with its AST retained. Nested scopes hand their routines to the enclosing routine's own table, so the chunks would have to be re-attached into tables that have already moved. And a callee that turns out to decline would force its callers to recompile. All of that serves a question the running program already answers.

What landed links each `CallGen` site **at run time, to what the generic dispatch did** (`src/trir/gen_link.rs`):

- The first call of a site goes the generic way, with an observer armed for the callee name.
- `call_function_fallback`'s plain user-routine branch records the def it picks for that name, but not when multi dispatch is involved.
- If the def has a chunk, the site keeps it, keyed by what the resolution read: `fn_resolve_gen`, the current package, and the running frame's lexical package, the two inputs of `bare_name_packages_syms`.
- A later call in the same state binds its arguments with `CallTr`'s own checks, by peeking before consuming anything, and runs the chunk.
- A wrapped callee, a junction or aggregate argument, a type the chunk cannot bind, or a changed state takes the generic path, as the first call did.
- Observers nest. An untyped callee can reach a TRIR routine that makes its own generic call, so the enclosing observer is set aside and restored rather than cleared. Clearing it lost every link whose callee makes a generic call, which is `parse-thing` reached from `parse-array`.

This is sound because the routine linked is by construction the one the generic path picks in that state, and running its chunk is the equivalence ADR-0110's differential gate holds TRIR to. It also covers what compile-time linkage could not: a call into another compunit's routine.

It does not by itself give Step 4 a compile-time unit. Step 4 can link at lowering time, when every routine exists, by the same observation.

Measured, release, 4-core container:

| | before | after | gate |
|---|---:|---:|---|
| forward-call microbenchmark (§1.3) | 8,857 ns | **174.5 ns** | ≤ 1.2x the backward call (174.5 ns): **met** |
| `trir: entries` from outside TRIR, 100-record decode | 1,949 | **70** | ~1 per `from-json`: partly met; the rest are first-call observations and the untyped string path's own calls |
| `gen-links` (calls served through a link), same run | 0 | 1,088 | — |
| 727-record `from-json` | 1.67 s | ~1.55 s (3 runs: 1.50 / 1.61 / 1.55) | — (as §3 estimated: Step 1 alone is worth ~0.07 s) |

The decoded result is byte-identical to rakudo's. Pins: `t/vm/codegen/adr0111-trir-forward-link.t`, which covers a forward `is rw` write, mutual recursion, an aggregate declining then a scalar linking, and a `.wrap` installed after linking, all with TRIR on = off and `gen-links` > 0. Also `t/modules/adr0110-trir-module-linkage.t`.

