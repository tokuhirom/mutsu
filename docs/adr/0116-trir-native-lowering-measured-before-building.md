# ADR-0116: ADR-0112 Step 4 measured before it was built — native lowering of TRIR is capped at ~1/5 of the decode, so Step 4 becomes "shrink the op bodies"

- **Status**: Proposed (2026-09-23). Replaces the *ordering* of ADR-0112 Step 4 only; ADR-0112's
  Steps 1-3 and its goal stand.
- **Deciders**: tokuhirom (pending), Claude
- **Context**: [#8673](https://github.com/tokuhirom/mutsu/issues/8673) (goal: the SPDX `from-json`
  faster than rakudo), [ADR-0112](0112-trir-completion-plan-for-beating-rakudo.md) §2 Step 4,
  [ADR-0110](0110-typed-resolved-ir-for-statically-typed-routines.md),
  [ADR-0004](0004-jit-strategy.md) (the Cranelift backend).

## 1. Context

ADR-0112 Step 4 says: lower TRIR chunks to Cranelift, keep native values in SSA across a chunk,
inline small callees, and that closes the remaining gap. Its §3 table rests on one estimate:
"TRIR-resident code is ~2.5x rakudo when interpreted; native code with inlining must remove that
factor". The 2026-09-23 handoff on #8673 asked for the detailed design as an ADR before
implementation.

Where the decode stood when this was written (`main` at `38133ec8`, release, 4-core container,
`benchmarks/bench-json-fast-spdx.raku`, 727 records):

| | mutsu | rakudo |
|---|---:|---:|
| `from-json` (section time), 3 runs | 0.131-0.141 s | 0.046-0.066 s |

So the goal needs roughly **2-3x**.

Rather than design against the estimate, the first slice was built and measured. This ADR records
what it showed.

## 2. What was built: the S1 prototype

Commit `7681f6f9` on this ADR's branch (reverted by the next commit, so it stays in `main`'s
history). A hot `TrChunk` (the bytecode JIT's call threshold) is lowered to one Cranelift function:

- **The int bank in SSA.** Every `TrOp` pops and pushes a fixed number of int-bank entries, and for
  a call the `TrInnerCall` fixes it, so the bank's depth at every op is a static fact
  (`trir/jit/shape.rs`, a worklist over the ops that rejects a chunk whose depths disagree at a
  join). The value at depth `d` is one Cranelift variable.
- **Native slots in SSA.** A slot a call can reach by reference (a `TrArg::Native` argument) is
  stored to the frame before such a call and reloaded after it. Nothing else reaches a slot.
  `is rw` references read and write `nl` directly, through a base pointer re-read after every
  call-out (a callee may reallocate `nl`).
- **Native control flow.** Jumps are branches, and a backward jump polls the safepoint.
- **One definition per op.** The ~40 native int/num/jump/ref ops are emitted inline. Every other op
  calls a shim that runs `Interpreter::trir_step`, the switch loop's own arm split out into a
  function, with the op's int operands passed in and out through a small stack buffer. So no op had
  two implementations.
- The Cranelift module is shared with the bytecode JIT (`vm/vm_jit_engine.rs`).

It is **correct**. `t/vm/codegen/adr0116-trir-native.t` ran every `t/fixtures/trir-*.raku` three
ways: native from the first call (`MUTSU_JIT_THRESHOLD=1`), the switch loop, and TRIR off. All
three agreed, with chunks compiled and run. It is also **slower**.

## 3. Measurements

### 3.1 Wall clock (release, 4 cores)

| 727-record decode | native on | native off |
|---|---:|---:|
| `bench-json-fast-spdx` section, 3 runs | 0.168 / 0.177 / 0.183 s | 0.131 / 0.131 / 0.141 s |
| the same decode repeated 6x in one process | 0.161-0.219 s | 0.132-0.141 s |

The repeated decode compiles nothing after its first iteration, and native is still ~20% slower
in steady state.

### 3.2 Instructions (callgrind, the 100-record `BENCH_DET` run, whole process)

| | native on | native off |
|---|---:|---:|
| total | 407.5 M | 266.3 M |
| `trir::jit::lower::compile`, inclusive (10 chunks) | 128.1 M | — |
| of which `cranelift_codegen::verifier` | 13.9 M | — |
| everything but compilation | 279.5 M | 266.3 M |

- **Compiling costs ~12.8 M instructions per chunk**, about as much as decoding 8 records.
- **Execution got 5% worse, not better.** The ops emitted inline were already cheap in the switch
  loop. Every stepped op now pays a shim call, an operand round trip through the interpreter's
  bank, and a `nl` pointer reload.

### 3.3 The ceiling: where a record's instructions go

The decisive number does not need a JIT at all. Callgrind on the switch loop at 1 record and at 101
records, differenced per function, gives the cost of 100 records with startup, module loading and
compilation cancelled out: **165.0 M instructions**.

| where | Ir per 100 records | share |
|---|---:|---:|
| `run_trir_routine` self: the switch loop, with the arms the compiler inlined into it | 35.2 M | **21.4%** |
| NaN-box refcounting and decoding (`payload_op`, `view_kind`, `from_repr`, `arc_op`) | 22.0 M | 13.3% |
| the allocator (`malloc`, `free`, `_int_malloc`, `_int_free`) | 13.2 M | 8.0% |
| `nqp::` list op bodies (`nqp_backing_array`, `nqp_with_elems_mut`, `nqp_shift_elem`, `nqp_elems_*`, `shift_front`) | 12.5 M | 7.6% |
| untyped-path residue (`call_compiled_closure_in_unit`, `LocalKey::with`, `package_scope_lexical`, `Env::*`) | 9.4 M | 5.7% |
| NFD normalization for `nqp::strtocodes` (`Decompositions`, `canonical_combining_class`, `compose`) | 8.6 M | 5.2% |
| TRIR frames and calls (`exec_trir_inner_call`, `trir_seed_outers`, `pop_frame`, routine-frame push) | 5.8 M | 3.5% |
| `TrCharCache::index_of` | 2.2 M | 1.3% |
| long tail | ~56 M | ~34% |

`payload_op`'s callers name the refcount traffic: `run_trir_routine` (6.9 M, 263 K calls, the
`LoadObj` clones and the pops that drop them), `nqp_backing_array` (1.9 M), `pop_frame` (1.6 M),
and the list-op bodies (~4 M).

**Native lowering can remove at most the first row.** Even with a lowering that costs nothing to
compile and nothing per stepped op, 100% of the switch loop and its inlined arms gone is
1 / (1 − 0.214) = **1.27x**. The goal needs 2-3x. ADR-0112's "~2.5x of interpretation overhead"
was a microbenchmark of a loop made of native ops. The decode is made mostly of op *bodies*: boxed
values, allocation, list storage, normalization. Those run the same Rust whichever tier dispatches
them.

## 4. Decision

### D1. The Cranelift lowering of TRIR is not landed now

The S1 prototype stays in history (`7681f6f9`), with its differential test and its shape analysis,
as the starting point for later. ADR-0112 Step 4 in its current form is on hold. It cannot meet its
gate by construction (§3.3), and every chunk it compiles costs a measurable amount of time (§3.2).

### D2. Step 4 becomes "shrink the op bodies", ordered by measured share

Each item is one slice with its own gate. Each gate is measured by the same 1-vs-101-record
callgrind difference, plus the section wall clock, and each item is semantics-preserving by
construction.

1. **Refcount traffic on the operand bank (13.3%).** The compiler pairs `LoadObj(n)` with the op
   that consumes it (`ElemsO`, `ShiftIO`, `PushIO`, `TruthyDefined`, the `nqp::` generic ops). An
   operand-direct form that borrows the slot (`ElemsLocal(n)`, as `OrdAtLocal` already does for
   strings) removes a clone and its drop, two `payload_op` calls, per use. The per-character loop
   of `unjsonify-string` has three such pairs.
2. **`nqp::` list op bodies (7.6%).** `nqp_backing_array` clones the backing storage (1.9 M of
   refcounting on top of its own 4.3 M). Operate on the storage in place.
3. **`nqp::strtocodes` normalization (5.2%).** NFD of an ASCII string is the string itself.
   Skipping the normalizer when the input is ASCII is exact, and every string in this document is
   ASCII.
4. **Allocation (8.0%).** Find which per-call and per-value allocations remain (argument vectors,
   frames, `Vec` growth) with the `alloc_scope!` recipe of the perf-tuning skill, and remove them
   where the allocation is not the result.
5. **The untyped residue (5.7%).** Something in the decode still reaches the untyped call path
   (`call_compiled_closure_in_unit`) and by-name env lookups. Identify the construct, and either
   admit it into TRIR or explain it in #8673.

Estimated together, these remove ~35-40% of a record, which is **~1.6x**. That alone does not
reach the goal either, and this ADR does not claim it does. It is the part of the gap that no
backend can remove, so it comes first, which is the same ordering argument ADR-0112 §3 made for
Steps 1-3.

### D3. When native lowering comes back

Revisit the S1 design when both of these hold:

- the switch loop's own share (§3.3's first row, re-measured) is **above 50%** of a record, so that
  removing dispatch is worth more than 2x; and
- compilation is cheaper than what it saves, via Cranelift's verifier off in release builds (13.9 M
  of the 128 M), lowering only chunks that contain a loop or have run enough times to repay their
  op count, or both.

At that point the prototype's shape analysis, its SSA int bank, its spill set and the
`trir_step` split are directly reusable. What it lacked, and would then need, is the boxed bank in
registers with inline refcounting (ADR-0004 §2.4 already allows this without stack maps) and
callee inlining. Both are only worth it once D2 has made boxed ops cheap enough to be worth
emitting inline.

### D4. Two TRIR correctness bugs, fixed with this ADR

The prototype's differential fixture, checked against rakudo, found two answers on which TRIR (both
tiers) disagreed with the untyped path and with rakudo for negative operands:

- `nqp::div_i` truncated: `-17 div 5` gave `-3`, where rakudo gives `-4`. TRIR's `DivI` now shares
  the untyped op's floor division (`nqp_ops::floor_div_i`).
- Raku's `%` on native ints was lowered to `ModI`, which is `nqp::mod_i` and takes the dividend's
  sign: `-17 % 5` gave `-2`, where rakudo gives `3`. TRIR now declines `%`, and the untyped path
  answers it.

Pinned by `t/vm/codegen/adr0110-trir-int-ops.t` (TRIR on = off = rakudo's transcript).

## 5. Consequences

- #8673's next slices are D2's five items, in that order. Each one also helps the untyped path and
  every other `nqp::`-style module, because none of them is JSON-specific.
- The bench CI series decide, not local runs. §3's numbers are from one 4-core container and are
  for choosing the order, not for quoting as results.
- ADR-0112 keeps its Step 4 text, with a pointer here in its implementation status. If D3's
  conditions are met later, a new ADR re-accepts native lowering with a design based on
  `7681f6f9`.

## 6. Rejected alternatives

- **Land S1 off by default.** It would be code that is maintained and never runs, for a gain §3.3
  caps at 1.27x.
- **Make S1's stepped ops cheaper** (a specialized shim per op, no bank round trip). This recovers
  part of the 5% it lost, but stays under the same 1.27x ceiling.
- **Inline small callees first** (`nom-ws` into its callers, ADR-0112's example). The TRIR frames
  and calls row is 3.5% of a record, and calls are already linked (Step 1). There is not much left
  there to inline away.

## 7. Implementation status

- **D2.1-D2.3 landed together** (`news/2026-09/trir-slot-direct-list-ops.md`):
  the `ElemsLocal` / `ShiftILocal` / `PushILocal` operand-direct forms, the
  borrowed backing array (`with_nqp_backing_array`), and the ASCII bypass of
  the normalizer. Section time on the 4-core container went from
  0.165-0.176 s to 0.116-0.121 s. D2.4 (allocation) and D2.5 (the untyped
  residue) are next, after re-measuring §3.3's table.

## 8. Reproduction

- Wall clock: `benchmarks/bench-json-fast-spdx.raku`, with and without `MUTSU_TRIR_JIT=off`, on a
  build of `7681f6f9`.
- The per-record profile: copy the benchmark, replace `$records` with `+%*ENV<NREC>`, and run
  `valgrind --tool=callgrind` at `NREC=1` and `NREC=101` with `MUTSU_TRIR_JIT=off` (or on any build
  after the revert). Then difference `callgrind_annotate`'s per-function totals.
  (With `NREC=1` the script dies on its own sanity check *after* the decode; that is harmless for
  the difference.)
