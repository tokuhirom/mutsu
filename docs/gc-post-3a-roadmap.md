# GC post-3a roadmap — hardening / Track B remainder / layer 3b NaN-boxing / layer 3c

Status: Draft (2026-07-05)
Related: [ADR-0001](adr/0001-gc-strategy-and-phasing.md), [ADR-0003](adr/0003-default-on-gc-trigger.md),
[gc-level1-detailed-design.md](gc-level1-detailed-design.md), [ADR-0004 (JIT)](adr/0004-jit-strategy.md), [PLAN.md](../PLAN.md) §2/§5

This document is a roadmap that breaks the GC work remaining **after layer 3a (cycle collector on Arc) is complete**
into slices with entry conditions (triggers) and acceptance gates.
It does not change the phase order of ADR-0001 (3a → 3b → 4=JIT → 3c decision).

## 0. Where we are (2026-07-05)

- **Layer 3a complete**: `MUTSU_GC` defaults to on (ADR-0003 §5). All safepoint kinds emitted, cooperative
  cross-thread STW, dead sweep, adaptive threshold trigger, CI gc-stress blocking.
- **Track B**: complete through slice 1 (atomic-store element cells #4241) / 2 (state aggregate cell writeback #4245) /
  3 (cell-ification of all state-aggregation modes #4251).
- **Residual from the acceptance measurement**: bench-class GC-on +8% (= the per-drop
  buffered-bit branch riding on `Value` clone/drop traffic. Accepted by user decision; **recovering it is layer 3b's job**).
- perf status: fib(25) 1.0x / method-call 2.7x / bench-class 2.3x / bench-fib 3.2x (vs raku).

## 1. Layer 3a hardening backlog (as-needed, small-grained)

> Principle: **do it when measurement shows a problem**. No preventive complexity (do not break
> the ADR-0001 §3-8 chain of reasoning that keeping 1a simple is what makes the JIT easy to land).

| ID | Item | Entry trigger | Gate |
|----|------|-----------|--------|
| H1 | Continuous observation of GC overhead: the `MUTSU_VM_STATS` gc counters + periodic re-measurement of the bench table (`benchmarks/run-all.sh`) | After each large PR | `gc_candidate_pushes == 0` maintained on fib/int; the GC-on residual of method-call/bench-class does not worsen |
| H2 | Splitting the `Instance` body node and the `attributes` cell (design doc §13-1, the sole open question) | Only if measurement shows insufficient cycle-reclaim precision, or a leak / excess tracing via attrs | reclaim counts match on the target pin (attr cycle); VERIFY green |
| H3 | Reduce the unwrapped blocking sites in STW quiescence: make the firing rate of the timeout→requeue fallback visible, and move the firing sources to `spawn_user_thread`/safe-region wrapping | If fallback firing is observed steadily via `MUTSU_GC_LOG` | fallback firing ≈ 0 on the churn-type benches; pause_ns_max bounded |
| H4 | Root-consuming full-root VERIFY mode (the §11 step 11 hardening list: include `Value` edges such as AsyncSocketConnMap in the root visitor) | When a tracing-style verification/debugging capability becomes necessary (also useful for verifying the 3b flip) | VERIFY(full-root) green under gc-stress |
| H5 | Level 1b: background / incremental collect (what the design doc §12 excluded from 1a) | Only if there is a measurement showing `pause_ns_max` becoming a problem on real workloads. **New ADR required** | — |

## 2. Track B remaining slices (small; the tail of the GC campaign)

Slices 1–3 established the "structure = COW snapshot / element values = cell in-place" template.
Everything that remains is a **missed application of this template**, and each slice is gated by
"raku cross-check pin + make test + related roast".

- ✅ **T4: multidim cas / cell-ification of the remaining atomic paths — done (2026-07-11)**. Unified multidimensional `cas`
  onto the same element-cell template as 1-dim `cas` (nested compare+set inside the
  top-level slot's cell lock; COW only inside the cell). Added container-only
  cell transparency to the multidim read/assign paths
  (`multi_dim_index_read` / `index_array_multidim` / each assign arm). ★Cells holding a scalar are not
  dereffed (the raw `\target` aliasing depends on the scalar-as-list wrap returning
  the cell itself — `t/multislice-lvalue.t` test 10).
  pin = `t/cas-multidim-cells.t`.
- ✅ **T5: typed-constraint support for `ContainerRef` — probe complete, gaps fixed (2026-07-11)**.
  Probe result: ordinary assignment, cross-thread assignment, scalar cas, and hash 3-arg cas (assignment desugar) are
  already enforced. Gaps = **the paths where the cas-only builtins write directly to the cell** (array 3-arg /
  multidim / hash code-form) + array code-form cas unimplemented (`Unknown function`). Fix =
  wire `check_atomic_elem_type` (declared constraint or the `value_type` embedded in the store node; native int
  types allow Int/Num) into all cas writes — raku type-checks the swap value even when the compare fails,
  so do it before taking the lock. Array code-form cas (1-dim/multidim) was also newly implemented as a celled retry-loop
  (`builtins_atomic_cas_code.rs`). pin = `t/cas-typed-constraint.t` (16 tests, matches raku).
  ★Remaining gap (a different axis, non-cell): raku rejects intermediate autoviv of `@m[1;1]` into
  an un-shaped `my Int @m` with X::TypeCheck::Assignment; mutsu lets it through (missing intermediate type check in multidim autoviv).
  Introducing a full `CellValue` type will be decided together with 3b (the principle of touching the Value representation once per campaign).
- ✅ **T6: investigation of the remaining non-state escaped-aggregate captures — probe complete, gap fixed (2026-07-11)**.
  Probe result: plain/compound element assign, push/pop, whole reassign, and cross-thread all
  match raku on every path. Gap = **the closure free-variable analysis does not recognize element incdec
  (`%h{$k}++`/`@a[$i]--`) and element `:delete` as uses of the aggregate**
  (the PostIncrementIndex family / DeleteIndexNamed were not registered in
  `op_container_mutate_const_idx`) → a closure whose only use is one of those does not capture the aggregate,
  and mutations after the escape were lost. Fixed by registering the 5 opcodes. pin = `t/escaped-closure-elem-incdec-delete.t`
  (12 tests, matches raku, including threads).
- **Out of scope (unchanged)**: lost-updates in highly contended concurrent "structural" insertion remain outside the spec
  (real rakudo gets a MoarVM oops in the same shape. mutsu doesn't break, which is an advantage — slice 2's conclusion stands).

## 3. Layer 3b: NaN-boxing (groundwork for the JIT; the main event after GC)

### 3.1 Purpose and what it recovers

- `Value` 48B → **8B**. Recovers in one stroke the ~50% of bench-class (`Value::clone`/`drop_in_place`/`Vec::clone`,
  PERFORMANCE.md) and the bench-class +8% residual of GC default-on (the traffic volume that the per-drop branch rides on).
- Prerequisite of the JIT (ADR-0004): fixed 8B width, tag checks take a few instructions, fits in registers. **Do 3b before the JIT**
  (ADR-0001's order 3a→3b→4).

### 3.2 Representation policy (confirmation + concretization of ADR-0001 §3-6)

- 64-bit NaN-box: `f64` is the raw value; non-f64 is a tag in the quiet-NaN space + a 48-bit payload.
  - inline scalars: `Int` (small integers), `Bool`, `Nil`, `Package/Symbol`, small enum-like variants.
  - pointer tags: container kinds put **the `Gc<T>` pointer directly in the payload** (the cycle collector is
    non-moving, so pointers are stable — no mark-bit relocation problem arises, ADR-0001 §3-6-3b).
  - boxed fallback: `BigInt`/`BigRat`/`Complex`/`Rat`/the Range family/other large variants are pushed out to a
    heap box (`Gc<BoxedValue>` or `Arc`). `Str(Arc<String>)` is a pointer tag.
- Note that the full i64 range does not fit in the payload: small integers (48-bit class) are inline; anything outside
  goes to a BigInt box or a `Gc<i64>` box. **Which one is decided at implementation time using the int-arith bench**.

### 3.3 Slice split (the 5b lesson: a wholesale flip is 479 errors/type → staging is mandatory)

1. **3b-0 API wall**: mechanically funnel `Value`'s direct `match`/construction sites through accessors/constructors
   (`as_int()`/`Value::int()`/`is_*()` etc.). Behavior unchanged, enum stays as-is.
   This is the largest surface (thousands of sites), but it is completely mechanical and sliceable in parallel.
   Gate: make test byte-identical for each slice.
   **Started (slice a)**: the wall API itself, `src/value/view.rs` (a `ValueView<'a>` mirror of all variants
   + `view()` + scalar accessors/constructors); design and migration recipe =
   [nanbox-3b0-api-wall.md](nanbox-3b0-api-wall.md); ratchet =
   `scripts/check-value-wall.sh` (built into `make test`; only monotonic decrease from baseline 17757 allowed);
   exemplar migrations = `vm_arith_ops.rs` / `vm_loop_writeback.rs` / `vm_native_map.rs`.
2. **3b-1 representation switch**: swap `Value`'s internal representation to the NaN-box implementation via `#[cfg]`/newtype
   (only the accessor internals change). Update `value_size_guard` from 48→8.
   Gate: make test + full roast + gc-stress green, GC counters unchanged (the type filter's
   scalar/container decision merely becomes a tag check).
   **Step A (newtype seal, byte-identical) is complete (2026-07-11 — see ADR-0005 §2.3)**:
   `struct Value(ValueRepr)` + private repr + constructor shims. Remaining = step B (representation swap,
   awaiting ADR-0005 Accepted).
3. **3b-2 traffic pruning**: after clone/drop becomes an 8B memcpy, work through the remaining hotspots
   (`attributes.to_map()` cloning every time, `call_compiled_method`'s `format!`, etc. —
   the known items of PLAN §5) in profile order.
- Gate (3b overall): int-arith 2x, fib +30% (the Phase 4a expectations in PERFORMANCE.md),
  bench-class `Value` clone/drop share 50% → 20% or below, GC-on bench-class residual +8% → +3% or below.

### 3.4 Risks and countermeasures

- **Send/Sync**: do not change the current `Arc`/`Gc`-based cross-thread model (payloads remain
  pointers).
- **Miri/UB**: confine tag manipulation to a single module and unit-verify it under Miri (same procedure as 5a/5b).
- **The sheer size of the flip**: if the 3b-0 API wall is finished, 3b-1 is small. Do not touch
  3b-1 before the wall is done (prevent a repeat of the 5b flip failure).

## 4. Layer 3c: biased reference counting

- **Frozen** (unchanged from ADR-0001 §3-6-3c). Entry trigger: **only if the atomic inc/dec of
  `Arc`/`Gc` remains near the top of the profile after the JIT (J3/J4) is complete**. If the JIT compiles int loops
  to native code, refcounting is highly likely to disappear from the hot path, in which case this is never needed.

## 5. Order and dependencies (overall)

```
[done] 3a GC default-on ──┬── H1..H4 hardening (as-needed, small-grained)
                          ├── T4..T6 Track B tail (small, parallelizable)
                          └── 3b NaN-boxing (3b-0 → 3b-1 → 3b-2)
                                    └── layer 4 JIT (ADR-0004: J1 → J2 → J3 → J4)
                                              └── 3c biased RC (only on a measured trigger)
```

- Batteries (PLAN §1) can proceed in parallel with this entire sequence (it does not depend on the Value representation).
- threaded dispatch (PLAN §5 Lever 3) is proposed **frozen because it overlaps with the JIT**
  (see ADR-0004 §4 J0).
