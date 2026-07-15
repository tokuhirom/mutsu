# ADR-0005: NaN-boxing representation switch (3b-1) — encoding choice and newtype-seal integration

- **Status**: **Accepted** (approved by tokuhirom 2026-07-12)
- **Date**: 2026-07-07 (Proposed) / 2026-07-12 (Accepted)
- **Deciders**: tokuhirom, Claude
- **Related**: [ADR-0001](0001-gc-strategy-and-phasing.md) (positioning of layer 3b; order 3a→3b→4),
  [ADR-0004](0004-jit-strategy.md) (the JIT assumes an 8B fixed-width `Value`),
  [gc-post-3a-roadmap.md](../gc-post-3a-roadmap.md) §3.2/§3.3 (representation policy, slice breakdown),
  [nanbox-3b0-api-wall.md](../nanbox-3b0-api-wall.md) (3b-0 API wall, migration recipe, §6 open questions, §7 seal),
  [PLAN.md](../../PLAN.md) §5 Lever 2

> 3b-0 (the API wall) is complete — direct `Value::<Variant>` references outside `src/value/` are **0**
> (guaranteed by the ratchet `check-value-wall.sh`, run as part of `make test`). This ADR decides the
> **single blocking design decision of 3b-1 (the `Value` 48B→8B representation switch), namely the
> encoding choice**, and also fixes the policy of folding the variant-privacy seal from wall doc §7.1
> into 3b-1. Implementation starts only after this ADR is Accepted.

---

## 1. Context

- `Value` is currently a 48B `enum` (~50 variants; the `value_size_guard` test pins it at `<= 48`).
  ~50% of bench-class is `Value::clone`/`drop_in_place`/`Vec::clone` (PERFORMANCE.md), and the
  +8% GC-default-on residual on bench-class rides on the same traffic. 3b changes `Value` into an
  **8B fixed-width** NaN-box and recovers all of these at once (roadmap §3.1). The JIT (ADR-0004)
  assumes an 8B fixed width, tag checks in a few instructions, and register residency — 3b is the
  groundwork for it.
- After 3b-0, all call sites go through `view()`/`ValueView`/accessors/constructors. Therefore 3b-1
  in principle rewrites **only the inside of `src/value/` (the representation module)**. As long as
  the wall holds, no changes to external call sites are needed (this was the point of 3b-0).
- An important fact discovered at 3b-0 completion (correcting one premise of this ADR):
  the **module-boundary approach to the variant-privacy seal (private submodule + `pub use`
  re-export), which wall doc §7.1 estimated as "small, mechanical, ~zero churn", does not actually
  seal anything**. Rust enum variants inherit the visibility of the enum itself, and re-exporting
  the type makes the variants reachable from outside as well (verified: after `pub use repr::Value;`,
  `Value::Int(3)` compiles in an external module). Since it is the same crate, `#[non_exhaustive]`
  is also ineffective. The only mechanism that truly seals at compile time is a **newtype wrapper**
  `struct Value(ValueRepr)` (private field + private `ValueRepr`), and that is a large refactor
  rewriting the **1293 sites** of direct variant references inside `src/value/` to go through `.0`.

### The 3-way variant classification fixed in 3b-0 (concretizing roadmap §3.2 / wall doc §2)

Classify the current enum by payload shape (this decides where each variant is stored in the 8B box):

- **inline scalar** (no heap; decoded value stored directly in the box payload; view is by-value):
  `Int` (small integers), `Bool`, `Nil`, `Whatever`, `HyperWhatever`, `Num` (raw f64),
  `Package(Symbol)`. **Note**: `Range*` (2×i64), `Rat`/`FatRat` (2×i64), and `Complex` (2×f64) have
  payloads exceeding 48 bits and cannot be inlined → boxed.
- **single-pointer (pointer tag)** (raw `Gc<T>`/`Arc<T>` pointer in the payload):
  `Str`, `Array`, `Hash`, `Set`, `Bag`, `Mix`, `Sub`, `WeakSub`, `LazyList`, `ContainerRef`,
  `Seq`/`HyperSeq`/`RaceSeq`/`Slip`, `Junction`, `Regex`, `BigInt`, `Promise`/`Channel`,
  `LazyThunk`, `Instance` (has the companion-field problem described below).
- **heap-boxed struct** (multiple fields → evacuated into a single heap box):
  `GenericRange`, `Range*`, `Rat`/`FatRat`/`BigRat`/`Complex`, `Pair`, `ValuePair`, `Enum`,
  `RegexWithAdverbs`, `Version`, `Capture`, `Uni`, `Proxy`, `ParametricRole`, `CustomType`,
  `CustomTypeInstance`, `Scalar`, `Mixin` (2 pointers), `LazyIoLines`, `HashEntryRef`,
  `CompUnitDepSpec`, `Routine` (3 Symbols).

**Companion-field problem** (an open question from wall doc §6): `Array(Gc, ArrayKind)`,
`Set/Bag/Mix(Gc, bool)`, and `Instance{Symbol, Gc, u64}` carry small fields besides the pointer.
There is a choice between co-locating pointer + companion bits in the 48-bit box, or moving the
companion bits into the pointee (§3.3).

## 2. Decision (proposed)

### 2.1 Encoding = **pointer-favored (NuN-boxing style)**, recommended for adoption

The box is a single 64-bit word. **Pointers are stored cleanly in the low bits**, and `f64` is
evacuated out of the quiet-NaN space via **offset encoding** (adding/subtracting a constant bias on
load/store). Inline small integers and tags are assigned to the remaining bit space (the exact bit
allocation is finalized at implementation time; §3.1).

**Reasons for choosing pointer-favored**:

1. **mutsu's hot paths are pointer-dominated.** Method dispatch and container access (`Str`/`Array`/
   `Hash`/`Instance`/`Sub`) are the most frequent, and pointer-favored makes their derefs
   **mask-free** (the raw pointer is used as-is).
2. **Minimal migration cost.** Since the raw pointer *is* the payload, `&self.0` can be transmuted
   to `&Arc<T>`/`&Gc<T>`, and the pointer-variant fields of `ValueView` can be **kept as the current
   `&'a Arc<T>`/`&'a Gc<T>`** (wall doc §2/§7.2). → Minimal changes to `view.rs`, zero changes to
   external call sites.
3. **The headline benchmarks are Int-bound.** int-arith and fib use `Int` (inline under either
   encoding); `Num` (f64) is secondary. The f64 offset add/subtract is 1 ALU instruction and lands
   on a non-primary path.

**Rejected alternative = NaN-favored (native double)**: hold `f64` as a raw value and tag-store
non-f64 in the quiet-NaN payload (the JSC/LuaJIT approach). It has the advantage of zero-cost `Num`
arithmetic, but (a) pointer derefs need a mask (AND), and (b) `&self.0` does not become a clean
`&Arc<T>`, so the pointer-variant fields of `ValueView` would need extra implementation as
**by-value guard types `ArcRef<'a,T>`/`GcRef<'a,T>`** (reconstructed types that `Deref` without
touching the refcount via `ManuallyDrop`). In mutsu, pointer/Int dominate over Num, so the payoff
is inverted.

**Proceed with both exits kept open**: the migration rules of wall doc §2 (restrict call sites to
deref-compatible usage — `s.clone()`/`s.is_empty()`/`&*s`/`Arc::ptr_eq` allowed; storing the `&Arc`
itself or comparing addresses not allowed) are already upheld as of 3b-0. Therefore, only if the
benchmarks below (§3.2) empirically show pointer-favored losing on Num-heavy workloads, an escape
route remains to introduce the guard types and switch to NaN-favored (no call-site changes needed;
only the pointer-field types in `view.rs` and the box internals are swapped).

### 2.2 The variant-privacy seal is **folded into 3b-1** (not a standalone PR)

Do not carry out the seal from wall doc §7.1 as a preceding independent step. Reasons:

1. The only effective mechanism for the seal (the newtype wrapper) is **exactly the structural first
   half of 3b-1 step 2**. 3b-1 turns `Value` into the newtype `struct Value(internal repr)`, so the
   newtype-ification seals the variants at compile time as a side effect. Inserting a standalone
   seal would touch the same 1293 sites twice.
2. Regression of the wall is already **deterministically detected** by the ratchet
   (`check-value-wall.sh`, integrated into `make test`). The only added value of a standalone seal
   is a belt-and-suspenders "move the CI failure forward to a compile failure", which does not
   justify a double refactor of 1293 sites.
3. The ratchet stays in place after the seal (a cheap regression net).

→ The first commit of 3b-1 becomes a **byte-identical newtype-ification** (the internal
representation stays the current enum; it is merely wrapped in `struct Value(ValueRepr)`), and this
doubles as the seal (§3.3 step A).

### 2.3 Slice structure (subdividing roadmap §3.3 step 2)

Split 3b-1 into "safe mechanical transformation (structure)" and "dangerous representation swap
(substance)":

- **step A (newtype seal, byte-identical)**: `pub enum Value` → `pub struct Value(ValueRepr)`
  (`ValueRepr` is a private enum in `src/value/`, keeping the current variants as-is). Mechanically
  convert the 1293 sites inside `src/value/` to go through `.0`. `ValueRepr` is still 48B, so
  **both behavior and size are unchanged**. Variants become sealed at compile time. Gate: make test
  byte-identical; `value_size_guard` stays at 48.
  **✅ Implemented (2026-07-11)**. Implementation shape: pattern / struct-like variant expression
  sites became `Value(ValueRepr::..)` (~1800 sites mechanically converted, driven by compile-error
  spans); tuple/unit variant **expression** sites use variant-name constructor shims (`fn Int(..)` /
  `const Nil` etc., visible only within `crate::value` — in step B these shim bodies become the
  NaN-box tag-packing constructors). Seal verification = `Value::Int(3)` outside `src/value/` fails
  to compile with E0624 (private). The 3 sites doing `mem::discriminant` variant comparison were
  replaced with the wall API `Value::same_variant()` (taking a discriminant of a struct is an
  `enum_intrinsics_non_enums` error — the seal also flushed out remaining idioms that assumed a
  non-enum was an enum).
- **step B (representation swap, substance)**: replace `ValueRepr` (48B enum) with the
  pointer-favored NaN-box (8B). Only the internals of `view()`/constructors/accessors/`with_*_mut`
  change. Update `value_size_guard` from 48 → 8.
  Gates: make test + full roast + gc-stress green; GC counters unchanged (the scalar/container
  discrimination of the type filter merely becomes a tag check); the microbenchmarks of §3.2
  achieved.
- Splitting step A from step B means the **intermediate state is always byte-identical** (step A) →
  the dangerous change (step B) stays confined behind the already-established newtype boundary.
  Bisecting/review also becomes easy.

## 3. Subordinate decisions finalized at implementation time (this ADR sets policy only)

### 3.1 Small-integer width

The width of inline `Int` (48-bit inline / 32-bit inline / out-of-range goes to a `Gc<i64>` box or
the `BigInt` box) is **decided at flip time using the int-arith / fib benchmarks** (roadmap §3.2).
Note that pointer-favored has a somewhat narrower tag space. Default proposal: ~48-bit inline;
out-of-range delegates to the existing `BigInt` path (a `Gc<i64>` box only if measurements demand
it).

### 3.2 Acceptance microbenchmarks (step B gate)

- **int-arith 2x**, **fib +30%** (PERFORMANCE.md Phase 4a expectations).
- bench-class `Value` clone/drop share **50% → 20% or below**.
- GC-on bench-class residual **+8% → +3% or below** (the overall 3b gate from roadmap §3.3).
- **pointer-favored validity check**: on a Num-heavy workload (add one microbenchmark with a
  complex-number / floating-point loop), confirm that the regression **stays within the single f64
  offset instruction**. Only if an unexpected regression shows up here, trigger the escape route of
  §2.1 (guard types, NaN-favored).

### 3.3 Placement of companion fields

The default is to **put the `ArrayKind` of `Array` and the mutability bool of `Set/Bag/Mix` in the
box's spare tag bits**, moving them into the pointee if bits get tight. `Instance`'s
`{class_name: Symbol, id: u64}` does not fit in a single pointer, so it is either pushed into the
`InstanceAttrs` side (already `Gc`) or given a dedicated heap box (finalized during step B design).

## 4. Consequences

- At step A completion the variants are sealed at compile time, achieving the goal of wall doc §7.1
  (the ratchet stays in place).
- Adopting pointer-favored keeps the pointer-variant fields of `ValueView` as the current
  `&Arc<T>`/`&Gc<T>`, minimizing changes to `view.rs` and external impact.
- `Num` arithmetic pays 1 f64 offset add/subtract instruction (non-primary path; validity measured
  in §3.2).
- Send/Sync is unchanged (the payload remains a pointer; roadmap §3.4). Tag manipulation is confined
  to a single module and unit-verified with Miri (roadmap §3.4).
- Completing 3b-1 satisfies the JIT's (ADR-0004 J1) prerequisite of an "8B fixed-width `Value`".
  Next comes 3b-2 (traffic pruning).

## 5. Alternatives considered

| Option | Pros | Cons | Verdict |
|---|---|---|---|
| **pointer-favored NaN-box (this ADR)** | No mask on pointer deref; minimal view migration (`&Arc<T>` kept); zero external impact | 1 offset instruction on f64 | **Recommended, adopted** |
| NaN-favored (native double) | Zero-cost `Num` arithmetic | Mask on pointer deref; view needs extra guard-type implementation (ArcRef/GcRef); Num is not dominant in mutsu | Rejected (kept as escape route) |
| Standalone variant-privacy seal first | Reviewable separately from the representation change | The effective mechanism = newtype = 3b-1 step A, so the 1293 sites get touched twice; regression already prevented by the ratchet | Rejected (folded into 3b-1) |
| module-boundary seal (`pub use` re-export) | Was expected to be zero churn | **Does not seal** (variants leak through the re-export; verified) | Rejected (infeasible) |
| Single flip (no step A/B split) | Fewer commits | 479 errors/type in 5b; unreviewable/unbisectable (roadmap §3.3/§3.4) | Rejected (staging mandatory) |

---

*This ADR was Accepted on 2026-07-12. 3b-1 step A (newtype seal, byte-identical) was completed ahead
of time (2026-07-11, news/2026-07.md). With the approval, **step B (the pointer-favored
representation swap) may begin** — accepted against the benchmark gates of §3.2 (make test + roast +
gc-stress green, plus the microbenchmarks).*
