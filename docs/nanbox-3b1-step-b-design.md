# 3b-1 step B: NaN-box representation — concrete design and campaign plan

Status: **Done** (2026-07-12 — B-guards #4467, B-flip #4469; `Value` is the
packed 8-byte word, gates all met: GC counters invariant vs main, every
release bench 5-9% faster, no Num regression).
Related: [ADR-0005](adr/0005-nanbox-representation-encoding.md) (encoding decision, Accepted
2026-07-12), [nanbox-3b0-api-wall.md](nanbox-3b0-api-wall.md) (wall API + guard-type exit),
[gc-post-3a-roadmap.md](gc-post-3a-roadmap.md) §3 (gates), [PLAN.md](../PLAN.md) §2.

This document fixes the decisions ADR-0005 deferred to implementation time
(§3.1 small-int width, §3.3 companion-field placement, exact bit allocation)
and lays out the slice plan for the representation flip. The bit layout lives
in code in `src/value/nanbox/` — that module is the single owner of the
encoding; this document explains the *why*.

## 1. Bit layout (implemented in `src/value/nanbox/mod.rs`)

One 64-bit word (`NonZeroU64`, so `Option<Value>` keeps the 8-byte niche):

| page (high 16 bits) | meaning | payload (low 48 bits) |
|---|---|---|
| `0x0000` | never constructed (the `NonZeroU64` niche lives here) | — |
| `0x0001` | inline `Int` | 48-bit two's complement (±2^47) |
| `0x0002..=0xFFF2` | `Num` | word = `f64::to_bits` + `0x0002_0000_0000_0000` |
| `0xFFF3..=0xFFFF` | kind pages | kind = (page − 0xFFF3)·8 + subkind; 13×8 = **104 kinds** |

- **Doubles are offset-encoded** (pointer-favored, ADR-0005 §2.1): one `add`
  on pack, one `sub` on unpack. NaN is **canonicalized to +qNaN** at pack time
  — raw negative-NaN pages (`0xFFF1..=0xFFFF`) would collide with the kind
  pages. Raku never exposes NaN payload bits, so this is unobservable.
- **Subkind = 3 payload bits.** On 64-bit targets they are the pointer's low
  (alignment) bits: every payload allocation is ≥8-aligned (const-asserted in
  `boxes.rs`), so a pointer deref is a single AND with
  `0x0000_FFFF_FFFF_FFF8`. On 32-bit targets (wasm32) allocations may be
  4-aligned, so the subkind moves to payload bits 40..43, above any address.
- **Inline payloads** (`Bool`, `Symbol` ids for `Package`/`CompUnitDepSpec`)
  sit in bits 8..40 — clear of the subkind under both placements.
- **48-bit address assumption**: debug-asserted at every pack. This is the
  same assumption every production NaN-boxing JS engine makes; wasm32 (32-bit)
  is trivially within range.

Deviation from ADR-0005 to record: the ADR hoped pointer-favored would let
`view()` keep returning `&'a Arc<T>` / `&'a Gc<T>` by transmuting `&self.0`.
That only works if the *whole word* equals the pointer bits — impossible here,
because several variants share a pointee type (`Str`/`Regex` are both
`Arc<String>`; `Seq`/`HyperSeq`/`RaceSeq`/`Slip`/`Junction` are all
`Arc<Vec<Value>>`), so the variant tag **must** live in the word and the view
must reconstruct smart pointers by value. The flip therefore uses the **guard
types** (`ArcRef<'a,T>`/`GcRef<'a,T>`: `ManuallyDrop` reconstructions that
`Deref` without touching refcounts) that the wall kept as the documented exit
(wall doc §2, ADR-0005 §2.1 "両出口"). Call sites are already deref-compatible
by the 3b-0 migration rule and `ValueView` is deliberately non-`Copy`, so this
does not disturb the wall. The *rest* of the pointer-favored rationale stands:
pointer deref pays one AND (vs. the Num-favored layout's win on `Num` only),
and Int/pointer traffic dominates mutsu's hot paths.

## 2. Kind table decisions (the ADR §3.1/§3.3 deferred points)

- **Small-int width = 48 bits inline** (±2^47). An `i64` outside that range
  packs as `Kind::IntBoxed` (`Arc<i64>`) and still decodes to
  `ValueRepr::Int` — **lossless**, no coupling to the `BigInt` semantic path
  (`.WHAT`, `as_int()` behavior unchanged). If int-arith benches at flip time
  show the range check hurting, narrowing to 32-bit inline is a two-constant
  change in `nanbox/mod.rs`.
- **Companion fields ride the tag** (ADR §3.3 "spare tag bits" option):
  `ArrayKind` → 6 kinds, `Hash` itemization → 2, `Set`/`Bag`/`Mix` mutability
  → 2 each, `JunctionKind` → 4. Kind space used: 66 of 104.
- **`Instance` stores only the `Gc<InstanceAttrs>` pointer.** The variant's
  `class_name`/`id` copies are always equal to the pointee's own fields:
  rebless goes through `instance_sharing_cell`, which forks a fresh
  `InstanceAttrs` node when the class differs (and `debug_assert`s the id).
  `from_repr` debug-asserts this invariant; `into_repr` reads both back from
  the pointee. No per-value heap box, so Instance clone stays one refcount op.
- **Multi-field variants move behind one `Arc<...Box>`** (`Pair`, `Capture`,
  `Proxy`, `Version`, `Routine`, `GenericRange`, …, defined in
  `nanbox/boxes.rs`). `Rat`/`FatRat`/`Range*` (2×i64) and `Complex` (2×f64)
  also box — they exceed 48 bits. Consequences, accepted per ADR:
  - constructing one allocates (today they are inline in the 48-byte enum);
  - `Value::clone` of one becomes a refcount bump (today: deep copy for
    `Box`-payload variants like `BigRat`/`Capture` — cheaper now);
  - owned decode (`into_repr`) moves the payload out when the box is uniquely
    owned (`Arc::unwrap_or_clone`), clones field-wise when shared. Sharing is
    never observable: the boxes are immutable-by-convention — mutation decodes,
    edits, re-packs a fresh box (same visibility semantics as today's `&mut`
    on an unshared enum payload).
  - If a boxed-variant hot path shows up in benches (e.g. `Rat` arithmetic,
    `Range` loop headers), the fix is a dedicated inline kind (e.g. small-Rat
    with 22-bit num/den) — kind space has 38 free slots.
- **`Scalar(Box<Value>)` → `Arc<Value>`**: clone drops from deep-copy to bump;
  decode gives `Box` back via unwrap-or-clone.
- **`Promise`/`Channel`** pack the `Gc` inside `SharedPromise`/`SharedChannel`
  directly (the wrapper structs are just a `Gc` newtype).

## 3. Ownership model (`src/value/nanbox/`)

A kind word **owns exactly one strong reference** of its payload
(`Arc::into_raw` / `Gc::into_raw` / `WeakGc::into_raw` at pack;
`from_raw` at consume). `Clone`/`Drop` reconstruct the smart pointer and call
its own `clone`/`drop`, so **all Bacon-Rajan bookkeeping is preserved
verbatim** — `Gc::drop`'s survivor-buffering, `finalize` (DESTROY queueing),
the buffered-bit dedup, and the `collecting()` reclaim-window gate all run
exactly as they do for the enum. GC counters must be invariant across the flip
(ADR-0005 step-B gate); the type filter (scalars GC-free) becomes "scalar
pages/kinds have no `Gc` payload".

Raw-pointer round-trips use `expose_provenance`/`with_exposed_provenance`, so
the module is Miri-checkable (`cargo miri test value::nanbox`).

`Gc`/`WeakGc` gained `into_raw`/`from_raw` (`src/gc/gc_ptr.rs`); `Symbol`
gained `from_id` (inline payload round-trip).

## 4. Campaign plan (remaining slices)

The flip cannot be one PR: 866 sites inside `src/value/` still name
`ValueRepr::` in patterns (they were sealed relative to the *outside* in step
A, but the *inside* still assumes the enum is the storage). Plan, each slice
gated on `make test` byte-identical + CI roast:

1. **B-core (this slice)**: `src/value/nanbox/` — the packed word, lossless
   `from_repr`/`into_repr`, Clone/Drop, exhaustive round-trip tests. Unwired.
2. **B-wall-in (multiple mechanical PRs)**: migrate `src/value/` internals off
   direct `ValueRepr::` patterns onto the same wall the outside already uses:
   - borrowed reads → `view()` matches;
   - owned matches (`match self.0`) → `match self.into_repr()` (identity while
     the enum is still the storage; zero refcount traffic after the flip);
   - in-place payload mutation → `with_*_mut` closures / decode-edit-repack.
   Worklist = `grep -c 'ValueRepr::'` per file (types_isa 142, value_eq 97,
   display 91, …). The step-A error-span fixer technique applies. `view.rs`
   and the `mod.rs` constructor shims are the seam and stay direct.
3. **B-guards**: switch `ValueView`'s pointer fields from `&'a Arc<T>` /
   `&'a Gc<T>` to the guard types. Call sites are deref-compatible already;
   fallout is expected to be explicit type annotations only. Still enum
   storage; byte-identical.
4. **B-flip**: `Value(ValueRepr)` → `Value(NanBox)`; constructor shims call
   `NanBox::from_repr`-shaped packers, `view()` decodes tags into guard-type
   views, `same_variant` compares tag/kind, Trace/serde/Debug ride the seam.
   `value_size_guard` 48→8. Gates (ADR-0005 §3.2): make test + full roast +
   gc-stress green, GC counters invariant, int-arith 2x / fib +30%,
   bench-class clone/drop share 50%→≤20%, GC-on residual +8%→≤+3%, and the
   Num-regression check (offset must cost ~1 ALU; otherwise the NaN-favored
   fallback re-opens per ADR §2.1).
5. **3b-2 traffic pruning** (roadmap §3.3) once clone/drop is an 8-byte copy.
