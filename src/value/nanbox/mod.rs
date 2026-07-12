//! NaN-boxed 8-byte `Value` representation (layer 3b-1 step B core).
//!
//! [`NanBox`] is the packed storage the representation flip will put behind
//! the `Value(...)` newtype seal: one 64-bit word encoding every [`ValueRepr`]
//! variant. This module is the **only** place that knows the bit layout;
//! everything else converses in `ValueRepr` (the decode/working enum) through
//! [`NanBox::from_repr`] / [`NanBox::into_repr`].
//!
//! Encoding (pointer-favored per ADR-0005 §2.1, bit allocation fixed here —
//! see `docs/nanbox-3b1-step-b-design.md`):
//!
//! ```text
//! word = [ page: 16 bits ][ payload: 48 bits ]
//!
//! page 0x0000          : never constructed (0 stays the NonZeroU64 niche)
//! page 0x0001          : small Int — payload = 48-bit two's complement
//! page 0x0002..=0xFFF2 : f64 — word = f64::to_bits(n) + 0x0002_0000_0000_0000
//!                        (NaN canonicalized to +qNaN at pack time)
//! page 0xFFF3..=0xFFFF : kind pages — kind = (page - 0xFFF3)*8 + subkind,
//!                        subkind = 3 payload bits (the pointer's tag-free
//!                        alignment bits on 64-bit targets)
//! ```
//!
//! Payload ownership: a pointer-kind word **owns** one strong reference of its
//! `Arc<T>` / `Gc<T>` / `WeakGc<T>` payload (moved in via `into_raw`, moved
//! out via `from_raw`). `Clone`/`Drop` reconstruct the smart pointer and use
//! its own `clone`/`drop`, so all refcount and Bacon-Rajan candidate
//! bookkeeping (`Gc::drop`'s buffered-bit path) is preserved exactly.

use std::mem::ManuallyDrop;
use std::num::NonZeroU64;
use std::sync::Arc;

use crate::gc::{Gc, GcBox, Trace, WeakGc};

use super::*;

mod boxes;
mod decode;
mod encode;
mod peek;
#[cfg(test)]
mod tests;

pub(in crate::value) use boxes::*;

// ---- bit layout -------------------------------------------------------------

const TAG_SHIFT: u32 = 48;
/// Page of the inline small-Int encoding.
const INT_PAGE: u64 = 0x0001;
/// Added to `f64::to_bits` at pack time. Raw (NaN-canonicalized) doubles span
/// pages `0x0000..=0xFFF0`, so encoded doubles span `0x0002..=0xFFF2` — leaving
/// page 0 (niche), page 1 (Int) and `0xFFF3..=0xFFFF` (kinds) free.
const DOUBLE_OFFSET: u64 = 0x0002 << TAG_SHIFT;
/// First page of the kind space: 13 pages x 8 subkinds = 104 encodable kinds.
const KIND_PAGE_BASE: u64 = 0xFFF3;
const PAYLOAD48_MASK: u64 = 0x0000_FFFF_FFFF_FFFF;

/// Where the 3 subkind bits live inside the payload. On 64-bit targets they
/// are the pointer's low (alignment) bits — every payload allocation is
/// 8-aligned (checked in `boxes`), so the pointer address itself is tag-free
/// and deref needs a single AND. On 32-bit targets (wasm32) allocations may be
/// only 4-aligned, but pointers fit in 32 bits, so the subkind moves to bits
/// 40..43, far above any address bit.
#[cfg(target_pointer_width = "64")]
const SUB_SHIFT: u32 = 0;
#[cfg(not(target_pointer_width = "64"))]
const SUB_SHIFT: u32 = 40;
const SUB_MASK: u64 = 0x7 << SUB_SHIFT;
/// Payload bits that hold a pointer-kind's address.
const PTR_MASK: u64 = PAYLOAD48_MASK & !SUB_MASK;
/// Inline (non-pointer) kind payloads sit in bits 8..40: clear of the subkind
/// bits under BOTH `SUB_SHIFT` choices, and 32 bits is enough for every inline
/// payload (`Symbol` id, bool).
const INLINE_SHIFT: u32 = 8;

/// Smallest/largest i64 that inline-encode in the 48-bit Int payload.
const SMALL_INT_MIN: i64 = -(1 << 47);
const SMALL_INT_MAX: i64 = (1 << 47) - 1;

#[inline]
fn small_int_fits(v: i64) -> bool {
    (SMALL_INT_MIN..=SMALL_INT_MAX).contains(&v)
}

/// The canonical quiet NaN every packed NaN collapses to. Raw negative-NaN bit
/// patterns (pages 0xFFF1..=0xFFFF) would collide with the kind pages after the
/// double offset, so NaN payload/sign bits are NOT representable — Raku never
/// exposes them.
const CANONICAL_NAN: u64 = 0x7FF8_0000_0000_0000;

// ---- kinds ------------------------------------------------------------------

/// Discriminates every non-Int, non-Num variant. Discriminants are contiguous
/// from 0 (checked by `KIND_COUNT` fitting the 104-kind capacity in a const
/// assert below, and round-tripped exhaustively in tests).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub(in crate::value) enum Kind {
    // -- Arc-backed pointer kinds (payload = Arc<T> raw address) --
    Str = 0,
    Regex,
    BigInt,
    /// An `i64` outside the 48-bit inline range, boxed (`ValueRepr::Int` is
    /// lossless: the boxed form still decodes to `Int`, never `BigInt`).
    IntBoxed,
    Seq,
    HyperSeq,
    RaceSeq,
    Slip,
    JunctionAny,
    JunctionAll,
    JunctionOne,
    JunctionNone,
    Mixin,
    Scalar,
    LazyThunk,
    Uni,
    RegexWithAdverbs,
    CustomType,
    CustomTypeInstance,
    Range,
    RangeExcl,
    RangeExclStart,
    RangeExclBoth,
    Rat,
    FatRat,
    BigRat,
    Complex,
    Pair,
    ValuePair,
    Enum,
    GenericRange,
    Version,
    Capture,
    Proxy,
    ParametricRole,
    Routine,
    LazyIoLines,
    HashEntryRef,
    // -- Gc-backed pointer kinds (payload = GcBox<T> raw address) --
    Sub,
    WeakSub,
    LazyList,
    ContainerRef,
    Promise,
    Channel,
    /// `class_name` and `id` are NOT stored: `Value::Instance` keeps them equal
    /// to the pointee's own fields (`instance_sharing_cell` reblesses into a
    /// fresh `InstanceAttrs` node), so the pointee is the single source.
    Instance,
    ArrayList,
    ArrayArray,
    ArrayItemList,
    ArrayItemArray,
    ArrayShaped,
    ArrayLazy,
    HashPlain,
    HashItemized,
    SetImm,
    SetMut,
    BagImm,
    BagMut,
    MixImm,
    MixMut,
    // -- inline kinds (payload in bits 8..40) --
    Nil,
    Whatever,
    HyperWhatever,
    Bool,
    Package,
    CompUnitDepSpec,
}

pub(in crate::value) const KIND_COUNT: u8 = Kind::CompUnitDepSpec as u8 + 1;

// 13 kind pages x 8 subkinds.
const _: () = assert!(KIND_COUNT as u64 <= (0xFFFF - KIND_PAGE_BASE + 1) * 8);

#[inline]
fn kind_from_u8(k: u8) -> Kind {
    debug_assert!(k < KIND_COUNT, "invalid nanbox kind {k}");
    // SAFETY: `Kind` is a fieldless #[repr(u8)] enum with contiguous
    // discriminants 0..KIND_COUNT, and every word carrying a kind was packed
    // by `word_for_kind` from a valid `Kind`.
    unsafe { std::mem::transmute::<u8, Kind>(k) }
}

// ---- word assembly / disassembly ---------------------------------------------

#[inline]
const fn word_for_kind(kind: Kind, payload: u64) -> NonZeroU64 {
    debug_assert!(payload & !PAYLOAD48_MASK == 0, "payload exceeds 48 bits");
    debug_assert!(
        payload & SUB_MASK == 0,
        "payload collides with subkind bits"
    );
    let k = kind as u64;
    let bits = ((KIND_PAGE_BASE + (k >> 3)) << TAG_SHIFT) | payload | ((k & 0x7) << SUB_SHIFT);
    // The page is >= 0xFFF3, so the word is never 0.
    match NonZeroU64::new(bits) {
        Some(w) => w,
        None => panic!("kind words are always nonzero"),
    }
}

#[inline]
fn pack_small_int(v: i64) -> NonZeroU64 {
    debug_assert!(small_int_fits(v));
    let bits = (INT_PAGE << TAG_SHIFT) | ((v as u64) & PAYLOAD48_MASK);
    NonZeroU64::new(bits).expect("int words carry page 1")
}

#[inline]
fn pack_num(f: f64) -> NonZeroU64 {
    let raw = if f.is_nan() {
        CANONICAL_NAN
    } else {
        f.to_bits()
    };
    // raw <= 0xFFF0.. (-Inf) after canonicalization, so no wrap: encoded page
    // is 0x0002..=0xFFF2 and the word is never 0.
    NonZeroU64::new(raw.wrapping_add(DOUBLE_OFFSET)).expect("num words carry page >= 2")
}

#[inline]
const fn pack_inline(kind: Kind, payload: u32) -> NonZeroU64 {
    word_for_kind(kind, (payload as u64) << INLINE_SHIFT)
}

#[inline]
fn inline_payload(bits: u64) -> u32 {
    (((bits & PAYLOAD48_MASK) & !SUB_MASK) >> INLINE_SHIFT) as u32
}

/// What a word's tag bits say it is, before touching any payload.
enum Classified {
    Int(i64),
    Num(f64),
    Kind(Kind),
}

#[inline]
fn classify(bits: u64) -> Classified {
    let page = bits >> TAG_SHIFT;
    if page == INT_PAGE {
        // Sign-extend the 48-bit payload.
        Classified::Int(((bits << 16) as i64) >> 16)
    } else if page < KIND_PAGE_BASE {
        debug_assert!(page >= 2, "page 0 is the niche; page 1 handled above");
        Classified::Num(f64::from_bits(bits.wrapping_sub(DOUBLE_OFFSET)))
    } else {
        let k = ((page - KIND_PAGE_BASE) << 3) | ((bits & SUB_MASK) >> SUB_SHIFT);
        Classified::Kind(kind_from_u8(k as u8))
    }
}

// ---- pointer payload round-trip ----------------------------------------------

#[inline]
fn addr_of_payload(bits: u64) -> usize {
    (bits & PTR_MASK) as usize
}

#[inline]
fn pack_addr(kind: Kind, addr: usize) -> NonZeroU64 {
    let a = addr as u64;
    debug_assert_eq!(
        a & !PTR_MASK,
        0,
        "payload pointer outside the encodable address space"
    );
    word_for_kind(kind, a)
}

#[inline]
fn pack_arc<T>(kind: Kind, a: Arc<T>) -> NonZeroU64 {
    pack_addr(kind, Arc::into_raw(a).expose_provenance())
}

/// # Safety
/// `bits` must be a word packed by `pack_arc::<T>` whose ownership is being
/// consumed (at most once per packed word).
#[inline]
unsafe fn take_arc<T>(bits: u64) -> Arc<T> {
    unsafe {
        Arc::from_raw(std::ptr::with_exposed_provenance::<T>(addr_of_payload(
            bits,
        )))
    }
}

#[inline]
fn pack_gc<T: Trace + 'static>(kind: Kind, g: Gc<T>) -> NonZeroU64 {
    pack_addr(kind, Gc::into_raw(g).expose_provenance())
}

/// # Safety
/// `bits` must be a word packed by `pack_gc::<T>` whose ownership is being
/// consumed (at most once per packed word).
#[inline]
unsafe fn take_gc<T: Trace + 'static>(bits: u64) -> Gc<T> {
    unsafe {
        Gc::from_raw(std::ptr::with_exposed_provenance::<GcBox<T>>(
            addr_of_payload(bits),
        ))
    }
}

#[inline]
fn pack_weak<T: Trace + 'static>(kind: Kind, w: WeakGc<T>) -> NonZeroU64 {
    pack_addr(kind, WeakGc::into_raw(w).expose_provenance())
}

/// # Safety
/// `bits` must be a word packed by `pack_weak::<T>` whose ownership is being
/// consumed (at most once per packed word).
#[inline]
unsafe fn take_weak<T: Trace + 'static>(bits: u64) -> WeakGc<T> {
    unsafe {
        WeakGc::from_raw(std::ptr::with_exposed_provenance::<GcBox<T>>(
            addr_of_payload(bits),
        ))
    }
}

// ---- clone / drop dispatch -----------------------------------------------------

enum PayloadOp {
    /// Add one owned reference to the payload (the cloned word becomes a
    /// second owner).
    CloneBump,
    /// Release this word's owned reference.
    Release,
}

/// # Safety
/// `bits` must be a live word whose payload matches `T` under `pack_arc`.
unsafe fn arc_op<T>(bits: u64, op: PayloadOp) {
    match op {
        PayloadOp::CloneBump => unsafe {
            Arc::increment_strong_count(std::ptr::with_exposed_provenance::<T>(addr_of_payload(
                bits,
            )));
        },
        PayloadOp::Release => drop(unsafe { take_arc::<T>(bits) }),
    }
}

/// # Safety
/// `bits` must be a live word whose payload matches `T` under `pack_gc`.
unsafe fn gc_op<T: Trace + 'static>(bits: u64, op: PayloadOp) {
    match op {
        PayloadOp::CloneBump => {
            // Reconstruct without consuming, clone through `Gc::clone` (which
            // does the Bacon-Rajan strong-count + Black recolor bookkeeping),
            // and forget the handle wrapper: the new word owns the bump.
            let g = ManuallyDrop::new(unsafe { take_gc::<T>(bits) });
            let dup = Gc::clone(&g);
            let addr = Gc::into_raw(dup).expose_provenance();
            debug_assert_eq!(addr, addr_of_payload(bits));
        }
        // Full `Gc::drop` semantics: candidate buffering, finalize, sweep.
        PayloadOp::Release => drop(unsafe { take_gc::<T>(bits) }),
    }
}

/// # Safety
/// `bits` must be a live word whose payload matches `T` under `pack_weak`.
unsafe fn weak_op<T: Trace + 'static>(bits: u64, op: PayloadOp) {
    match op {
        PayloadOp::CloneBump => {
            let w = ManuallyDrop::new(unsafe { take_weak::<T>(bits) });
            let dup = WeakGc::clone(&w);
            let addr = WeakGc::into_raw(dup).expose_provenance();
            debug_assert_eq!(addr, addr_of_payload(bits));
        }
        PayloadOp::Release => drop(unsafe { take_weak::<T>(bits) }),
    }
}

/// Apply `op` to the payload reference owned by `bits`. The single
/// kind -> payload-type map for `Clone`/`Drop`; `decode` has its own exhaustive
/// match because it also moves the payload into `ValueRepr` fields.
///
/// # Safety
/// `bits` must be a live kind word (not yet released).
unsafe fn payload_op(kind: Kind, bits: u64, op: PayloadOp) {
    unsafe {
        match kind {
            Kind::Str | Kind::Regex => arc_op::<String>(bits, op),
            Kind::BigInt => arc_op::<NumBigInt>(bits, op),
            Kind::IntBoxed => arc_op::<i64>(bits, op),
            Kind::Seq
            | Kind::HyperSeq
            | Kind::RaceSeq
            | Kind::Slip
            | Kind::JunctionAny
            | Kind::JunctionAll
            | Kind::JunctionOne
            | Kind::JunctionNone => arc_op::<Vec<Value>>(bits, op),
            Kind::Mixin => arc_op::<MixinBox>(bits, op),
            Kind::Scalar => arc_op::<Value>(bits, op),
            Kind::LazyThunk => arc_op::<LazyThunkData>(bits, op),
            Kind::Uni => arc_op::<UniData>(bits, op),
            Kind::RegexWithAdverbs => arc_op::<RegexAdverbs>(bits, op),
            Kind::CustomType => arc_op::<CustomTypeData>(bits, op),
            Kind::CustomTypeInstance => arc_op::<CustomTypeInstanceData>(bits, op),
            Kind::Range
            | Kind::RangeExcl
            | Kind::RangeExclStart
            | Kind::RangeExclBoth
            | Kind::Rat
            | Kind::FatRat => arc_op::<I64Pair>(bits, op),
            Kind::BigRat => arc_op::<BigRatBox>(bits, op),
            Kind::Complex => arc_op::<F64Pair>(bits, op),
            Kind::Pair => arc_op::<PairBox>(bits, op),
            Kind::ValuePair => arc_op::<ValuePairBox>(bits, op),
            Kind::Enum => arc_op::<EnumBox>(bits, op),
            Kind::GenericRange => arc_op::<GenericRangeBox>(bits, op),
            Kind::Version => arc_op::<VersionBox>(bits, op),
            Kind::Capture => arc_op::<CaptureBox>(bits, op),
            Kind::Proxy => arc_op::<ProxyBox>(bits, op),
            Kind::ParametricRole => arc_op::<ParametricRoleBox>(bits, op),
            Kind::Routine => arc_op::<RoutineBox>(bits, op),
            Kind::LazyIoLines => arc_op::<LazyIoLinesBox>(bits, op),
            Kind::HashEntryRef => arc_op::<HashEntryRefBox>(bits, op),
            Kind::Sub => gc_op::<SubData>(bits, op),
            Kind::WeakSub => weak_op::<SubData>(bits, op),
            Kind::LazyList => gc_op::<LazyList>(bits, op),
            Kind::ContainerRef => gc_op::<Mutex<Value>>(bits, op),
            Kind::Promise => gc_op::<(Mutex<PromiseState>, Condvar)>(bits, op),
            Kind::Channel => gc_op::<(Mutex<ChannelState>, Condvar)>(bits, op),
            Kind::Instance => gc_op::<InstanceAttrs>(bits, op),
            Kind::ArrayList
            | Kind::ArrayArray
            | Kind::ArrayItemList
            | Kind::ArrayItemArray
            | Kind::ArrayShaped
            | Kind::ArrayLazy => gc_op::<ArrayData>(bits, op),
            Kind::HashPlain | Kind::HashItemized => gc_op::<HashData>(bits, op),
            Kind::SetImm | Kind::SetMut => gc_op::<SetData>(bits, op),
            Kind::BagImm | Kind::BagMut => gc_op::<BagData>(bits, op),
            Kind::MixImm | Kind::MixMut => gc_op::<MixData>(bits, op),
            Kind::Nil
            | Kind::Whatever
            | Kind::HyperWhatever
            | Kind::Bool
            | Kind::Package
            | Kind::CompUnitDepSpec => {}
        }
    }
}

// ---- JIT Tier B raw-word exports ------------------------------------------------

/// Word-level constants and probes for the Cranelift Tier B inline emitter
/// (`vm/vm_jit_tier_b.rs`), which manipulates NaN-box words directly in
/// native code (ADR-0004 §2.3 Tier B). This is the ONLY bit-layout knowledge
/// exported outside `crate::value`; every item is derived from the layout
/// constants above, so a layout change flows into the JIT automatically.
#[cfg(feature = "jit")]
pub(crate) mod jit_words {
    use super::*;

    /// `word >> PAGE_SHIFT` is the 16-bit page.
    pub(crate) const PAGE_SHIFT: u32 = TAG_SHIFT;
    /// Page of inline small Ints (48-bit two's complement payload).
    pub(crate) const INT_PAGE: u64 = super::INT_PAGE;
    /// Subtracted from an encoded Num word to recover `f64::to_bits`.
    pub(crate) const DOUBLE_OFFSET: u64 = super::DOUBLE_OFFSET;
    /// Encoded doubles span pages `NUM_PAGE_MIN..=NUM_PAGE_MAX`.
    pub(crate) const NUM_PAGE_MIN: u64 = 0x0002;
    pub(crate) const NUM_PAGE_MAX: u64 = 0xFFF2;
    pub(crate) const PAYLOAD48_MASK: u64 = super::PAYLOAD48_MASK;

    /// Full-word constants for the payload-free singletons.
    pub(crate) const TRUE_BITS: u64 = pack_inline(Kind::Bool, 1).get();
    pub(crate) const FALSE_BITS: u64 = pack_inline(Kind::Bool, 0).get();
    /// The encoded word of the canonical quiet NaN (every packed NaN
    /// collapses to this — the Tier B float-arith fast path selects it
    /// whenever a native float op produces any NaN).
    pub(crate) const NUM_CANONICAL_NAN_WORD: u64 = CANONICAL_NAN.wrapping_add(DOUBLE_OFFSET);

    /// Kind probe: `word & KIND_MASK == <kind pattern>` ⟺ the word is that
    /// exact kind (page bits + subkind bits, payload ignored).
    pub(crate) const KIND_MASK: u64 = (0xFFFF << TAG_SHIFT) | SUB_MASK;
    /// `Kind::Pair` probe pattern (for the `ContainerizePair` fast skip).
    pub(crate) const PAIR_PATTERN: u64 = word_for_kind(Kind::Pair, 0).get();

    /// True when duplicating/discarding this word needs no refcount or GC
    /// bookkeeping: small Int, Num, or a payload-free inline kind. The Tier B
    /// `LoadConst` emitter inlines the push of such a constant as a raw
    /// immediate store.
    pub(crate) fn is_refcount_free(bits: u64) -> bool {
        match classify(bits) {
            Classified::Int(_) | Classified::Num(_) => true,
            Classified::Kind(k) => matches!(
                k,
                Kind::Nil
                    | Kind::Whatever
                    | Kind::HyperWhatever
                    | Kind::Bool
                    | Kind::Package
                    | Kind::CompUnitDepSpec
            ),
        }
    }
}

// ---- NanBox --------------------------------------------------------------------

/// The packed 8-byte value word. Owns one reference of any pointer payload.
pub(in crate::value) struct NanBox(NonZeroU64);

// SAFETY: a NanBox is semantically one `ValueRepr` (all of whose payloads are
// `Send + Sync` — asserted on `Value` in `value::mod`); the packed form adds
// no thread affinity.
unsafe impl Send for NanBox {}
unsafe impl Sync for NanBox {}

impl NanBox {
    /// Raw word bits (tests / future tag-dispatch fast paths).
    #[inline]
    #[allow(dead_code)]
    pub(in crate::value) fn bits(&self) -> u64 {
        self.0.get()
    }

    // Inline-kind constants (no payload ownership, safe to duplicate freely) —
    // the post-flip bodies of `Value::NIL` / `Value::TRUE` / `Value::WHATEVER`.
    pub(in crate::value) const NIL: NanBox = NanBox(pack_inline(Kind::Nil, 0));
    pub(in crate::value) const WHATEVER: NanBox = NanBox(pack_inline(Kind::Whatever, 0));
    pub(in crate::value) const HYPER_WHATEVER: NanBox = NanBox(pack_inline(Kind::HyperWhatever, 0));
    pub(in crate::value) const TRUE: NanBox = NanBox(pack_inline(Kind::Bool, 1));
    pub(in crate::value) const FALSE: NanBox = NanBox(pack_inline(Kind::Bool, 0));
}

impl Clone for NanBox {
    fn clone(&self) -> Self {
        let bits = self.0.get();
        if let Classified::Kind(kind) = classify(bits) {
            // SAFETY: `self` is live, so the word owns its payload reference;
            // CloneBump adds one for the new word.
            unsafe { payload_op(kind, bits, PayloadOp::CloneBump) };
        }
        NanBox(self.0)
    }
}

impl Drop for NanBox {
    fn drop(&mut self) {
        let bits = self.0.get();
        if let Classified::Kind(kind) = classify(bits) {
            // SAFETY: dropping consumes this word's payload ownership exactly
            // once.
            unsafe { payload_op(kind, bits, PayloadOp::Release) };
        }
    }
}

impl std::fmt::Debug for NanBox {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Decode through a clone so Debug (used by tests and trace logs)
        // matches the repr's output byte-for-byte.
        self.clone().into_repr().fmt(f)
    }
}
