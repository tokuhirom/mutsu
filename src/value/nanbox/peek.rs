//! `NanBox` -> `ValueView` borrowed decode (the post-flip `Value::view()`
//! body), plus the word-level accessors behind the wall's `as_*` probes.
//!
//! Lifetimes: every `&'a` handed out here points into a payload allocation
//! whose one strong reference is owned by the word itself. The caller borrows
//! the `NanBox` for `'a`, so the payload cannot be released while the borrow
//! lives; smart-pointer payloads are reconstructed by value inside
//! [`RefGuard`]s (never dropped), pointee references are derived from the raw
//! payload address.

use super::*;

/// Borrow the pointee of an `Arc`-kind word.
///
/// # Safety
/// `bits` must be a live word packed with an `Arc<T>` payload, borrowed for `'a`.
#[inline]
unsafe fn peek_arc<'a, T>(bits: u64) -> &'a T {
    unsafe { &*std::ptr::with_exposed_provenance::<T>(addr_of_payload(bits)) }
}

/// Borrow the pointee of a `Gc`-kind word.
///
/// # Safety
/// `bits` must be a live word packed with a `Gc<T>` payload, borrowed for `'a`.
#[inline]
unsafe fn peek_gc<'a, T: Trace + 'static>(bits: u64) -> &'a T {
    let g = ManuallyDrop::new(unsafe { take_gc::<T>(bits) });
    // SAFETY: the pointee outlives 'a because the word owns a strong Gc
    // reference for at least that long; `g` is only a borrow-shaped handle.
    unsafe { std::mem::transmute::<&T, &'a T>(&g) }
}

/// # Safety
/// `bits` must be a live word packed with an `Arc<T>` payload, borrowed for `'a`.
#[inline]
unsafe fn arc_guard<'a, T>(bits: u64) -> ArcRef<'a, T> {
    RefGuard::from_reconstructed(unsafe { take_arc::<T>(bits) })
}

/// # Safety
/// `bits` must be a live word packed with a `Gc<T>` payload, borrowed for `'a`.
#[inline]
unsafe fn gc_guard<'a, T: Trace + 'static>(bits: u64) -> GcRef<'a, T> {
    RefGuard::from_reconstructed(unsafe { take_gc::<T>(bits) })
}

impl NanBox {
    /// Decode into a borrowed [`ValueView`] — the post-flip `Value::view()`.
    #[inline]
    pub(in crate::value) fn view(&self) -> ValueView<'_> {
        let bits = self.0.get();
        match classify(bits) {
            Classified::Int(v) => ValueView::Int(v),
            Classified::Num(f) => ValueView::Num(f),
            // SAFETY: the word is live for the duration of the &self borrow
            // and was packed with exactly this kind -> payload-type mapping.
            Classified::Kind(kind) => unsafe { view_kind(kind, bits) },
        }
    }

    /// The payload if this is an `Int` (inline or boxed). No coercion.
    #[inline]
    pub(in crate::value) fn as_int(&self) -> Option<i64> {
        let bits = self.0.get();
        match classify(bits) {
            Classified::Int(v) => Some(v),
            // SAFETY: IntBoxed words carry an Arc<i64>.
            Classified::Kind(Kind::IntBoxed) => Some(*unsafe { peek_arc::<i64>(bits) }),
            _ => None,
        }
    }

    /// The payload if this is a `Num`. No coercion.
    #[inline]
    pub(in crate::value) fn as_num(&self) -> Option<f64> {
        match classify(self.0.get()) {
            Classified::Num(f) => Some(f),
            _ => None,
        }
    }

    /// The payload if this is a `Bool`. No coercion.
    #[inline]
    pub(in crate::value) fn as_bool(&self) -> Option<bool> {
        let bits = self.0.get();
        match classify(bits) {
            Classified::Kind(Kind::Bool) => Some(inline_payload(bits) != 0),
            _ => None,
        }
    }

    /// The string slice if this is a `Str`. No coercion.
    #[inline]
    pub(in crate::value) fn as_str(&self) -> Option<&str> {
        let bits = self.0.get();
        match classify(bits) {
            // SAFETY: Str words carry an Arc<String>.
            Classified::Kind(Kind::Str) => Some(unsafe { peek_arc::<String>(bits) }),
            _ => None,
        }
    }

    /// Whether this word is `Nil`.
    #[inline]
    pub(in crate::value) fn is_nil(&self) -> bool {
        matches!(classify(self.0.get()), Classified::Kind(Kind::Nil))
    }

    /// The `SubData` pointee if this is a `Sub` (the `as_sub` probe).
    #[inline]
    pub(in crate::value) fn as_sub_data(&self) -> Option<&SubData> {
        let bits = self.0.get();
        match classify(bits) {
            // SAFETY: Sub words carry a Gc<SubData>.
            Classified::Kind(Kind::Sub) => Some(unsafe { peek_gc::<SubData>(bits) }),
            _ => None,
        }
    }

    /// Whether this word is an itemized `Hash` (the `hash_is_itemized` probe).
    #[inline]
    pub(in crate::value) fn is_hash_itemized(&self) -> bool {
        matches!(classify(self.0.get()), Classified::Kind(Kind::HashItemized))
    }

    /// Element slice if this is an Array/Seq/Slip (optionally Hyper/Race),
    /// borrowed from the payload (the `as_list_items*` accessors).
    #[inline]
    pub(in crate::value) fn as_list_slice(&self, with_hyper: bool) -> Option<&[Value]> {
        let bits = self.0.get();
        let Classified::Kind(kind) = classify(bits) else {
            return None;
        };
        // SAFETY (both arms): kind-checked payload types per `payload_op`.
        match kind {
            Kind::ArrayList
            | Kind::ArrayArray
            | Kind::ArrayItemList
            | Kind::ArrayItemArray
            | Kind::ArrayShaped
            | Kind::ArrayLazy => Some(&unsafe { peek_gc::<ArrayData>(bits) }.items[..]),
            Kind::Seq | Kind::Slip => Some(&unsafe { peek_arc::<Vec<Value>>(bits) }[..]),
            Kind::HyperSeq | Kind::RaceSeq if with_hyper => {
                Some(&unsafe { peek_arc::<Vec<Value>>(bits) }[..])
            }
            _ => None,
        }
    }

    /// Whether this word's `Arc` payload — for the kinds whose payload is an
    /// `Arc`-shared wrapper that can hold `Value`s (and therefore `Gc` edges)
    /// — is uniquely owned. `true` for every other kind.
    ///
    /// The cycle collector inlines a non-node wrapper into every holder's
    /// child list; for a wrapper shared by N holders that over-counts its
    /// inner `Gc` edges N times, under-flowing strong counts during trial
    /// deletion. Shared wrappers are treated as external root holders instead
    /// (conservatively alive; see `value_gc::uniquely_owned`). Pre-flip the
    /// multi-field payloads were `Box`-owned (always unique); post-flip they
    /// share through one `Arc<...Box>`, so cloning any such `Value` makes the
    /// wrapper shared and this gate is what keeps `gc_trace` sound.
    pub(in crate::value) fn value_bearing_arc_payload_uniquely_owned(&self) -> bool {
        let bits = self.0.get();
        let Classified::Kind(kind) = classify(bits) else {
            return true;
        };
        /// # Safety
        /// `bits` must be a live word packed with an `Arc<T>` payload.
        #[inline]
        unsafe fn unique<T>(bits: u64) -> bool {
            let a = ManuallyDrop::new(unsafe { take_arc::<T>(bits) });
            Arc::strong_count(&a) == 1
        }
        // SAFETY (all arms): kind-checked payload types per `payload_op`.
        unsafe {
            match kind {
                Kind::Scalar => unique::<Value>(bits),
                Kind::Pair => unique::<PairBox>(bits),
                Kind::ValuePair => unique::<ValuePairBox>(bits),
                Kind::Capture => unique::<CaptureBox>(bits),
                Kind::VarRef => unique::<VarRefBox>(bits),
                Kind::Proxy => unique::<ProxyBox>(bits),
                Kind::GenericRange => unique::<GenericRangeBox>(bits),
                Kind::LazyIoLines => unique::<LazyIoLinesBox>(bits),
                Kind::Enum => unique::<EnumBox>(bits),
                Kind::ParametricRole => unique::<ParametricRoleBox>(bits),
                Kind::CustomType => unique::<CustomTypeData>(bits),
                Kind::CustomTypeInstance => unique::<CustomTypeInstanceData>(bits),
                Kind::Mixin => unique::<MixinBox>(bits),
                // Seq/Slip/Junction/LazyThunk payloads are the shared Arc
                // itself (not a box) — their existing `uniquely_owned` gates
                // in `value_gc` read the same count. Node kinds (Array, Hash,
                // Sub, ...) are real collector nodes: each holder legitimately
                // owns one edge, no gate needed.
                _ => true,
            }
        }
    }

    /// A representation-variant tag for `same_variant`: collapses the kind
    /// space back onto `ValueRepr` discriminants (all six Array kinds are one
    /// variant, IntBoxed is Int, the four Junction kinds are one, etc.).
    #[inline]
    pub(in crate::value) fn variant_tag(&self) -> u8 {
        match classify(self.0.get()) {
            Classified::Int(_) => VARIANT_INT,
            Classified::Num(_) => VARIANT_NUM,
            Classified::Kind(kind) => match kind {
                Kind::IntBoxed => VARIANT_INT,
                Kind::ArrayList
                | Kind::ArrayArray
                | Kind::ArrayItemList
                | Kind::ArrayItemArray
                | Kind::ArrayShaped
                | Kind::ArrayLazy => VARIANT_ARRAY,
                Kind::HashPlain | Kind::HashItemized => VARIANT_HASH,
                Kind::SetImm | Kind::SetMut => VARIANT_SET,
                Kind::BagImm | Kind::BagMut => VARIANT_BAG,
                Kind::MixImm | Kind::MixMut => VARIANT_MIX,
                Kind::JunctionAny | Kind::JunctionAll | Kind::JunctionOne | Kind::JunctionNone => {
                    VARIANT_JUNCTION
                }
                other => VARIANT_KIND_BASE + other as u8,
            },
        }
    }
}

const VARIANT_INT: u8 = 0;
const VARIANT_NUM: u8 = 1;
const VARIANT_ARRAY: u8 = 2;
const VARIANT_HASH: u8 = 3;
const VARIANT_SET: u8 = 4;
const VARIANT_BAG: u8 = 5;
const VARIANT_MIX: u8 = 6;
const VARIANT_JUNCTION: u8 = 7;
/// Every kind not collapsed above maps 1:1; offset past the collapsed tags.
const VARIANT_KIND_BASE: u8 = 8;

/// # Safety
/// `bits` must be a live kind word of kind `kind`, borrowed for `'a`.
unsafe fn view_kind<'a>(kind: Kind, bits: u64) -> ValueView<'a> {
    // SAFETY (all arms): kind-checked payload types, same mapping as
    // `payload_op` / `decode_kind`.
    unsafe {
        match kind {
            Kind::Str => ValueView::Str(arc_guard(bits)),
            Kind::Regex => ValueView::Regex(arc_guard(bits)),
            Kind::BigInt => ValueView::BigInt(arc_guard(bits)),
            Kind::IntBoxed => ValueView::Int(*peek_arc::<i64>(bits)),
            Kind::Seq => ValueView::Seq(arc_guard(bits)),
            Kind::HyperSeq => ValueView::HyperSeq(arc_guard(bits)),
            Kind::RaceSeq => ValueView::RaceSeq(arc_guard(bits)),
            Kind::Slip => ValueView::Slip(arc_guard(bits)),
            Kind::JunctionAny | Kind::JunctionAll | Kind::JunctionOne | Kind::JunctionNone => {
                ValueView::Junction {
                    kind: match kind {
                        Kind::JunctionAny => JunctionKind::Any,
                        Kind::JunctionAll => JunctionKind::All,
                        Kind::JunctionOne => JunctionKind::One,
                        _ => JunctionKind::None,
                    },
                    values: arc_guard(bits),
                }
            }
            Kind::Mixin => {
                let b = peek_arc::<MixinBox>(bits);
                ValueView::Mixin(&b.0, &b.1)
            }
            Kind::Scalar => ValueView::Scalar(peek_arc::<Value>(bits)),
            Kind::LazyThunk => ValueView::LazyThunk(arc_guard(bits)),
            Kind::Uni => ValueView::Uni(peek_arc::<UniData>(bits)),
            Kind::RegexWithAdverbs => ValueView::RegexWithAdverbs(peek_arc::<RegexAdverbs>(bits)),
            Kind::CustomType => ValueView::CustomType(peek_arc::<CustomTypeData>(bits)),
            Kind::CustomTypeInstance => {
                ValueView::CustomTypeInstance(peek_arc::<CustomTypeInstanceData>(bits))
            }
            Kind::Range => {
                let p = peek_arc::<I64Pair>(bits);
                ValueView::Range(p.0, p.1)
            }
            Kind::RangeExcl => {
                let p = peek_arc::<I64Pair>(bits);
                ValueView::RangeExcl(p.0, p.1)
            }
            Kind::RangeExclStart => {
                let p = peek_arc::<I64Pair>(bits);
                ValueView::RangeExclStart(p.0, p.1)
            }
            Kind::RangeExclBoth => {
                let p = peek_arc::<I64Pair>(bits);
                ValueView::RangeExclBoth(p.0, p.1)
            }
            Kind::Rat => {
                let p = peek_arc::<I64Pair>(bits);
                ValueView::Rat(p.0, p.1)
            }
            Kind::FatRat => {
                let p = peek_arc::<I64Pair>(bits);
                ValueView::FatRat(p.0, p.1)
            }
            Kind::BigRat => {
                let b = peek_arc::<BigRatBox>(bits);
                ValueView::BigRat(&b.0, &b.1)
            }
            Kind::Complex => {
                let p = peek_arc::<F64Pair>(bits);
                ValueView::Complex(p.0, p.1)
            }
            Kind::Pair => {
                let p = peek_arc::<PairBox>(bits);
                ValueView::Pair(&p.0, &p.1)
            }
            Kind::ValuePair => {
                let p = peek_arc::<ValuePairBox>(bits);
                ValueView::ValuePair(&p.0, &p.1)
            }
            Kind::Enum => {
                let e = peek_arc::<EnumBox>(bits);
                ValueView::Enum {
                    enum_type: e.enum_type,
                    key: e.key,
                    value: &e.value,
                    index: e.index,
                }
            }
            Kind::GenericRange => {
                let g = peek_arc::<GenericRangeBox>(bits);
                ValueView::GenericRange {
                    start: &g.start,
                    end: &g.end,
                    excl_start: g.excl_start,
                    excl_end: g.excl_end,
                }
            }
            Kind::Version => {
                let v = peek_arc::<VersionBox>(bits);
                ValueView::Version {
                    parts: &v.parts,
                    plus: v.plus,
                    minus: v.minus,
                }
            }
            Kind::Capture => {
                let c = peek_arc::<CaptureBox>(bits);
                ValueView::Capture {
                    positional: &c.positional,
                    named: &c.named,
                }
            }
            Kind::VarRef => {
                let r = peek_arc::<VarRefBox>(bits);
                ValueView::VarRef {
                    name: r.name,
                    value: &r.value,
                    index: r.index,
                }
            }
            Kind::Proxy => {
                let p = peek_arc::<ProxyBox>(bits);
                ValueView::Proxy {
                    fetcher: &p.fetcher,
                    storer: &p.storer,
                    subclass: &p.subclass,
                    decontainerized: p.decontainerized,
                }
            }
            Kind::ParametricRole => {
                let p = peek_arc::<ParametricRoleBox>(bits);
                ValueView::ParametricRole {
                    base_name: p.base_name,
                    type_args: &p.type_args,
                }
            }
            Kind::Routine => {
                let r = peek_arc::<RoutineBox>(bits);
                ValueView::Routine {
                    package: r.package,
                    name: r.name,
                    is_regex: r.is_regex,
                }
            }
            Kind::LazyIoLines => {
                let l = peek_arc::<LazyIoLinesBox>(bits);
                ValueView::LazyIoLines {
                    handle: &l.handle,
                    kv: l.kv,
                    words: l.words,
                }
            }
            Kind::HashEntryRef => {
                let h = peek_arc::<HashEntryRefBox>(bits);
                ValueView::HashEntryRef {
                    hash: &h.hash,
                    path: &h.path,
                    eager: h.eager,
                }
            }
            Kind::Sub => ValueView::Sub(gc_guard(bits)),
            Kind::WeakSub => {
                ValueView::WeakSub(RefGuard::from_reconstructed(take_weak::<SubData>(bits)))
            }
            Kind::LazyList => ValueView::LazyList(gc_guard(bits)),
            Kind::ContainerRef => ValueView::ContainerRef(gc_guard(bits)),
            Kind::Promise => ValueView::Promise(RefGuard::from_reconstructed(SharedPromise {
                inner: take_gc(bits),
            })),
            Kind::Channel => ValueView::Channel(RefGuard::from_reconstructed(SharedChannel {
                inner: take_gc(bits),
            })),
            Kind::Instance => {
                let attrs = peek_gc::<InstanceAttrs>(bits);
                ValueView::Instance {
                    class_name: attrs.class_name,
                    attributes: gc_guard(bits),
                    id: attrs.id,
                }
            }
            Kind::ArrayList => ValueView::Array(gc_guard(bits), ArrayKind::List),
            Kind::ArrayArray => ValueView::Array(gc_guard(bits), ArrayKind::Array),
            Kind::ArrayItemList => ValueView::Array(gc_guard(bits), ArrayKind::ItemList),
            Kind::ArrayItemArray => ValueView::Array(gc_guard(bits), ArrayKind::ItemArray),
            Kind::ArrayShaped => ValueView::Array(gc_guard(bits), ArrayKind::Shaped),
            Kind::ArrayLazy => ValueView::Array(gc_guard(bits), ArrayKind::Lazy),
            Kind::HashPlain | Kind::HashItemized => ValueView::Hash(gc_guard(bits)),
            Kind::SetImm => ValueView::Set(gc_guard(bits), false),
            Kind::SetMut => ValueView::Set(gc_guard(bits), true),
            Kind::BagImm => ValueView::Bag(gc_guard(bits), false),
            Kind::BagMut => ValueView::Bag(gc_guard(bits), true),
            Kind::MixImm => ValueView::Mix(gc_guard(bits), false),
            Kind::MixMut => ValueView::Mix(gc_guard(bits), true),
            Kind::Nil => ValueView::Nil,
            Kind::Whatever => ValueView::Whatever,
            Kind::HyperWhatever => ValueView::HyperWhatever,
            Kind::Bool => ValueView::Bool(inline_payload(bits) != 0),
            Kind::Package => ValueView::Package(Symbol::from_id(inline_payload(bits))),
            Kind::CompUnitDepSpec => ValueView::CompUnitDepSpec {
                short_name: Symbol::from_id(inline_payload(bits)),
            },
        }
    }
}
