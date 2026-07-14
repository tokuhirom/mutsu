//! `ValueRepr` -> `NanBox` packing (the future constructor-shim bodies).

use super::*;

impl NanBox {
    /// Pack a `ValueRepr` into one word, taking ownership of its payloads.
    /// Lossless: `into_repr` returns an equivalent `ValueRepr` (pointer
    /// payloads identical, multi-field payloads moved through one heap box).
    pub(in crate::value) fn from_repr(repr: ValueRepr) -> NanBox {
        let word = match repr {
            ValueRepr::Int(v) => {
                if small_int_fits(v) {
                    pack_small_int(v)
                } else {
                    pack_arc(Kind::IntBoxed, Arc::new(v))
                }
            }
            ValueRepr::Num(f) => pack_num(f),
            ValueRepr::Str(s) => pack_arc(Kind::Str, s),
            ValueRepr::Regex(s) => pack_arc(Kind::Regex, s),
            ValueRepr::BigInt(n) => pack_arc(Kind::BigInt, n),
            ValueRepr::Bool(b) => pack_inline(Kind::Bool, b as u32),
            ValueRepr::Nil => pack_inline(Kind::Nil, 0),
            ValueRepr::Whatever => pack_inline(Kind::Whatever, 0),
            ValueRepr::HyperWhatever => pack_inline(Kind::HyperWhatever, 0),
            ValueRepr::Package(sym) => pack_inline(Kind::Package, sym.id()),
            ValueRepr::CompUnitDepSpec { short_name } => {
                pack_inline(Kind::CompUnitDepSpec, short_name.id())
            }
            ValueRepr::Range(a, b) => pack_arc(Kind::Range, Arc::new(I64Pair(a, b))),
            ValueRepr::RangeExcl(a, b) => pack_arc(Kind::RangeExcl, Arc::new(I64Pair(a, b))),
            ValueRepr::RangeExclStart(a, b) => {
                pack_arc(Kind::RangeExclStart, Arc::new(I64Pair(a, b)))
            }
            ValueRepr::RangeExclBoth(a, b) => {
                pack_arc(Kind::RangeExclBoth, Arc::new(I64Pair(a, b)))
            }
            ValueRepr::Rat(n, d) => pack_arc(Kind::Rat, Arc::new(I64Pair(n, d))),
            ValueRepr::FatRat(n, d) => pack_arc(Kind::FatRat, Arc::new(I64Pair(n, d))),
            ValueRepr::BigRat(n, d) => pack_arc(Kind::BigRat, Arc::new(BigRatBox(*n, *d))),
            ValueRepr::Complex(re, im) => pack_arc(Kind::Complex, Arc::new(F64Pair(re, im))),
            ValueRepr::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => pack_arc(
                Kind::GenericRange,
                Arc::new(GenericRangeBox {
                    start,
                    end,
                    excl_start,
                    excl_end,
                }),
            ),
            ValueRepr::Array(data, kind) => pack_gc(
                match kind {
                    ArrayKind::List => Kind::ArrayList,
                    ArrayKind::Array => Kind::ArrayArray,
                    ArrayKind::ItemList => Kind::ArrayItemList,
                    ArrayKind::ItemArray => Kind::ArrayItemArray,
                    ArrayKind::Shaped => Kind::ArrayShaped,
                    ArrayKind::Lazy => Kind::ArrayLazy,
                },
                data,
            ),
            ValueRepr::Hash(data, itemized) => pack_gc(
                if itemized {
                    Kind::HashItemized
                } else {
                    Kind::HashPlain
                },
                data,
            ),
            ValueRepr::Set(data, mutable) => {
                pack_gc(if mutable { Kind::SetMut } else { Kind::SetImm }, data)
            }
            ValueRepr::Bag(data, mutable) => {
                pack_gc(if mutable { Kind::BagMut } else { Kind::BagImm }, data)
            }
            ValueRepr::Mix(data, mutable) => {
                pack_gc(if mutable { Kind::MixMut } else { Kind::MixImm }, data)
            }
            ValueRepr::Pair(k, v) => pack_arc(Kind::Pair, Arc::new(PairBox(k, *v))),
            ValueRepr::ValuePair(k, v) => pack_arc(Kind::ValuePair, Arc::new(ValuePairBox(*k, *v))),
            ValueRepr::Enum {
                enum_type,
                key,
                value,
                index,
            } => pack_arc(
                Kind::Enum,
                Arc::new(EnumBox {
                    enum_type,
                    key,
                    value,
                    index,
                }),
            ),
            ValueRepr::Routine {
                package,
                name,
                is_regex,
            } => pack_arc(
                Kind::Routine,
                Arc::new(RoutineBox {
                    package,
                    name,
                    is_regex,
                }),
            ),
            ValueRepr::RegexWithAdverbs(adv) => pack_arc(Kind::RegexWithAdverbs, Arc::new(*adv)),
            ValueRepr::Sub(data) => pack_gc(Kind::Sub, data),
            ValueRepr::WeakSub(w) => pack_weak(Kind::WeakSub, w),
            ValueRepr::Instance {
                class_name,
                attributes,
                id,
            } => {
                // The variant's copies must mirror the pointee (rebless goes
                // through `instance_sharing_cell`, which forks the node when
                // the class differs) — the pointee is the single stored source.
                debug_assert_eq!(
                    attributes.class_name, class_name,
                    "Instance class_name diverged from its InstanceAttrs"
                );
                debug_assert_eq!(
                    attributes.id, id,
                    "Instance id diverged from its InstanceAttrs"
                );
                pack_gc(Kind::Instance, attributes)
            }
            ValueRepr::Junction { kind, values } => pack_arc(
                match kind {
                    JunctionKind::Any => Kind::JunctionAny,
                    JunctionKind::All => Kind::JunctionAll,
                    JunctionKind::One => Kind::JunctionOne,
                    JunctionKind::None => Kind::JunctionNone,
                },
                values,
            ),
            ValueRepr::Seq(items) => pack_arc(Kind::Seq, items),
            ValueRepr::HyperSeq(items) => pack_arc(Kind::HyperSeq, items),
            ValueRepr::RaceSeq(items) => pack_arc(Kind::RaceSeq, items),
            ValueRepr::Slip(items) => pack_arc(Kind::Slip, items),
            ValueRepr::LazyList(l) => pack_gc(Kind::LazyList, l),
            ValueRepr::Version { parts, plus, minus } => {
                pack_arc(Kind::Version, Arc::new(VersionBox { parts, plus, minus }))
            }
            ValueRepr::Promise(p) => pack_gc(Kind::Promise, p.inner),
            ValueRepr::Channel(c) => pack_gc(Kind::Channel, c.inner),
            ValueRepr::Mixin(inner, overrides) => {
                pack_arc(Kind::Mixin, Arc::new(MixinBox(inner, overrides)))
            }
            ValueRepr::Capture { positional, named } => {
                pack_arc(Kind::Capture, Arc::new(CaptureBox { positional, named }))
            }
            ValueRepr::VarRef { name, value, index } => pack_arc(
                Kind::VarRef,
                Arc::new(VarRefBox {
                    name,
                    value: *value,
                    index,
                }),
            ),
            ValueRepr::Uni(u) => pack_arc(Kind::Uni, Arc::new(*u)),
            ValueRepr::Proxy {
                fetcher,
                storer,
                subclass,
                decontainerized,
            } => pack_arc(
                Kind::Proxy,
                Arc::new(ProxyBox {
                    fetcher,
                    storer,
                    subclass,
                    decontainerized,
                }),
            ),
            ValueRepr::ParametricRole {
                base_name,
                type_args,
            } => pack_arc(
                Kind::ParametricRole,
                Arc::new(ParametricRoleBox {
                    base_name,
                    type_args,
                }),
            ),
            ValueRepr::CustomType(d) => pack_arc(Kind::CustomType, Arc::new(*d)),
            ValueRepr::CustomTypeInstance(d) => pack_arc(Kind::CustomTypeInstance, Arc::new(*d)),
            ValueRepr::Scalar(inner) => pack_arc(Kind::Scalar, Arc::new(*inner)),
            ValueRepr::ContainerRef(cell) => {
                // Latch for the Tier B inline GetLocal fast path: this is the
                // single point every ContainerRef word passes through, so a
                // zero counter proves no cell exists anywhere (see
                // `vm_jit::CONTAINER_CELLS`).
                crate::vm::vm_jit::note_container_cell();
                pack_gc(Kind::ContainerRef, cell)
            }
            ValueRepr::LazyThunk(t) => pack_arc(Kind::LazyThunk, t),
            ValueRepr::LazyIoLines { handle, kv, words } => pack_arc(
                Kind::LazyIoLines,
                Arc::new(LazyIoLinesBox { handle, kv, words }),
            ),
            ValueRepr::HashEntryRef { hash, path, eager } => pack_arc(
                Kind::HashEntryRef,
                Arc::new(HashEntryRefBox { hash, path, eager }),
            ),
        };
        NanBox(word)
    }
}
