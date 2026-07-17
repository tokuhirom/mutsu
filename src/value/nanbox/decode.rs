//! `NanBox` -> `ValueRepr` owned decode (the future `into_repr` seam for
//! owned matches; borrowed reads will go through `view()` guard types).

use super::*;

impl NanBox {
    /// Decode into the working enum, consuming this word's payload ownership
    /// (no refcount traffic for pointer kinds; a shared multi-field box is
    /// cloned, a uniquely-owned one is moved out).
    pub(in crate::value) fn into_repr(self) -> ValueRepr {
        let bits = self.0.get();
        // The payload ownership transfers to the decoded repr below.
        std::mem::forget(self);
        match classify(bits) {
            Classified::Int(v) => ValueRepr::Int(v),
            Classified::Num(f) => ValueRepr::Num(f),
            // SAFETY (all arms): `bits` was packed with exactly this kind ->
            // payload-type mapping (see `encode.rs` / `payload_op`), and the
            // ownership consumed here is the one `forget(self)` released.
            Classified::Kind(kind) => unsafe { decode_kind(kind, bits) },
        }
    }
}

/// # Safety
/// `bits` must be a live kind word of kind `kind` whose payload ownership is
/// being consumed exactly once.
unsafe fn decode_kind(kind: Kind, bits: u64) -> ValueRepr {
    match kind {
        Kind::Str => ValueRepr::Str(unsafe { take_arc::<String>(bits) }),
        Kind::Regex => ValueRepr::Regex(unsafe { take_arc::<String>(bits) }),
        Kind::BigInt => ValueRepr::BigInt(unsafe { take_arc::<NumBigInt>(bits) }),
        Kind::IntBoxed => ValueRepr::Int(*unsafe { take_arc::<i64>(bits) }),
        Kind::Seq => ValueRepr::Seq(unsafe { take_arc::<Vec<Value>>(bits) }),
        Kind::HyperSeq => ValueRepr::HyperSeq(unsafe { take_arc::<Vec<Value>>(bits) }),
        Kind::RaceSeq => ValueRepr::RaceSeq(unsafe { take_arc::<Vec<Value>>(bits) }),
        Kind::Slip => ValueRepr::Slip(unsafe { take_arc::<Vec<Value>>(bits) }),
        Kind::JunctionAny | Kind::JunctionAll | Kind::JunctionOne | Kind::JunctionNone => {
            ValueRepr::Junction {
                kind: match kind {
                    Kind::JunctionAny => JunctionKind::Any,
                    Kind::JunctionAll => JunctionKind::All,
                    Kind::JunctionOne => JunctionKind::One,
                    _ => JunctionKind::None,
                },
                values: unsafe { take_arc::<Vec<Value>>(bits) },
            }
        }
        Kind::Mixin => {
            let b = Arc::unwrap_or_clone(unsafe { take_arc::<MixinBox>(bits) });
            ValueRepr::Mixin(b.0, b.1)
        }
        Kind::Scalar => {
            let v = Arc::unwrap_or_clone(unsafe { take_arc::<Value>(bits) });
            ValueRepr::Scalar(Box::new(v))
        }
        Kind::LazyThunk => ValueRepr::LazyThunk(unsafe { take_arc::<LazyThunkData>(bits) }),
        Kind::RakuAst => {
            let n = Arc::unwrap_or_clone(unsafe { take_arc::<crate::rakuast::RakuAstNode>(bits) });
            ValueRepr::RakuAst(Box::new(n))
        }
        Kind::Uni => {
            let u = Arc::unwrap_or_clone(unsafe { take_arc::<UniData>(bits) });
            ValueRepr::Uni(Box::new(u))
        }
        Kind::RegexWithAdverbs => {
            let a = Arc::unwrap_or_clone(unsafe { take_arc::<RegexAdverbs>(bits) });
            ValueRepr::RegexWithAdverbs(Box::new(a))
        }
        Kind::CustomType => {
            let d = Arc::unwrap_or_clone(unsafe { take_arc::<CustomTypeData>(bits) });
            ValueRepr::CustomType(Box::new(d))
        }
        Kind::CustomTypeInstance => {
            let d = Arc::unwrap_or_clone(unsafe { take_arc::<CustomTypeInstanceData>(bits) });
            ValueRepr::CustomTypeInstance(Box::new(d))
        }
        Kind::Range
        | Kind::RangeExcl
        | Kind::RangeExclStart
        | Kind::RangeExclBoth
        | Kind::Rat
        | Kind::FatRat => {
            let p = Arc::unwrap_or_clone(unsafe { take_arc::<I64Pair>(bits) });
            match kind {
                Kind::Range => ValueRepr::Range(p.0, p.1),
                Kind::RangeExcl => ValueRepr::RangeExcl(p.0, p.1),
                Kind::RangeExclStart => ValueRepr::RangeExclStart(p.0, p.1),
                Kind::RangeExclBoth => ValueRepr::RangeExclBoth(p.0, p.1),
                Kind::Rat => ValueRepr::Rat(p.0, p.1),
                _ => ValueRepr::FatRat(p.0, p.1),
            }
        }
        Kind::BigRat => {
            let b = Arc::unwrap_or_clone(unsafe { take_arc::<BigRatBox>(bits) });
            ValueRepr::BigRat(Box::new(b.0), Box::new(b.1))
        }
        Kind::Complex => {
            let p = Arc::unwrap_or_clone(unsafe { take_arc::<F64Pair>(bits) });
            ValueRepr::Complex(p.0, p.1)
        }
        Kind::Pair => {
            let p = Arc::unwrap_or_clone(unsafe { take_arc::<PairBox>(bits) });
            ValueRepr::Pair(p.0, Box::new(p.1))
        }
        Kind::ValuePair => {
            let p = Arc::unwrap_or_clone(unsafe { take_arc::<ValuePairBox>(bits) });
            ValueRepr::ValuePair(Box::new(p.0), Box::new(p.1))
        }
        Kind::Enum => {
            let e = Arc::unwrap_or_clone(unsafe { take_arc::<EnumBox>(bits) });
            ValueRepr::Enum {
                enum_type: e.enum_type,
                key: e.key,
                value: e.value,
                index: e.index,
            }
        }
        Kind::GenericRange => {
            let g = Arc::unwrap_or_clone(unsafe { take_arc::<GenericRangeBox>(bits) });
            ValueRepr::GenericRange {
                start: g.start,
                end: g.end,
                excl_start: g.excl_start,
                excl_end: g.excl_end,
            }
        }
        Kind::Version => {
            let v = Arc::unwrap_or_clone(unsafe { take_arc::<VersionBox>(bits) });
            ValueRepr::Version {
                parts: v.parts,
                plus: v.plus,
                minus: v.minus,
            }
        }
        Kind::Capture => {
            let c = Arc::unwrap_or_clone(unsafe { take_arc::<CaptureBox>(bits) });
            ValueRepr::Capture {
                positional: c.positional,
                named: c.named,
            }
        }
        Kind::VarRef => {
            let r = Arc::unwrap_or_clone(unsafe { take_arc::<VarRefBox>(bits) });
            ValueRepr::VarRef {
                name: r.name,
                value: Box::new(r.value),
                index: r.index,
            }
        }
        Kind::Proxy => {
            let p = Arc::unwrap_or_clone(unsafe { take_arc::<ProxyBox>(bits) });
            ValueRepr::Proxy {
                fetcher: p.fetcher,
                storer: p.storer,
                subclass: p.subclass,
                decontainerized: p.decontainerized,
            }
        }
        Kind::ParametricRole => {
            let p = Arc::unwrap_or_clone(unsafe { take_arc::<ParametricRoleBox>(bits) });
            ValueRepr::ParametricRole {
                base_name: p.base_name,
                type_args: p.type_args,
            }
        }
        Kind::Routine => {
            let r = Arc::unwrap_or_clone(unsafe { take_arc::<RoutineBox>(bits) });
            ValueRepr::Routine {
                package: r.package,
                name: r.name,
                is_regex: r.is_regex,
            }
        }
        Kind::LazyIoLines => {
            let l = Arc::unwrap_or_clone(unsafe { take_arc::<LazyIoLinesBox>(bits) });
            ValueRepr::LazyIoLines {
                handle: l.handle,
                kv: l.kv,
                words: l.words,
            }
        }
        Kind::HashEntryRef => {
            let h = Arc::unwrap_or_clone(unsafe { take_arc::<HashEntryRefBox>(bits) });
            ValueRepr::HashEntryRef {
                hash: h.hash,
                path: h.path,
                eager: h.eager,
            }
        }
        Kind::Sub => ValueRepr::Sub(unsafe { take_gc::<SubData>(bits) }),
        Kind::WeakSub => ValueRepr::WeakSub(unsafe { take_weak::<SubData>(bits) }),
        Kind::LazyList => ValueRepr::LazyList(unsafe { take_gc::<LazyList>(bits) }),
        Kind::ContainerRef => ValueRepr::ContainerRef(unsafe { take_gc::<Mutex<Value>>(bits) }),
        Kind::Promise => ValueRepr::Promise(SharedPromise {
            inner: unsafe { take_gc::<(Mutex<PromiseState>, Condvar)>(bits) },
        }),
        Kind::Channel => ValueRepr::Channel(SharedChannel {
            inner: unsafe { take_gc::<(Mutex<ChannelState>, Condvar)>(bits) },
        }),
        Kind::Instance => {
            let attributes = unsafe { take_gc::<InstanceAttrs>(bits) };
            ValueRepr::Instance {
                class_name: attributes.class_name,
                id: attributes.id,
                attributes,
            }
        }
        Kind::ArrayList
        | Kind::ArrayArray
        | Kind::ArrayItemList
        | Kind::ArrayItemArray
        | Kind::ArrayShaped
        | Kind::ArrayLazy => ValueRepr::Array(
            unsafe { take_gc::<ArrayData>(bits) },
            match kind {
                Kind::ArrayList => ArrayKind::List,
                Kind::ArrayArray => ArrayKind::Array,
                Kind::ArrayItemList => ArrayKind::ItemList,
                Kind::ArrayItemArray => ArrayKind::ItemArray,
                Kind::ArrayShaped => ArrayKind::Shaped,
                _ => ArrayKind::Lazy,
            },
        ),
        Kind::HashPlain => ValueRepr::Hash(unsafe { take_gc::<HashData>(bits) }, false),
        Kind::HashItemized => ValueRepr::Hash(unsafe { take_gc::<HashData>(bits) }, true),
        Kind::SetImm => ValueRepr::Set(unsafe { take_gc::<SetData>(bits) }, false),
        Kind::SetMut => ValueRepr::Set(unsafe { take_gc::<SetData>(bits) }, true),
        Kind::BagImm => ValueRepr::Bag(unsafe { take_gc::<BagData>(bits) }, false),
        Kind::BagMut => ValueRepr::Bag(unsafe { take_gc::<BagData>(bits) }, true),
        Kind::MixImm => ValueRepr::Mix(unsafe { take_gc::<MixData>(bits) }, false),
        Kind::MixMut => ValueRepr::Mix(unsafe { take_gc::<MixData>(bits) }, true),
        Kind::Nil => ValueRepr::Nil,
        Kind::Whatever => ValueRepr::Whatever,
        Kind::HyperWhatever => ValueRepr::HyperWhatever,
        Kind::Bool => ValueRepr::Bool(inline_payload(bits) != 0),
        Kind::Package => ValueRepr::Package(Symbol::from_id(inline_payload(bits))),
        Kind::CompUnitDepSpec => ValueRepr::CompUnitDepSpec {
            short_name: Symbol::from_id(inline_payload(bits)),
        },
    }
}
