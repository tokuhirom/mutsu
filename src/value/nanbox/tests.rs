//! NaN-box invariants: size/niche, exhaustive lossless round-trips, pointer
//! identity, refcount neutrality (Arc + Gc header bookkeeping), encoding edge
//! cases. These are the step-B ground truth BEFORE the representation flip
//! wires `NanBox` into `Value` — run under Miri to validate the raw-pointer
//! round-trips (`cargo miri test value::nanbox`).

use super::*;
use crate::ast::ParamDef;
use crate::env::Env;

fn roundtrip(repr: ValueRepr) -> ValueRepr {
    NanBox::from_repr(repr).into_repr()
}

/// Debug output is the equality proxy: `ValueRepr` has no `PartialEq`, and
/// `Debug` prints through to every payload (Gc/Arc Debug delegate to the
/// pointee), so equal output means an equal decoded structure.
fn assert_roundtrip(repr: ValueRepr) {
    let expect = format!("{repr:?}");
    let got = format!("{:?}", roundtrip(repr));
    assert_eq!(got, expect);
}

#[test]
fn word_is_8_bytes_with_a_niche() {
    assert_eq!(std::mem::size_of::<NanBox>(), 8);
    assert_eq!(std::mem::size_of::<Option<NanBox>>(), 8);
}

#[test]
fn kind_tags_roundtrip_exhaustively() {
    for k in 0..KIND_COUNT {
        let kind = kind_from_u8(k);
        let word = word_for_kind(kind, 0);
        match classify(word.get()) {
            Classified::Kind(back) => assert_eq!(back, kind, "kind {k} corrupted"),
            _ => panic!("kind {k} classified as Int/Num"),
        }
    }
}

#[test]
fn small_ints_inline_and_big_ints_box_losslessly() {
    for v in [
        0,
        1,
        -1,
        42,
        SMALL_INT_MIN,
        SMALL_INT_MAX,
        SMALL_INT_MIN - 1,
        SMALL_INT_MAX + 1,
        i64::MIN,
        i64::MAX,
    ] {
        match roundtrip(ValueRepr::Int(v)) {
            ValueRepr::Int(back) => assert_eq!(back, v),
            other => panic!("Int({v}) decoded as {other:?}"),
        }
    }
    // Inline vs boxed split is where the design says it is.
    let inline = NanBox::from_repr(ValueRepr::Int(SMALL_INT_MAX));
    assert_eq!(inline.bits() >> TAG_SHIFT, INT_PAGE);
    let boxed = NanBox::from_repr(ValueRepr::Int(SMALL_INT_MAX + 1));
    assert!(boxed.bits() >> TAG_SHIFT >= KIND_PAGE_BASE);
}

#[test]
fn nums_roundtrip_bit_exactly_and_nan_canonicalizes() {
    for f in [
        0.0,
        -0.0,
        1.5,
        -1.5,
        std::f64::consts::PI,
        1e300,
        -1e300,
        f64::MIN_POSITIVE,
        5e-324, // subnormal
        f64::INFINITY,
        f64::NEG_INFINITY,
        f64::MAX,
        f64::MIN,
    ] {
        match roundtrip(ValueRepr::Num(f)) {
            ValueRepr::Num(back) => {
                assert_eq!(back.to_bits(), f.to_bits(), "bits changed for {f}")
            }
            other => panic!("Num({f}) decoded as {other:?}"),
        }
    }
    // Every NaN (sign/payload included) collapses to a NaN.
    for nan_bits in [
        CANONICAL_NAN,
        0x7FF8_0000_0000_0001,
        0xFFF8_0000_0000_0000,
        0xFFFF_FFFF_FFFF_FFFF,
        0x7FF0_0000_0000_0001, // signaling
    ] {
        match roundtrip(ValueRepr::Num(f64::from_bits(nan_bits))) {
            ValueRepr::Num(back) => assert!(back.is_nan()),
            other => panic!("NaN decoded as {other:?}"),
        }
    }
}

#[test]
fn str_pointer_identity_and_refcounts_survive() {
    let s = Arc::new("shared".to_string());
    assert_eq!(Arc::strong_count(&s), 1);
    let b = NanBox::from_repr(ValueRepr::Str(s.clone()));
    assert_eq!(Arc::strong_count(&s), 2, "the word owns one reference");
    let b2 = b.clone();
    assert_eq!(Arc::strong_count(&s), 3, "clone bumps");
    drop(b2);
    assert_eq!(Arc::strong_count(&s), 2, "drop releases");
    match b.into_repr() {
        ValueRepr::Str(back) => {
            assert!(Arc::ptr_eq(&back, &s), "identity preserved");
        }
        other => panic!("decoded as {other:?}"),
    }
    // `into_repr` consumed the word's reference and `back` dropped with the
    // match arm — only the local `s` remains.
    assert_eq!(Arc::strong_count(&s), 1);
}

#[test]
fn gc_container_identity_and_header_bookkeeping_survive() {
    let data = Gc::new(ArrayData {
        items: vec![Value::int(1), Value::int(2)],
        ..Default::default()
    });
    assert_eq!(data.strong_count(), 1);
    let b = NanBox::from_repr(ValueRepr::Array(data.clone(), ArrayKind::Array));
    assert_eq!(
        data.strong_count(),
        2,
        "the word owns one GC-visible handle"
    );
    let b2 = b.clone();
    assert_eq!(
        data.strong_count(),
        3,
        "NanBox::clone goes through Gc::clone"
    );
    drop(b2);
    assert_eq!(data.strong_count(), 2, "NanBox::drop goes through Gc::drop");
    match b.into_repr() {
        ValueRepr::Array(back, kind) => {
            assert_eq!(kind, ArrayKind::Array);
            assert!(Gc::ptr_eq(&back, &data), "container identity preserved");
            assert_eq!(back.items.len(), 2);
        }
        other => panic!("decoded as {other:?}"),
    }
    assert_eq!(data.strong_count(), 1, "all transferred handles released");
}

#[test]
fn array_kind_and_container_flags_travel_in_the_tag() {
    for kind in [
        ArrayKind::List,
        ArrayKind::Array,
        ArrayKind::ItemList,
        ArrayKind::ItemArray,
        ArrayKind::Shaped,
        ArrayKind::Lazy,
    ] {
        match roundtrip(ValueRepr::Array(Gc::new(ArrayData::default()), kind)) {
            ValueRepr::Array(_, back) => assert_eq!(back, kind),
            other => panic!("decoded as {other:?}"),
        }
    }
    for itemized in [false, true] {
        match roundtrip(ValueRepr::Hash(Gc::new(HashData::default()), itemized)) {
            ValueRepr::Hash(_, back) => assert_eq!(back, itemized),
            other => panic!("decoded as {other:?}"),
        }
    }
    for mutable in [false, true] {
        match roundtrip(ValueRepr::Set(
            Gc::new(SetData {
                elements: HashSet::new(),
                original_keys: None,
                value_type: None,
                key_type: None,
                declared_type: None,
            }),
            mutable,
        )) {
            ValueRepr::Set(_, back) => assert_eq!(back, mutable),
            other => panic!("decoded as {other:?}"),
        }
    }
}

#[test]
fn container_ref_cell_identity_survives() {
    let cell = Gc::new(Mutex::new(Value::int(7)));
    match roundtrip(ValueRepr::ContainerRef(cell.clone())) {
        ValueRepr::ContainerRef(back) => {
            assert!(Gc::ptr_eq(&back, &cell), ":= binding identity preserved");
            *back.lock().unwrap() = Value::int(9);
        }
        other => panic!("decoded as {other:?}"),
    }
    assert_eq!(cell.lock().unwrap().as_int(), Some(9));
}

fn sample_sub() -> Gc<SubData> {
    Gc::new(SubData {
        package: Symbol::intern("Main"),
        name: Symbol::intern("nanbox-test-sub"),
        params: vec![],
        param_defs: Vec::<ParamDef>::new(),
        body: vec![],
        is_rw: false,
        is_raw: false,
        env: Env::new(),
        assumed_positional: vec![],
        assumed_named: HashMap::new(),
        id: 12345,
        empty_sig: false,
        is_bare_block: false,
        compiled_code: None,
        deprecated_message: None,
        source_line: None,
        source_file: None,
        owned_captures: vec![],
        authoritative_captures: vec![],
        upvalues: vec![],
    })
}

#[test]
fn weak_sub_stays_weak_and_expires() {
    let sub = sample_sub();
    let weak = Gc::downgrade(&sub);
    let b = NanBox::from_repr(ValueRepr::WeakSub(weak));
    assert_eq!(
        sub.strong_count(),
        1,
        "a weak payload adds no strong handle"
    );
    let b2 = b.clone();
    match b2.into_repr() {
        ValueRepr::WeakSub(w) => {
            let up = w.upgrade().expect("still alive");
            assert!(Gc::ptr_eq(&up, &sub));
        }
        other => panic!("decoded as {other:?}"),
    }
    drop(sub);
    match b.into_repr() {
        ValueRepr::WeakSub(w) => assert!(w.upgrade().is_none(), "expired after drop"),
        other => panic!("decoded as {other:?}"),
    }
}

#[test]
fn instance_reads_class_and_id_back_from_the_pointee() {
    let class = Symbol::intern("NanboxTestClass");
    let mut attrs = AttrMap::new();
    attrs.insert("x".to_string(), Value::int(1));
    // queue_destroy=false: keep the DESTROY registry out of a unit test.
    let node = Gc::new(InstanceAttrs::new(class, attrs, 777, false));
    match roundtrip(ValueRepr::Instance {
        class_name: class,
        attributes: node.clone(),
        id: 777,
    }) {
        ValueRepr::Instance {
            class_name,
            attributes,
            id,
        } => {
            assert_eq!(class_name, class);
            assert_eq!(id, 777);
            assert!(Gc::ptr_eq(&attributes, &node));
        }
        other => panic!("decoded as {other:?}"),
    }
}

#[test]
fn shared_multifield_box_decodes_by_clone_unique_by_move() {
    // Unique word: decode moves the payload (no clone) — observable via the
    // inner Arc's count staying 1 through the round-trip.
    let inner = Arc::new(Value::int(5));
    let b = NanBox::from_repr(ValueRepr::GenericRange {
        start: inner.clone(),
        end: Arc::new(Value::int(9)),
        excl_start: false,
        excl_end: true,
    });
    assert_eq!(Arc::strong_count(&inner), 2);
    match b.into_repr() {
        ValueRepr::GenericRange {
            start,
            excl_start,
            excl_end,
            ..
        } => {
            assert!(Arc::ptr_eq(&start, &inner), "field identity preserved");
            assert!(!excl_start);
            assert!(excl_end);
        }
        other => panic!("decoded as {other:?}"),
    }
    // Shared word: both clones decode to the same field pointers.
    let b = NanBox::from_repr(ValueRepr::Mixin(inner.clone(), Arc::new(HashMap::new())));
    let b2 = b.clone();
    let (r1, r2) = (b.into_repr(), b2.into_repr());
    match (r1, r2) {
        (ValueRepr::Mixin(a1, _), ValueRepr::Mixin(a2, _)) => {
            assert!(Arc::ptr_eq(&a1, &a2), "shared box decodes to shared fields");
            assert!(Arc::ptr_eq(&a1, &inner));
        }
        other => panic!("decoded as {other:?}"),
    }
}

/// One sample per remaining variant, Debug-compared through the round-trip.
#[test]
fn every_variant_roundtrips_losslessly() {
    let samples: Vec<ValueRepr> = vec![
        ValueRepr::Int(42),
        ValueRepr::Int(i64::MIN),
        ValueRepr::BigInt(Arc::new(NumBigInt::from(10).pow(30))),
        ValueRepr::Num(2.5),
        ValueRepr::Str(Arc::new("hello".to_string())),
        ValueRepr::Bool(true),
        ValueRepr::Bool(false),
        ValueRepr::Range(1, 10),
        ValueRepr::RangeExcl(-3, 7),
        ValueRepr::RangeExclStart(0, 5),
        ValueRepr::RangeExclBoth(i64::MIN, i64::MAX),
        ValueRepr::GenericRange {
            start: Arc::new(Value::str("a".to_string())),
            end: Arc::new(Value::str("z".to_string())),
            excl_start: true,
            excl_end: false,
        },
        ValueRepr::Array(
            Gc::new(ArrayData {
                items: vec![Value::int(1), Value::str("two".to_string())],
                value_type: Some("Int".to_string()),
                default: Some(Box::new(Value::int(0))),
                ..Default::default()
            }),
            ArrayKind::List,
        ),
        ValueRepr::Hash(
            Gc::new(HashData {
                map: HashMap::from([("k".to_string(), Value::int(1))]),
                ..Default::default()
            }),
            false,
        ),
        ValueRepr::Rat(3, 4),
        ValueRepr::FatRat(-7, 2),
        ValueRepr::BigRat(
            Box::new(NumBigInt::from(10).pow(25)),
            Box::new(NumBigInt::from(3)),
        ),
        ValueRepr::Complex(1.5, -2.5),
        ValueRepr::Set(
            Gc::new(SetData {
                elements: HashSet::from(["a".to_string()]),
                original_keys: None,
                value_type: None,
                key_type: None,
                declared_type: Some("SetHash".to_string()),
            }),
            true,
        ),
        ValueRepr::Bag(
            Gc::new(BagData {
                counts: HashMap::from([("b".to_string(), NumBigInt::from(2))]),
                original_keys: None,
                value_type: None,
                key_type: None,
                declared_type: None,
            }),
            false,
        ),
        ValueRepr::Mix(
            Gc::new(MixData {
                weights: HashMap::from([("m".to_string(), 0.5)]),
                original_keys: None,
                value_type: None,
                key_type: None,
                declared_type: None,
            }),
            true,
        ),
        ValueRepr::CompUnitDepSpec {
            short_name: Symbol::intern("Test::Module"),
        },
        ValueRepr::Package(Symbol::intern("Foo::Bar")),
        ValueRepr::Routine {
            package: Symbol::intern("Main"),
            name: Symbol::intern("frobnicate"),
            is_regex: true,
        },
        ValueRepr::Pair("key".to_string(), Box::new(Value::int(9))),
        ValueRepr::ValuePair(
            Box::new(Value::int(1)),
            Box::new(Value::str("v".to_string())),
        ),
        ValueRepr::Enum {
            enum_type: Symbol::intern("Color"),
            key: Symbol::intern("Red"),
            value: EnumValue::Int(0),
            index: 0,
        },
        ValueRepr::Regex(Arc::new("\\d+".to_string())),
        ValueRepr::RegexWithAdverbs(Box::new(RegexAdverbs {
            pattern: Arc::new("a.b".to_string()),
            global: true,
            exhaustive: false,
            overlap: false,
            repeat: Some(2),
            nth: None,
            perl5: false,
            pos: false,
            continue_: false,
            ignore_case: true,
            sigspace: false,
            samecase: false,
            samespace: false,
        })),
        ValueRepr::Sub(sample_sub()),
        ValueRepr::Junction {
            kind: JunctionKind::Any,
            values: Arc::new(vec![Value::int(1), Value::int(2)]),
        },
        ValueRepr::Junction {
            kind: JunctionKind::None,
            values: Arc::new(vec![]),
        },
        ValueRepr::Seq(Arc::new(vec![Value::int(1)])),
        ValueRepr::HyperSeq(Arc::new(vec![Value::int(2)])),
        ValueRepr::RaceSeq(Arc::new(vec![Value::int(3)])),
        ValueRepr::Slip(Arc::new(vec![Value::int(4)])),
        ValueRepr::Version {
            parts: vec![
                VersionPart::Num(1),
                VersionPart::Str("beta".to_string()),
                VersionPart::Whatever,
            ],
            plus: true,
            minus: false,
        },
        ValueRepr::Nil,
        ValueRepr::Whatever,
        ValueRepr::HyperWhatever,
        ValueRepr::Mixin(
            Arc::new(Value::int(1)),
            Arc::new(HashMap::from([("Bool".to_string(), Value::truth(true))])),
        ),
        ValueRepr::Capture {
            positional: Box::new(vec![Value::int(1)]),
            named: Box::new(HashMap::from([("n".to_string(), Value::int(2))])),
        },
        ValueRepr::Uni(Box::new(UniData {
            form: "NFC".to_string(),
            text: "abc".to_string(),
        })),
        ValueRepr::Proxy {
            fetcher: Box::new(Value::NIL),
            storer: Box::new(Value::NIL),
            subclass: Some((
                Symbol::intern("MyProxy"),
                Arc::new(Mutex::new(HashMap::new())),
            )),
            decontainerized: true,
        },
        ValueRepr::ParametricRole {
            base_name: Symbol::intern("R1"),
            type_args: vec![Value::str("C1".to_string())],
        },
        ValueRepr::CustomType(Box::new(CustomTypeData {
            how: Box::new(Value::NIL),
            repr: "P6opaque".to_string(),
            name: Symbol::intern("CustomT"),
            id: 1,
        })),
        ValueRepr::CustomTypeInstance(Box::new(CustomTypeInstanceData {
            type_id: 1,
            how: Box::new(Value::NIL),
            repr: "P6opaque".to_string(),
            type_name: Symbol::intern("CustomT"),
            attributes: Arc::new(HashMap::new()),
            id: 2,
        })),
        ValueRepr::Scalar(Box::new(Value::int(11))),
        ValueRepr::ContainerRef(Gc::new(Mutex::new(Value::int(3)))),
        ValueRepr::LazyThunk(Arc::new(LazyThunkData {
            thunk: Value::NIL,
            cache: Mutex::new(Some(Value::int(5))),
        })),
        ValueRepr::LazyIoLines {
            handle: Box::new(Value::NIL),
            kv: true,
            words: false,
        },
        ValueRepr::HashEntryRef {
            hash: Gc::new(HashData::default()),
            path: vec!["a".to_string(), "b".to_string()],
            eager: false,
        },
    ];
    for repr in samples {
        assert_roundtrip(repr);
    }
}

#[test]
fn lazy_list_roundtrips_by_identity() {
    // `LazyList` has interior Mutex state and no meaningful Debug shape, so
    // check node identity instead of Debug equality.
    let node = Gc::new(LazyList {
        body: vec![],
        env: Env::new(),
        cache: Mutex::new(Some(vec![Value::int(1)])),
        compiled_code: None,
        compiled_fns: None,
        elems_count: None,
        scan_spec: None,
        sequence_spec: None,
        coroutine: None,
        lazy_pipe: None,
        closure_seq: None,
        walk_pending: None,
        cat_pull: None,
    });
    match roundtrip(ValueRepr::LazyList(node.clone())) {
        ValueRepr::LazyList(back) => assert!(Gc::ptr_eq(&back, &node)),
        other => panic!("decoded as {other:?}"),
    }
    assert_eq!(node.strong_count(), 1);
}

#[test]
fn promise_and_channel_roundtrip_sharing_state() {
    let p = SharedPromise::new();
    let b = NanBox::from_repr(ValueRepr::Promise(p.clone()));
    match b.into_repr() {
        ValueRepr::Promise(back) => {
            back.keep(Value::int(42), String::new(), String::new());
            assert_eq!(p.status(), "Kept", "decoded promise shares state");
        }
        other => panic!("decoded as {other:?}"),
    }

    let c = SharedChannel::new();
    let b = NanBox::from_repr(ValueRepr::Channel(c.clone()));
    match b.into_repr() {
        ValueRepr::Channel(back) => {
            back.send(Value::int(7));
            assert_eq!(c.receive_result().ok().and_then(|v| v.as_int()), Some(7));
        }
        other => panic!("decoded as {other:?}"),
    }
}

#[test]
fn debug_matches_the_repr_debug() {
    let b = NanBox::from_repr(ValueRepr::Pair("k".to_string(), Box::new(Value::int(1))));
    assert_eq!(
        format!("{b:?}"),
        format!(
            "{:?}",
            ValueRepr::Pair("k".to_string(), Box::new(Value::int(1)))
        )
    );
}
