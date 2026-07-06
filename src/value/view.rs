//! 3b-0 API wall: representation-independent access to `Value`.
//!
//! `ValueView<'a>` is an exhaustive borrowed mirror of the `Value` enum. Code
//! outside `src/value/` must match on `v.view()` (or use the `as_*`/`with_*`
//! accessors below) instead of naming `Value::` variants directly, so that the
//! NaN-boxing representation switch (layer 3b-1) only has to change this
//! module's internals. Design + migration recipe:
//! [docs/nanbox-3b0-api-wall.md](../../docs/nanbox-3b0-api-wall.md).
//!
//! Payload shape rules (why each field is by-value vs by-reference) are
//! derived from what a NaN-boxed representation can reconstruct — see the
//! design doc §2. Deliberately NOT `Copy`/`Clone`: re-obtain a fresh view via
//! `v.view()` (free), so the smart-pointer reference fields can later become
//! non-Copy guard types without breaking call sites.

use super::*;

/// Borrowed, representation-independent view of a [`Value`].
///
/// Variant names and payload orders mirror `Value` one-to-one so migration is
/// mechanical: `match v { Value::X(..) => ... }` becomes
/// `match v.view() { ValueView::X(..) => ... }`.
#[allow(private_interfaces)]
#[derive(Debug)]
pub enum ValueView<'a> {
    Int(i64),
    BigInt(&'a Arc<NumBigInt>),
    Num(f64),
    Str(&'a Arc<String>),
    Bool(bool),
    Range(i64, i64),
    RangeExcl(i64, i64),
    RangeExclStart(i64, i64),
    RangeExclBoth(i64, i64),
    GenericRange {
        start: &'a Arc<Value>,
        end: &'a Arc<Value>,
        excl_start: bool,
        excl_end: bool,
    },
    Array(&'a Gc<ArrayData>, ArrayKind),
    Hash(&'a Gc<HashData>),
    Rat(i64, i64),
    FatRat(i64, i64),
    BigRat(&'a NumBigInt, &'a NumBigInt),
    Complex(f64, f64),
    Set(&'a Gc<SetData>, bool),
    Bag(&'a Gc<BagData>, bool),
    Mix(&'a Gc<MixData>, bool),
    CompUnitDepSpec {
        short_name: Symbol,
    },
    Package(Symbol),
    Routine {
        package: Symbol,
        name: Symbol,
        is_regex: bool,
    },
    Pair(&'a String, &'a Value),
    ValuePair(&'a Value, &'a Value),
    Enum {
        enum_type: Symbol,
        key: Symbol,
        value: &'a EnumValue,
        index: usize,
    },
    Regex(&'a Arc<String>),
    RegexWithAdverbs(&'a RegexAdverbs),
    Sub(&'a Gc<SubData>),
    WeakSub(&'a crate::gc::WeakGc<SubData>),
    Instance {
        class_name: Symbol,
        attributes: &'a Gc<InstanceAttrs>,
        id: u64,
    },
    Junction {
        kind: &'a JunctionKind,
        values: &'a Arc<Vec<Value>>,
    },
    Seq(&'a Arc<Vec<Value>>),
    HyperSeq(&'a Arc<Vec<Value>>),
    RaceSeq(&'a Arc<Vec<Value>>),
    Slip(&'a Arc<Vec<Value>>),
    LazyList(&'a Gc<LazyList>),
    Version {
        parts: &'a Vec<VersionPart>,
        plus: bool,
        minus: bool,
    },
    Promise(&'a SharedPromise),
    Channel(&'a SharedChannel),
    Nil,
    Whatever,
    HyperWhatever,
    Mixin(&'a Arc<Value>, &'a Arc<HashMap<String, Value>>),
    Capture {
        positional: &'a Vec<Value>,
        named: &'a HashMap<String, Value>,
    },
    Uni(&'a UniData),
    Proxy {
        fetcher: &'a Value,
        storer: &'a Value,
        subclass: &'a Option<(Symbol, ProxySubclassAttrs)>,
        decontainerized: bool,
    },
    ParametricRole {
        base_name: Symbol,
        type_args: &'a Vec<Value>,
    },
    CustomType(&'a CustomTypeData),
    CustomTypeInstance(&'a CustomTypeInstanceData),
    Scalar(&'a Value),
    ContainerRef(&'a Gc<Mutex<Value>>),
    LazyThunk(&'a Arc<LazyThunkData>),
    LazyIoLines {
        handle: &'a Value,
        kv: bool,
        words: bool,
    },
    HashEntryRef {
        hash: &'a Gc<HashData>,
        path: &'a Vec<String>,
    },
}

impl Value {
    /// Decode this value into a borrowed [`ValueView`]. While `Value` is the
    /// enum this is a zero-cost re-borrow; post NaN-boxing it becomes the tag
    /// decode. Always cheap — call it freshly wherever a view is needed.
    #[inline]
    pub fn view(&self) -> ValueView<'_> {
        match self {
            Value::Int(n) => ValueView::Int(*n),
            Value::BigInt(n) => ValueView::BigInt(n),
            Value::Num(f) => ValueView::Num(*f),
            Value::Str(s) => ValueView::Str(s),
            Value::Bool(b) => ValueView::Bool(*b),
            Value::Range(a, b) => ValueView::Range(*a, *b),
            Value::RangeExcl(a, b) => ValueView::RangeExcl(*a, *b),
            Value::RangeExclStart(a, b) => ValueView::RangeExclStart(*a, *b),
            Value::RangeExclBoth(a, b) => ValueView::RangeExclBoth(*a, *b),
            Value::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => ValueView::GenericRange {
                start,
                end,
                excl_start: *excl_start,
                excl_end: *excl_end,
            },
            Value::Array(items, kind) => ValueView::Array(items, *kind),
            Value::Hash(h) => ValueView::Hash(h),
            Value::Rat(n, d) => ValueView::Rat(*n, *d),
            Value::FatRat(n, d) => ValueView::FatRat(*n, *d),
            Value::BigRat(n, d) => ValueView::BigRat(n, d),
            Value::Complex(r, i) => ValueView::Complex(*r, *i),
            Value::Set(s, m) => ValueView::Set(s, *m),
            Value::Bag(b, m) => ValueView::Bag(b, *m),
            Value::Mix(x, m) => ValueView::Mix(x, *m),
            Value::CompUnitDepSpec { short_name } => ValueView::CompUnitDepSpec {
                short_name: *short_name,
            },
            Value::Package(sym) => ValueView::Package(*sym),
            Value::Routine {
                package,
                name,
                is_regex,
            } => ValueView::Routine {
                package: *package,
                name: *name,
                is_regex: *is_regex,
            },
            Value::Pair(k, v) => ValueView::Pair(k, v),
            Value::ValuePair(k, v) => ValueView::ValuePair(k, v),
            Value::Enum {
                enum_type,
                key,
                value,
                index,
            } => ValueView::Enum {
                enum_type: *enum_type,
                key: *key,
                value,
                index: *index,
            },
            Value::Regex(r) => ValueView::Regex(r),
            Value::RegexWithAdverbs(adv) => ValueView::RegexWithAdverbs(adv),
            Value::Sub(s) => ValueView::Sub(s),
            Value::WeakSub(w) => ValueView::WeakSub(w),
            Value::Instance {
                class_name,
                attributes,
                id,
            } => ValueView::Instance {
                class_name: *class_name,
                attributes,
                id: *id,
            },
            Value::Junction { kind, values } => ValueView::Junction { kind, values },
            Value::Seq(items) => ValueView::Seq(items),
            Value::HyperSeq(items) => ValueView::HyperSeq(items),
            Value::RaceSeq(items) => ValueView::RaceSeq(items),
            Value::Slip(items) => ValueView::Slip(items),
            Value::LazyList(l) => ValueView::LazyList(l),
            Value::Version { parts, plus, minus } => ValueView::Version {
                parts,
                plus: *plus,
                minus: *minus,
            },
            Value::Promise(p) => ValueView::Promise(p),
            Value::Channel(c) => ValueView::Channel(c),
            Value::Nil => ValueView::Nil,
            Value::Whatever => ValueView::Whatever,
            Value::HyperWhatever => ValueView::HyperWhatever,
            Value::Mixin(inner, overrides) => ValueView::Mixin(inner, overrides),
            Value::Capture { positional, named } => ValueView::Capture { positional, named },
            Value::Uni(u) => ValueView::Uni(u),
            Value::Proxy {
                fetcher,
                storer,
                subclass,
                decontainerized,
            } => ValueView::Proxy {
                fetcher,
                storer,
                subclass,
                decontainerized: *decontainerized,
            },
            Value::ParametricRole {
                base_name,
                type_args,
            } => ValueView::ParametricRole {
                base_name: *base_name,
                type_args,
            },
            Value::CustomType(d) => ValueView::CustomType(d),
            Value::CustomTypeInstance(d) => ValueView::CustomTypeInstance(d),
            Value::Scalar(inner) => ValueView::Scalar(inner),
            Value::ContainerRef(cell) => ValueView::ContainerRef(cell),
            Value::LazyThunk(t) => ValueView::LazyThunk(t),
            Value::LazyIoLines { handle, kv, words } => ValueView::LazyIoLines {
                handle,
                kv: *kv,
                words: *words,
            },
            Value::HashEntryRef { hash, path } => ValueView::HashEntryRef { hash, path },
        }
    }

    // ---- representation-independent scalar constructors ----

    /// True boolean constant (post-box: a tag constant).
    pub const TRUE: Value = Value::Bool(true);
    /// False boolean constant (post-box: a tag constant).
    pub const FALSE: Value = Value::Bool(false);
    /// Nil constant (post-box: a tag constant).
    pub const NIL: Value = Value::Nil;

    /// Construct an integer value.
    #[inline]
    pub fn int(n: i64) -> Self {
        Value::Int(n)
    }

    /// Construct a floating-point value.
    #[inline]
    pub fn num(f: f64) -> Self {
        Value::Num(f)
    }

    /// Construct a boolean value.
    #[inline]
    pub fn truth(b: bool) -> Self {
        Value::Bool(b)
    }

    /// Construct an array value from an existing backing store and kind
    /// (the wall-side pairing of [`Value::into_array`]).
    #[inline]
    pub(crate) fn array_with_kind(items: Gc<ArrayData>, kind: ArrayKind) -> Self {
        Value::Array(items, kind)
    }

    /// Construct a `Seq` value from an element vector (the `Seq` sibling of
    /// the existing `Value::slip` constructor).
    #[inline]
    pub fn seq(items: Vec<Value>) -> Self {
        Value::Seq(Arc::new(items))
    }

    // ---- single-variant probes (no coercion) ----
    //
    // These test for exactly one representation variant. They are NOT the
    // Raku-semantic coercions (`.Int`, `.Num`, truthiness, stringification) —
    // those live elsewhere and go through method dispatch.

    /// The payload if this is exactly an `Int`. No coercion.
    #[inline]
    pub fn as_int(&self) -> Option<i64> {
        match self {
            Value::Int(n) => Some(*n),
            _ => None,
        }
    }

    /// The payload if this is exactly a `Num`. No coercion.
    #[inline]
    pub fn as_num(&self) -> Option<f64> {
        match self {
            Value::Num(f) => Some(*f),
            _ => None,
        }
    }

    /// The payload if this is exactly a `Bool`. No coercion.
    #[inline]
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(b) => Some(*b),
            _ => None,
        }
    }

    /// The string slice if this is exactly a `Str`. No coercion.
    #[inline]
    pub fn as_str(&self) -> Option<&str> {
        match self {
            Value::Str(s) => Some(s),
            _ => None,
        }
    }

    /// Whether this is `Nil`.
    #[inline]
    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
    }

    // ---- consuming extractors (move the payload out) ----

    /// The backing store and kind if this is exactly an `Array`, consuming
    /// `self` without touching the refcount. Post NaN-boxing: unpack + forget.
    #[inline]
    pub(crate) fn into_array(self) -> Option<(Gc<ArrayData>, ArrayKind)> {
        match self {
            Value::Array(items, kind) => Some((items, kind)),
            _ => None,
        }
    }

    // In-place payload mutation (`match &mut v { Value::Array(gc, k) => ... }`
    // sites) gets closure-based `with_*_mut` entry points — a shape that
    // survives any representation by unpack/run/repack. They are added
    // together with their first migrated caller (design doc §3), not
    // speculatively.
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn view_roundtrips_scalars() {
        assert!(matches!(Value::int(42).view(), ValueView::Int(42)));
        assert!(matches!(Value::num(1.5).view(), ValueView::Num(f) if f == 1.5));
        assert!(matches!(Value::TRUE.view(), ValueView::Bool(true)));
        assert!(matches!(Value::FALSE.view(), ValueView::Bool(false)));
        assert!(matches!(Value::NIL.view(), ValueView::Nil));
    }

    #[test]
    fn probes_are_exact_not_coercing() {
        assert_eq!(Value::int(7).as_int(), Some(7));
        assert_eq!(Value::num(7.0).as_int(), None);
        assert_eq!(Value::str("x".to_string()).as_str(), Some("x"));
        assert_eq!(Value::int(0).as_str(), None);
        assert!(Value::NIL.is_nil());
        assert!(!Value::int(0).is_nil());
    }

    #[test]
    fn view_borrows_pointer_payloads() {
        let v = Value::array(vec![Value::int(1), Value::int(2)]);
        match v.view() {
            ValueView::Array(items, kind) => {
                assert_eq!(items.items.len(), 2);
                assert_eq!(kind, ArrayKind::List);
            }
            other => panic!("expected array view, got {other:?}"),
        }
    }
}
