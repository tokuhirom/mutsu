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
        eager: bool,
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
            Value::HashEntryRef { hash, path, eager } => ValueView::HashEntryRef {
                hash,
                path,
                eager: *eager,
            },
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

    /// Whatever (`*`) constant (post-box: a tag constant).
    pub const WHATEVER: Value = Value::Whatever;
    /// HyperWhatever (`**`) constant (post-box: a tag constant).
    pub const HYPER_WHATEVER: Value = Value::HyperWhatever;

    // Representation-independent mirrors of the remaining variant
    // constructors, added for the 3b-0 call-site migration. Raw payload
    // pass-throughs: no normalization (unlike e.g. `make_rat`).

    /// Construct a `Str` from an existing shared string.
    #[inline]
    pub fn str_arc(s: Arc<String>) -> Self {
        Value::Str(s)
    }

    /// Construct a `BigInt` from an existing shared bigint.
    #[inline]
    pub fn bigint_arc(n: Arc<NumBigInt>) -> Self {
        Value::BigInt(n)
    }

    /// Construct a `Package` type object.
    #[inline]
    pub(crate) fn package(sym: Symbol) -> Self {
        Value::Package(sym)
    }

    /// Construct a `Routine` stub value.
    #[inline]
    pub(crate) fn routine_parts(package: Symbol, name: Symbol, is_regex: bool) -> Self {
        Value::Routine {
            package,
            name,
            is_regex,
        }
    }

    /// Construct a `Pair` with a string key.
    #[inline]
    pub fn pair(key: String, val: Value) -> Self {
        Value::Pair(key, Box::new(val))
    }

    /// Construct a `ValuePair` (a pair with a non-string key).
    #[inline]
    pub fn value_pair(key: Value, val: Value) -> Self {
        Value::ValuePair(Box::new(key), Box::new(val))
    }

    /// Construct a `Sub` value from its shared payload.
    #[inline]
    pub(crate) fn sub_value(data: crate::gc::Gc<SubData>) -> Self {
        Value::Sub(data)
    }

    /// Construct a `WeakSub` value.
    #[inline]
    pub(crate) fn weak_sub(w: crate::gc::WeakGc<SubData>) -> Self {
        Value::WeakSub(w)
    }

    /// Construct a `Scalar` (itemized) wrapper.
    #[inline]
    pub fn scalar(inner: Value) -> Self {
        Value::Scalar(Box::new(inner))
    }

    /// Construct a `ContainerRef` cell value from an existing cell.
    #[inline]
    pub(crate) fn container_ref(cell: Gc<Mutex<Value>>) -> Self {
        Value::ContainerRef(cell)
    }

    /// Construct a `Promise` value from its shared payload.
    #[inline]
    pub(crate) fn promise(p: SharedPromise) -> Self {
        Value::Promise(p)
    }

    /// Construct a `Channel` value from its shared payload.
    #[inline]
    pub(crate) fn channel(c: SharedChannel) -> Self {
        Value::Channel(c)
    }

    /// Construct a `Version` value from its parts.
    #[inline]
    pub(crate) fn version(parts: Vec<VersionPart>, plus: bool, minus: bool) -> Self {
        Value::Version { parts, plus, minus }
    }

    /// Construct a `CompUnitDepSpec` from its short name.
    #[inline]
    pub(crate) fn comp_unit_dep_spec(short_name: Symbol) -> Self {
        Value::CompUnitDepSpec { short_name }
    }

    /// Construct a `RegexWithAdverbs` from its adverb payload (boxing it).
    #[inline]
    pub(crate) fn regex_with_adverbs(adverbs: RegexAdverbs) -> Self {
        Value::RegexWithAdverbs(Box::new(adverbs))
    }

    /// Construct an inclusive integer `Range`.
    #[inline]
    pub fn range(start: i64, end: i64) -> Self {
        Value::Range(start, end)
    }

    /// Construct an end-exclusive integer range (`a ..^ b`).
    #[inline]
    pub fn range_excl(start: i64, end: i64) -> Self {
        Value::RangeExcl(start, end)
    }

    /// Construct a start-exclusive integer range (`a ^.. b`).
    #[inline]
    pub fn range_excl_start(start: i64, end: i64) -> Self {
        Value::RangeExclStart(start, end)
    }

    /// Construct a both-exclusive integer range (`a ^..^ b`).
    #[inline]
    pub fn range_excl_both(start: i64, end: i64) -> Self {
        Value::RangeExclBoth(start, end)
    }

    /// Construct a `Rat` from an already-normalized numerator/denominator
    /// (use `make_rat` for the normalizing constructor).
    #[inline]
    pub fn rat_raw(num: i64, den: i64) -> Self {
        Value::Rat(num, den)
    }

    /// Construct a `FatRat` from an already-normalized numerator/denominator.
    #[inline]
    pub fn fat_rat_raw(num: i64, den: i64) -> Self {
        Value::FatRat(num, den)
    }

    /// Construct a `Complex` from real/imaginary parts.
    #[inline]
    pub fn complex(re: f64, im: f64) -> Self {
        Value::Complex(re, im)
    }

    /// Construct a `Seq` from an existing shared element vector.
    #[inline]
    pub fn seq_arc(items: Arc<Vec<Value>>) -> Self {
        Value::Seq(items)
    }

    /// Construct a `HyperSeq` from an existing shared element vector.
    #[inline]
    pub fn hyper_seq_arc(items: Arc<Vec<Value>>) -> Self {
        Value::HyperSeq(items)
    }

    /// Construct a `RaceSeq` from an existing shared element vector.
    #[inline]
    pub fn race_seq_arc(items: Arc<Vec<Value>>) -> Self {
        Value::RaceSeq(items)
    }

    /// Construct a `Slip` from an existing shared element vector.
    #[inline]
    pub fn slip_arc(items: Arc<Vec<Value>>) -> Self {
        Value::Slip(items)
    }

    /// Construct a `Hash` from an existing backing store.
    #[inline]
    pub(crate) fn hash_with_data(h: Gc<HashData>) -> Self {
        Value::Hash(h)
    }

    /// Construct a `Set` from an existing backing store and mutability flag.
    #[inline]
    pub(crate) fn set_parts(s: Gc<SetData>, is_mut: bool) -> Self {
        Value::Set(s, is_mut)
    }

    /// Construct a `Bag` from an existing backing store and mutability flag.
    #[inline]
    pub(crate) fn bag_parts(b: Gc<BagData>, is_mut: bool) -> Self {
        Value::Bag(b, is_mut)
    }

    /// Construct a `Mix` from an existing backing store and mutability flag.
    #[inline]
    pub(crate) fn mix_parts(m: Gc<MixData>, is_mut: bool) -> Self {
        Value::Mix(m, is_mut)
    }

    /// Construct an `Instance` from its parts (an existing identity: reuses
    /// `id`; use `make_instance`/`new_with_class` to mint a fresh object).
    #[inline]
    pub(crate) fn instance_parts(
        class_name: Symbol,
        attributes: crate::gc::Gc<InstanceAttrs>,
        id: u64,
    ) -> Self {
        Value::Instance {
            class_name,
            attributes,
            id,
        }
    }

    /// Construct a `LazyList` value from its shared payload.
    #[inline]
    pub(crate) fn lazy_list(l: crate::gc::Gc<LazyList>) -> Self {
        Value::LazyList(l)
    }

    /// Construct a DEFERRED `HashEntryRef` vivification token (`eager: false`
    /// — connects on read only through the cell a bound-var write installs).
    #[inline]
    pub(crate) fn hash_entry_ref(hash: Gc<HashData>, path: Vec<String>) -> Self {
        Value::HashEntryRef {
            hash,
            path,
            eager: false,
        }
    }

    /// Construct an EAGER `HashEntryRef` that reads the current node value on
    /// each access (used for live `for %h -> $p` iteration pairs, whose
    /// `.value` must reflect an in-loop `%h{$p.key} = X`).
    #[inline]
    pub(crate) fn hash_entry_ref_eager(hash: Gc<HashData>, path: Vec<String>) -> Self {
        Value::HashEntryRef {
            hash,
            path,
            eager: true,
        }
    }

    /// Construct a `LazyIoLines` iterator over a file handle (boxing the
    /// handle internally, mirroring `Value::pair`/`Value::scalar`).
    #[inline]
    pub(crate) fn lazy_io_lines(handle: Value, kv: bool, words: bool) -> Self {
        Value::LazyIoLines {
            handle: Box::new(handle),
            kv,
            words,
        }
    }

    /// Construct a `LazyThunk` from an existing shared thunk payload
    /// (a raw pass-through mirroring `seq_arc`).
    #[inline]
    pub(crate) fn lazy_thunk(data: Arc<LazyThunkData>) -> Self {
        Value::LazyThunk(data)
    }

    /// Construct a `Mixin` from its parts, preserving the inner value's
    /// `Arc` identity (unlike `Value::mixin`, which re-wraps).
    #[inline]
    pub(crate) fn mixin_parts(inner: Arc<Value>, overrides: Arc<HashMap<String, Value>>) -> Self {
        Value::Mixin(inner, overrides)
    }

    /// Construct an `Enum` value from its parts.
    #[inline]
    pub(crate) fn enum_parts(
        enum_type: Symbol,
        key: Symbol,
        value: EnumValue,
        index: usize,
    ) -> Self {
        Value::Enum {
            enum_type,
            key,
            value,
            index,
        }
    }

    /// Construct a `Proxy` from its parts.
    #[inline]
    pub(crate) fn proxy_parts(
        fetcher: Value,
        storer: Value,
        subclass: Option<(Symbol, ProxySubclassAttrs)>,
        decontainerized: bool,
    ) -> Self {
        Value::Proxy {
            fetcher: Box::new(fetcher),
            storer: Box::new(storer),
            subclass,
            decontainerized,
        }
    }

    /// Construct a `ParametricRole` type object.
    #[inline]
    pub(crate) fn parametric_role(base_name: Symbol, type_args: Vec<Value>) -> Self {
        Value::ParametricRole {
            base_name,
            type_args,
        }
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

    /// The proxy parts (fetcher, storer, subclass, decontainerized) if this
    /// is exactly a `Proxy`, consuming `self`. Post NaN-boxing: unbox + move.
    #[inline]
    #[allow(clippy::type_complexity)]
    pub(crate) fn into_proxy_parts(
        self,
    ) -> Option<(Value, Value, Option<(Symbol, ProxySubclassAttrs)>, bool)> {
        match self {
            Value::Proxy {
                fetcher,
                storer,
                subclass,
                decontainerized,
            } => Some((*fetcher, *storer, subclass, decontainerized)),
            _ => None,
        }
    }

    // ---- in-place payload mutation ----
    //
    // Closure-based `with_*_mut` entry points replace
    // `match &mut v { Value::Array(gc, k) => ... }` sites — a shape that
    // survives any representation by unpack/run/repack. While `Value` is the
    // enum they compile to a plain match (zero cost). Multi-variant mutable
    // matches become `if let`-chains over these (probe order preserved).

    /// Run `f` on the array payload if this is exactly an `Array`.
    /// Returns `None` (without calling `f`) otherwise.
    #[inline]
    pub(crate) fn with_array_mut<R>(
        &mut self,
        f: impl FnOnce(&mut Gc<ArrayData>, &mut ArrayKind) -> R,
    ) -> Option<R> {
        match self {
            Value::Array(items, kind) => Some(f(items, kind)),
            _ => None,
        }
    }

    /// Container-identity in-place mutation (§3): run `f` on the SHARED
    /// `ArrayData` backing this `Array` value, writing through the node so
    /// every by-value holder of the same container (an `@a` captured into a
    /// list, a `\(@a)`, an element holding the array) observes the write.
    /// `Gc::make_mut` (COW) is wrong for a mutation of a variable's own
    /// container — it detaches the container from its aliases the moment the
    /// backing is shared; Raku `=` copy semantics are enforced at copy time
    /// instead (`detach_shared_container`). Returns `None` for non-`Array`.
    #[inline]
    pub(crate) fn with_array_inplace<R>(
        &self,
        f: impl FnOnce(&mut ArrayData, ArrayKind) -> R,
    ) -> Option<R> {
        match self {
            Value::Array(gc, kind) => {
                // SAFETY: audited aliased in-place container write (see
                // value::aliased_mut) — callers ensure no other borrow into
                // this node is live across `f` and no cross-thread access.
                let data = unsafe { crate::value::gc_contents_mut(gc) };
                Some(f(data, *kind))
            }
            _ => None,
        }
    }

    /// Raku `=` copy semantics: if this Array/Hash's backing `Gc` is SHARED
    /// with another holder, return a fresh-`Gc` shallow copy (elements/values
    /// stay shared references — only the outer container is duplicated); a
    /// singly-owned container or non-container value passes through
    /// unchanged. This is the copy-time counterpart of the in-place mutation
    /// paths (container identity §3): mutations write through the shared
    /// node, so every place with copy semantics (`my @b = @a`, `is copy`
    /// params) must detach at copy time.
    pub(crate) fn detach_shared_container(self) -> Value {
        match &self {
            Value::Array(gc, kind) if gc.strong_count() > 1 => {
                Value::array_with_kind(crate::gc::Gc::new((**gc).clone()), *kind)
            }
            Value::Hash(gc) if gc.strong_count() > 1 => {
                Value::hash_with_data(crate::gc::Gc::new((**gc).clone()))
            }
            _ => self,
        }
    }

    /// Run `f` on the hash payload if this is exactly a `Hash`.
    /// Returns `None` (without calling `f`) otherwise.
    #[inline]
    pub(crate) fn with_hash_mut<R>(&mut self, f: impl FnOnce(&mut Gc<HashData>) -> R) -> Option<R> {
        match self {
            Value::Hash(h) => Some(f(h)),
            _ => None,
        }
    }

    /// Run `f` on the set payload (store + mutability flag) if this is
    /// exactly a `Set`. Returns `None` (without calling `f`) otherwise.
    #[inline]
    pub(crate) fn with_set_mut<R>(
        &mut self,
        f: impl FnOnce(&mut Gc<SetData>, &mut bool) -> R,
    ) -> Option<R> {
        match self {
            Value::Set(s, is_mut) => Some(f(s, is_mut)),
            _ => None,
        }
    }

    /// Run `f` on the bag payload (store + mutability flag) if this is
    /// exactly a `Bag`. Returns `None` (without calling `f`) otherwise.
    #[inline]
    pub(crate) fn with_bag_mut<R>(
        &mut self,
        f: impl FnOnce(&mut Gc<BagData>, &mut bool) -> R,
    ) -> Option<R> {
        match self {
            Value::Bag(b, is_mut) => Some(f(b, is_mut)),
            _ => None,
        }
    }

    /// Run `f` on the mix payload (store + mutability flag) if this is
    /// exactly a `Mix`. Returns `None` (without calling `f`) otherwise.
    #[inline]
    pub(crate) fn with_mix_mut<R>(
        &mut self,
        f: impl FnOnce(&mut Gc<MixData>, &mut bool) -> R,
    ) -> Option<R> {
        match self {
            Value::Mix(m, is_mut) => Some(f(m, is_mut)),
            _ => None,
        }
    }

    /// Run `f` on the sub payload if this is exactly a `Sub`.
    /// Returns `None` (without calling `f`) otherwise.
    #[inline]
    pub(crate) fn with_sub_mut<R>(
        &mut self,
        f: impl FnOnce(&mut crate::gc::Gc<SubData>) -> R,
    ) -> Option<R> {
        match self {
            Value::Sub(data) => Some(f(data)),
            _ => None,
        }
    }

    /// Run `f` on the hash-entry-ref payload (target hash + key path) if this
    /// is exactly a `HashEntryRef`. Returns `None` (without calling `f`)
    /// otherwise. Used e.g. to resync the COW hash pointer after a store.
    #[inline]
    pub(crate) fn with_hash_entry_ref_mut<R>(
        &mut self,
        f: impl FnOnce(&mut Gc<HashData>, &mut Vec<String>) -> R,
    ) -> Option<R> {
        match self {
            Value::HashEntryRef { hash, path, .. } => Some(f(hash, path)),
            _ => None,
        }
    }
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
