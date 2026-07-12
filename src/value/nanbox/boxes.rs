//! Heap payload structs for multi-field / oversized `ValueRepr` variants.
//!
//! A NaN-box payload is one 48-bit pointer, so every variant whose fields
//! exceed that moves behind a single `Arc<...Box>` allocation. The boxes are
//! immutable-by-convention: mutation decodes to `ValueRepr` (via
//! `Arc::try_unwrap`-or-clone in `decode.rs`) and re-packs, so sharing a box
//! between clones of a `Value` is never observable.
//!
//! On 64-bit targets the subkind lives in the pointer's low 3 bits, so every
//! payload allocation must be at least 8-aligned — asserted per type below
//! (only `RoutineBox` needs an explicit `repr(align)`; the rest carry a
//! pointer-sized field already).

use super::super::*;

/// Payload of `Range`/`RangeExcl`/`RangeExclStart`/`RangeExclBoth`/`Rat`/
/// `FatRat` (the kind disambiguates).
#[derive(Debug, Clone)]
pub(in crate::value) struct I64Pair(pub(in crate::value) i64, pub(in crate::value) i64);

#[derive(Debug, Clone)]
pub(in crate::value) struct F64Pair(pub(in crate::value) f64, pub(in crate::value) f64);

#[derive(Debug, Clone)]
pub(in crate::value) struct BigRatBox(
    pub(in crate::value) NumBigInt,
    pub(in crate::value) NumBigInt,
);

#[derive(Debug, Clone)]
pub(in crate::value) struct PairBox(pub(in crate::value) String, pub(in crate::value) Value);

#[derive(Debug, Clone)]
pub(in crate::value) struct ValuePairBox(pub(in crate::value) Value, pub(in crate::value) Value);

#[derive(Debug, Clone)]
pub(in crate::value) struct EnumBox {
    pub(in crate::value) enum_type: Symbol,
    pub(in crate::value) key: Symbol,
    pub(in crate::value) value: EnumValue,
    pub(in crate::value) index: usize,
}

#[derive(Debug, Clone)]
pub(in crate::value) struct GenericRangeBox {
    pub(in crate::value) start: Arc<Value>,
    pub(in crate::value) end: Arc<Value>,
    pub(in crate::value) excl_start: bool,
    pub(in crate::value) excl_end: bool,
}

#[derive(Debug, Clone)]
pub(in crate::value) struct VersionBox {
    pub(in crate::value) parts: Vec<VersionPart>,
    pub(in crate::value) plus: bool,
    pub(in crate::value) minus: bool,
}

#[derive(Debug, Clone)]
pub(in crate::value) struct CaptureBox {
    #[allow(clippy::box_collection)]
    pub(in crate::value) positional: Box<Vec<Value>>,
    #[allow(clippy::box_collection)]
    pub(in crate::value) named: Box<HashMap<String, Value>>,
}

#[derive(Debug, Clone)]
pub(in crate::value) struct ProxyBox {
    pub(in crate::value) fetcher: Box<Value>,
    pub(in crate::value) storer: Box<Value>,
    pub(in crate::value) subclass: Option<(Symbol, ProxySubclassAttrs)>,
    pub(in crate::value) decontainerized: bool,
}

#[derive(Debug, Clone)]
pub(in crate::value) struct ParametricRoleBox {
    pub(in crate::value) base_name: Symbol,
    pub(in crate::value) type_args: Vec<Value>,
}

/// Two `Symbol`s + flag = 65 bits, just over the inline budget. 8-aligned
/// explicitly: its natural alignment is only 4 (no pointer-sized field).
#[derive(Debug, Clone)]
#[repr(align(8))]
pub(in crate::value) struct RoutineBox {
    pub(in crate::value) package: Symbol,
    pub(in crate::value) name: Symbol,
    pub(in crate::value) is_regex: bool,
}

#[derive(Debug, Clone)]
pub(in crate::value) struct MixinBox(
    pub(in crate::value) Arc<Value>,
    pub(in crate::value) Arc<HashMap<String, Value>>,
);

#[derive(Debug, Clone)]
pub(in crate::value) struct LazyIoLinesBox {
    pub(in crate::value) handle: Box<Value>,
    pub(in crate::value) kv: bool,
    pub(in crate::value) words: bool,
}

#[derive(Debug, Clone)]
pub(in crate::value) struct HashEntryRefBox {
    pub(in crate::value) hash: Gc<HashData>,
    pub(in crate::value) path: Vec<String>,
    pub(in crate::value) eager: bool,
}

/// On 64-bit targets the pointer's low 3 bits carry the subkind, so every
/// `Arc<T>` payload type must be >= 8-aligned (`Arc::into_raw` points at the
/// value, aligned to `align_of::<T>()`).
#[cfg(target_pointer_width = "64")]
const _: () = {
    macro_rules! assert_align8 {
        ($($t:ty),+ $(,)?) => {
            $(assert!(std::mem::align_of::<$t>() >= 8);)+
        };
    }
    assert_align8!(
        String,
        NumBigInt,
        i64,
        Vec<Value>,
        Value,
        LazyThunkData,
        UniData,
        RegexAdverbs,
        CustomTypeData,
        CustomTypeInstanceData,
        I64Pair,
        F64Pair,
        BigRatBox,
        PairBox,
        ValuePairBox,
        EnumBox,
        GenericRangeBox,
        VersionBox,
        CaptureBox,
        ProxyBox,
        ParametricRoleBox,
        RoutineBox,
        MixinBox,
        LazyIoLinesBox,
        HashEntryRefBox,
    );
};
