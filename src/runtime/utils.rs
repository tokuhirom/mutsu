use crate::symbol::Symbol;
use std::collections::{HashMap, HashSet};

use crate::value::{ArrayKind, EnumValue, JunctionKind, RuntimeError, Value, ValueView};
use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{Signed, ToPrimitive, Zero};

/// Maximum number of elements when expanding an infinite range to a list.
pub(crate) const MAX_RANGE_EXPAND: i64 = 1_000_000;

/// The env key recording the `:=` alias target of the sigilless/aliased variable
/// `name` (`my $b := $a` stores `a` under the key for `b`). The single definition
/// of the key shape, so a hot path can pre-intern it instead of rebuilding it per
/// store — see `CompiledCode::locals_alias_sym`.
pub(crate) fn sigilless_alias_key(name: &str) -> String {
    format!("__mutsu_sigilless_alias::{name}")
}

/// The env key marking the sigilless variable `name` as readonly (`my \x = 42`).
/// Companion of [`sigilless_alias_key`]; both are only ever present once the
/// program creates a sigilless/`:=` binding, which `closure_meta_keys_possible`
/// reports.
pub(crate) fn sigilless_readonly_key(name: &str) -> String {
    format!("__mutsu_sigilless_readonly::{name}")
}

/// The env key tracking which indices of `name` were `:delete`d. A `my`
/// redeclaration clears it so a fresh variable cannot inherit an earlier
/// same-named one's holes.
pub(crate) fn deleted_index_key(name: &str) -> String {
    format!("__mutsu_deleted_index::{name}")
}

/// The env key marking `name` as a genuine bound array SLICE (`@slice :=
/// @array[1,2]`), i.e. "this variable's elements are write-through cells". Set
/// only at the bind moment that produces them, and cleared on every
/// redeclaration of the same name.
pub(crate) fn bound_array_slice_key(name: &str) -> String {
    format!("__mutsu_bound_array_slice::{name}")
}

/// True for the per-call-site internal temp names of the Index-argument `is rw`
/// writeback machinery (`__mutsu_index_rw_arg_N` / `__mutsu_index_rw_orig_N` /
/// `__mutsu_call_result_N`, see `compile_call_arg_with_escape` /
/// `emit_index_rw_writebacks`). The names are compile-time-fixed, so a callee
/// that executes a same-named call site of its own (recursion, or an accidental
/// cross-chunk numbering collision) holds entries under the SAME names — the
/// cross-frame env merges must never copy them back into the caller, or the
/// caller's pending post-call compare reads the callee's values and fires a
/// bogus writeback (`g(@xs[1..*])` recursion assigning into an immutable List,
/// 99problems-21-to-30.t P26/P27). The designed writeback channel
/// (`apply_rw_bindings_to_env`) is separate and unaffected.
pub(crate) fn is_index_rw_call_temp(name: &str) -> bool {
    name.starts_with("__mutsu_index_rw_") || name.starts_with("__mutsu_call_result_")
}

/// Build the `Failure` value raku yields when a count/numeric coercion is
/// attempted on a lazy iterable (e.g. `(1..*).elems` / `.Int` / `+@a`):
/// `X::Cannot::Lazy` with the message `Cannot .<action> a lazy list`.
pub(crate) fn cannot_lazy_failure(action: &str) -> Value {
    let mut ex_attrs = HashMap::new();
    ex_attrs.insert(
        "message".to_string(),
        Value::str(format!("Cannot .{} a lazy list", action)),
    );
    ex_attrs.insert("action".to_string(), Value::str(format!(".{}", action)));
    let exception = Value::make_instance(Symbol::intern("X::Cannot::Lazy"), ex_attrs);
    let mut failure_attrs = HashMap::new();
    failure_attrs.insert("exception".to_string(), exception);
    failure_attrs.insert("handled".to_string(), Value::FALSE);
    Value::make_instance(Symbol::intern("Failure"), failure_attrs)
}

/// If `value` is an infinite *integer* range (`1..*`, `^Inf`, `0..^*`, …),
/// return a reify-on-demand `LazyList` (arithmetic sequence, step 1) tagged as
/// living in `@` array context, so `my @a = 1..*` stays lazy (`@a[200000]`
/// reifies, `@a.gist` is `[...]`, `.elems` throws) instead of being capped to a
/// 100k `ArrayKind::Lazy` Array. Returns `None` for finite or non-integer
/// ranges (the caller falls back to `coerce_to_array`).
pub(crate) fn infinite_int_range_to_lazy_array(value: &Value) -> Option<Value> {
    use crate::value::{LazyList, SequenceSpec};
    let start = match value.view() {
        ValueView::Range(a, i64::MAX) | ValueView::RangeExcl(a, i64::MAX) => a,
        ValueView::RangeExclStart(a, i64::MAX) | ValueView::RangeExclBoth(a, i64::MAX) => a + 1,
        ValueView::GenericRange { start, end, .. } => {
            let end_f = end.to_f64();
            if !(end_f.is_infinite() && end_f.is_sign_positive()) {
                return None;
            }
            match start.as_ref().view() {
                ValueView::Int(a) => a,
                _ => return None,
            }
        }
        _ => return None,
    };
    // Seed the cache with just the start element — true memory-laziness (L2b).
    // The `SequenceSpec` lets every read op extend the cache on demand:
    // `@a[N]` (`force_lazy_list_vm_n`), `.head(n)`/`.first` (bounded pull),
    // `.map`/`.grep` (lazy pipe over the sequence). This makes `my @a = 1..*`
    // O(1) memory instead of materializing a 100k-element prefix.
    let seeds: Vec<Value> = vec![Value::int(start)];
    let ll = LazyList::new_sequence(
        seeds,
        SequenceSpec::Arithmetic {
            step: 1,
            all_int: true,
        },
    )
    .with_array_context();
    Some(Value::lazy_list(crate::gc::Gc::new(ll)))
}

/// Saturating conversion of an arbitrary-precision BigInt to i64.
/// Bag/Set arithmetic helpers operate on i64 maps (min/max/diff semantics
/// don't need arbitrary precision); a count that overflows i64 saturates.
pub(crate) fn bigint_to_i64_sat(n: &BigInt) -> i64 {
    n.to_i64()
        .unwrap_or(if n.is_negative() { i64::MIN } else { i64::MAX })
}

/// Saturating conversion of an arbitrary-precision BigInt to i128.
pub(crate) fn bigint_to_i128_sat(n: &BigInt) -> i128 {
    n.to_i128().unwrap_or(if n.is_negative() {
        i128::MIN
    } else {
        i128::MAX
    })
}

/// Saturating conversion of an arbitrary-precision BigInt to f64
/// (out-of-range magnitudes become +/- infinity).
pub(crate) fn bigint_to_f64_sat(n: &BigInt) -> f64 {
    n.to_f64().unwrap_or(if n.is_negative() {
        f64::NEG_INFINITY
    } else {
        f64::INFINITY
    })
}

/// Saturating clone of a Bag's BigInt count map into an i64 count map, used by
/// the set-arithmetic helpers that operate on native i64 weights.
pub(crate) fn bag_counts_as_i64(counts: &HashMap<String, BigInt>) -> HashMap<String, i64> {
    counts
        .iter()
        .map(|(k, v)| (k.clone(), bigint_to_i64_sat(v)))
        .collect()
}

/// Strip a leading UTF-8 BOM (U+FEFF) from a string, as Raku does when reading files.
pub(crate) fn strip_utf8_bom(s: String) -> String {
    if let Some(stripped) = s.strip_prefix('\u{FEFF}') {
        stripped.to_string()
    } else {
        s
    }
}

/// Check if a class name represents a (mutable) Buf-like type (Buf, Buf[uint8],
/// buf8, etc.). The encoding types (utf8/utf16/...) are immutable Blobs, not
/// Bufs, so they are excluded here (see `is_blob_like_class`).
pub(crate) fn is_buf_like_class(cn: &str) -> bool {
    matches!(cn, "Buf" | "buf8" | "buf16" | "buf32" | "buf64")
        || cn.starts_with("Buf[")
        || cn.starts_with("buf")
}

/// Check if a class name represents a Blob-like type (Blob, Blob[uint8], blob8,
/// and the immutable encoding buffers utf8/utf16/utf32).
pub(crate) fn is_blob_like_class(cn: &str) -> bool {
    matches!(
        cn,
        "Blob" | "blob8" | "blob16" | "blob32" | "blob64" | "utf8" | "utf16" | "utf32"
    ) || cn.starts_with("Blob[")
        || cn.starts_with("blob")
}

/// Check if a class name represents any Buf or Blob type
pub(crate) fn is_buf_or_blob_class(cn: &str) -> bool {
    is_buf_like_class(cn) || is_blob_like_class(cn)
}

/// Normalize Buf/Blob type aliases to canonical form.
pub(crate) fn normalize_buf_type_name(name: &str) -> String {
    match name {
        "blob8" => "Blob[uint8]".to_string(),
        "blob16" => "Blob[uint16]".to_string(),
        "blob32" => "Blob[uint32]".to_string(),
        "blob64" => "Blob[uint64]".to_string(),
        "buf8" => "Buf[uint8]".to_string(),
        "buf16" => "Buf[uint16]".to_string(),
        "buf32" => "Buf[uint32]".to_string(),
        "buf64" => "Buf[uint64]".to_string(),
        "utf8" => "Blob[uint8]".to_string(),
        "utf16" => "Blob[uint16]".to_string(),
        _ => name.to_string(),
    }
}

/// Create a Failure value for operations on empty arrays (pop, shift, etc.)
pub(crate) fn make_empty_array_failure(op: &str) -> Value {
    make_empty_array_failure_what(op, "Array")
}

/// Like `make_empty_array_failure`, but with an explicit `what` (the container
/// description, e.g. `array[num]` for a native typed array). Sets the
/// `X::Cannot::Empty` `action` and `what` attributes that roast inspects.
pub(crate) fn make_empty_array_failure_what(op: &str, what: &str) -> Value {
    let mut ex_attrs = HashMap::new();
    ex_attrs.insert(
        "message".to_string(),
        Value::str(format!("Cannot {op} from an empty {what}")),
    );
    ex_attrs.insert("action".to_string(), Value::str(op.to_string()));
    ex_attrs.insert("what".to_string(), Value::str(what.to_string()));
    let exception = Value::make_instance(Symbol::intern("X::Cannot::Empty"), ex_attrs);
    let mut failure_attrs = HashMap::new();
    failure_attrs.insert("exception".to_string(), exception);
    failure_attrs.insert("handled".to_string(), Value::FALSE);
    Value::make_instance(Symbol::intern("Failure"), failure_attrs)
}
/// Embed original (non-string) keys for an object hash into its `HashData`,
/// returning the (possibly rebuilt) value. The map travels WITH the hash
/// through copy-on-write — replacing the old Arc-pointer-keyed side tables, so
/// no `migrate`/`by_id` pointer bookkeeping is needed across COW. Callers must
/// use the returned value (store it back into its slot).
pub(crate) fn set_hash_original_keys(
    mut value: Value,
    original_keys: HashMap<String, Value>,
) -> Value {
    if original_keys.is_empty() {
        return value;
    }
    if value
        .with_hash_mut(|arc| {
            crate::gc::Gc::make_mut(arc).original_keys = Some(original_keys);
        })
        .is_some()
    {
        return value;
    }
    value
}

/// Snapshot the original keys embedded in an object hash, if any.
pub(crate) fn hash_original_keys_snapshot(hash: &Value) -> Option<HashMap<String, Value>> {
    if let ValueView::Hash(arc) = hash.view() {
        return arc.original_keys.clone();
    }
    None
}

/// Whether reading this hash's entries should yield typed (original) keys
/// rather than `Str` keys. See [`crate::value::HashData::has_typed_keys`].
pub(crate) fn hash_uses_typed_keys(hash: &Value) -> bool {
    matches!(hash.view(), ValueView::Hash(arc) if arc.has_typed_keys())
}

/// Retrieve the original (typed) key value for a hash entry, if available.
/// Falls back to the string key if no original key is embedded. Honors the
/// object-hash gate (see [`HashData::typed_key`]): a plain hash always yields a
/// `Str` key.
pub(crate) fn hash_typed_key(hash: &Value, str_key: &str) -> Value {
    if let ValueView::Hash(arc) = hash.view() {
        return arc.typed_key(str_key);
    }
    Value::str(str_key.to_string())
}
pub(crate) fn make_order(ord: std::cmp::Ordering) -> Value {
    match ord {
        std::cmp::Ordering::Less => Value::enum_parts(
            Symbol::intern("Order"),
            Symbol::intern("Less"),
            EnumValue::Int(-1),
            0,
        ),
        std::cmp::Ordering::Equal => Value::enum_parts(
            Symbol::intern("Order"),
            Symbol::intern("Same"),
            EnumValue::Int(0),
            1,
        ),
        std::cmp::Ordering::Greater => Value::enum_parts(
            Symbol::intern("Order"),
            Symbol::intern("More"),
            EnumValue::Int(1),
            2,
        ),
    }
}

pub(crate) fn version_cmp_parts(
    a_parts: &[crate::value::VersionPart],
    b_parts: &[crate::value::VersionPart],
) -> std::cmp::Ordering {
    use crate::value::VersionPart;
    let max_len = a_parts.len().max(b_parts.len());
    for i in 0..max_len {
        let a = a_parts.get(i);
        let b = b_parts.get(i);
        match (a, b) {
            (Some(VersionPart::Num(an)), Some(VersionPart::Num(bn))) => match an.cmp(bn) {
                std::cmp::Ordering::Equal => continue,
                other => return other,
            },
            (Some(VersionPart::Str(sa)), Some(VersionPart::Str(sb))) => match sa.cmp(sb) {
                std::cmp::Ordering::Equal => continue,
                other => return other,
            },
            // Str parts sort before Num parts (alpha/pre-release comes before release)
            (Some(VersionPart::Num(_)), Some(VersionPart::Str(_))) => {
                return std::cmp::Ordering::Greater;
            }
            (Some(VersionPart::Str(_)), Some(VersionPart::Num(_))) => {
                return std::cmp::Ordering::Less;
            }
            // Missing part defaults: Num(0) for missing
            (None, Some(VersionPart::Num(n))) => {
                if *n != 0 {
                    return std::cmp::Ordering::Less;
                }
            }
            (Some(VersionPart::Num(n)), None) => {
                if *n != 0 {
                    return std::cmp::Ordering::Greater;
                }
            }
            // Missing vs Str: missing (treated as Num(0)) is Greater than Str
            // (Str parts are pre-release, so they come before the plain version)
            (None, Some(VersionPart::Str(_))) => return std::cmp::Ordering::Greater,
            (Some(VersionPart::Str(_)), None) => return std::cmp::Ordering::Less,
            // A `*` (Whatever) part sorts *before* any concrete part (it acts as
            // -infinity for ordering: `v1.* <=> v1.0` is `Less`). This is distinct
            // from smart-matching, where a Whatever in the *matcher* accepts anything.
            (Some(VersionPart::Whatever), Some(VersionPart::Whatever)) => continue,
            (Some(VersionPart::Whatever), _) => return std::cmp::Ordering::Less,
            (_, Some(VersionPart::Whatever)) => return std::cmp::Ordering::Greater,
            (None, None) => continue,
        }
    }
    std::cmp::Ordering::Equal
}

/// Full `Version` ordering, including the trailing `+` / `-` flag as a
/// tie-breaker: when the parts compare equal, `v1+ <=> v1` is `More` (a `+`
/// version sorts *after* the bare version) and `-` sorts before it.
pub(crate) fn version_cmp(
    a_parts: &[crate::value::VersionPart],
    a_plus: bool,
    a_minus: bool,
    b_parts: &[crate::value::VersionPart],
    b_plus: bool,
    b_minus: bool,
) -> std::cmp::Ordering {
    match version_cmp_parts(a_parts, b_parts) {
        std::cmp::Ordering::Equal => {
            let a_rank = a_plus as i8 - a_minus as i8;
            let b_rank = b_plus as i8 - b_minus as i8;
            a_rank.cmp(&b_rank)
        }
        other => other,
    }
}

mod coerce_containers;
mod compare;
mod errors;
mod gist;
mod list;
mod radix_numeric;
mod rat;
mod set_coerce;
mod set_ops;
mod shaped;
mod type_constraints;
mod type_misc;

pub(crate) use coerce_containers::*;
pub(crate) use compare::*;
pub(crate) use errors::*;
pub(crate) use gist::*;
pub(crate) use list::*;
pub(crate) use radix_numeric::*;
pub(crate) use rat::*;
pub(crate) use set_coerce::*;
pub(crate) use set_ops::*;
pub(crate) use shaped::*;
pub(crate) use type_constraints::*;
pub(crate) use type_misc::*;

pub(crate) use super::sprintf::format_sprintf;
pub(crate) use super::sprintf::format_sprintf_args;
pub(crate) use super::sprintf::format_zprintf;
