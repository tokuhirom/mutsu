use crate::meta_ns::MetaNs;
use crate::symbol::Symbol;
use std::collections::{HashMap, HashSet};

use crate::value::ValueMap;
use crate::value::{ArrayKind, EnumValue, JunctionKind, RuntimeError, Value, ValueView};
use num_bigint::BigInt;
use num_traits::{Signed, ToPrimitive};

pub(crate) use crate::value::to_list::MAX_RANGE_EXPAND;

pub(crate) use crate::value::array_coerce::MAX_LAZY_RANGE_PREFIX;

/// The env key recording the `:=` alias target of the sigilless/aliased variable
/// `name` (`my $b := $a` stores `a` under the key for `b`), pre-interned.
///
/// Returns a `Symbol` rather than a `String` because the key is built on paths
/// that run per store and per closure call: the caller probes the env with
/// `get_sym` / `contains_key_sym` / `remove_sym` and writes with
/// `insert_sym_noting`, so neither the key string nor its hash is rebuilt
/// (#8087). [`MetaNs`] memoizes the mapping.
pub(crate) fn sigilless_alias_key(name: &str) -> crate::symbol::Symbol {
    crate::meta_ns::MetaNs::SigillessAlias.key_for_str(name)
}

/// The env key marking the sigilless variable `name` as readonly (`my \x = 42`).
/// Companion of [`sigilless_alias_key`]; both are only ever present once the
/// program creates a sigilless/`:=` binding, which `closure_meta_keys_possible`
/// reports.
pub(crate) fn sigilless_readonly_key(name: &str) -> crate::symbol::Symbol {
    crate::meta_ns::MetaNs::SigillessReadonly.key_for_str(name)
}

/// The env key tracking which indices of `name` were `:delete`d. A `my`
/// redeclaration clears it so a fresh variable cannot inherit an earlier
/// same-named one's holes.
pub(crate) fn deleted_index_key(name: &str) -> String {
    MetaNs::DeletedIndex.owned_key_for_str(name)
}

/// The other spelling of a `$*`-twigil dynamic variable's env key: `$*OUT` <->
/// `*OUT`. A dynamic var is stored under both the sigilled and the sigilless
/// form (seeded together — see `BASE_TIER_DYNAMICS`), and every writer or
/// restorer that touches one spelling by name must mirror the other or the two
/// desync (issue #8645). This is the single canonical implementation; do not
/// re-derive this transform at a new call site.
pub(crate) fn twigil_dynamic_alias(name: &str) -> Option<String> {
    if let Some(rest) = name.strip_prefix("$*") {
        return Some(format!("*{}", rest));
    }
    if let Some(rest) = name.strip_prefix('*') {
        return Some(format!("$*{}", rest));
    }
    None
}

/// The env key marking `name` as a genuine bound array SLICE (`@slice :=
/// @array[1,2]`), i.e. "this variable's elements are write-through cells". Set
/// only at the bind moment that produces them, and cleared on every
/// redeclaration of the same name.
pub(crate) fn bound_array_slice_key(name: &str) -> String {
    MetaNs::BoundArraySlice.owned_key_for_str(name)
}

/// The env key recording that a `$` name was `:=`-bound straight to a value and
/// so owns no Scalar container (`my $i := 42`). Written at the declaration's
/// store and speculatively cleared on every other scalar declaration, so — like
/// the four keys above — a hot path pre-interns it per local rather than
/// rebuilding it per store (`CompiledCode::scalar_no_container_sym`).
///
/// The `$` is trimmed because the compiler stores scalars under their bare name,
/// but a caller that still holds the sigiled spelling must land on the same key.
pub(crate) fn scalar_bind_no_container_key(name: &str) -> String {
    MetaNs::ScalarBindNoContainer.owned_key_for_str(name.trim_start_matches('$'))
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

/// Build the Failure returned by an empty reduction whose operator has no
/// identity element.
pub(crate) fn no_zero_arg_meaning_failure(op: &str) -> Value {
    // Operators containing a closing angle bracket use Raku's alternate
    // guillemet form for their long name so the `name` attribute is exact.
    let long_name = if op.contains('>') || op.ends_with('<') {
        format!("infix:«{}»", op)
    } else {
        format!("infix:<{}>", op)
    };
    let mut ex_attrs = HashMap::new();
    ex_attrs.insert(
        "message".to_string(),
        Value::str(format!("No zero-argument meaning for: {}", long_name)),
    );
    ex_attrs.insert("name".to_string(), Value::str(long_name));
    let exception = Value::make_instance(Symbol::intern("X::NoZeroArgMeaning"), ex_attrs);
    let mut failure_attrs = HashMap::new();
    failure_attrs.insert("exception".to_string(), exception);
    failure_attrs.insert("handled".to_string(), Value::FALSE);
    Value::make_instance(Symbol::intern("Failure"), failure_attrs)
}

/// If `value` is an unbounded range of any element type (`1..*`, `^Inf`,
/// `1.5..*`, `"a"..*`; see `runtime::unbounded_range`), return it as a
/// reify-on-demand `LazyList` tagged as living in `@` array context, so
/// `my @a = 1..*` stays lazy (`@a[200000]` reifies, `@a.gist` is `[...]`,
/// `.elems` throws) instead of being capped to a 100k `ArrayKind::Lazy` Array.
/// Returns `None` for anything else (the caller falls back to
/// `coerce_to_array`).
pub(crate) fn infinite_range_to_lazy_array(value: &Value) -> Option<Value> {
    let ll = crate::runtime::unbounded_range::lazy_list(value)?.with_array_context();
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

/// Whether `v` is an *itemized* aggregate, i.e. a `Scalar` container.
///
/// Itemization has three spellings in mutsu and this covers all of them: a
/// `ValueView::Scalar` wrapper; an itemized `List`/`Array` (`$(1, 2)` / `$[1, 2]`
/// share their backing storage with the plain form and carry the itemization in
/// the `ArrayKind`); and a `Hash` carrying an itemization flag on its repr. A
/// check for the wrapper alone misses the other two.
pub(crate) fn value_is_itemized_container(v: &Value) -> bool {
    match v.view() {
        ValueView::Scalar(_) => true,
        ValueView::Array(_, kind) => kind.is_itemized(),
        ValueView::Hash(_) => v.hash_is_itemized(),
        _ => false,
    }
}

/// A Bag weight read out of a `Value`, at full precision.
///
/// Bag weights are `Int` in raku -- `(a => 2.7).Bag` is `("a"=>2).Bag` -- so a
/// fractional operand truncates toward zero, exactly as `Rat.Int` does, and a
/// `Bool` weighs 1/0. The point of going through `to_bigint` rather than
/// `to_f64() as i64` is that `(a => 10**30)` keeps all thirty digits:
/// `BagData.counts` is a `BigInt` map, so nothing downstream needs to
/// saturate.
pub(crate) fn bag_weight(v: &Value) -> BigInt {
    let v = v.deref_container().deitemize_element();
    match v.view() {
        ValueView::Bool(b) => BigInt::from(i64::from(b)),
        _ => v.to_bigint(),
    }
}

/// Strip a leading UTF-8 BOM (U+FEFF) from a string, as Raku does when reading files.
pub(crate) fn strip_utf8_bom(s: String) -> String {
    if let Some(stripped) = s.strip_prefix('\u{FEFF}') {
        stripped.to_string()
    } else {
        s
    }
}

/// Translate the CRLF line ending to LF, as Raku's decoder does on every
/// *text-mode* read (the handle's default `nl-in` is `["\n", "\r\n"]`, and a
/// matched ending is normalized to `"\n"`). It applies to whole-file reads too,
/// not just the chomping routines: `"f".IO.slurp` on `"a\r\nb"` yields
/// `"a\nb"`, while `:bin` and `Blob.decode` keep the bytes verbatim. A lone
/// `\r` is NOT a line ending and stays as it is.
pub(crate) fn translate_nl_in(s: String) -> String {
    if s.contains('\r') {
        s.replace("\r\n", "\n")
    } else {
        s
    }
}

/// The standard text-mode decode fixups Raku applies to file content: strip a
/// leading BOM, then normalize CRLF line endings.
pub(crate) fn decode_text_content(s: String) -> String {
    // NFC like every other decode (ADR-0118 §2.4): rakudo's strings are NFG,
    // so a slurped file and a `.decode`d Blob of the same bytes are equal.
    crate::builtins::nfc(translate_nl_in(strip_utf8_bom(s)))
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
pub(crate) fn set_hash_original_keys(mut value: Value, original_keys: ValueMap) -> Value {
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

/// Enforce the object-hash keying invariant on a hash that carries (or is
/// about to carry) a `key_type`: every entry is stored under the canonical
/// `.WHICH` string of its key *object*, and `original_keys` maps every store
/// key back to that object. A hash built by a key-type-blind path (hash
/// literal, list→hash coercion) has stringified keys; this re-keys it
/// losslessly using the key objects those paths record in `original_keys`
/// (a key with no recorded object was a genuine `Str` key). Idempotent:
/// a hash already `.WHICH`-keyed is left untouched (checked first, so a
/// shared Gc is not cloned).
pub(crate) fn ensure_object_hash_which_keys(value: &mut crate::gc::Gc<crate::value::HashData>) {
    let which_keyed = value.map.keys().all(|k| {
        value
            .original_keys
            .as_ref()
            .and_then(|orig| orig.get(k))
            .is_some_and(|obj| value_which_key(obj) == *k)
    });
    if which_keyed {
        return;
    }
    let data = crate::gc::Gc::make_mut(value);
    let old_map = std::mem::take(&mut data.map);
    let old_orig = data.original_keys.take().unwrap_or_default();
    let mut new_map = crate::value::user_key_map::with_capacity(old_map.len());
    let mut new_orig = crate::value::user_key_map::with_capacity(old_map.len());
    for (key, val) in old_map {
        let key_obj = old_orig
            .get(&key)
            .cloned()
            .unwrap_or_else(|| Value::hash_key_decode(&key));
        let which = value_which_key(&key_obj);
        new_orig.insert(which.clone(), key_obj);
        new_map.insert(which, val);
    }
    data.map = new_map;
    data.original_keys = Some(new_orig);
}

/// Turn a freshly-built plain hash into an object hash (`:{ ... }` — key type
/// `Mu`), embedding the key type metadata and enforcing the `.WHICH` keying
/// invariant. The value type is left unset: like rakudo, a missing-key read
/// on `:{...}` yields `Any`, not `Mu`. Callers must use the returned value.
pub(crate) fn into_object_hash(mut value: Value, key_type: &str) -> Value {
    value.with_hash_mut(|arc| {
        let data = crate::gc::Gc::make_mut(arc);
        data.key_type = Some(key_type.to_string());
        ensure_object_hash_which_keys(arc);
    });
    value
}

/// Snapshot the original keys embedded in an object hash, if any.
pub(crate) fn hash_original_keys_snapshot(hash: &Value) -> Option<ValueMap> {
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
/// object-hash gate (see [`HashData::typed_key`](crate::value::HashData::typed_key)): a plain hash always yields a
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

/// The three parts of an `IO::Path::Parts`, in the fixed order used by
/// positional indexing (`$parts[0]`/`[1]`/`[2]`), `.flat`, and map coercion.
pub(crate) fn io_path_parts_keys() -> &'static [&'static str] {
    &["volume", "dirname", "basename"]
}

mod binding_errors;
mod char_cursor;
mod coerce_containers;
mod errors;
mod list;
mod list_borrow;
mod rat;
mod set_algebra;
mod set_coerce;
mod set_operand;
mod set_ops;
mod type_check_errors;
mod type_constraints;
mod type_misc;
mod zero_denominator;

pub(crate) use crate::value::gist::*;
pub(crate) use binding_errors::*;
pub(crate) use char_cursor::*;
pub(crate) use coerce_containers::*;
pub(crate) use errors::*;
pub(crate) use list::*;
pub(crate) use list_borrow::*;
pub(crate) use rat::*;
pub(crate) use set_algebra::*;
pub(crate) use set_coerce::*;
pub(crate) use set_operand::*;
pub(crate) use set_ops::*;
// The name-marker byte scans live below the runtime (issue #10779); the
// glob re-export keeps them reachable as `runtime::utils::*`.
pub(crate) use crate::str_scan::*;
pub(crate) use crate::value::array_coerce::itemize_real_array_elements;
pub(crate) use crate::value::buf_class_names::*;
pub(crate) use crate::value::compare::*;
pub(crate) use crate::value::identity::*;
pub(crate) use crate::value::identity_index::*;
pub(crate) use crate::value::numeric_coerce::*;
pub(crate) use crate::value::quanthash_keys::*;
pub(crate) use crate::value::radix_numeric::*;
pub(crate) use crate::value::rat_parts::*;
pub(crate) use crate::value::shaped_array::*;
pub(crate) use crate::value::to_list::{stash_symbol_entries, value_to_list};
pub(crate) use crate::value::type_name::value_type_name;
pub(crate) use crate::value::version_cmp::*;
pub(crate) use crate::value::which_key::value_which_key;
pub(crate) use type_check_errors::*;
pub(crate) use type_constraints::*;
pub(crate) use type_misc::*;
pub(crate) use zero_denominator::*;

pub(crate) use super::sprintf::format_sprintf;
pub(crate) use super::sprintf::format_sprintf_args;
pub(crate) use super::sprintf::format_zprintf;
pub(crate) use super::sprintf::sprintf_directive_count;
