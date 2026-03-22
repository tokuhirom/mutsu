use crate::symbol::Symbol;
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex, OnceLock};

use crate::value::{ArrayKind, EnumValue, JunctionKind, RuntimeError, Value};
use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{Signed, ToPrimitive, Zero};

/// Maximum number of elements when expanding an infinite range to a list.
const MAX_RANGE_EXPAND: i64 = 1_000_000;

/// Check if a class name represents a Buf-like type (Buf, Buf[uint8], buf8, etc.)
pub(crate) fn is_buf_like_class(cn: &str) -> bool {
    matches!(
        cn,
        "Buf" | "buf8" | "buf16" | "buf32" | "buf64" | "utf8" | "utf16"
    ) || cn.starts_with("Buf[")
        || cn.starts_with("buf")
}

/// Check if a class name represents a Blob-like type (Blob, Blob[uint8], blob8, etc.)
pub(crate) fn is_blob_like_class(cn: &str) -> bool {
    matches!(cn, "Blob" | "blob8" | "blob16" | "blob32" | "blob64")
        || cn.starts_with("Blob[")
        || cn.starts_with("blob")
}

/// Check if a class name represents any Buf or Blob type
pub(crate) fn is_buf_or_blob_class(cn: &str) -> bool {
    is_buf_like_class(cn) || is_blob_like_class(cn)
}

/// Create a Failure value for operations on empty arrays (pop, shift, etc.)
pub(crate) fn make_empty_array_failure(op: &str) -> Value {
    let mut ex_attrs = HashMap::new();
    ex_attrs.insert(
        "message".to_string(),
        Value::str(format!("Cannot {op} from an empty Array")),
    );
    let exception = Value::make_instance(Symbol::intern("X::Cannot::Empty"), ex_attrs);
    let mut failure_attrs = HashMap::new();
    failure_attrs.insert("exception".to_string(), exception);
    failure_attrs.insert("handled".to_string(), Value::Bool(false));
    Value::make_instance(Symbol::intern("Failure"), failure_attrs)
}
type GrepViewBinding = (Arc<Vec<Value>>, Vec<usize>, ArrayKind);
type GrepViewMap = HashMap<usize, GrepViewBinding>;

fn shaped_array_ids() -> &'static Mutex<HashMap<usize, Vec<usize>>> {
    static SHAPED_ARRAY_IDS: OnceLock<Mutex<HashMap<usize, Vec<usize>>>> = OnceLock::new();
    SHAPED_ARRAY_IDS.get_or_init(|| Mutex::new(HashMap::new()))
}

fn grep_view_bindings() -> &'static Mutex<GrepViewMap> {
    static GREP_VIEW_BINDINGS: OnceLock<Mutex<GrepViewMap>> = OnceLock::new();
    GREP_VIEW_BINDINGS.get_or_init(|| Mutex::new(HashMap::new()))
}

fn shaped_array_key(items: &Arc<Vec<Value>>) -> usize {
    Arc::as_ptr(items) as usize
}

fn grep_view_key(items: &Arc<Vec<Value>>) -> usize {
    Arc::as_ptr(items) as usize
}

pub(crate) fn register_grep_view_binding(
    filtered: &Arc<Vec<Value>>,
    source: &Arc<Vec<Value>>,
    source_indices: Vec<usize>,
    source_is_array: ArrayKind,
) {
    if let Ok(mut bindings) = grep_view_bindings().lock() {
        bindings.insert(
            grep_view_key(filtered),
            (source.clone(), source_indices, source_is_array),
        );
    }
}

pub(crate) fn get_grep_view_binding(
    filtered: &Arc<Vec<Value>>,
) -> Option<(Arc<Vec<Value>>, Vec<usize>, ArrayKind)> {
    let bindings = grep_view_bindings().lock().ok()?;
    bindings.get(&grep_view_key(filtered)).cloned()
}

/// Check if an array is a shaped (multidimensional) array.
/// A shaped array is one explicitly created as multidimensional via `:shape`.
pub(crate) fn is_shaped_array(value: &Value) -> bool {
    if let Value::Array(_, kind) = value
        && *kind == ArrayKind::Shaped
    {
        return true;
    }
    shaped_array_shape(value).is_some()
}

pub(crate) fn shaped_array_shape(value: &Value) -> Option<Vec<usize>> {
    let Value::Array(items, kind) = value else {
        return None;
    };
    // Only arrays explicitly created as shaped can be shaped
    if *kind != ArrayKind::Shaped {
        return None;
    }
    let key = shaped_array_key(items);

    fn shape_matches_structure(value: &Value, shape: &[usize]) -> bool {
        if shape.is_empty() {
            return false;
        }
        let Value::Array(items, ..) = value else {
            return false;
        };
        if items.len() != shape[0] {
            return false;
        }
        if shape.len() == 1 {
            return items.iter().all(|v| !matches!(v, Value::Array(..)));
        }
        items
            .iter()
            .all(|child| shape_matches_structure(child, &shape[1..]))
    }

    if items.is_empty() {
        return None;
    }

    fn infer_shape_from_array(
        items: &[Value],
        ids: &HashMap<usize, Vec<usize>>,
    ) -> Option<Vec<usize>> {
        let first = items.first()?;
        let Value::Array(first_items, ..) = first else {
            return None;
        };
        let first_shape = ids.get(&shaped_array_key(first_items))?;
        if !items.iter().all(|child| {
            if let Value::Array(child_items, ..) = child {
                ids.get(&shaped_array_key(child_items)) == Some(first_shape)
            } else {
                false
            }
        }) {
            return None;
        }
        let mut shape = Vec::with_capacity(1 + first_shape.len());
        shape.push(items.len());
        shape.extend_from_slice(first_shape);
        Some(shape)
    }

    let ids = shaped_array_ids().lock().ok()?;
    let cached_shape = ids.get(&key).cloned();
    if let Some(cached_shape) = cached_shape
        && shape_matches_structure(value, &cached_shape)
    {
        return Some(cached_shape);
    }
    let inferred_shape = infer_shape_from_array(items.as_ref(), &ids)?;
    if !shape_matches_structure(value, &inferred_shape) {
        return None;
    }
    drop(ids);
    if let Ok(mut ids) = shaped_array_ids().lock() {
        ids.insert(key, inferred_shape.clone());
    }
    Some(inferred_shape)
}

pub(crate) fn mark_shaped_array(value: &Value, shape: Option<&[usize]>) {
    let Value::Array(items, ..) = value else {
        return;
    };
    mark_shaped_array_items(items, shape);
}

pub(crate) fn mark_shaped_array_items(items: &Arc<Vec<Value>>, shape: Option<&[usize]>) {
    if shape.is_none() {
        return;
    }
    let key = shaped_array_key(items);
    if let Ok(mut ids) = shaped_array_ids().lock() {
        if let Some(current_shape) = ids.get(&key)
            && shape.is_some_and(|s| current_shape.as_slice() == s)
        {
            return;
        }
        ids.insert(key, shape.unwrap_or(&[]).to_vec());
    }
}

/// Collect all leaf values from a shaped (multidimensional) array.
pub(crate) fn shaped_array_leaves(value: &Value) -> Vec<Value> {
    let mut leaves = Vec::new();
    collect_leaves(value, &mut leaves);
    leaves
}

fn collect_leaves(value: &Value, out: &mut Vec<Value>) {
    if let Value::Array(items, ..) = value {
        if items.iter().any(|v| matches!(v, Value::Array(..))) {
            for item in items.iter() {
                collect_leaves(item, out);
            }
        } else {
            out.extend(items.iter().cloned());
        }
    } else {
        out.push(value.clone());
    }
}

/// Collect all (index-tuple, leaf-value) pairs from a shaped array.
pub(crate) fn shaped_array_indexed_leaves(value: &Value) -> Vec<(Vec<i64>, Value)> {
    let mut result = Vec::new();
    let mut indices = Vec::new();
    collect_indexed_leaves(value, &mut indices, &mut result);
    result
}

fn collect_indexed_leaves(value: &Value, indices: &mut Vec<i64>, out: &mut Vec<(Vec<i64>, Value)>) {
    if let Value::Array(items, ..) = value {
        if items.iter().any(|v| matches!(v, Value::Array(..))) {
            for (i, item) in items.iter().enumerate() {
                indices.push(i as i64);
                collect_indexed_leaves(item, indices, out);
                indices.pop();
            }
        } else {
            for (i, item) in items.iter().enumerate() {
                let mut idx = indices.clone();
                idx.push(i as i64);
                out.push((idx, item.clone()));
            }
        }
    }
}

pub(crate) fn values_identical(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Package(name), Value::Int(0)) | (Value::Int(0), Value::Package(name))
            if name.resolve() == "int" =>
        {
            true
        }
        (Value::Array(a, _), Value::Array(b, _)) => std::sync::Arc::ptr_eq(a, b),
        (Value::Seq(a), Value::Seq(b)) => std::sync::Arc::ptr_eq(a, b),
        (Value::Slip(a), Value::Slip(b)) => {
            // Empty is a singleton semantic value in Raku even when represented
            // by distinct empty Slip allocations.
            (a.is_empty() && b.is_empty()) || std::sync::Arc::ptr_eq(a, b)
        }
        (Value::LazyList(a), Value::LazyList(b)) => std::sync::Arc::ptr_eq(a, b),
        (Value::Hash(a), Value::Hash(b)) => std::sync::Arc::ptr_eq(a, b),
        (Value::Sub(a), Value::Sub(b)) => {
            if std::sync::Arc::ptr_eq(a, b) {
                return true;
            }
            // Named subs with the same package and name are identical
            // (e.g. &foo === &EXPORT::ALL::foo when both resolve to the same definition)
            let a_name = a.name.resolve();
            let b_name = b.name.resolve();
            !a_name.is_empty() && a_name == b_name && a.package == b.package
        }
        (Value::WeakSub(a), Value::WeakSub(b)) => a.ptr_eq(b),
        (Value::Mixin(a_inner, a_mix), Value::Mixin(b_inner, b_mix)) => {
            a_inner.eqv(b_inner) && a_mix == b_mix
        }
        (Value::Mixin(_, _), _) | (_, Value::Mixin(_, _)) => false,
        (
            Value::Instance {
                class_name: a_class,
                id: a_id,
                ..
            },
            Value::Instance {
                class_name: b_class,
                id: b_id,
                ..
            },
        ) => {
            let a_name = a_class.resolve();
            let b_name = b_class.resolve();
            if a_name == b_name && (a_name == "Stash" || a_name == "Supply") {
                left.eqv(right)
            } else {
                a_id == b_id
            }
        }
        _ => left.eqv(right),
    }
}

pub(crate) fn make_order(ord: std::cmp::Ordering) -> Value {
    match ord {
        std::cmp::Ordering::Less => Value::Enum {
            enum_type: Symbol::intern("Order"),
            key: Symbol::intern("Less"),
            value: EnumValue::Int(-1),
            index: 0,
        },
        std::cmp::Ordering::Equal => Value::Enum {
            enum_type: Symbol::intern("Order"),
            key: Symbol::intern("Same"),
            value: EnumValue::Int(0),
            index: 1,
        },
        std::cmp::Ordering::Greater => Value::Enum {
            enum_type: Symbol::intern("Order"),
            key: Symbol::intern("More"),
            value: EnumValue::Int(1),
            index: 2,
        },
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
            // Whatever matches anything
            (Some(VersionPart::Whatever), _) | (_, Some(VersionPart::Whatever)) => continue,
            (None, None) => continue,
        }
    }
    std::cmp::Ordering::Equal
}

pub(crate) fn coerce_to_hash(value: Value) -> Value {
    let mix_weight_value = |weight: f64| {
        if weight.is_finite() && weight.fract() == 0.0 {
            Value::Int(weight as i64)
        } else {
            Value::Num(weight)
        }
    };
    match value {
        Value::Hash(_) => value,
        Value::Array(items, ..) => {
            let mut map = HashMap::new();
            let mut i = 0;
            while i < items.len() {
                if let Value::Pair(k, v) = &items[i] {
                    map.insert(k.clone(), *v.clone());
                    i += 1;
                } else if let Value::ValuePair(k, v) = &items[i] {
                    map.insert(k.to_string_value(), *v.clone());
                    i += 1;
                } else {
                    let key = items[i].to_string_value();
                    let val = if i + 1 < items.len() {
                        items[i + 1].clone()
                    } else {
                        Value::Nil
                    };
                    map.insert(key, val);
                    i += 2;
                }
            }
            Value::hash(map)
        }
        Value::Seq(items) | Value::Slip(items) => {
            let mut map = HashMap::new();
            let mut i = 0;
            while i < items.len() {
                if let Value::Pair(k, v) = &items[i] {
                    map.insert(k.clone(), *v.clone());
                    i += 1;
                } else if let Value::ValuePair(k, v) = &items[i] {
                    map.insert(k.to_string_value(), *v.clone());
                    i += 1;
                } else {
                    let key = items[i].to_string_value();
                    let val = if i + 1 < items.len() {
                        items[i + 1].clone()
                    } else {
                        Value::Nil
                    };
                    map.insert(key, val);
                    i += 2;
                }
            }
            Value::hash(map)
        }
        Value::Pair(k, v) => {
            let mut map = HashMap::new();
            map.insert(k, *v);
            Value::hash(map)
        }
        Value::ValuePair(k, v) => {
            let mut map = HashMap::new();
            map.insert(k.to_string_value(), *v);
            Value::hash(map)
        }
        Value::Set(items) => {
            let mut map = HashMap::new();
            for key in items.iter() {
                map.insert(key.clone(), Value::Bool(true));
            }
            Value::hash(map)
        }
        Value::Bag(items) => {
            let mut map = HashMap::new();
            for (key, count) in items.iter() {
                map.insert(key.clone(), Value::Int(*count));
            }
            Value::hash(map)
        }
        Value::Mix(items) => {
            let mut map = HashMap::new();
            for (key, weight) in items.iter() {
                map.insert(key.clone(), mix_weight_value(*weight));
            }
            Value::hash(map)
        }
        Value::Nil => Value::hash(HashMap::new()),
        Value::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Match" => {
            // %($/) returns the named captures hash
            if let Some(named) = attributes.get("named") {
                named.clone()
            } else {
                Value::hash(HashMap::new())
            }
        }
        _ => {
            let mut map = HashMap::new();
            map.insert(value.to_string_value(), Value::Nil);
            Value::hash(map)
        }
    }
}

pub(crate) fn build_hash_from_items(items: Vec<Value>) -> Result<Value, RuntimeError> {
    let total_items = items.len();
    let last_item = items
        .last()
        .map(Value::to_string_value)
        .unwrap_or_else(|| "Nil".to_string());
    let mut map = HashMap::new();
    let mut iter = items.into_iter();
    while let Some(item) = iter.next() {
        match item {
            Value::Pair(key, boxed_val) => {
                map.insert(key, *boxed_val);
            }
            Value::ValuePair(key, boxed_val) => {
                map.insert(key.to_string_value(), *boxed_val);
            }
            other => {
                let Some(value) = iter.next() else {
                    let message = format!(
                        "Odd number of elements found where hash initializer expected: found {total_items} element(s); last element seen: {last_item}"
                    );
                    return Err(RuntimeError::new(message));
                };
                map.insert(other.to_string_value(), value);
            }
        }
    }
    Ok(Value::hash(map))
}

/// Maximum number of elements when expanding an infinite range into an Array.
/// TODO: Properly implement lazy arrays that reify elements on demand.
const MAX_ARRAY_EXPAND: i64 = 100_000;

pub(crate) fn coerce_to_array(value: Value) -> Value {
    fn metadata_shape_for_items(items: &Arc<Vec<Value>>) -> Option<Vec<usize>> {
        let key = shaped_array_key(items);
        shaped_array_ids()
            .lock()
            .ok()
            .and_then(|ids| ids.get(&key).cloned())
    }

    match value {
        Value::Array(items, kind) => {
            if kind == ArrayKind::Shaped {
                Value::Array(items, kind)
            } else if let Some(shape) = metadata_shape_for_items(&items) {
                let value = Value::Array(items, ArrayKind::Shaped);
                mark_shaped_array(&value, Some(&shape));
                value
            } else {
                Value::Array(items, ArrayKind::Array)
            }
        }
        Value::Nil => Value::real_array(Vec::new()),
        Value::Range(a, b) => {
            let end = b.min(a.saturating_add(MAX_ARRAY_EXPAND));
            Value::real_array((a..=end).map(Value::Int).collect())
        }
        Value::RangeExcl(a, b) => {
            let end = b.min(a.saturating_add(MAX_ARRAY_EXPAND));
            Value::real_array((a..end).map(Value::Int).collect())
        }
        Value::RangeExclStart(a, b) => {
            let end = b.min(a.saturating_add(MAX_ARRAY_EXPAND));
            Value::real_array((a + 1..=end).map(Value::Int).collect())
        }
        Value::RangeExclBoth(a, b) => {
            let end = b.min(a.saturating_add(MAX_ARRAY_EXPAND));
            Value::real_array((a + 1..end).map(Value::Int).collect())
        }
        Value::GenericRange {
            ref start, ref end, ..
        } if matches!(start.as_ref(), Value::Str(_)) && matches!(end.as_ref(), Value::Str(_)) => {
            Value::real_array(value_to_list(&value))
        }
        Value::GenericRange { .. } => Value::real_array(value_to_list(&value)),
        Value::Slip(items) | Value::Seq(items) => Value::Array(items, ArrayKind::Array),
        Value::LazyList(_) => value,
        other => Value::real_array(vec![other]),
    }
}

pub(crate) fn coerce_to_str(value: &Value) -> String {
    value.to_str_context()
}

pub(crate) fn gist_value(value: &Value) -> String {
    match value {
        Value::Rat(_, _) | Value::FatRat(_, _) | Value::BigRat(_, _) => {
            // Rat.gist is identical to Rat.Str in Raku
            value.to_string_value()
        }
        Value::Array(items, kind) => {
            let inner = items.iter().map(gist_value).collect::<Vec<_>>().join(" ");
            match kind {
                crate::value::ArrayKind::Array | crate::value::ArrayKind::Shaped => {
                    format!("[{}]", inner)
                }
                crate::value::ArrayKind::List => format!("({})", inner),
                crate::value::ArrayKind::ItemArray => format!("$[{}]", inner),
                crate::value::ArrayKind::ItemList => format!("$({})", inner),
            }
        }
        Value::Hash(items) => {
            let mut sorted_keys: Vec<&String> = items.keys().collect();
            sorted_keys.sort();
            let parts: Vec<String> = sorted_keys
                .iter()
                .map(|k| format!("{} => {}", k, gist_value(&items[*k])))
                .collect();
            format!("{{{}}}", parts.join(", "))
        }
        Value::Pair(k, v) => format!("{} => {}", k, gist_value(v)),
        Value::ValuePair(k, v) => format!("{} => {}", gist_value(k), gist_value(v)),
        Value::Seq(items) | Value::Slip(items) => {
            format!(
                "({})",
                items.iter().map(gist_value).collect::<Vec<_>>().join(" ")
            )
        }
        Value::Version { .. } => format!("v{}", value.to_string_value()),
        Value::Nil => "Nil".to_string(),
        // Range gist shows the range notation, not the expanded elements
        Value::Range(a, b) => format!("{}..{}", a, b),
        Value::RangeExcl(a, b) => format!("{}..^{}", a, b),
        Value::RangeExclStart(a, b) => format!("^{}..{}", a, b),
        Value::RangeExclBoth(a, b) => format!("^{}..^{}", a, b),
        Value::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => {
            let prefix = if *excl_start { "^" } else { "" };
            let sep = if *excl_end { "..^" } else { ".." };
            format!("{}{}{}{}", prefix, gist_value(start), sep, gist_value(end))
        }
        _ => value.to_string_value(),
    }
}

pub(crate) fn is_known_type_constraint(constraint: &str) -> bool {
    if super::native_types::is_native_int_type(constraint) {
        return true;
    }
    matches!(
        constraint,
        "Int"
            | "UInt"
            | "Num"
            | "Str"
            | "Bool"
            | "Array"
            | "Hash"
            | "Rat"
            | "FatRat"
            | "Complex"
            | "atomicint"
            | "int"
            | "num"
            | "str"
    )
}

pub(crate) fn value_type_name(value: &Value) -> &'static str {
    match value {
        Value::Int(_) => "Int",
        Value::BigInt(_) => "Int",
        Value::Num(_) => "Num",
        Value::Str(_) => "Str",
        Value::Bool(_) => "Bool",
        Value::Array(_, kind) if kind.is_real_array() => "Array",
        Value::Array(_, _) => "List",
        Value::LazyList(_) => "Array",
        Value::Hash(_) => "Hash",
        Value::Range(_, _)
        | Value::RangeExcl(_, _)
        | Value::RangeExclStart(_, _)
        | Value::RangeExclBoth(_, _)
        | Value::GenericRange { .. } => "Range",
        Value::Pair(_, _) | Value::ValuePair(_, _) => "Pair",
        Value::Rat(_, _) => "Rat",
        Value::FatRat(_, _) => "FatRat",
        Value::BigRat(_, _) => "Rat",
        Value::Complex(_, _) => "Complex",
        Value::Set(_) => "Set",
        Value::Bag(_) => "Bag",
        Value::Mix(_) => "Mix",
        Value::Nil => "Any",
        Value::Sub(data) => match data.env.get("__mutsu_callable_type") {
            Some(Value::Str(kind)) if kind.as_str() == "Method" => "Method",
            Some(Value::Str(kind)) if kind.as_str() == "WhateverCode" => "WhateverCode",
            _ => "Sub",
        },
        Value::WeakSub(_) => "Sub",
        Value::Routine { .. } => "Routine",
        Value::Package(_) => "Package",
        Value::CompUnitDepSpec { .. } => "Any",
        Value::Enum { .. } => "Int",
        Value::Instance { .. } => "Any",
        Value::Junction { .. } => "Junction",
        Value::Regex(_) | Value::RegexWithAdverbs { .. } => "Regex",
        Value::Version { .. } => "Version",
        Value::Seq(_) => "Seq",
        Value::Slip(_) => "Slip",
        Value::Promise(_) => "Promise",
        Value::Channel(_) => "Channel",
        Value::Whatever => "Whatever",
        Value::HyperWhatever => "HyperWhatever",
        Value::Capture { .. } => "Capture",
        Value::Uni { form, .. } => match form.as_str() {
            "NFC" => "NFC",
            "NFD" => "NFD",
            "NFKC" => "NFKC",
            "NFKD" => "NFKD",
            _ => "Uni",
        },
        Value::Mixin(inner, mixins) => {
            if mixins.contains_key("Str") {
                match inner.as_ref() {
                    Value::Int(_) | Value::BigInt(_) => "IntStr",
                    Value::Num(_) => "NumStr",
                    Value::Rat(_, _) | Value::FatRat(_, _) | Value::BigRat(_, _) => "RatStr",
                    Value::Complex(_, _) => "ComplexStr",
                    _ => value_type_name(inner),
                }
            } else {
                value_type_name(inner)
            }
        }
        Value::Proxy { .. } => "Proxy",
        Value::ParametricRole { .. } => "Package",
        Value::CustomType { .. } => "CustomType",
        Value::CustomTypeInstance { .. } => "CustomTypeInstance",
        Value::Scalar(inner) => value_type_name(inner),
    }
}

pub(crate) fn is_chain_comparison_op(op: &str) -> bool {
    matches!(
        op,
        "==" | "!="
            | "<"
            | ">"
            | "<="
            | ">="
            | "==="
            | "!=="
            | "=:="
            | "eqv"
            | "eq"
            | "ne"
            | "lt"
            | "gt"
            | "le"
            | "ge"
            | "before"
            | "after"
            | "~~"
            | "!~~"
            | "cmp"
            | "leg"
            | "<=>"
            | "%%"
            | "!%%"
    ) || matches!(
        op.strip_prefix('!'),
        Some("==")
            | Some("===")
            | Some("=:=")
            | Some("eqv")
            | Some("eq")
            | Some("ne")
            | Some("lt")
            | Some("gt")
            | Some("le")
            | Some("ge")
            | Some("before")
            | Some("after")
            | Some("cmp")
            | Some("leg")
            | Some("<=>")
    )
}

pub(crate) fn reduction_identity(op: &str) -> Value {
    if is_chain_comparison_op(op) {
        return Value::Bool(true);
    }
    match op {
        "+" | "-" | "+|" | "+^" => Value::Int(0),
        "*" | "**" => Value::Int(1),
        "+&" => Value::Int(-1), // +^0 (all bits set)
        "~" | "~|" | "~^" => Value::str(String::new()),
        "&&" | "and" | "?&" => Value::Bool(true),
        "||" | "or" | "?|" | "^^" => Value::Bool(false),
        "?^" => Value::Bool(false),
        "//" | "orelse" => Value::Package(Symbol::intern("Any")),
        "andthen" => Value::Bool(true),
        "xor" => Value::Bool(false),
        "min" => Value::Num(f64::INFINITY),
        "max" => Value::Num(f64::NEG_INFINITY),
        // Junction operators
        "&" => Value::Junction {
            kind: crate::value::JunctionKind::All,
            values: std::sync::Arc::new(Vec::new()),
        },
        "|" => Value::Junction {
            kind: crate::value::JunctionKind::Any,
            values: std::sync::Arc::new(Vec::new()),
        },
        "^" => Value::Junction {
            kind: crate::value::JunctionKind::One,
            values: std::sync::Arc::new(Vec::new()),
        },
        // Set operators
        "(-)" | "∖" | "(|)" | "∪" | "(&)" | "∩" | "(^)" | "⊖" => Value::set(HashSet::new()),
        "(.)" | "⊍" | "(+)" | "⊎" => Value::bag(HashMap::new()),
        // Comma: empty list
        "," => Value::Array(std::sync::Arc::new(Vec::new()), ArrayKind::List),
        // Zip: empty Seq (Raku returns a Seq for arity-0 Z)
        "Z" => Value::Seq(std::sync::Arc::new(Vec::new())),
        _ => {
            // Hyper operator forms: >>op<<, >>op>>, <<op<<, <<op>>
            if let Some(inner) = strip_hyper_delimiters_for_identity(op) {
                return reduction_identity(inner);
            }
            Value::Nil
        }
    }
}

/// Strip hyper operator delimiters to find the inner operator for identity lookup.
fn strip_hyper_delimiters_for_identity(s: &str) -> Option<&str> {
    let after_left = s
        .strip_prefix(">>")
        .or_else(|| s.strip_prefix("<<"))
        .or_else(|| s.strip_prefix('\u{00BB}'))
        .or_else(|| s.strip_prefix('\u{00AB}'))?;
    let inner = after_left
        .strip_suffix(">>")
        .or_else(|| after_left.strip_suffix("<<"))
        .or_else(|| after_left.strip_suffix('\u{00BB}'))
        .or_else(|| after_left.strip_suffix('\u{00AB}'))?;
    if inner.is_empty() {
        return None;
    }
    Some(inner)
}

pub(crate) fn char_idx_to_byte(text: &str, idx: usize) -> usize {
    if idx == 0 {
        return 0;
    }
    for (count, (b, _)) in text.char_indices().enumerate() {
        if count == idx {
            return b;
        }
    }
    text.len()
}

pub(crate) fn value_to_list(val: &Value) -> Vec<Value> {
    match val {
        Value::Array(items, kind) if kind.is_itemized() => vec![val.clone()],
        Value::Array(items, ..) => items.to_vec(),
        Value::Seq(items) => items.to_vec(),
        Value::LazyList(ll) => ll.cache.lock().unwrap().clone().unwrap_or_default(),
        Value::Hash(items) => items
            .iter()
            .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.clone())))
            .collect(),
        Value::Range(a, b) => {
            let end = (*b).min(*a + MAX_RANGE_EXPAND);
            (*a..=end).map(Value::Int).collect()
        }
        Value::RangeExcl(a, b) => {
            let end = (*b).min(*a + MAX_RANGE_EXPAND);
            (*a..end).map(Value::Int).collect()
        }
        Value::RangeExclStart(a, b) => {
            let start = *a + 1;
            let end = (*b).min(start + MAX_RANGE_EXPAND);
            (start..=end).map(Value::Int).collect()
        }
        Value::RangeExclBoth(a, b) => {
            let start = *a + 1;
            let end = (*b).min(start + MAX_RANGE_EXPAND);
            (start..end).map(Value::Int).collect()
        }
        Value::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => {
            let next_numeric = |v: &Value| -> Option<Value> {
                match v {
                    Value::Int(i) => Some(Value::Int(i + 1)),
                    Value::BigInt(n) => Some(Value::bigint(n.as_ref() + 1)),
                    Value::Num(f) => Some(Value::Num(*f + 1.0)),
                    Value::Rat(n, d) => Some(crate::value::make_rat(n + d, *d)),
                    Value::FatRat(n, d) => Some(Value::FatRat(n + d, *d)),
                    Value::BigRat(n, d) => Some(Value::BigRat(n + d, d.clone())),
                    other if other.is_numeric() => Some(Value::Num(other.to_f64() + 1.0)),
                    _ => None,
                }
            };
            // String ranges: expand as character sequences
            if let (Value::Str(a), Value::Str(b)) = (start.as_ref(), end.as_ref()) {
                if a.chars().count() == 1 && b.chars().count() == 1 {
                    let s = a.chars().next().unwrap() as u32;
                    let e = b.chars().next().unwrap() as u32;
                    let s = if *excl_start { s + 1 } else { s };
                    if *excl_end {
                        (s..e)
                            .filter_map(char::from_u32)
                            .map(|c| Value::str(c.to_string()))
                            .collect()
                    } else {
                        (s..=e)
                            .filter_map(char::from_u32)
                            .map(|c| Value::str(c.to_string()))
                            .collect()
                    }
                } else {
                    let split_numeric_core = |s: &str| -> Option<(String, String, String)> {
                        let mut start_byte = None;
                        let mut end_byte = None;
                        for (idx, ch) in s.char_indices() {
                            if ch.is_ascii_digit() {
                                if start_byte.is_none() {
                                    start_byte = Some(idx);
                                }
                            } else if start_byte.is_some() && end_byte.is_none() {
                                end_byte = Some(idx);
                                break;
                            }
                        }
                        let start = start_byte?;
                        let end = end_byte.unwrap_or(s.len());
                        if end <= start {
                            return None;
                        }
                        Some((
                            s[..start].to_string(),
                            s[start..end].to_string(),
                            s[end..].to_string(),
                        ))
                    };
                    if let (Some((ap, an, asuf)), Some((bp, bn, bsuf))) =
                        (split_numeric_core(a), split_numeric_core(b))
                        && ap == bp
                        && asuf == bsuf
                        && let (Ok(mut n), Ok(e)) = (an.parse::<i128>(), bn.parse::<i128>())
                    {
                        if *excl_start {
                            n += 1;
                        }
                        if n > e {
                            return Vec::new();
                        }
                        let width = an.len().max(bn.len());
                        let pad = an.starts_with('0') || bn.starts_with('0');
                        let mut result = Vec::new();
                        let limit = MAX_RANGE_EXPAND as usize;
                        while n <= e && result.len() < limit {
                            if *excl_end && n == e {
                                break;
                            }
                            let digits = if pad {
                                format!("{n:0width$}")
                            } else {
                                n.to_string()
                            };
                            result.push(Value::str(format!("{ap}{digits}{asuf}")));
                            n += 1;
                        }
                        return result;
                    }
                    // Multi-char string ranges: use string succession
                    let mut result = Vec::new();
                    let mut current = if *excl_start {
                        crate::runtime::Interpreter::string_succ(a)
                    } else {
                        a.to_string()
                    };
                    let limit = MAX_RANGE_EXPAND as usize;
                    while current.as_str() <= b.as_str() && result.len() < limit {
                        if *excl_end && current.as_str() == b.as_str() {
                            break;
                        }
                        result.push(Value::str(current.clone()));
                        current = crate::runtime::Interpreter::string_succ(&current);
                    }
                    result
                }
            } else if let (Value::Str(a), Value::HyperWhatever | Value::Whatever) =
                (start.as_ref(), end.as_ref())
            {
                let mut result = Vec::new();
                let mut current = if *excl_start {
                    crate::runtime::Interpreter::string_succ(a)
                } else {
                    a.to_string()
                };
                let limit = MAX_RANGE_EXPAND as usize;
                while result.len() < limit {
                    result.push(Value::str(current.clone()));
                    current = crate::runtime::Interpreter::string_succ(&current);
                }
                result
            } else {
                // Numeric GenericRange: expand using .succ semantics to preserve endpoint type.
                let start_num = if start.is_numeric() {
                    Some(start.as_ref().clone())
                } else if let Value::Str(s) = start.as_ref() {
                    let coerced = coerce_to_numeric(Value::str((**s).clone()));
                    if coerced.is_numeric() {
                        Some(coerced)
                    } else {
                        None
                    }
                } else {
                    None
                };
                let end_num = if matches!(end.as_ref(), Value::Whatever | Value::HyperWhatever) {
                    Some(Value::Num(f64::INFINITY))
                } else if end.is_numeric() {
                    Some(end.as_ref().clone())
                } else if let Value::Str(s) = end.as_ref() {
                    let coerced = coerce_to_numeric(Value::str((**s).clone()));
                    if coerced.is_numeric() {
                        Some(coerced)
                    } else {
                        None
                    }
                } else {
                    None
                };
                let (start_num, end_num) = match (start_num, end_num) {
                    (Some(s), Some(e)) => (s, e),
                    _ => return vec![val.clone()],
                };
                if start_num.to_f64().is_infinite()
                    || start_num.to_f64().is_nan()
                    || end_num.to_f64().is_nan()
                {
                    vec![val.clone()]
                } else {
                    let mut result = Vec::new();
                    let mut current = if *excl_start {
                        next_numeric(&start_num).unwrap_or(start_num)
                    } else {
                        start_num
                    };
                    let limit = MAX_RANGE_EXPAND as usize;
                    while result.len() < limit {
                        let cmp = compare_values(&current, &end_num);
                        if cmp > 0 || (*excl_end && cmp == 0) {
                            break;
                        }
                        result.push(current.clone());
                        let Some(next) = next_numeric(&current) else {
                            break;
                        };
                        if values_identical(&next, &current) {
                            break;
                        }
                        current = next;
                    }
                    result
                }
            }
        }
        Value::Set(items) => items
            .iter()
            .map(|s| Value::Pair(s.clone(), Box::new(Value::Bool(true))))
            .collect(),
        Value::Bag(items) => items
            .iter()
            .map(|(k, v)| Value::Pair(k.clone(), Box::new(Value::Int(*v))))
            .collect(),
        Value::Mix(items) => items
            .iter()
            .map(|(k, v)| Value::Pair(k.clone(), Box::new(Value::Num(*v))))
            .collect(),
        Value::Slip(items) => items.to_vec(),
        Value::Instance { attributes, .. } => {
            if let Some(Value::Array(items, ..)) = attributes.get("__array_items") {
                return items.to_vec();
            }
            vec![val.clone()]
        }
        Value::Nil => vec![],
        other => vec![other.clone()],
    }
}

pub(crate) use super::sprintf::format_sprintf;
pub(crate) use super::sprintf::format_sprintf_args;

fn parse_unicode_decimal_digits(input: &str) -> Option<(&str, String)> {
    let mut end = 0;
    let mut clean = String::new();
    let mut saw_digit = false;
    for c in input.chars() {
        if c == '_' {
            end += c.len_utf8();
            continue;
        }
        let Some(dv) = crate::builtins::unicode::unicode_decimal_digit_value(c) else {
            break;
        };
        saw_digit = true;
        clean.push(char::from_digit(dv, 10).unwrap());
        end += c.len_utf8();
    }
    if saw_digit {
        Some((&input[end..], clean))
    } else {
        None
    }
}

pub(crate) fn parse_radix_number_body(body: &str, base: u32) -> Option<Value> {
    if !(2..=36).contains(&base) {
        return None;
    }
    let compact: String = body.chars().filter(|c| !c.is_whitespace()).collect();
    if compact.is_empty() {
        return None;
    }

    let (digits_body, exponent_scale) =
        if let Some((digits, exp_part)) = compact.split_once("*10**") {
            let exp_part = exp_part.trim();
            if exp_part.is_empty() {
                return None;
            }
            let (sign, after_sign) = if let Some(rest) = exp_part.strip_prefix('+') {
                (1_i64, rest)
            } else if let Some(rest) = exp_part.strip_prefix('-') {
                (-1_i64, rest)
            } else {
                (1_i64, exp_part)
            };
            let (exp_rest, exp_clean) = parse_unicode_decimal_digits(after_sign)?;
            if !exp_rest.is_empty() {
                return None;
            }
            let exp_abs: i64 = exp_clean.parse().ok()?;
            (digits.trim(), sign * exp_abs)
        } else {
            (compact.trim(), 0_i64)
        };
    if digits_body.is_empty() {
        return None;
    }

    let mut int_clean = String::new();
    let mut frac_clean = String::new();
    let mut saw_dot = false;
    let mut saw_digit = false;
    for c in digits_body.chars() {
        if c == '_' {
            continue;
        }
        if c == '.' {
            if saw_dot {
                return None;
            }
            saw_dot = true;
            continue;
        }
        let value =
            crate::builtins::unicode::unicode_decimal_digit_value(c).or_else(|| match c {
                'a'..='z' => Some(10 + (c as u32 - 'a' as u32)),
                'A'..='Z' => Some(10 + (c as u32 - 'A' as u32)),
                'ａ'..='ｚ' => Some(10 + (c as u32 - 'ａ' as u32)),
                'Ａ'..='Ｚ' => Some(10 + (c as u32 - 'Ａ' as u32)),
                _ => None,
            })?;
        if value >= base {
            return None;
        }
        let digit = char::from_digit(value, 36)?;
        saw_digit = true;
        if saw_dot {
            frac_clean.push(digit);
        } else {
            int_clean.push(digit);
        }
    }
    if !saw_digit {
        return None;
    }

    if !saw_dot && exponent_scale == 0 {
        if let Ok(n) = i64::from_str_radix(&int_clean, base) {
            return Some(Value::Int(n));
        }
        if let Some(n) = num_bigint::BigInt::parse_bytes(int_clean.as_bytes(), base) {
            return Some(Value::from_bigint(n));
        }
        return None;
    }

    let base_big = num_bigint::BigInt::from(base);
    let int_value = if int_clean.is_empty() {
        num_bigint::BigInt::from(0_i64)
    } else {
        num_bigint::BigInt::parse_bytes(int_clean.as_bytes(), base)?
    };
    let frac_value = if frac_clean.is_empty() {
        num_bigint::BigInt::from(0_i64)
    } else {
        num_bigint::BigInt::parse_bytes(frac_clean.as_bytes(), base)?
    };
    let frac_scale = base_big.pow(frac_clean.len() as u32);
    let mut numerator = int_value * &frac_scale + frac_value;
    let mut denominator = frac_scale;
    if exponent_scale != 0 {
        let exp_abs = exponent_scale.unsigned_abs() as u32;
        let scale10 = num_bigint::BigInt::from(10_u32).pow(exp_abs);
        if exponent_scale > 0 {
            numerator *= scale10;
        } else {
            denominator *= scale10;
        }
    }
    Some(crate::value::make_big_rat(numerator, denominator))
}

/// Parse `0x`, `0o`, `0b`, `0d` prefixed integer literals from strings.
/// E.g. `"0xFF"` → 255, `"0o77"` → 63, `"0b1010"` → 10, `"0d42"` → 42.
pub(crate) fn parse_0_prefixed_radix_literal(s: &str) -> Option<Value> {
    let s = s.trim();
    if !s.starts_with('0') || s.len() < 3 {
        return None;
    }
    let (base, digits) = match s.as_bytes()[1] {
        b'x' | b'X' => (16, &s[2..]),
        b'o' | b'O' => (8, &s[2..]),
        b'b' | b'B' => (2, &s[2..]),
        b'd' | b'D' => (10, &s[2..]),
        _ => return None,
    };
    // Strip underscores (Raku allows underscores in numeric literals)
    let clean: String = digits.chars().filter(|&c| c != '_').collect();
    if clean.is_empty() {
        return None;
    }
    i64::from_str_radix(&clean, base)
        .ok()
        .map(Value::Int)
        .or_else(|| {
            use num_bigint::BigInt;
            use num_traits::Num;
            BigInt::from_str_radix(&clean, base)
                .ok()
                .map(|b| Value::BigInt(std::sync::Arc::new(b)))
        })
}

pub(crate) fn parse_prefixed_generic_radix_literal(s: &str) -> Option<Value> {
    let rest = s.strip_prefix(':')?;
    let (rest, base_clean) = parse_unicode_decimal_digits(rest)?;
    let base: u32 = base_clean.parse().ok()?;
    let rest = rest.strip_prefix('<')?;
    let close_pos = rest.find('>')?;
    if !rest[close_pos + 1..].trim().is_empty() {
        return None;
    }
    parse_radix_number_body(&rest[..close_pos], base)
}

pub(crate) fn coerce_to_numeric(val: Value) -> Value {
    match val {
        Value::Mixin(inner, _) => coerce_to_numeric(inner.as_ref().clone()),
        Value::Int(_)
        | Value::BigInt(_)
        | Value::Num(_)
        | Value::Rat(_, _)
        | Value::FatRat(_, _)
        | Value::BigRat(_, _)
        | Value::Complex(_, _) => val,
        Value::Bool(b) => Value::Int(if b { 1 } else { 0 }),
        Value::Enum { value, .. } => Value::Int(value.as_i64()),
        Value::Str(ref s) => {
            let s = s.trim();
            if let Some(v) = parse_prefixed_generic_radix_literal(s) {
                v
            } else if let Some(v) = parse_0_prefixed_radix_literal(s) {
                v
            } else if let Ok(i) = s.parse::<i64>() {
                Value::Int(i)
            } else if let Ok(f) = s.parse::<f64>() {
                Value::Num(f)
            } else {
                Value::Int(0)
            }
        }
        _ if val.as_list_items().is_some() => Value::Int(val.as_list_items().unwrap().len() as i64),
        Value::Hash(items) => Value::Int(items.len() as i64),
        Value::Set(items) => Value::Int(items.len() as i64),
        Value::Bag(items) => Value::Int(items.values().sum()),
        Value::Mix(items) => Value::Num(items.values().sum()),
        Value::LazyList(ll) => {
            if let Some(cached) = ll.cache.lock().unwrap().as_ref() {
                Value::Int(cached.len() as i64)
            } else {
                Value::Int(0)
            }
        }
        Value::Range(..)
        | Value::RangeExcl(..)
        | Value::RangeExclStart(..)
        | Value::RangeExclBoth(..)
        | Value::GenericRange { .. } => Value::Int(value_to_list(&val).len() as i64),
        Value::Nil => Value::Int(0),
        Value::Instance {
            ref class_name,
            ref attributes,
            ..
        } if class_name == "Instant" => attributes.get("value").cloned().unwrap_or(Value::Num(0.0)),
        Value::Instance {
            ref class_name,
            ref attributes,
            ..
        } if class_name == "Duration" => {
            attributes.get("value").cloned().unwrap_or(Value::Num(0.0))
        }
        Value::Instance {
            ref class_name,
            ref attributes,
            ..
        } if class_name == "Date" => {
            let (y, m, d) = crate::builtins::methods_0arg::temporal::date_attrs(attributes);
            let epoch = crate::builtins::methods_0arg::temporal::civil_to_epoch_days(y, m, d);
            Value::Int(epoch * 86400)
        }
        Value::Instance {
            ref class_name,
            ref attributes,
            ..
        } if class_name == "DateTime" => {
            let (y, mo, d, h, mi, s, tz) =
                crate::builtins::methods_0arg::temporal::datetime_attrs(attributes);
            Value::Num(crate::builtins::methods_0arg::temporal::datetime_to_posix(
                y, mo, d, h, mi, s, tz,
            ))
        }
        _ => Value::Int(0),
    }
}

pub(crate) fn coerce_to_set(val: &Value) -> HashSet<String> {
    fn insert_set_elem(elems: &mut HashSet<String>, value: &Value) {
        let pair_selected = |weight: &Value| weight.truthy() || matches!(weight, Value::Nil);
        match value {
            Value::Set(items) => {
                elems.extend(items.iter().cloned());
            }
            Value::Bag(items) => {
                for (k, v) in items.iter() {
                    if *v > 0 {
                        elems.insert(k.clone());
                    }
                }
            }
            Value::Mix(items) => {
                for (k, v) in items.iter() {
                    if *v != 0.0 {
                        elems.insert(k.clone());
                    }
                }
            }
            Value::Hash(items) => {
                for (k, v) in items.iter() {
                    if v.truthy() || matches!(v, Value::Nil) {
                        elems.insert(k.clone());
                    }
                }
            }
            _ if value.as_list_items().is_some() => {
                for item in value.as_list_items().unwrap().iter() {
                    insert_set_elem(elems, item);
                }
            }
            range if range.is_range() => {
                for item in value_to_list(range) {
                    insert_set_elem(elems, &item);
                }
            }
            Value::Pair(key, weight) => {
                if pair_selected(weight) {
                    elems.insert(key.clone());
                }
            }
            Value::ValuePair(key, weight) => {
                if pair_selected(weight) {
                    elems.insert(key.to_string_value());
                }
            }
            other => {
                let sv = other.to_string_value();
                if !sv.is_empty() {
                    elems.insert(sv);
                }
            }
        }
    }

    match val {
        Value::Scalar(inner) => coerce_to_set(inner),
        Value::Set(s) => (**s).clone(),
        Value::Bag(b) => {
            let resolved = resolve_bag_tab_keys(b);
            resolved.keys().cloned().collect()
        }
        Value::Mix(m) => m.keys().cloned().collect(),
        Value::Hash(items) => items
            .iter()
            .filter_map(|(k, v)| {
                if v.truthy() || matches!(v, Value::Nil) {
                    Some(k.clone())
                } else {
                    None
                }
            })
            .collect(),
        _ if val.as_list_items().is_some() => {
            let mut elems = HashSet::new();
            for item in val.as_list_items().unwrap().iter() {
                insert_set_elem(&mut elems, item);
            }
            elems
        }
        Value::Pair(_, _) | Value::ValuePair(_, _) => {
            let mut elems = HashSet::new();
            insert_set_elem(&mut elems, val);
            elems
        }
        range if range.is_range() => {
            let mut elems = HashSet::new();
            for item in value_to_list(range) {
                insert_set_elem(&mut elems, &item);
            }
            elems
        }
        _ => {
            let mut s = HashSet::new();
            let sv = val.to_string_value();
            if !sv.is_empty() {
                s.insert(sv);
            }
            s
        }
    }
}

/// Coerce a value to a QuantHash (Set/Bag/Mix) for use as a single operand to set operators.
/// - Set/Bag/Mix pass through as-is
/// - Hash: include keys with truthy values as Set elements
/// - List/Array: convert items to Set (excluding Pairs with falsy value)
/// - Pair with falsy value → empty Set
/// - Other scalars → Set with one element
pub(crate) fn coerce_value_to_quanthash(val: &Value) -> Value {
    match val {
        Value::Scalar(inner) => coerce_value_to_quanthash(inner),
        Value::Set(_) | Value::Bag(_) | Value::Mix(_) => val.clone(),
        Value::Hash(h) => {
            let mut set = HashSet::new();
            for (k, v) in h.iter() {
                if v.truthy() {
                    set.insert(k.clone());
                }
            }
            Value::set(set)
        }
        _ if val.as_list_items().is_some() => {
            let mut set = HashSet::new();
            for item in val.as_list_items().unwrap().iter() {
                match item {
                    Value::Pair(k, v) => {
                        if v.truthy() {
                            set.insert(k.clone());
                        }
                    }
                    other => {
                        set.insert(other.to_string_value());
                    }
                }
            }
            Value::set(set)
        }
        Value::Pair(k, v) => {
            let mut set = HashSet::new();
            if v.truthy() {
                set.insert(k.clone());
            }
            Value::set(set)
        }
        _ => {
            let mut set = HashSet::new();
            let sv = val.to_string_value();
            if !sv.is_empty() {
                set.insert(sv);
            }
            Value::set(set)
        }
    }
}

/// Determine the promotion level for set operations: 0=Set, 1=Bag, 2=Mix
fn set_type_level(v: &Value) -> u8 {
    match v {
        Value::Mix(_) => 2,
        Value::Bag(_) => 1,
        _ => 0,
    }
}

/// Convert a value to a Mix-level HashMap (key → f64 count)
fn to_mix_map(v: &Value) -> HashMap<String, f64> {
    match v {
        Value::Mix(m) => (**m).clone(),
        Value::Bag(b) => {
            let resolved = resolve_bag_tab_keys(b);
            resolved.into_iter().map(|(k, v)| (k, v as f64)).collect()
        }
        Value::Set(s) => s.iter().map(|k| (k.clone(), 1.0)).collect(),
        Value::Hash(h) => {
            let mut result = HashMap::new();
            for (k, v) in h.iter() {
                let w = match v {
                    Value::Int(i) => *i as f64,
                    Value::Num(n) => *n,
                    Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                    Value::Bool(b) => {
                        if *b {
                            1.0
                        } else {
                            0.0
                        }
                    }
                    _ => {
                        if v.truthy() {
                            1.0
                        } else {
                            0.0
                        }
                    }
                };
                if w != 0.0 {
                    result.insert(k.clone(), w);
                }
            }
            result
        }
        _ => {
            // Count occurrences for list-like values (e.g. (a, a, b) → {a: 2.0, b: 1.0})
            let items = value_to_list(v);
            let mut result = HashMap::new();
            for item in &items {
                *result.entry(item.to_string_value()).or_insert(0.0f64) += 1.0;
            }
            result
        }
    }
}

/// Resolve Bag entries that use the internal "key\tweight" tab format
/// into plain key→weight entries.
pub(crate) fn resolve_bag_tab_keys(bag: &HashMap<String, i64>) -> HashMap<String, i64> {
    let mut result = HashMap::new();
    for (k, c) in bag.iter() {
        if let Some((base, raw_weight)) = k.split_once('\t') {
            let weight = match raw_weight {
                "True" => 1i64,
                "False" => 0,
                _ => raw_weight.parse::<i64>().unwrap_or(1),
            };
            *result.entry(base.to_string()).or_insert(0) += weight * c;
        } else {
            *result.entry(k.clone()).or_insert(0) += c;
        }
    }
    // Remove zero/negative entries for Bag semantics
    result.retain(|_, v| *v > 0);
    result
}

/// Convert a value to a Bag-level HashMap (key → i64 count)
fn to_bag_map(v: &Value) -> HashMap<String, i64> {
    match v {
        Value::Bag(b) => resolve_bag_tab_keys(b),
        Value::Set(s) => s.iter().map(|k| (k.clone(), 1i64)).collect(),
        Value::Hash(h) => {
            let mut result = HashMap::new();
            for (k, v) in h.iter() {
                let count = match v {
                    Value::Int(i) => *i,
                    Value::Num(n) => *n as i64,
                    Value::Bool(b) => i64::from(*b),
                    _ => i64::from(v.truthy()),
                };
                if count > 0 {
                    result.insert(k.clone(), count);
                }
            }
            result
        }
        _ => {
            // Count occurrences for list-like values (e.g. (a, a, b) → {a: 2, b: 1})
            let items = value_to_list(v);
            let mut result = HashMap::new();
            for item in &items {
                *result.entry(item.to_string_value()).or_insert(0i64) += 1;
            }
            result
        }
    }
}

/// Standalone set difference: left (-) right
/// Implements Raku type promotion: Mix > Bag > Set
///
/// In Raku, when the RHS is a Hash, it is treated as a Set (truthy keys
/// with weight 1) rather than using its numeric values as weights.
pub(crate) fn set_diff_values(left: &Value, right: &Value) -> Value {
    // When the RHS is a Hash, coerce it to a Set for subtraction.
    // The Hash's truthy keys each count as weight 1.
    let right_as_set;
    let effective_right = if matches!(right, Value::Hash(_)) {
        right_as_set = Value::set(coerce_to_set(right));
        &right_as_set
    } else {
        right
    };

    let level = set_type_level(left).max(set_type_level(effective_right));
    match level {
        2 => {
            // Mix-level difference: include all keys, keep non-zero results
            let a = to_mix_map(left);
            let b = to_mix_map(effective_right);
            let mut result = HashMap::new();
            for (k, v) in &a {
                let bv = b.get(k).copied().unwrap_or(0.0);
                let diff = v - bv;
                if diff != 0.0 {
                    result.insert(k.clone(), diff);
                }
            }
            for (k, v) in &b {
                if !a.contains_key(k) && *v != 0.0 {
                    result.insert(k.clone(), -*v);
                }
            }
            Value::mix(result)
        }
        1 => {
            // Bag-level difference: only positive results
            let a = to_bag_map(left);
            let b = to_bag_map(effective_right);
            let mut result = HashMap::new();
            for (k, v) in &a {
                let bv = b.get(k).copied().unwrap_or(0);
                if *v > bv {
                    result.insert(k.clone(), *v - bv);
                }
            }
            Value::bag(result)
        }
        _ => {
            // Set-level difference
            let a = coerce_to_set(left);
            let b = coerce_to_set(effective_right);
            Value::set(a.difference(&b).cloned().collect())
        }
    }
}

/// Standalone set intersection: left (&) right
pub(crate) fn set_intersect_values(left: &Value, right: &Value) -> Value {
    // Determine result type level: 0=Set, 1=Bag, 2=Mix
    let type_level = |v: &Value| -> u8 {
        match v {
            Value::Mix(_) => 2,
            Value::Bag(_) => 1,
            _ => 0,
        }
    };
    let result_level = type_level(left).max(type_level(right));
    match result_level {
        2 => {
            let a = coerce_to_mix(left);
            let b = coerce_to_mix(right);
            let mut result = HashMap::new();
            for (k, v) in a.iter() {
                if let Some(bv) = b.get(k) {
                    result.insert(k.clone(), v.min(*bv));
                }
            }
            Value::mix(result)
        }
        1 => {
            let a = coerce_to_bag(left);
            let b = coerce_to_bag(right);
            let mut result = HashMap::new();
            for (k, v) in a.iter() {
                if let Some(bv) = b.get(k) {
                    result.insert(k.clone(), (*v).min(*bv));
                }
            }
            Value::bag(result)
        }
        _ => {
            let a = coerce_to_set(left);
            let b = coerce_to_set(right);
            Value::set(a.intersection(&b).cloned().collect())
        }
    }
}

/// Coerce a value to a Bag (HashMap<String, i64>)
fn coerce_to_bag(val: &Value) -> HashMap<String, i64> {
    match val {
        Value::Bag(b) => resolve_bag_tab_keys(b),
        Value::Set(s) => s.iter().map(|k| (k.clone(), 1)).collect(),
        Value::Mix(m) => m.iter().map(|(k, v)| (k.clone(), *v as i64)).collect(),
        _ => {
            // Count occurrences for list-like values
            let items = value_to_list(val);
            let mut result = HashMap::new();
            for item in &items {
                *result.entry(item.to_string_value()).or_insert(0i64) += 1;
            }
            result
        }
    }
}

/// Coerce a value to a Mix (HashMap<String, f64>)
fn coerce_to_mix(val: &Value) -> HashMap<String, f64> {
    match val {
        Value::Mix(m) => (**m).clone(),
        Value::Bag(b) => {
            let resolved = resolve_bag_tab_keys(b);
            resolved.into_iter().map(|(k, v)| (k, v as f64)).collect()
        }
        Value::Set(s) => s.iter().map(|k| (k.clone(), 1.0)).collect(),
        _ => {
            // Count occurrences for list-like values
            let items = value_to_list(val);
            let mut result = HashMap::new();
            for item in &items {
                *result.entry(item.to_string_value()).or_insert(0.0f64) += 1.0;
            }
            result
        }
    }
}

/// Standalone set symmetric difference: left (^) right
pub(crate) fn set_sym_diff_values(left: &Value, right: &Value) -> Value {
    match (left, right) {
        (Value::Set(a), Value::Set(b)) => Value::set(a.symmetric_difference(b).cloned().collect()),
        _ => {
            let a = coerce_to_set(left);
            let b = coerce_to_set(right);
            Value::set(a.symmetric_difference(&b).cloned().collect())
        }
    }
}

pub(crate) fn coerce_numeric(left: Value, right: Value) -> (Value, Value) {
    // Unwrap allomorphic types (Mixin) to their inner numeric value
    let left = unwrap_mixin(left);
    let right = unwrap_mixin(right);
    let l = match &left {
        Value::Int(_)
        | Value::BigInt(_)
        | Value::Num(_)
        | Value::Rat(_, _)
        | Value::FatRat(_, _)
        | Value::BigRat(_, _)
        | Value::Complex(_, _) => left,
        _ => coerce_to_numeric(left),
    };
    let r = match &right {
        Value::Int(_)
        | Value::BigInt(_)
        | Value::Num(_)
        | Value::Rat(_, _)
        | Value::FatRat(_, _)
        | Value::BigRat(_, _)
        | Value::Complex(_, _) => right,
        _ => coerce_to_numeric(right),
    };
    (l, r)
}

/// Unwrap a Mixin (allomorphic type) to its inner value.
pub(crate) fn unwrap_mixin(val: Value) -> Value {
    match val {
        Value::Mixin(inner, _) => inner.as_ref().clone(),
        other => other,
    }
}

pub(crate) fn to_rat_parts(val: &Value) -> Option<(i64, i64)> {
    match val {
        Value::Mixin(inner, _) => to_rat_parts(inner),
        Value::Int(i) => Some((*i, 1)),
        Value::Rat(n, d) => Some((*n, *d)),
        Value::FatRat(n, d) => Some((*n, *d)),
        _ => None,
    }
}

pub(crate) fn to_big_rat_parts(val: &Value) -> Option<(BigInt, BigInt)> {
    match val {
        Value::Mixin(inner, _) => to_big_rat_parts(inner),
        Value::Int(i) => Some((BigInt::from(*i), BigInt::from(1))),
        Value::BigInt(i) => Some(((**i).clone(), BigInt::from(1))),
        Value::Rat(n, d) | Value::FatRat(n, d) => Some((BigInt::from(*n), BigInt::from(*d))),
        Value::BigRat(n, d) => Some((n.clone(), d.clone())),
        _ => None,
    }
}

fn big_rat_parts_to_f64(num: &BigInt, den: &BigInt) -> f64 {
    if den.is_zero() {
        if num.is_zero() {
            f64::NAN
        } else if num.is_positive() {
            f64::INFINITY
        } else {
            f64::NEG_INFINITY
        }
    } else {
        num.to_f64().unwrap_or(0.0) / den.to_f64().unwrap_or(1.0)
    }
}

pub(crate) fn compare_big_rat_parts(
    a: (BigInt, BigInt),
    b: (BigInt, BigInt),
) -> Option<std::cmp::Ordering> {
    let (an, ad) = a;
    let (bn, bd) = b;
    if ad.is_zero() || bd.is_zero() {
        return big_rat_parts_to_f64(&an, &ad).partial_cmp(&big_rat_parts_to_f64(&bn, &bd));
    }
    Some((an * &bd).cmp(&(bn * &ad)))
}

pub(crate) fn big_rat_parts_equal(a: (BigInt, BigInt), b: (BigInt, BigInt)) -> bool {
    let (an, ad) = a;
    let (bn, bd) = b;
    if ad.is_zero() || bd.is_zero() {
        if (ad.is_zero() && an.is_zero()) || (bd.is_zero() && bn.is_zero()) {
            return false;
        }
        return big_rat_parts_to_f64(&an, &ad) == big_rat_parts_to_f64(&bn, &bd);
    }
    let ga = an.gcd(&ad);
    let gb = bn.gcd(&bd);
    let mut an = an / &ga;
    let mut ad = ad / ga;
    let mut bn = bn / &gb;
    let mut bd = bd / gb;
    if ad.is_negative() {
        an = -an;
        ad = -ad;
    }
    if bd.is_negative() {
        bn = -bn;
        bd = -bd;
    }
    an == bn && ad == bd
}

fn rat_parts_to_f64(num: i64, den: i64) -> f64 {
    if den != 0 {
        num as f64 / den as f64
    } else if num > 0 {
        f64::INFINITY
    } else if num < 0 {
        f64::NEG_INFINITY
    } else {
        f64::NAN
    }
}

pub(crate) fn compare_rat_parts(a: (i64, i64), b: (i64, i64)) -> std::cmp::Ordering {
    let (an, ad) = a;
    let (bn, bd) = b;
    if ad == 0 || bd == 0 {
        return rat_parts_to_f64(an, ad)
            .partial_cmp(&rat_parts_to_f64(bn, bd))
            .unwrap_or(std::cmp::Ordering::Equal);
    }
    let lhs = an as i128 * bd as i128;
    let rhs = bn as i128 * ad as i128;
    lhs.cmp(&rhs)
}

pub(crate) fn to_float_value(val: &Value) -> Option<f64> {
    match val {
        Value::Mixin(inner, _) => to_float_value(inner),
        Value::Num(f) => Some(*f),
        Value::Int(i) => Some(*i as f64),
        Value::BigInt(n) => n.to_f64(),
        Value::Rat(n, d) => {
            if *d != 0 {
                Some(*n as f64 / *d as f64)
            } else if *n > 0 {
                Some(f64::INFINITY)
            } else if *n < 0 {
                Some(f64::NEG_INFINITY)
            } else {
                Some(f64::NAN)
            }
        }
        Value::FatRat(n, d) => {
            if *d != 0 {
                Some(*n as f64 / *d as f64)
            } else if *n > 0 {
                Some(f64::INFINITY)
            } else if *n < 0 {
                Some(f64::NEG_INFINITY)
            } else {
                Some(f64::NAN)
            }
        }
        Value::BigRat(n, d) => {
            if !d.is_zero() {
                if let (Some(nn), Some(dd)) = (n.to_f64(), d.to_f64())
                    && nn.is_finite()
                    && dd.is_finite()
                {
                    Some(nn / dd)
                } else {
                    let scale_pow = 30u32;
                    let scale = num_bigint::BigInt::from(10u8).pow(scale_pow);
                    let scaled = (n * &scale) / d;
                    if let Some(scaled_f) = scaled
                        .to_f64()
                        .or_else(|| scaled.to_string().parse::<f64>().ok())
                    {
                        Some(scaled_f / 10f64.powi(scale_pow as i32))
                    } else if n.is_zero() {
                        Some(0.0)
                    } else if n.is_positive() {
                        Some(f64::INFINITY)
                    } else {
                        Some(f64::NEG_INFINITY)
                    }
                }
            } else if n.is_positive() {
                Some(f64::INFINITY)
            } else if n.is_negative() {
                Some(f64::NEG_INFINITY)
            } else {
                Some(f64::NAN)
            }
        }
        Value::Complex(r, i) => {
            if *i == 0.0 {
                Some(*r)
            } else {
                None
            }
        }
        Value::Enum { value, .. } => Some(value.as_i64() as f64),
        Value::Bool(b) => Some(if *b { 1.0 } else { 0.0 }),
        Value::Str(s) => s.trim().parse::<f64>().ok(),
        Value::Nil => Some(0.0),
        _ if val.as_list_items().is_some() => Some(val.as_list_items().unwrap().len() as f64),
        Value::LazyList(ll) => {
            if let Some(cached) = ll.cache.lock().unwrap().as_ref() {
                Some(cached.len() as f64)
            } else {
                Some(0.0)
            }
        }
        Value::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Instant" => attributes.get("value").and_then(to_float_value),
        Value::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Match" => attributes.get("str").and_then(to_float_value),
        Value::Instance { attributes, .. } => {
            attributes.get("__mutsu_int_value").and_then(to_float_value)
        }
        Value::Hash(map) => Some(map.len() as f64),
        _ => None,
    }
}

pub(crate) fn to_complex_parts(val: &Value) -> Option<(f64, f64)> {
    match val {
        Value::Complex(r, i) => Some((*r, *i)),
        _ => to_float_value(val).map(|v| (v, 0.0)),
    }
}

pub(crate) fn compare_values(a: &Value, b: &Value) -> i32 {
    fn compare_infinite_num_against_nonnumeric_str(num: f64, s: &str) -> Option<i32> {
        if !num.is_infinite() || s.trim().parse::<f64>().is_ok() {
            return None;
        }
        Some(if num.is_sign_positive() {
            std::cmp::Ordering::Greater
        } else {
            std::cmp::Ordering::Less
        } as i32)
    }

    match (a, b) {
        (Value::Version { parts: ap, .. }, Value::Version { parts: bp, .. }) => {
            version_cmp_parts(ap, bp) as i32
        }
        (Value::Int(a), Value::Int(b)) => a.cmp(b) as i32,
        (Value::BigInt(a), Value::BigInt(b)) => a.as_ref().cmp(b.as_ref()) as i32,
        (Value::BigInt(a), Value::Int(b)) => a.as_ref().cmp(&num_bigint::BigInt::from(*b)) as i32,
        (Value::Int(a), Value::BigInt(b)) => num_bigint::BigInt::from(*a).cmp(b.as_ref()) as i32,
        (Value::Num(a), Value::Num(b)) => {
            // NaN sorts after everything (including Inf)
            match (a.is_nan(), b.is_nan()) {
                (true, true) => 0,
                (true, false) => 1,
                (false, true) => -1,
                _ => a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal) as i32,
            }
        }
        (Value::BigInt(a), Value::Num(b)) => {
            a.as_ref()
                .to_f64()
                .unwrap_or(if a.as_ref().is_positive() {
                    f64::INFINITY
                } else {
                    f64::NEG_INFINITY
                })
                .partial_cmp(b)
                .unwrap_or(std::cmp::Ordering::Equal) as i32
        }
        (Value::Num(a), Value::BigInt(b)) => {
            a.partial_cmp(&b.as_ref().to_f64().unwrap_or(if b.as_ref().is_positive() {
                f64::INFINITY
            } else {
                f64::NEG_INFINITY
            }))
            .unwrap_or(std::cmp::Ordering::Equal) as i32
        }
        (Value::Int(a), Value::Num(b)) => (*a as f64)
            .partial_cmp(b)
            .unwrap_or(std::cmp::Ordering::Equal) as i32,
        (Value::Num(a), Value::Int(b)) => a
            .partial_cmp(&(*b as f64))
            .unwrap_or(std::cmp::Ordering::Equal) as i32,
        (Value::Num(a), Value::Rat(n, d)) => {
            let rat_f = if *d != 0 {
                *n as f64 / *d as f64
            } else {
                f64::NAN
            };
            a.partial_cmp(&rat_f).unwrap_or(std::cmp::Ordering::Equal) as i32
        }
        (Value::Rat(n, d), Value::Num(b)) => {
            let rat_f = if *d != 0 {
                *n as f64 / *d as f64
            } else {
                f64::NAN
            };
            rat_f.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal) as i32
        }
        (Value::Num(n), Value::Str(s)) => {
            if let Some(ord) = compare_infinite_num_against_nonnumeric_str(*n, s) {
                ord
            } else {
                a.to_string_value().cmp(&b.to_string_value()) as i32
            }
        }
        (Value::Str(s), Value::Num(n)) => {
            if let Some(ord) = compare_infinite_num_against_nonnumeric_str(*n, s) {
                -ord
            } else {
                a.to_string_value().cmp(&b.to_string_value()) as i32
            }
        }
        // Pair/ValuePair comparison: compare by key first, then by value
        (Value::Pair(ak, av), Value::Pair(bk, bv)) => {
            let key_cmp = compare_values(&Value::str(ak.clone()), &Value::str(bk.clone()));
            if key_cmp != 0 {
                key_cmp
            } else {
                compare_values(av, bv)
            }
        }
        (Value::ValuePair(ak, av), Value::ValuePair(bk, bv)) => {
            let key_cmp = compare_values(ak, bk);
            if key_cmp != 0 {
                key_cmp
            } else {
                compare_values(av, bv)
            }
        }
        (Value::Pair(ak, av), Value::ValuePair(bk, bv)) => {
            let key_cmp = compare_values(&Value::str(ak.clone()), bk);
            if key_cmp != 0 {
                key_cmp
            } else {
                compare_values(av, bv)
            }
        }
        (Value::ValuePair(ak, av), Value::Pair(bk, bv)) => {
            let key_cmp = compare_values(ak, &Value::str(bk.clone()));
            if key_cmp != 0 {
                key_cmp
            } else {
                compare_values(av, bv)
            }
        }
        _ => {
            if let (Some((an, ad)), Some((bn, bd))) = (to_rat_parts(a), to_rat_parts(b)) {
                let cmp = compare_rat_parts((an, ad), (bn, bd)) as i32;
                if cmp != 0 {
                    return cmp;
                }
                // For allomorphic types (IntStr, RatStr, etc.), break numeric ties
                // with string comparison
                if matches!(a, Value::Mixin(..)) || matches!(b, Value::Mixin(..)) {
                    return a.to_string_value().cmp(&b.to_string_value()) as i32;
                }
                return cmp;
            }
            a.to_string_value().cmp(&b.to_string_value()) as i32
        }
    }
}

pub(crate) fn to_int(v: &Value) -> i64 {
    match v {
        Value::Mixin(inner, _) => to_int(inner),
        Value::Int(i) => *i,
        Value::BigInt(n) => {
            use num_traits::ToPrimitive;
            n.as_ref()
                .to_i64()
                .unwrap_or(if **n > num_bigint::BigInt::from(0i64) {
                    i64::MAX
                } else {
                    i64::MIN
                })
        }
        Value::Num(f) => *f as i64,
        Value::Range(a, b) => {
            if b >= a {
                b - a + 1
            } else {
                0
            }
        }
        Value::RangeExcl(a, b) | Value::RangeExclStart(a, b) => {
            if b > a {
                b - a
            } else {
                0
            }
        }
        Value::RangeExclBoth(a, b) => {
            if *b > *a + 1 {
                b - a - 1
            } else {
                0
            }
        }
        Value::Rat(n, d) => {
            if *d != 0 {
                n / d
            } else {
                0
            }
        }
        Value::Complex(r, _) => *r as i64,
        Value::Str(s) => s.parse().unwrap_or(0),
        Value::Array(items, ..) => items.len() as i64,
        Value::Hash(items) => items.len() as i64,
        Value::Seq(items) => items.len() as i64,
        Value::Slip(items) => items.len() as i64,
        Value::Instance { attributes, .. } => attributes.get("__mutsu_int_value").map_or(0, to_int),
        _ => 0,
    }
}

pub(crate) fn merge_junction(kind: JunctionKind, left: Value, right: Value) -> Value {
    let left_is_other_junction = matches!(&left, Value::Junction { kind: k, .. } if *k != kind);
    let right_is_other_junction = matches!(&right, Value::Junction { kind: k, .. } if *k != kind);
    let mut values = Vec::new();

    match left {
        Value::Junction {
            kind: inner_kind,
            values: inner_values,
        } if inner_kind == kind && !right_is_other_junction => {
            values.extend(inner_values.iter().cloned());
        }
        other => values.push(other),
    }
    match right {
        Value::Junction {
            kind: inner_kind,
            values: inner_values,
        } if inner_kind == kind && !left_is_other_junction => {
            values.extend(inner_values.iter().cloned());
        }
        other => values.push(other),
    }

    Value::junction(kind, values)
}

/// Format a short representation of a value for type-check error messages,
/// matching Raku's format: e.g. `("hello")`, `(42)`.
pub(crate) fn value_short_repr(val: &Value) -> String {
    match val {
        Value::Str(s) => format!("(\"{}\")", s),
        Value::Int(n) => format!("({})", n),
        Value::BigInt(n) => format!("({})", n),
        Value::Num(n) => format!("({})", n),
        Value::Bool(b) => format!("({})", if *b { "True" } else { "False" }),
        Value::Rat(n, d) => format!("({}/{})", n, d),
        Value::BigRat(n, d) => format!("({}/{})", n, d),
        Value::FatRat(n, d) => format!("(FatRat.new({}, {}))", n, d),
        Value::Nil => "(Nil)".to_string(),
        _ => String::new(),
    }
}

/// Format the variable name for error messages, adding `$` sigil for
/// scalar variables that don't already have a sigil prefix.
pub(crate) fn format_var_name_for_error(name: &str) -> String {
    if name.starts_with('$')
        || name.starts_with('@')
        || name.starts_with('%')
        || name.starts_with('&')
    {
        name.to_string()
    } else {
        format!("${}", name)
    }
}

/// Build the standard X::TypeCheck::Assignment error message, matching Raku's format:
/// `Type check failed in assignment to $x; expected Int but got Str ("hello")`
pub(crate) fn type_check_assignment_error(var_name: &str, expected: &str, val: &Value) -> String {
    let display_name = format_var_name_for_error(var_name);
    let got_type = value_type_name(val);
    let repr = value_short_repr(val);
    if repr.is_empty() {
        format!(
            "X::TypeCheck::Assignment: Type check failed in assignment to {}; expected {} but got {}",
            display_name, expected, got_type
        )
    } else {
        format!(
            "X::TypeCheck::Assignment: Type check failed in assignment to {}; expected {} but got {} {}",
            display_name, expected, got_type, repr
        )
    }
}

/// Build the standard X::TypeCheck::Assignment error for array/hash elements:
/// `Type check failed for an element of @a; expected Int but got Str ("hi")`
pub(crate) fn type_check_element_error(var_name: &str, expected: &str, val: &Value) -> String {
    let display_name = format_var_name_for_error(var_name);
    let got_type = value_type_name(val);
    let repr = value_short_repr(val);
    if repr.is_empty() {
        format!(
            "X::TypeCheck::Assignment: Type check failed for an element of {}; expected {} but got {}",
            display_name, expected, got_type
        )
    } else {
        format!(
            "X::TypeCheck::Assignment: Type check failed for an element of {}; expected {} but got {} {}",
            display_name, expected, got_type, repr
        )
    }
}
