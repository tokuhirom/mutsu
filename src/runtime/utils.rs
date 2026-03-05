use crate::symbol::Symbol;
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex, OnceLock};

use crate::value::{ArrayKind, JunctionKind, RuntimeError, Value};
use num_traits::{Signed, ToPrimitive, Zero};

/// Maximum number of elements when expanding an infinite range to a list.
const MAX_RANGE_EXPAND: i64 = 1_000_000;

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
    shaped_array_shape(value).is_some()
}

pub(crate) fn shaped_array_shape(value: &Value) -> Option<Vec<usize>> {
    let Value::Array(items, ..) = value else {
        return None;
    };
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
        (Value::Slip(a), Value::Slip(b)) => std::sync::Arc::ptr_eq(a, b),
        (Value::LazyList(a), Value::LazyList(b)) => std::sync::Arc::ptr_eq(a, b),
        (Value::Hash(a), Value::Hash(b)) => std::sync::Arc::ptr_eq(a, b),
        (Value::Sub(a), Value::Sub(b)) => std::sync::Arc::ptr_eq(a, b),
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
            value: -1,
            index: 0,
        },
        std::cmp::Ordering::Equal => Value::Enum {
            enum_type: Symbol::intern("Order"),
            key: Symbol::intern("Same"),
            value: 0,
            index: 1,
        },
        std::cmp::Ordering::Greater => Value::Enum {
            enum_type: Symbol::intern("Order"),
            key: Symbol::intern("More"),
            value: 1,
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
    match value {
        Value::Array(items, _) => Value::Array(items, ArrayKind::Array),
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
        Value::Rat(n, d) => {
            if *d == 0 {
                if *n == 0 {
                    "NaN".to_string()
                } else if *n > 0 {
                    "Inf".to_string()
                } else {
                    "-Inf".to_string()
                }
            } else if *n % *d == 0 {
                // Exact integer: Rat(10, 2) => "5"
                format!("{}", *n / *d)
            } else {
                let mut dd = *d;
                while dd % 2 == 0 {
                    dd /= 2;
                }
                while dd % 5 == 0 {
                    dd /= 5;
                }
                if dd == 1 {
                    let val = *n as f64 / *d as f64;
                    format!("{}", val)
                } else {
                    // Non-terminating decimals: display as decimal with 6 places
                    let val = *n as f64 / *d as f64;
                    format!("{:.6}", val)
                }
            }
        }
        Value::Array(items, ..) => format!(
            "[{}]",
            items.iter().map(gist_value).collect::<Vec<_>>().join(" ")
        ),
        Value::Hash(items) => items
            .iter()
            .map(|(k, v)| format!("{}\t{}", k, gist_value(v)))
            .collect::<Vec<_>>()
            .join("\n"),
        Value::Pair(k, v) => format!("{} => {}", k, gist_value(v)),
        Value::ValuePair(k, v) => format!("{} => {}", gist_value(k), gist_value(v)),
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
        Value::Sub(data) => {
            if matches!(
                data.env.get("__mutsu_callable_type"),
                Some(Value::Str(kind)) if kind.as_str() == "Method"
            ) {
                "Method"
            } else {
                "Sub"
            }
        }
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
        "//" => Value::Package(Symbol::intern("Any")),
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
        "(.)" | "⊍" => Value::bag(HashMap::new()),
        // Comma/zip: empty list
        "," | "Z" => Value::Array(std::sync::Arc::new(Vec::new()), ArrayKind::List),
        _ => Value::Nil,
    }
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
            } else {
                // Numeric GenericRange: expand using integer steps
                let s_f = start.to_f64();
                let e_f = end.to_f64();
                if s_f.is_infinite() || e_f.is_infinite() || s_f.is_nan() || e_f.is_nan() {
                    vec![val.clone()]
                } else {
                    let s_i = if *excl_start {
                        s_f as i64 + 1
                    } else {
                        s_f as i64
                    };
                    let e_i = if *excl_end {
                        e_f as i64 - 1
                    } else {
                        e_f as i64
                    };
                    let end_capped = e_i.min(s_i + MAX_RANGE_EXPAND);
                    // Preserve type of start endpoint
                    match start.as_ref() {
                        Value::Num(_) => (s_i..=end_capped).map(|i| Value::Num(i as f64)).collect(),
                        Value::Rat(_, d) => (s_i..=end_capped)
                            .map(|i| crate::value::make_rat(i * d, *d))
                            .collect(),
                        _ => (s_i..=end_capped).map(Value::Int).collect(),
                    }
                }
            }
        }
        Value::Set(items) => items.iter().map(|s| Value::str(s.clone())).collect(),
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

pub(crate) fn coerce_to_numeric(val: Value) -> Value {
    match val {
        Value::Mixin(inner, _) => coerce_to_numeric(inner.as_ref().clone()),
        Value::Int(_)
        | Value::Num(_)
        | Value::Rat(_, _)
        | Value::FatRat(_, _)
        | Value::Complex(_, _) => val,
        Value::Bool(b) => Value::Int(if b { 1 } else { 0 }),
        Value::Enum { value, .. } => Value::Int(value),
        Value::Str(ref s) => {
            let s = s.trim();
            if let Ok(i) = s.parse::<i64>() {
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
        Value::Set(s) => (**s).clone(),
        Value::Bag(b) => b.keys().cloned().collect(),
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
        Value::Bag(b) => b.iter().map(|(k, v)| (k.clone(), *v as f64)).collect(),
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
            let s = coerce_to_set(v);
            s.into_iter().map(|k| (k, 1.0)).collect()
        }
    }
}

/// Convert a value to a Bag-level HashMap (key → i64 count)
fn to_bag_map(v: &Value) -> HashMap<String, i64> {
    match v {
        Value::Bag(b) => (**b).clone(),
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
            let s = coerce_to_set(v);
            s.into_iter().map(|k| (k, 1i64)).collect()
        }
    }
}

/// Standalone set difference: left (-) right
/// Implements Raku type promotion: Mix > Bag > Set
pub(crate) fn set_diff_values(left: &Value, right: &Value) -> Value {
    let level = set_type_level(left).max(set_type_level(right));
    match level {
        2 => {
            // Mix-level difference: include all keys, keep non-zero results
            let a = to_mix_map(left);
            let b = to_mix_map(right);
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
            let b = to_bag_map(right);
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
            let b = coerce_to_set(right);
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
        Value::Bag(b) => (**b).clone(),
        Value::Set(s) => s.iter().map(|k| (k.clone(), 1)).collect(),
        Value::Mix(m) => m.iter().map(|(k, v)| (k.clone(), *v as i64)).collect(),
        _ => {
            let set = coerce_to_set(val);
            set.into_iter().map(|k| (k, 1)).collect()
        }
    }
}

/// Coerce a value to a Mix (HashMap<String, f64>)
fn coerce_to_mix(val: &Value) -> HashMap<String, f64> {
    match val {
        Value::Mix(m) => (**m).clone(),
        Value::Bag(b) => b.iter().map(|(k, v)| (k.clone(), *v as f64)).collect(),
        Value::Set(s) => s.iter().map(|k| (k.clone(), 1.0)).collect(),
        _ => {
            let set = coerce_to_set(val);
            set.into_iter().map(|k| (k, 1.0)).collect()
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
        | Value::Complex(_, _) => left,
        _ => coerce_to_numeric(left),
    };
    let r = match &right {
        Value::Int(_)
        | Value::BigInt(_)
        | Value::Num(_)
        | Value::Rat(_, _)
        | Value::FatRat(_, _)
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
        Value::Int(i) => Some((*i, 1)),
        Value::Rat(n, d) => Some((*n, *d)),
        Value::FatRat(n, d) => Some((*n, *d)),
        _ => None,
    }
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
        Value::Bool(b) => Some(if *b { 1.0 } else { 0.0 }),
        Value::Str(s) => s.parse::<f64>().ok(),
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
        Value::Int(n) => Some((*n as f64, 0.0)),
        Value::Num(f) => Some((*f, 0.0)),
        Value::Rat(n, d) => {
            if *d != 0 {
                Some((*n as f64 / *d as f64, 0.0))
            } else {
                None
            }
        }
        _ => None,
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
            a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal) as i32
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
        _ => {
            if let (Some((an, ad)), Some((bn, bd))) = (to_rat_parts(a), to_rat_parts(b)) {
                return compare_rat_parts((an, ad), (bn, bd)) as i32;
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
    let mut values = Vec::new();
    push_junction_value(&kind, left, &mut values);
    push_junction_value(&kind, right, &mut values);
    Value::junction(kind, values)
}

fn push_junction_value(kind: &JunctionKind, value: Value, out: &mut Vec<Value>) {
    match value {
        Value::Junction {
            kind: inner_kind,
            values,
        } if &inner_kind == kind => out.extend(values.iter().cloned()),
        other => out.push(other),
    }
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
