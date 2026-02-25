use std::collections::{HashMap, HashSet};

use crate::value::{JunctionKind, RuntimeError, Value};
use num_traits::{Signed, ToPrimitive, Zero};

/// Maximum number of elements when expanding an infinite range to a list.
const MAX_RANGE_EXPAND: i64 = 1_000_000;

pub(crate) fn values_identical(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Instance { id: a, .. }, Value::Instance { id: b, .. }) => a == b,
        _ => left == right,
    }
}

pub(crate) fn make_order(ord: std::cmp::Ordering) -> Value {
    match ord {
        std::cmp::Ordering::Less => Value::Enum {
            enum_type: "Order".to_string(),
            key: "Less".to_string(),
            value: -1,
            index: 0,
        },
        std::cmp::Ordering::Equal => Value::Enum {
            enum_type: "Order".to_string(),
            key: "Same".to_string(),
            value: 0,
            index: 1,
        },
        std::cmp::Ordering::Greater => Value::Enum {
            enum_type: "Order".to_string(),
            key: "More".to_string(),
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

pub(crate) fn coerce_to_array(value: Value) -> Value {
    match value {
        Value::Array(..) => value,
        Value::Nil => Value::array(Vec::new()),
        Value::Range(a, b) if b == i64::MAX || a == i64::MIN => value,
        Value::Range(a, b) => Value::array((a..=b).map(Value::Int).collect()),
        Value::RangeExcl(a, b) if b == i64::MAX || a == i64::MIN => value,
        Value::RangeExcl(a, b) => Value::array((a..b).map(Value::Int).collect()),
        Value::RangeExclStart(a, b) if b == i64::MAX || a == i64::MIN => value,
        Value::RangeExclStart(a, b) => Value::array((a + 1..=b).map(Value::Int).collect()),
        Value::RangeExclBoth(a, b) if b == i64::MAX || a == i64::MIN => value,
        Value::RangeExclBoth(a, b) => Value::array((a + 1..b).map(Value::Int).collect()),
        Value::GenericRange {
            ref start, ref end, ..
        } if matches!(start.as_ref(), Value::Str(_)) && matches!(end.as_ref(), Value::Str(_)) => {
            Value::array(value_to_list(&value))
        }
        Value::GenericRange { .. } => value,
        Value::Slip(items) | Value::Seq(items) => Value::Array(items, false),
        other => Value::array(vec![other]),
    }
}

pub(crate) fn coerce_to_str(value: &Value) -> String {
    match value {
        // Type objects stringify as empty string in Str context.
        Value::Package(_) => String::new(),
        _ => value.to_string_value(),
    }
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
                    format!("<{}/{}>", n, d)
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
        _ => value.to_string_value(),
    }
}

pub(crate) fn is_known_type_constraint(constraint: &str) -> bool {
    matches!(
        constraint,
        "Int"
            | "Num"
            | "Str"
            | "Bool"
            | "Array"
            | "Hash"
            | "Rat"
            | "FatRat"
            | "Complex"
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
        Value::Array(_, true) => "Array",
        Value::Array(_, false) => "List",
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
        Value::Sub(_) | Value::WeakSub(_) => "Sub",
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
        Value::HyperWhatever => "HyperWhatever",
        Value::Capture { .. } => "Capture",
        Value::Uni { form, .. } => match form.as_str() {
            "NFC" => "NFC",
            "NFD" => "NFD",
            "NFKC" => "NFKC",
            "NFKD" => "NFKD",
            _ => "Uni",
        },
        Value::Mixin(inner, _) => value_type_name(inner),
        Value::Proxy { .. } => "Proxy",
        Value::ParametricRole { .. } => "Package",
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
        "~" | "~|" | "~^" => Value::Str(String::new()),
        "&&" | "and" | "?&" => Value::Bool(true),
        "||" | "or" | "?|" | "^^" => Value::Bool(false),
        "?^" => Value::Bool(false),
        "//" => Value::Package("Any".to_string()),
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
        // Comma/zip: empty list
        "," | "Z" => Value::Array(std::sync::Arc::new(Vec::new()), false),
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
                            .map(|c| Value::Str(c.to_string()))
                            .collect()
                    } else {
                        (s..=e)
                            .filter_map(char::from_u32)
                            .map(|c| Value::Str(c.to_string()))
                            .collect()
                    }
                } else {
                    // Multi-char string ranges: use string succession
                    let mut result = Vec::new();
                    let mut current = if *excl_start {
                        crate::runtime::Interpreter::string_succ(a)
                    } else {
                        a.clone()
                    };
                    let limit = MAX_RANGE_EXPAND as usize;
                    while current <= *b && result.len() < limit {
                        if *excl_end && current == *b {
                            break;
                        }
                        result.push(Value::Str(current.clone()));
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
        Value::Set(items) => items.iter().map(|s| Value::Str(s.clone())).collect(),
        Value::Bag(items) => items
            .iter()
            .map(|(k, v)| Value::Pair(k.clone(), Box::new(Value::Int(*v))))
            .collect(),
        Value::Mix(items) => items
            .iter()
            .map(|(k, v)| Value::Pair(k.clone(), Box::new(Value::Num(*v))))
            .collect(),
        Value::Slip(items) => items.to_vec(),
        Value::Nil => vec![],
        other => vec![other.clone()],
    }
}

pub(crate) use super::sprintf::format_sprintf;

pub(crate) fn coerce_to_numeric(val: Value) -> Value {
    match val {
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
        Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items) => {
            Value::Int(items.len() as i64)
        }
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
        _ => Value::Int(0),
    }
}

pub(crate) fn coerce_to_set(val: &Value) -> HashSet<String> {
    match val {
        Value::Set(s) => (**s).clone(),
        Value::Bag(b) => b.keys().cloned().collect(),
        Value::Mix(m) => m.keys().cloned().collect(),
        Value::Array(items, ..) => items.iter().map(|v| v.to_string_value()).collect(),
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

pub(crate) fn coerce_numeric(left: Value, right: Value) -> (Value, Value) {
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
                Some(n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0))
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
        Value::Array(items, ..) => Some(items.len() as f64),
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
    match (a, b) {
        (Value::Version { parts: ap, .. }, Value::Version { parts: bp, .. }) => {
            version_cmp_parts(ap, bp) as i32
        }
        (Value::Int(a), Value::Int(b)) => a.cmp(b) as i32,
        (Value::BigInt(a), Value::BigInt(b)) => a.cmp(b) as i32,
        (Value::BigInt(a), Value::Int(b)) => a.cmp(&num_bigint::BigInt::from(*b)) as i32,
        (Value::Int(a), Value::BigInt(b)) => num_bigint::BigInt::from(*a).cmp(b) as i32,
        (Value::Num(a), Value::Num(b)) => {
            a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal) as i32
        }
        (Value::BigInt(a), Value::Num(b)) => {
            a.to_f64()
                .unwrap_or(if a.is_positive() {
                    f64::INFINITY
                } else {
                    f64::NEG_INFINITY
                })
                .partial_cmp(b)
                .unwrap_or(std::cmp::Ordering::Equal) as i32
        }
        (Value::Num(a), Value::BigInt(b)) => {
            a.partial_cmp(&b.to_f64().unwrap_or(if b.is_positive() {
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
        Value::Int(i) => *i,
        Value::BigInt(n) => {
            use num_traits::ToPrimitive;
            n.to_i64()
                .unwrap_or(if *n > num_bigint::BigInt::from(0i64) {
                    i64::MAX
                } else {
                    i64::MIN
                })
        }
        Value::Num(f) => *f as i64,
        Value::Rat(n, d) => {
            if *d != 0 {
                n / d
            } else {
                0
            }
        }
        Value::Complex(r, _) => *r as i64,
        Value::Str(s) => s.parse().unwrap_or(0),
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
