use std::collections::{HashMap, HashSet};

use crate::value::{JunctionKind, Value};

/// Maximum number of elements when expanding an infinite range to a list.
const MAX_RANGE_EXPAND: i64 = 1_000_000;

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
        let a = a_parts.get(i).unwrap_or(&VersionPart::Num(0));
        let b = b_parts.get(i).unwrap_or(&VersionPart::Num(0));
        match (a, b) {
            (VersionPart::Num(an), VersionPart::Num(bn)) => match an.cmp(bn) {
                std::cmp::Ordering::Equal => continue,
                other => return other,
            },
            _ => continue,
        }
    }
    std::cmp::Ordering::Equal
}

pub(crate) fn coerce_to_hash(value: Value) -> Value {
    match value {
        Value::Hash(_) => value,
        Value::Array(items) => {
            let mut map = HashMap::new();
            let mut i = 0;
            while i < items.len() {
                if let Value::Pair(k, v) = &items[i] {
                    map.insert(k.clone(), *v.clone());
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
            Value::Hash(map)
        }
        Value::Pair(k, v) => {
            let mut map = HashMap::new();
            map.insert(k, *v);
            Value::Hash(map)
        }
        Value::Nil => Value::Hash(HashMap::new()),
        _ => {
            let mut map = HashMap::new();
            map.insert(value.to_string_value(), Value::Nil);
            Value::Hash(map)
        }
    }
}

pub(crate) fn coerce_to_array(value: Value) -> Value {
    match value {
        Value::Array(_) => value,
        Value::Nil => Value::Array(Vec::new()),
        Value::Range(a, b) if b == i64::MAX || a == i64::MIN => value,
        Value::Range(a, b) => Value::Array((a..=b).map(Value::Int).collect()),
        Value::RangeExcl(a, b) if b == i64::MAX || a == i64::MIN => value,
        Value::RangeExcl(a, b) => Value::Array((a..b).map(Value::Int).collect()),
        Value::RangeExclStart(a, b) if b == i64::MAX || a == i64::MIN => value,
        Value::RangeExclStart(a, b) => Value::Array((a + 1..=b).map(Value::Int).collect()),
        Value::RangeExclBoth(a, b) if b == i64::MAX || a == i64::MIN => value,
        Value::RangeExclBoth(a, b) => Value::Array((a + 1..b).map(Value::Int).collect()),
        Value::Slip(items) => Value::Array(items),
        other => Value::Array(vec![other]),
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
                    let s = format!("{}", val);
                    if s.contains('.') {
                        s
                    } else {
                        format!("{}.0", val)
                    }
                } else {
                    format!("<{}/{}>", n, d)
                }
            }
        }
        Value::Array(items) => format!(
            "[{}]",
            items.iter().map(gist_value).collect::<Vec<_>>().join(" ")
        ),
        Value::Hash(items) => items
            .iter()
            .map(|(k, v)| format!("{}\t{}", k, gist_value(v)))
            .collect::<Vec<_>>()
            .join("\n"),
        Value::Pair(k, v) => format!("{}\t{}", k, gist_value(v)),
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
        Value::Array(_) => "Array",
        Value::LazyList(_) => "Array",
        Value::Hash(_) => "Hash",
        Value::Range(_, _)
        | Value::RangeExcl(_, _)
        | Value::RangeExclStart(_, _)
        | Value::RangeExclBoth(_, _) => "Range",
        Value::Pair(_, _) => "Pair",
        Value::Rat(_, _) => "Rat",
        Value::FatRat(_, _) => "FatRat",
        Value::Complex(_, _) => "Complex",
        Value::Set(_) => "Set",
        Value::Bag(_) => "Bag",
        Value::Mix(_) => "Mix",
        Value::Nil => "Any",
        Value::Sub { .. } => "Sub",
        Value::Routine { .. } => "Routine",
        Value::Package(_) => "Package",
        Value::CompUnitDepSpec { .. } => "Any",
        Value::Enum { .. } => "Int",
        Value::Instance { .. } => "Any",
        Value::Junction { .. } => "Junction",
        Value::Regex(_) => "Regex",
        Value::Version { .. } => "Version",
        Value::Slip(_) => "Slip",
    }
}

pub(crate) fn reduction_identity(op: &str) -> Value {
    match op {
        "+" => Value::Int(0),
        "*" => Value::Int(1),
        "~" => Value::Str(String::new()),
        "&&" | "and" | "?&" => Value::Bool(true),
        "||" | "or" | "?|" => Value::Bool(false),
        "?^" => Value::Bool(false),
        "//" => Value::Nil,
        "min" => Value::Num(f64::INFINITY),
        "max" => Value::Num(f64::NEG_INFINITY),
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
        Value::Array(items) => items.clone(),
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
        Value::Set(items) => items.iter().map(|s| Value::Str(s.clone())).collect(),
        Value::Bag(items) => items
            .iter()
            .map(|(k, v)| Value::Pair(k.clone(), Box::new(Value::Int(*v))))
            .collect(),
        Value::Mix(items) => items
            .iter()
            .map(|(k, v)| Value::Pair(k.clone(), Box::new(Value::Num(*v))))
            .collect(),
        Value::Slip(items) => items.clone(),
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
        Value::Array(items) => Value::Int(items.len() as i64),
        Value::Nil => Value::Int(0),
        _ => Value::Int(0),
    }
}

pub(crate) fn coerce_to_set(val: &Value) -> HashSet<String> {
    match val {
        Value::Set(s) => s.clone(),
        Value::Bag(b) => b.keys().cloned().collect(),
        Value::Mix(m) => m.keys().cloned().collect(),
        Value::Array(items) => items.iter().map(|v| v.to_string_value()).collect(),
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
        | Value::Num(_)
        | Value::Rat(_, _)
        | Value::FatRat(_, _)
        | Value::Complex(_, _) => left,
        _ => coerce_to_numeric(left),
    };
    let r = match &right {
        Value::Int(_)
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

pub(crate) fn to_float_value(val: &Value) -> Option<f64> {
    match val {
        Value::Num(f) => Some(*f),
        Value::Int(i) => Some(*i as f64),
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
        (Value::Int(a), Value::Int(b)) => a.cmp(b) as i32,
        (Value::Num(a), Value::Num(b)) => {
            a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal) as i32
        }
        (Value::Int(a), Value::Num(b)) => (*a as f64)
            .partial_cmp(b)
            .unwrap_or(std::cmp::Ordering::Equal) as i32,
        (Value::Num(a), Value::Int(b)) => a
            .partial_cmp(&(*b as f64))
            .unwrap_or(std::cmp::Ordering::Equal) as i32,
        _ => {
            if let (Some((an, ad)), Some((bn, bd))) = (to_rat_parts(a), to_rat_parts(b)) {
                let lhs = an as i128 * bd as i128;
                let rhs = bn as i128 * ad as i128;
                return lhs.cmp(&rhs) as i32;
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
    Value::Junction { kind, values }
}

fn push_junction_value(kind: &JunctionKind, value: Value, out: &mut Vec<Value>) {
    match value {
        Value::Junction {
            kind: inner_kind,
            values,
        } if &inner_kind == kind => out.extend(values),
        other => out.push(other),
    }
}
