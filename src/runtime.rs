use std::collections::{HashMap, HashSet};

use crate::value::{JunctionKind, Value};

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
        Value::Range(a, b) => Value::Array((a..=b).map(Value::Int).collect()),
        Value::RangeExcl(a, b) => Value::Array((a..b).map(Value::Int).collect()),
        Value::RangeExclStart(a, b) => Value::Array((a + 1..=b).map(Value::Int).collect()),
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
        Value::Array(items) => items.iter().map(gist_value).collect::<Vec<_>>().join(" "),
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
        "&&" | "and" => Value::Bool(true),
        "||" | "or" => Value::Bool(false),
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
        Value::Range(a, b) => (*a..=*b).map(Value::Int).collect(),
        Value::RangeExcl(a, b) => (*a..*b).map(Value::Int).collect(),
        Value::RangeExclStart(a, b) => (*a + 1..=*b).map(Value::Int).collect(),
        Value::RangeExclBoth(a, b) => (*a + 1..*b).map(Value::Int).collect(),
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

pub(crate) fn format_sprintf(fmt: &str, arg: Option<&Value>) -> String {
    let mut chars = fmt.chars().peekable();
    let mut out = String::new();
    while let Some(c) = chars.next() {
        if c != '%' {
            out.push(c);
            continue;
        }
        if chars.peek() == Some(&'%') {
            chars.next();
            out.push('%');
            continue;
        }
        let mut flags = String::new();
        while let Some(f) = chars.peek().copied() {
            if f == '-' || f == '+' || f == ' ' || f == '#' {
                flags.push(f);
                chars.next();
            } else {
                break;
            }
        }
        let mut width = String::new();
        while let Some(d) = chars.peek().copied() {
            if d.is_ascii_digit() {
                width.push(d);
                chars.next();
            } else {
                break;
            }
        }
        let mut precision = String::new();
        if chars.peek() == Some(&'.') {
            chars.next();
            while let Some(d) = chars.peek().copied() {
                if d.is_ascii_digit() {
                    precision.push(d);
                    chars.next();
                } else {
                    break;
                }
            }
        }
        let spec = chars.next().unwrap_or('s');
        let width_num = width.parse::<usize>().unwrap_or(0);
        let prec_num = precision.parse::<usize>().ok();
        let zero_pad = width.starts_with('0') && !flags.contains('-');
        let left_align = flags.contains('-');
        let plus_sign = flags.contains('+');
        let hash_flag = flags.contains('#');
        let int_val = || match arg {
            Some(Value::Int(i)) => *i,
            Some(Value::Num(f)) => *f as i64,
            Some(Value::Str(s)) => s.trim().parse::<i64>().unwrap_or(0),
            Some(Value::Bool(b)) => {
                if *b {
                    1
                } else {
                    0
                }
            }
            _ => 0,
        };
        let float_val = || match arg {
            Some(Value::Int(i)) => *i as f64,
            Some(Value::Num(f)) => *f,
            Some(Value::Str(s)) => s.trim().parse::<f64>().unwrap_or(0.0),
            _ => 0.0,
        };
        let rendered = match spec {
            's' => match arg {
                Some(v) => {
                    let s = v.to_string_value();
                    if let Some(p) = prec_num {
                        s[..p.min(s.len())].to_string()
                    } else {
                        s
                    }
                }
                _ => String::new(),
            },
            'd' | 'i' => {
                let i = int_val();
                if plus_sign && i >= 0 {
                    format!("+{}", i)
                } else {
                    format!("{}", i)
                }
            }
            'u' => format!("{}", int_val().max(0) as u64),
            'x' => {
                if hash_flag {
                    format!("0x{:x}", int_val())
                } else {
                    format!("{:x}", int_val())
                }
            }
            'X' => {
                if hash_flag {
                    format!("0X{:X}", int_val())
                } else {
                    format!("{:X}", int_val())
                }
            }
            'o' => {
                if hash_flag {
                    format!("0o{:o}", int_val())
                } else {
                    format!("{:o}", int_val())
                }
            }
            'f' | 'F' => {
                let f = float_val();
                if let Some(p) = prec_num {
                    if plus_sign && f >= 0.0 {
                        format!("+{:.*}", p, f)
                    } else {
                        format!("{:.*}", p, f)
                    }
                } else if plus_sign && f >= 0.0 {
                    format!("+{}", f)
                } else {
                    format!("{}", f)
                }
            }
            'e' => {
                let f = float_val();
                if let Some(p) = prec_num {
                    format!("{:.*e}", p, f)
                } else {
                    format!("{:e}", f)
                }
            }
            'E' => {
                let f = float_val();
                if let Some(p) = prec_num {
                    format!("{:.*E}", p, f)
                } else {
                    format!("{:E}", f)
                }
            }
            'g' | 'G' => {
                let f = float_val();
                if let Some(p) = prec_num {
                    format!("{:.*}", p, f)
                } else {
                    format!("{}", f)
                }
            }
            'c' => {
                let i = int_val();
                char::from_u32(i as u32).unwrap_or('\0').to_string()
            }
            _ => match arg {
                Some(v) => v.to_string_value(),
                None => String::new(),
            },
        };
        if width_num > rendered.len() {
            let pad_len = width_num - rendered.len();
            let pad_char = if zero_pad { '0' } else { ' ' };
            let pad: String = std::iter::repeat_n(pad_char, pad_len).collect();
            if left_align {
                out.push_str(&rendered);
                out.push_str(&pad);
            } else {
                out.push_str(&pad);
                out.push_str(&rendered);
            }
        } else {
            out.push_str(&rendered);
        }
    }
    out
}

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
            } else {
                None
            }
        }
        Value::FatRat(n, d) => {
            if *d != 0 {
                Some(*n as f64 / *d as f64)
            } else {
                None
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
