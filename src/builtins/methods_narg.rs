#![allow(clippy::result_large_err)]

use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{ArrayKind, RuntimeError, Value};
use num_bigint::BigInt;
use num_traits::{ToPrimitive, Zero};
use std::collections::HashMap;
use std::sync::Arc;

fn flatten_with_depth(
    value: &Value,
    depth: Option<usize>,
    out: &mut Vec<Value>,
    flatten_arrays: bool,
) {
    if let Some(0) = depth {
        out.push(value.clone());
        return;
    }
    match value {
        Value::Array(items, kind) if *kind == ArrayKind::List || flatten_arrays => {
            let next_depth = depth.map(|d| d.saturating_sub(1));
            for item in items.iter() {
                flatten_with_depth(item, next_depth, out, flatten_arrays);
            }
        }
        Value::Range(..)
        | Value::RangeExcl(..)
        | Value::RangeExclStart(..)
        | Value::RangeExclBoth(..)
        | Value::GenericRange { .. } => out.extend(crate::runtime::utils::value_to_list(value)),
        other => out.push(other.clone()),
    }
}

fn parse_flat_depth(arg: &Value) -> Option<usize> {
    match arg {
        Value::Int(n) => Some((*n).max(0) as usize),
        _ => None,
    }
}

fn is_hammer_pair(arg: &Value) -> bool {
    matches!(arg, Value::Pair(key, val) if key == "hammer" && val.truthy())
}

fn flatten_target(target: &Value, depth: Option<usize>, flatten_arrays: bool) -> Value {
    let mut flat = Vec::new();
    if let Some(items) = target.as_list_items() {
        for item in items.iter() {
            flatten_with_depth(item, depth, &mut flat, flatten_arrays);
        }
    } else {
        flatten_with_depth(target, depth, &mut flat, flatten_arrays);
    }
    Value::Seq(Arc::new(flat))
}

fn fmt_joinable_target(target: &Value) -> bool {
    matches!(
        target,
        Value::Array(..)
            | Value::Seq(..)
            | Value::Slip(..)
            | Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. }
    )
}

fn contains_value_recursive(hay: &str, needle: &Value) -> Value {
    match needle {
        Value::Junction { kind, values } => {
            let mapped = values
                .iter()
                .map(|v| contains_value_recursive(hay, v))
                .collect::<Vec<_>>();
            Value::junction(kind.clone(), mapped)
        }
        _ => Value::Bool(hay.contains(&needle.to_string_value())),
    }
}

fn sample_weighted_mix_key(items: &HashMap<String, f64>) -> Option<Value> {
    let mut total = 0.0;
    for weight in items.values() {
        if weight.is_finite() && *weight > 0.0 {
            total += *weight;
        }
    }
    if total <= 0.0 {
        return None;
    }
    let mut needle = crate::builtins::rng::builtin_rand() * total;
    for (key, weight) in items {
        if !weight.is_finite() || *weight <= 0.0 {
            continue;
        }
        if needle <= *weight {
            return Some(Value::str(key.clone()));
        }
        needle -= *weight;
    }
    items
        .iter()
        .find_map(|(key, weight)| (*weight > 0.0).then(|| Value::str(key.clone())))
}

fn int_to_superscript(n: i64) -> String {
    const SUPER_DIGITS: [char; 10] = [
        '\u{2070}', '\u{00B9}', '\u{00B2}', '\u{00B3}', '\u{2074}', '\u{2075}', '\u{2076}',
        '\u{2077}', '\u{2078}', '\u{2079}',
    ];
    let s = n.to_string();
    s.chars()
        .map(|c| match c {
            '-' => '\u{207B}', // superscript minus
            d if d.is_ascii_digit() => SUPER_DIGITS[(d as u8 - b'0') as usize],
            _ => c,
        })
        .collect()
}

fn int_to_subscript(n: i64) -> String {
    const SUB_DIGITS: [char; 10] = [
        '\u{2080}', '\u{2081}', '\u{2082}', '\u{2083}', '\u{2084}', '\u{2085}', '\u{2086}',
        '\u{2087}', '\u{2088}', '\u{2089}',
    ];
    let s = n.to_string();
    s.chars()
        .map(|c| match c {
            '-' => '\u{208B}', // subscript minus
            d if d.is_ascii_digit() => SUB_DIGITS[(d as u8 - b'0') as usize],
            _ => c,
        })
        .collect()
}

// ── 1-arg method dispatch ────────────────────────────────────────────
/// Try to dispatch a 1-argument method call on a Value.
pub(crate) fn native_method_1arg(
    target: &Value,
    method_sym: Symbol,
    arg: &Value,
) -> Option<Result<Value, RuntimeError>> {
    let method = method_sym.resolve();
    let method = method.as_str();

    if let Value::Scalar(inner) = target {
        return native_method_1arg(inner, method_sym, arg);
    }
    // Cool numeric coercion: when a Str calls a numeric 1-arg method, coerce to numeric first.
    // Also coerce the arg if it's a Str for numeric methods.
    {
        let numeric_1arg_methods: &[&str] = &[
            "exp", "log", "round", "roots", "unpolar", "base", "polymod", "expmod", "atan2",
        ];
        if numeric_1arg_methods.contains(&method) {
            let coerced_target = if let Value::Str(s) = target {
                if let Ok(i) = s.parse::<i64>() {
                    Some(Value::Int(i))
                } else if let Ok(f) = s.parse::<f64>() {
                    Some(Value::Num(f))
                } else {
                    return None;
                }
            } else {
                None
            };
            let coerced_arg = if let Value::Str(s) = arg {
                if let Ok(i) = s.parse::<i64>() {
                    Some(Value::Int(i))
                } else if let Ok(f) = s.parse::<f64>() {
                    Some(Value::Num(f))
                } else {
                    None
                }
            } else {
                None
            };
            if coerced_target.is_some() || coerced_arg.is_some() {
                let t = coerced_target.as_ref().unwrap_or(target);
                let a = coerced_arg.as_ref().unwrap_or(arg);
                return native_method_1arg(t, method_sym, a);
            }
        }
    }
    match method {
        "Str" => {
            // Int.Str(:superscript) and Int.Str(:subscript)
            if let Value::Pair(key, val) = arg
                && val.truthy()
            {
                let int_val = match target {
                    Value::Int(i) => Some(*i),
                    Value::BigInt(bi) => bi.to_i64(),
                    Value::Num(f) => Some(*f as i64),
                    Value::Bool(b) => Some(if *b { 1 } else { 0 }),
                    _ => {
                        let s = target.to_string_value();
                        s.parse::<i64>().ok()
                    }
                };
                if let Some(n) = int_val {
                    match key.as_str() {
                        "superscript" => {
                            return Some(Ok(Value::str(int_to_superscript(n))));
                        }
                        "subscript" => {
                            return Some(Ok(Value::str(int_to_subscript(n))));
                        }
                        _ => {}
                    }
                }
            }
            // Default: just stringify
            Some(Ok(Value::str(target.to_string_value())))
        }
        "chop" => {
            // Type objects (Package) should throw
            if let Value::Package(type_name) = target {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller chop({}:U)",
                    type_name,
                ))));
            }
            let s = target.to_string_value();
            let n = match arg {
                Value::Int(i) => (*i).max(0) as usize,
                Value::BigInt(bi) => {
                    use num_traits::ToPrimitive;
                    bi.to_usize().unwrap_or(usize::MAX)
                }
                Value::Num(f) => (*f as i64).max(0) as usize,
                _ => arg.to_string_value().parse::<usize>().unwrap_or(1),
            };
            let char_count = s.chars().count();
            let keep = char_count.saturating_sub(n);
            let result: String = s.chars().take(keep).collect();
            Some(Ok(Value::str(result)))
        }
        "contains" => {
            if let Value::Package(type_name) = arg {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller contains({}:U)",
                    type_name,
                ))));
            }
            let s = target.to_string_value();
            Some(Ok(contains_value_recursive(&s, arg)))
        }
        // starts-with and ends-with are handled in runtime/methods.rs
        // to support named args (:i, :ignorecase, :m, :ignoremark)
        "samemark" => {
            let target_str = target.to_string_value();
            let source_str = arg.to_string_value();
            Some(Ok(Value::str(crate::builtins::samemark_string(
                &target_str,
                &source_str,
            ))))
        }
        "samecase" => {
            let source_str = target.to_string_value();
            let pattern_str = arg.to_string_value();
            Some(Ok(Value::str(crate::builtins::samecase_string(
                &source_str,
                &pattern_str,
            ))))
        }
        "decode" => {
            let encoding = arg.to_string_value();
            super::decode_buf_method(target, Some(&encoding))
        }
        "Rat" => {
            // .Rat(epsilon) — use continued fraction algorithm with given epsilon
            let epsilon = match arg {
                Value::Num(f) => *f,
                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                Value::Int(i) => *i as f64,
                _ => 1e-6,
            };
            let result = match target {
                Value::Rat(_, _) => target.clone(),
                Value::Int(i) => Value::Rat(*i, 1),
                Value::Num(f) => {
                    if f.is_nan() {
                        Value::Rat(0, 0)
                    } else if f.is_infinite() {
                        if f.is_sign_positive() {
                            Value::Rat(1, 0)
                        } else {
                            Value::Rat(-1, 0)
                        }
                    } else {
                        super::num_to_rat_with_epsilon(*f, epsilon)
                    }
                }
                Value::FatRat(n, d) => Value::Rat(*n, *d),
                Value::Str(s) => {
                    if let Ok(f) = s.parse::<f64>() {
                        super::num_to_rat_with_epsilon(f, epsilon)
                    } else {
                        Value::Rat(0, 1)
                    }
                }
                _ => Value::Rat(0, 1),
            };
            Some(Ok(result))
        }
        "FatRat" => {
            // .FatRat or .FatRat(epsilon) — convert to FatRat
            let result = match target {
                Value::FatRat(_, _) => target.clone(),
                Value::Int(i) => Value::FatRat(*i, 1),
                Value::Rat(n, d) => Value::FatRat(*n, *d),
                Value::Num(f) => {
                    let denom = 1_000_000i64;
                    let numer = (f * denom as f64).round() as i64;
                    Value::FatRat(numer, denom)
                }
                _ => Value::FatRat(0, 1),
            };
            Some(Ok(result))
        }
        "index" => {
            // Fall through to runtime dispatch for type objects, named args (Pairs),
            // array of needles, and multi-arg calls handled by dispatch_index
            if matches!(arg, Value::Package(_) | Value::Pair(..) | Value::Array(..)) {
                return None;
            }
            let s = target.to_string_value();
            let needle = arg.to_string_value();
            match s.find(&needle) {
                Some(pos) => {
                    let char_pos = s[..pos].chars().count();
                    Some(Ok(Value::Int(char_pos as i64)))
                }
                None => Some(Ok(Value::Nil)),
            }
        }
        "substr" => {
            let start = match arg {
                Value::Int(i) if *i >= 0 => *i as usize,
                Value::Int(_) => return None, // negative: let runtime handle
                _ => return None,
            };
            let s = target.to_string_value();
            let chars: Vec<char> = s.chars().collect();
            if start > chars.len() {
                return None; // out-of-range: let runtime handle (returns Failure)
            }
            Some(Ok(Value::str(chars[start..].iter().collect())))
        }
        "AT-POS" => {
            let idx = match arg {
                Value::Int(i) if *i >= 0 => *i as usize,
                Value::Num(f) if *f >= 0.0 => *f as usize,
                _ => return Some(Ok(Value::Nil)),
            };
            if let Some(items) = target.as_list_items() {
                Some(Ok(items.get(idx).cloned().unwrap_or(Value::Nil)))
            } else {
                match target {
                    Value::Str(s) => {
                        let ch = s.chars().nth(idx).map(|c| Value::str(c.to_string()));
                        Some(Ok(ch.unwrap_or(Value::Nil)))
                    }
                    Value::Instance {
                        class_name,
                        attributes,
                        ..
                    } if class_name == "Match" => {
                        if let Some(Value::Array(positional, ..)) = attributes.get("list") {
                            return Some(Ok(positional.get(idx).cloned().unwrap_or(Value::Nil)));
                        }
                        Some(Ok(Value::Nil))
                    }
                    Value::Instance {
                        class_name,
                        attributes,
                        ..
                    } if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) => {
                        if let Some(Value::Array(bytes, ..)) = attributes.get("bytes") {
                            return Some(Ok(bytes.get(idx).cloned().unwrap_or(Value::Int(0))));
                        }
                        Some(Ok(Value::Int(0)))
                    }
                    _ => None,
                }
            }
        }
        "split" => {
            if let Value::Instance { class_name, .. } = target
                && class_name == "Supply"
            {
                return None;
            }
            let s = target.to_string_value();
            let sep = arg.to_string_value();
            let parts: Vec<Value> = s.split(&sep).map(|p| Value::str(p.to_string())).collect();
            Some(Ok(Value::array(parts)))
        }
        "lines" => {
            if let Value::Instance { class_name, .. } = target
                && class_name == "Supply"
            {
                return None;
            }
            let s = target.to_string_value();
            if let Value::Pair(key, value) = arg {
                if key == "chomp" {
                    let lines: Vec<Value> =
                        crate::builtins::split_lines_with_chomp(&s, value.truthy())
                            .into_iter()
                            .map(Value::str)
                            .collect();
                    return Some(Ok(Value::array(lines)));
                }
                return None;
            }

            let mut lines = crate::builtins::split_lines_chomped(&s);
            let limit = match arg {
                Value::Int(i) => Some((*i).max(0) as usize),
                Value::BigInt(bi) => {
                    use num_traits::ToPrimitive;
                    Some(bi.to_usize().unwrap_or(usize::MAX))
                }
                Value::Whatever => None,
                Value::Num(f) if f.is_infinite() && f.is_sign_positive() => None,
                Value::Num(f) if *f >= 0.0 => Some(*f as usize),
                Value::Rat(n, d) if *d == 0 && *n > 0 => None,
                _ => return None,
            };
            if let Some(n) = limit {
                lines.truncate(n);
            }
            let lines: Vec<Value> = lines.into_iter().map(Value::str).collect();
            Some(Ok(Value::array(lines)))
        }
        "join" => {
            if let Some(items) = target.as_list_items() {
                let sep = arg.to_string_value();
                let joined = items
                    .iter()
                    .map(|v| v.to_str_context())
                    .collect::<Vec<_>>()
                    .join(&sep);
                return Some(Ok(Value::str(joined)));
            }
            match target {
                Value::Capture { positional, .. } => {
                    let sep = arg.to_string_value();
                    let joined = positional
                        .iter()
                        .map(|v| v.to_string_value())
                        .collect::<Vec<_>>()
                        .join(&sep);
                    Some(Ok(Value::str(joined)))
                }
                Value::Pair(k, v) => {
                    let sep = arg.to_string_value();
                    Some(Ok(Value::str(format!(
                        "{}{}{}",
                        k,
                        sep,
                        v.to_string_value()
                    ))))
                }
                Value::ValuePair(k, v) => {
                    let sep = arg.to_string_value();
                    Some(Ok(Value::str(format!(
                        "{}{}{}",
                        k.to_string_value(),
                        sep,
                        v.to_string_value()
                    ))))
                }
                Value::Hash(map) => {
                    let sep = arg.to_string_value();
                    let joined = map
                        .iter()
                        .map(|(k, v)| format!("{}\t{}", k, v.to_string_value()))
                        .collect::<Vec<_>>()
                        .join(&sep);
                    Some(Ok(Value::str(joined)))
                }
                // Scalar values: .join returns the value as a string
                Value::Str(_)
                | Value::Int(_)
                | Value::Num(_)
                | Value::Rat(..)
                | Value::Bool(_)
                | Value::Instance { .. }
                | Value::Nil => Some(Ok(Value::str(target.to_string_value()))),
                // Other types (LazyList, etc.) fall through to the runtime handler
                _ => None,
            }
        }
        "flat" => {
            if is_hammer_pair(arg) {
                return Some(Ok(flatten_target(target, None, true)));
            }
            if let Some(depth) = parse_flat_depth(arg) {
                return Some(Ok(flatten_target(target, Some(depth), false)));
            }
            None
        }
        "head" => match target {
            Value::Array(items, ..) => {
                let n = match arg {
                    Value::Int(i) => *i as usize,
                    _ => return None,
                };
                Some(Ok(Value::array(items[..n.min(items.len())].to_vec())))
            }
            Value::Range(a, b) => {
                let n = match arg {
                    Value::Int(i) => *i as usize,
                    _ => return None,
                };
                let items: Vec<Value> = (*a..=*b).take(n).map(Value::Int).collect();
                Some(Ok(Value::array(items)))
            }
            _ => {
                let n = match arg {
                    Value::Int(i) => *i as usize,
                    _ => return None,
                };
                let items = runtime::value_to_list(target);
                Some(Ok(Value::array(items[..n.min(items.len())].to_vec())))
            }
        },
        "tail" => match target {
            Value::Array(items, ..) => {
                let n = match arg {
                    Value::Int(i) => *i as usize,
                    _ => return None,
                };
                let start = items.len().saturating_sub(n);
                Some(Ok(Value::array(items[start..].to_vec())))
            }
            Value::Instance { class_name, .. } if class_name == "Supply" => None,
            _ => {
                let n = match arg {
                    Value::Int(i) => *i as usize,
                    _ => return None,
                };
                let items = runtime::value_to_list(target);
                let start = items.len().saturating_sub(n);
                Some(Ok(Value::array(items[start..].to_vec())))
            }
        },
        "combinations" => {
            let items = target
                .as_list_items()
                .map(|items| items.to_vec())
                .unwrap_or_else(|| runtime::value_to_list(target));
            match arg {
                Value::Range(a, b) => Some(Ok(Value::Seq(
                    super::methods_0arg::collection::combinations_range(&items, *a, *b).into(),
                ))),
                Value::RangeExcl(a, b) => Some(Ok(Value::Seq(
                    super::methods_0arg::collection::combinations_range(&items, *a, *b - 1).into(),
                ))),
                Value::RangeExclStart(a, b) => Some(Ok(Value::Seq(
                    super::methods_0arg::collection::combinations_range(&items, *a + 1, *b).into(),
                ))),
                Value::RangeExclBoth(a, b) => Some(Ok(Value::Seq(
                    super::methods_0arg::collection::combinations_range(&items, *a + 1, *b - 1)
                        .into(),
                ))),
                Value::GenericRange {
                    start,
                    end,
                    excl_start,
                    excl_end,
                } => {
                    let mut lo = runtime::to_int(start);
                    let mut hi = runtime::to_int(end);
                    if *excl_start {
                        lo += 1;
                    }
                    if *excl_end {
                        hi -= 1;
                    }
                    Some(Ok(Value::Seq(
                        super::methods_0arg::collection::combinations_range(&items, lo, hi).into(),
                    )))
                }
                _ => {
                    let k = runtime::to_int(arg);
                    if k < 0 {
                        Some(Ok(Value::Seq(Vec::new().into())))
                    } else {
                        Some(Ok(Value::Seq(
                            super::methods_0arg::collection::combinations_k(&items, k as usize)
                                .into(),
                        )))
                    }
                }
            }
        }
        "batch" => {
            let n = match arg {
                Value::Int(i) => *i,
                _ => return None,
            };
            if n < 1 {
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("got".to_string(), Value::Int(n));
                attrs.insert(
                    "message".to_string(),
                    Value::str(format!("batch size must be at least 1, got {}", n)),
                );
                let ex = Value::make_instance(Symbol::intern("X::OutOfRange"), attrs);
                let mut err = RuntimeError::new(format!(
                    "X::OutOfRange: batch size must be at least 1, got {}",
                    n
                ));
                err.exception = Some(Box::new(ex));
                return Some(Err(err));
            }
            let n = n as usize;
            let items = runtime::value_to_list(target);
            let batches: Vec<Value> = items
                .chunks(n)
                .map(|chunk| Value::array(chunk.to_vec()))
                .collect();
            Some(Ok(Value::Seq(batches.into())))
        }
        "rindex" => {
            // Fall through to runtime dispatch for arrays (list of needles)
            // and type objects
            if matches!(arg, Value::Array(..) | Value::Package(_) | Value::Pair(..)) {
                return None;
            }
            let s = target.to_string_value();
            let needle = arg.to_string_value();
            match s.rfind(&needle) {
                Some(pos) => {
                    let char_pos = s[..pos].chars().count();
                    Some(Ok(Value::Int(char_pos as i64)))
                }
                None => Some(Ok(Value::Nil)),
            }
        }
        "fmt" => {
            let fmt = arg.to_string_value();
            if fmt_joinable_target(target) {
                let rendered = runtime::value_to_list(target)
                    .into_iter()
                    .map(|item| runtime::format_sprintf(&fmt, Some(&item)))
                    .collect::<Vec<_>>()
                    .join(" ");
                Some(Ok(Value::str(rendered)))
            } else {
                let rendered = runtime::format_sprintf(&fmt, Some(target));
                Some(Ok(Value::str(rendered)))
            }
        }
        "sprintf" => {
            // Method form: '%f'.sprintf(value) — target is the format string
            let fmt = target.to_string_value();
            let rendered = runtime::format_sprintf(&fmt, Some(arg));
            Some(Ok(Value::str(rendered)))
        }
        "parse-base" => {
            let radix = match arg {
                Value::Int(n) => *n as u32,
                _ => return None,
            };
            let s = target.to_string_value();
            match i64::from_str_radix(&s, radix) {
                Ok(n) => Some(Ok(Value::Int(n))),
                Err(_) => Some(Err(RuntimeError::new(format!(
                    "Cannot parse '{}' as base {}",
                    s, radix
                )))),
            }
        }
        "base" => match target {
            Value::Int(i) => {
                let radix = match arg {
                    Value::Int(r) if (2..=36).contains(r) => *r as u32,
                    Value::Int(_) => {
                        return Some(Err(RuntimeError::new(
                            "X::OutOfRange: base requires radix 2..36",
                        )));
                    }
                    Value::Str(s) => match s.parse::<u32>() {
                        Ok(r) if (2..=36).contains(&r) => r,
                        _ => {
                            return Some(Err(RuntimeError::new(
                                "X::OutOfRange: base requires radix 2..36",
                            )));
                        }
                    },
                    _ => {
                        return Some(Err(RuntimeError::new(
                            "X::OutOfRange: base requires radix 2..36",
                        )));
                    }
                };
                let negative = *i < 0;
                let mut n = if negative { (-*i) as u64 } else { *i as u64 };
                if n == 0 {
                    return Some(Ok(Value::str_from("0")));
                }
                let digits = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
                let mut buf = Vec::new();
                while n > 0 {
                    buf.push(digits[(n % radix as u64) as usize]);
                    n /= radix as u64;
                }
                if negative {
                    buf.push(b'-');
                }
                buf.reverse();
                Some(Ok(Value::str(String::from_utf8(buf).unwrap())))
            }
            Value::Num(f) => {
                if f.is_infinite() || f.is_nan() {
                    return Some(Err(RuntimeError::new(format!(
                        "X::Numeric::CannotConvert: Cannot convert {} to base",
                        if f.is_nan() {
                            "NaN"
                        } else if *f > 0.0 {
                            "Inf"
                        } else {
                            "-Inf"
                        },
                    ))));
                }
                let radix = match arg {
                    Value::Int(r) if (2..=36).contains(r) => *r as u32,
                    Value::Int(_) => {
                        return Some(Err(RuntimeError::new(
                            "X::OutOfRange: base requires radix 2..36",
                        )));
                    }
                    Value::Str(s) => match s.parse::<u32>() {
                        Ok(r) if (2..=36).contains(&r) => r,
                        _ => {
                            return Some(Err(RuntimeError::new(
                                "X::OutOfRange: base requires radix 2..36",
                            )));
                        }
                    },
                    _ => {
                        return Some(Err(RuntimeError::new(
                            "X::OutOfRange: base requires radix 2..36",
                        )));
                    }
                };
                // Convert Num to Rat for precise base conversion
                let (n, d) = f64_to_rat(*f);
                Some(Ok(Value::str(rat_to_base(n, d, radix, BaseDigits::Auto))))
            }
            Value::Rat(n, d) | Value::FatRat(n, d) => {
                let radix = match parse_radix_checked(arg)? {
                    Ok(r) => r,
                    Err(e) => return Some(Err(e)),
                };
                Some(Ok(Value::str(rat_to_base(*n, *d, radix, BaseDigits::Auto))))
            }
            // Handle Instance types (Duration, Instant, etc.) by
            // extracting their numeric value
            Value::Instance { attributes, .. } => {
                let radix = match parse_radix_checked(arg)? {
                    Ok(r) => r,
                    Err(e) => return Some(Err(e)),
                };
                if let Some(val) = attributes.get("value") {
                    match val {
                        Value::Int(i) => {
                            Some(Ok(Value::str(rat_to_base(*i, 1, radix, BaseDigits::Auto))))
                        }
                        Value::Rat(n, d) | Value::FatRat(n, d) => {
                            Some(Ok(Value::str(rat_to_base(*n, *d, radix, BaseDigits::Auto))))
                        }
                        Value::Num(f) => {
                            let (n, d) = f64_to_rat(*f);
                            Some(Ok(Value::str(rat_to_base(n, d, radix, BaseDigits::Auto))))
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            }
            _ => None,
        },
        "base-repeating" => {
            let radix = match parse_radix_checked(arg)? {
                Ok(r) => r,
                Err(e) => return Some(Err(e)),
            };
            let (n, d) = match target {
                Value::Int(i) => (*i, 1i64),
                Value::Rat(n, d) => (*n, *d),
                Value::Num(f) => f64_to_rat(*f),
                _ => return None,
            };
            let (non_repeating, repeating) = rat_base_repeating(n, d, radix);
            Some(Ok(Value::Array(
                Arc::new(vec![Value::str(non_repeating), Value::str(repeating)]),
                ArrayKind::List,
            )))
        }
        "round" => {
            // Unwrap allomorphic types (IntStr, NumStr, RatStr, ComplexStr)
            // to get the underlying numeric value for the scale
            let unwrapped_arg = match arg {
                Value::Mixin(inner, _) => inner.as_ref(),
                other => other,
            };
            // Determine the scale type category for return type selection
            // Int/IntStr -> Int, Num/NumStr/Complex/ComplexStr -> Num,
            // Rat/RatStr -> Rat
            #[derive(Clone, Copy)]
            enum RoundResult {
                Int,
                Num,
                Rat,
            }
            let scale_type = match unwrapped_arg {
                Value::Int(_) | Value::BigInt(_) => RoundResult::Int,
                Value::Num(_) => RoundResult::Num,
                Value::Rat(_, _) | Value::FatRat(_, _) | Value::BigRat(_, _) => RoundResult::Rat,
                Value::Complex(_, _) => RoundResult::Num,
                _ => return None,
            };
            let scale = match unwrapped_arg {
                Value::Int(i) => *i as f64,
                Value::Num(f) => *f,
                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                Value::FatRat(n, d) if *d != 0 => *n as f64 / *d as f64,
                Value::Complex(re, _) => *re,
                _ => return None,
            };
            fn raku_round(v: f64) -> f64 {
                (v + 0.5).floor()
            }
            fn round_real(x: f64, scale: f64) -> f64 {
                if scale == 0.0 {
                    raku_round(x)
                } else {
                    raku_round(x / scale) * scale
                }
            }
            // Unwrap target allomorphic types too
            let unwrapped_target = match target {
                Value::Mixin(inner, _) => inner.as_ref(),
                other => other,
            };
            // Handle Complex target separately — always returns Complex
            if let Value::Complex(re, im) = unwrapped_target {
                let rr = round_real(*re, scale);
                let ri = round_real(*im, scale);
                return Some(Ok(Value::Complex(rr, ri)));
            }
            let x = match unwrapped_target {
                Value::Int(i) => *i as f64,
                Value::BigInt(bi) => bi.to_f64().unwrap_or(0.0),
                Value::Num(f) => *f,
                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                Value::FatRat(n, d) if *d != 0 => *n as f64 / *d as f64,
                _ => return None,
            };
            let result = round_real(x, scale);
            // Return type depends on the scale type
            match scale_type {
                RoundResult::Int => {
                    let r = result.floor();
                    if r >= i64::MIN as f64 && r <= i64::MAX as f64 {
                        Some(Ok(Value::Int(r as i64)))
                    } else {
                        Some(Ok(Value::Num(r)))
                    }
                }
                RoundResult::Num => Some(Ok(Value::Num(result))),
                RoundResult::Rat => {
                    let (n, d) = f64_to_rat(result);
                    Some(Ok(Value::Rat(n, d)))
                }
            }
        }
        "pick" => {
            if matches!(target, Value::Mix(_)) {
                return Some(Err(RuntimeError::new(
                    "Cannot call .pick on a Mix (immutable)",
                )));
            }
            let mut items = runtime::value_to_list(target);
            Some(Ok(match arg {
                Value::Whatever => {
                    // .pick(*) — Fisher-Yates shuffle
                    let len = items.len();
                    for i in (1..len).rev() {
                        let j = (crate::builtins::rng::builtin_rand() * (i + 1) as f64) as usize
                            % (i + 1);
                        items.swap(i, j);
                    }
                    Value::array(items)
                }
                Value::Num(f) if f.is_infinite() && f.is_sign_positive() => {
                    // .pick(Inf) — same as .pick(*)
                    let len = items.len();
                    for i in (1..len).rev() {
                        let j = (crate::builtins::rng::builtin_rand() * (i + 1) as f64) as usize
                            % (i + 1);
                        items.swap(i, j);
                    }
                    Value::array(items)
                }
                Value::Int(n) => {
                    let count = (*n).max(0) as usize;
                    if count == 0 || items.is_empty() {
                        Value::array(Vec::new())
                    } else {
                        let mut result = Vec::with_capacity(count.min(items.len()));
                        for _ in 0..count.min(items.len()) {
                            let idx = (crate::builtins::rng::builtin_rand() * items.len() as f64)
                                as usize
                                % items.len();
                            result.push(items.swap_remove(idx));
                        }
                        Value::array(result)
                    }
                }
                _ => return None,
            }))
        }
        "roll" => {
            if matches!(target, Value::Package(_)) {
                return None;
            }
            let count = match arg {
                Value::Int(i) if *i > 0 => Some(*i as usize),
                Value::Int(_) => Some(0),
                Value::Num(f) if f.is_infinite() && f.is_sign_positive() => None,
                Value::Whatever => None,
                Value::Str(s) => {
                    let parsed = s.trim().parse::<i64>().ok()?;
                    Some(parsed.max(0) as usize)
                }
                _ => return None,
            };
            if let Value::Mix(items) = target {
                if count.is_none() {
                    let generated = 131_072usize;
                    let mut out = Vec::with_capacity(generated);
                    for _ in 0..generated {
                        if let Some(v) = sample_weighted_mix_key(items) {
                            out.push(v);
                        }
                    }
                    return Some(Ok(Value::LazyList(Arc::new(crate::value::LazyList {
                        body: vec![],
                        env: crate::env::Env::new(),
                        cache: std::sync::Mutex::new(Some(out)),
                    }))));
                }
                let count = count.unwrap_or(0);
                if count == 0 {
                    return Some(Ok(Value::array(Vec::new())));
                }
                let mut result = Vec::with_capacity(count);
                for _ in 0..count {
                    if let Some(v) = sample_weighted_mix_key(items) {
                        result.push(v);
                    }
                }
                return Some(Ok(Value::array(result)));
            }
            let sample_from_range = |range: &Value| -> Option<Value> {
                let random_i64 = |lo: i64, hi: i64| -> Value {
                    if hi <= lo {
                        return Value::Int(lo);
                    }
                    let span = (hi as i128 - lo as i128 + 1) as f64;
                    let mut offset = (crate::builtins::rng::builtin_rand() * span) as i128;
                    let max_offset = hi as i128 - lo as i128;
                    if offset > max_offset {
                        offset = max_offset;
                    }
                    Value::Int((lo as i128 + offset) as i64)
                };
                match range {
                    Value::Range(start, end) => Some(random_i64(*start, *end)),
                    Value::RangeExcl(start, end) => {
                        if *start >= *end {
                            Some(Value::Nil)
                        } else {
                            Some(random_i64(*start, end.saturating_sub(1)))
                        }
                    }
                    Value::RangeExclStart(start, end) => {
                        if *start >= *end {
                            Some(Value::Nil)
                        } else {
                            Some(random_i64(start.saturating_add(1), *end))
                        }
                    }
                    Value::RangeExclBoth(start, end) => {
                        if start.saturating_add(1) >= *end {
                            Some(Value::Nil)
                        } else {
                            Some(random_i64(start.saturating_add(1), end.saturating_sub(1)))
                        }
                    }
                    Value::GenericRange {
                        start,
                        end,
                        excl_start,
                        excl_end,
                    } => {
                        if let (Some(s), Some(e)) =
                            (runtime::to_float_value(start), runtime::to_float_value(end))
                            && s.is_finite()
                            && e.is_finite()
                        {
                            let mut vals = Vec::new();
                            let mut cur = if *excl_start { s + 1.0 } else { s };
                            let limit = 10_000usize;
                            while vals.len() < limit {
                                if (*excl_end && cur >= e) || (!*excl_end && cur > e) {
                                    break;
                                }
                                vals.push(Value::Num(cur));
                                cur += 1.0;
                            }
                            if !vals.is_empty() {
                                let idx = (crate::builtins::rng::builtin_rand() * vals.len() as f64)
                                    as usize
                                    % vals.len();
                                return Some(vals[idx].clone());
                            }
                        }
                        Some(start.as_ref().clone())
                    }
                    _ => None,
                }
            };

            let items = if target.is_range() {
                Vec::new()
            } else {
                runtime::value_to_list(target)
            };
            if count.is_none() {
                if !target.is_range() && items.is_empty() {
                    return Some(Ok(Value::array(Vec::new())));
                }
                let generated = 1024usize;
                let mut out = Vec::with_capacity(generated);
                for _ in 0..generated {
                    if target.is_range() {
                        if let Some(v) = sample_from_range(target) {
                            out.push(v);
                        }
                    } else {
                        let mut idx =
                            (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize;
                        if idx >= items.len() {
                            idx = items.len() - 1;
                        }
                        out.push(items[idx].clone());
                    }
                }
                return Some(Ok(Value::LazyList(Arc::new(crate::value::LazyList {
                    body: vec![],
                    env: crate::env::Env::new(),
                    cache: std::sync::Mutex::new(Some(out)),
                }))));
            }
            let count = count.unwrap_or(0);
            if count == 0 || (!target.is_range() && items.is_empty()) {
                return Some(Ok(Value::array(Vec::new())));
            }
            let mut result = Vec::with_capacity(count);
            for _ in 0..count {
                if target.is_range() {
                    if let Some(v) = sample_from_range(target) {
                        result.push(v);
                    }
                } else {
                    let mut idx =
                        (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize;
                    if idx >= items.len() {
                        idx = items.len() - 1;
                    }
                    result.push(items[idx].clone());
                }
            }
            Some(Ok(Value::array(result)))
        }
        "log" => {
            let base_complex = match arg {
                Value::Int(i) => Some((*i as f64, 0.0)),
                Value::Num(f) => Some((*f, 0.0)),
                Value::Rat(n, d) if *d != 0 => Some((*n as f64 / *d as f64, 0.0)),
                Value::Complex(r, i) => Some((*r, *i)),
                _ => None,
            };
            let target_complex = match target {
                Value::Int(i) => Some((*i as f64, 0.0)),
                Value::Num(f) => Some((*f, 0.0)),
                Value::Rat(n, d) if *d != 0 => Some((*n as f64 / *d as f64, 0.0)),
                Value::Complex(r, i) => Some((*r, *i)),
                _ => None,
            };
            match (target_complex, base_complex) {
                (Some((xr, xi)), Some((br, bi))) => {
                    if bi == 0.0 && xi == 0.0 {
                        if br.is_finite() && br > 0.0 && br != 1.0 && xr > 0.0 {
                            return Some(Ok(Value::Num(xr.ln() / br.ln())));
                        }
                        return Some(Ok(Value::Num(f64::NAN)));
                    }
                    let ln_x_mag = (xr * xr + xi * xi).sqrt().ln();
                    let ln_x_arg = xi.atan2(xr);
                    let ln_b_mag = (br * br + bi * bi).sqrt().ln();
                    let ln_b_arg = bi.atan2(br);
                    let denom = ln_b_mag * ln_b_mag + ln_b_arg * ln_b_arg;
                    if denom == 0.0 {
                        return Some(Ok(Value::Num(f64::NAN)));
                    }
                    let re = (ln_x_mag * ln_b_mag + ln_x_arg * ln_b_arg) / denom;
                    let im = (ln_x_arg * ln_b_mag - ln_x_mag * ln_b_arg) / denom;
                    Some(Ok(Value::Complex(re, im)))
                }
                _ => None,
            }
        }
        "exp" => {
            // $x.exp($base) = $base ** $x
            // Get base as real or complex
            let (base_r, base_i) = match arg {
                Value::Int(i) => (*i as f64, 0.0),
                Value::Num(f) => (*f, 0.0),
                Value::Rat(n, d) if *d != 0 => (*n as f64 / *d as f64, 0.0),
                Value::Complex(r, i) => (*r, *i),
                _ => return None,
            };
            // Get exponent as real or complex
            let (exp_r, exp_i) = match target {
                Value::Int(i) => (*i as f64, 0.0),
                Value::Num(f) => (*f, 0.0),
                Value::Rat(n, d) if *d != 0 => (*n as f64 / *d as f64, 0.0),
                Value::Complex(r, i) => (*r, *i),
                _ => return None,
            };
            // Compute base^exp via exp(exp * ln(base))
            // ln(base) for complex: ln(|base|) + i*arg(base)
            let ln_r = (base_r * base_r + base_i * base_i).sqrt().ln();
            let ln_i = base_i.atan2(base_r);
            // exp * ln(base): (exp_r + exp_i*i) * (ln_r + ln_i*i)
            let prod_r = exp_r * ln_r - exp_i * ln_i;
            let prod_i = exp_r * ln_i + exp_i * ln_r;
            // exp(prod_r + prod_i*i)
            let ea = prod_r.exp();
            let result_r = ea * prod_i.cos();
            let result_i = ea * prod_i.sin();
            if result_i.abs() < 1e-15 && base_i == 0.0 && exp_i == 0.0 {
                Some(Ok(Value::Num(result_r)))
            } else {
                Some(Ok(Value::Complex(result_r, result_i)))
            }
        }
        "unpolar" => {
            // $magnitude.unpolar($angle) = $magnitude * cis($angle)
            let mag = match target {
                Value::Int(i) => *i as f64,
                Value::Num(f) => *f,
                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                _ => return None,
            };
            let angle = match arg {
                Value::Int(i) => *i as f64,
                Value::Num(f) => *f,
                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                _ => return None,
            };
            Some(Ok(Value::Complex(mag * angle.cos(), mag * angle.sin())))
        }
        "roots" => {
            let n = match arg {
                Value::Int(i) if *i > 0 => *i as usize,
                _ => return None,
            };
            let (re, im) = runtime::to_complex_parts(target)?;
            let r = (re * re + im * im).sqrt();
            let theta = im.atan2(re);
            let mag = r.powf(1.0 / n as f64);
            let mut roots = Vec::with_capacity(n);
            for k in 0..n {
                let angle = (theta + 2.0 * std::f64::consts::PI * k as f64) / n as f64;
                let rr = mag * angle.cos();
                let ii = mag * angle.sin();
                if ii.abs() < 1e-12 {
                    roots.push(Value::Num(rr));
                } else {
                    roots.push(Value::Complex(rr, ii));
                }
            }
            Some(Ok(Value::array(roots)))
        }
        "atan2" => {
            // User-defined types need runtime coercion via .Numeric/.Bridge
            if matches!(target, Value::Instance { .. }) || matches!(arg, Value::Instance { .. }) {
                return None;
            }
            let y = runtime::to_float_value(target).unwrap_or(0.0);
            let x = runtime::to_float_value(arg).unwrap_or(0.0);
            Some(Ok(Value::Num(y.atan2(x))))
        }
        // Buf/Blob read-num methods (1 arg: offset, uses NativeEndian)
        "read-num32" | "read-num64" => {
            if let Value::Package(type_name) = target {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller {}({}:U)",
                    method, type_name,
                ))));
            }
            let bytes = buf_get_bytes(target)?;
            let offset_i64 = to_int_val(arg);
            let size: usize = if method == "read-num32" { 4 } else { 8 };
            if offset_i64 < 0
                || (offset_i64 as usize)
                    .checked_add(size)
                    .is_none_or(|end| end > bytes.len())
            {
                return Some(Err(RuntimeError::new(format!(
                    "read from out of range. Is: {}, should be in 0..{}",
                    offset_i64,
                    bytes.len()
                ))));
            }
            let offset = offset_i64 as usize;
            let result = if size == 4 {
                read_f32_ne(&bytes[offset..offset + 4])
            } else {
                read_f64_ne(&bytes[offset..offset + 8])
            };
            Some(Ok(Value::Num(result)))
        }
        "EXISTS-KEY" => match target {
            Value::Hash(map) => {
                let key = arg.to_string_value();
                Some(Ok(Value::Bool(map.contains_key(&key))))
            }
            _ => None,
        },
        "subbuf" => {
            if !is_buf_like(target) {
                return None;
            }
            let items = buf_get_int_items(target)?;
            let cn = buf_class_name(target);
            // subbuf(Range)
            if let Some((start, end)) = range_bounds(arg) {
                let len = items.len() as i64;
                if end <= start || start >= len {
                    return Some(Ok(make_buf_from_int_items(&cn, &[])));
                }
                let s = start.max(0) as usize;
                let e = (end as usize).min(items.len());
                return Some(Ok(make_buf_from_int_items(&cn, &items[s..e])));
            }
            // subbuf(start) - from start to end
            let start = resolve_buf_index(arg, items.len());
            if start < 0 || start as usize > items.len() {
                return Some(Err(out_of_range_error(start, 0, items.len() as i64)));
            }
            Some(Ok(make_buf_from_int_items(&cn, &items[start as usize..])))
        }
        "isa" => {
            // Instance, Mixin(Instance), and Package values need interpreter
            // access for user-defined class hierarchies, role checks, and
            // subset type resolution, so fall through to the runtime handler.
            let needs_interpreter = match target {
                Value::Instance { .. } | Value::Package(_) => true,
                Value::Mixin(inner, _) => matches!(inner.as_ref(), Value::Instance { .. }),
                _ => false,
            };
            if needs_interpreter {
                return None;
            }
            let type_name = match arg {
                Value::Package(name) => name.resolve(),
                Value::Str(name) => name.to_string(),
                Value::Instance { class_name, .. } => class_name.resolve(),
                other => other.to_string_value(),
            };
            Some(Ok(Value::Bool(target.isa_check(&type_name))))
        }
        _ => None,
    }
}

fn is_buf_like(val: &Value) -> bool {
    if let Value::Instance { class_name, .. } = val {
        let cn = class_name.resolve();
        cn == "Buf"
            || cn == "Blob"
            || cn == "utf8"
            || cn == "utf16"
            || cn.starts_with("Buf[")
            || cn.starts_with("Blob[")
            || cn.starts_with("buf")
            || cn.starts_with("blob")
    } else {
        false
    }
}

fn buf_class_name(val: &Value) -> String {
    if let Value::Instance { class_name, .. } = val {
        class_name.resolve().to_string()
    } else {
        "Buf".to_string()
    }
}

fn buf_get_int_items(target: &Value) -> Option<Vec<Value>> {
    if let Value::Instance { attributes, .. } = target
        && let Some(Value::Array(items, ..)) = attributes.get("bytes")
    {
        Some(items.to_vec())
    } else {
        None
    }
}

fn make_buf_from_int_items(class_name: &str, items: &[Value]) -> Value {
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("bytes".to_string(), Value::array(items.to_vec()));
    Value::make_instance(Symbol::intern(class_name), attrs)
}

fn eval_whatever_code(sub_data: &std::sync::Arc<crate::value::SubData>, arg: i64) -> i64 {
    let param = sub_data
        .params
        .first()
        .map(|s: &String| s.as_str())
        .unwrap_or("_");
    let mut sub_env = sub_data.env.clone();
    sub_env.insert(param.to_string(), Value::Int(arg));
    let mut interpreter = crate::runtime::Interpreter::new();
    *interpreter.env_mut() = sub_env;
    if let Ok(result) = interpreter.eval_block_value(&sub_data.body) {
        match result {
            Value::Int(n) => n,
            Value::Num(f) => f as i64,
            _ => 0,
        }
    } else {
        0
    }
}

fn resolve_buf_index(arg: &Value, len: usize) -> i64 {
    match arg {
        Value::Int(n) => *n,
        Value::Num(f) => *f as i64,
        Value::Rat(n, d) => {
            if *d != 0 {
                *n / *d
            } else {
                0
            }
        }
        Value::Sub(data) => eval_whatever_code(data, len as i64),
        Value::Whatever => len as i64,
        _ => 0,
    }
}

fn resolve_buf_len(arg: &Value, total_len: usize, start: usize) -> i64 {
    match arg {
        Value::Int(n) => *n,
        Value::Num(f) => {
            if f.is_infinite() && *f > 0.0 {
                (total_len - start) as i64
            } else {
                *f as i64
            }
        }
        Value::Rat(n, d) => {
            if *d != 0 {
                *n / *d
            } else {
                0
            }
        }
        Value::Whatever => (total_len - start) as i64,
        Value::Sub(data) => {
            // WhateverCode receives total_len and returns an end index (inclusive).
            // Length = max(0, end_index - start + 1)
            let end_idx = eval_whatever_code(data, total_len as i64);
            let len = end_idx - start as i64 + 1;
            if len < 0 { 0 } else { len }
        }
        _ => 0,
    }
}

fn range_bounds(arg: &Value) -> Option<(i64, i64)> {
    match arg {
        Value::Range(start, end) => Some((*start, *end + 1)),
        Value::RangeExcl(start, end) => Some((*start, *end)),
        _ => None,
    }
}

fn out_of_range_error(got: i64, min: i64, max: i64) -> RuntimeError {
    let mut attrs = std::collections::HashMap::new();
    let msg = format!(
        "Index out of range. Is: {}, should be in {}..{}",
        got, min, max
    );
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    attrs.insert("got".to_string(), Value::Int(got));
    attrs.insert("range".to_string(), Value::str(format!("{}..{}", min, max)));
    let ex = Value::make_instance(Symbol::intern("X::OutOfRange"), attrs);
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

/// Extract byte array from a Buf/Blob instance.
fn buf_get_bytes(target: &Value) -> Option<Vec<u8>> {
    if let Value::Instance {
        class_name,
        attributes,
        ..
    } = target
        && crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve())
        && let Some(Value::Array(items, ..)) = attributes.get("bytes")
    {
        return Some(
            items
                .iter()
                .map(|v| match v {
                    Value::Int(i) => *i as u8,
                    _ => 0,
                })
                .collect(),
        );
    }
    None
}

fn to_int_val(v: &Value) -> i64 {
    match v {
        Value::Int(i) => *i,
        Value::Num(f) => *f as i64,
        _ => 0,
    }
}

fn read_ubits_from_bytes(bytes: &[u8], from: usize, bits: usize) -> BigInt {
    let mut acc = BigInt::ZERO;
    for i in 0..bits {
        let bit_index = from + i;
        let byte = bytes[bit_index / 8];
        let bit = (byte >> (7 - (bit_index % 8))) & 1;
        acc = (acc << 1) + BigInt::from(bit);
    }
    acc
}

fn bigint_to_value(value: BigInt) -> Value {
    if let Some(i) = value.to_i64() {
        Value::Int(i)
    } else {
        Value::bigint(value)
    }
}

fn read_f32_ne(bytes: &[u8]) -> f64 {
    let arr: [u8; 4] = [bytes[0], bytes[1], bytes[2], bytes[3]];
    f32::from_ne_bytes(arr) as f64
}

fn read_f64_ne(bytes: &[u8]) -> f64 {
    let arr: [u8; 8] = [
        bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
    ];
    f64::from_ne_bytes(arr)
}

fn read_f32_endian(bytes: &[u8], endian_val: i64) -> f64 {
    let arr: [u8; 4] = [bytes[0], bytes[1], bytes[2], bytes[3]];
    match endian_val {
        1 => f32::from_le_bytes(arr) as f64, // LittleEndian
        2 => f32::from_be_bytes(arr) as f64, // BigEndian
        _ => f32::from_ne_bytes(arr) as f64, // NativeEndian
    }
}

fn read_f64_endian(bytes: &[u8], endian_val: i64) -> f64 {
    let arr: [u8; 8] = [
        bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
    ];
    match endian_val {
        1 => f64::from_le_bytes(arr), // LittleEndian
        2 => f64::from_be_bytes(arr), // BigEndian
        _ => f64::from_ne_bytes(arr), // NativeEndian
    }
}

// ── 2-arg method dispatch ────────────────────────────────────────────
/// Try to dispatch a 2-argument method call on a Value.
pub(crate) fn native_method_2arg(
    target: &Value,
    method_sym: Symbol,
    arg1: &Value,
    arg2: &Value,
) -> Option<Result<Value, RuntimeError>> {
    let method = method_sym.resolve();
    let method = method.as_str();

    if let Value::Scalar(inner) = target {
        return native_method_2arg(inner, method_sym, arg1, arg2);
    }
    if method == "flat" {
        let (depth, hammer) = if let Some(depth) = parse_flat_depth(arg1) {
            (Some(depth), is_hammer_pair(arg2))
        } else if let Some(depth) = parse_flat_depth(arg2) {
            (Some(depth), is_hammer_pair(arg1))
        } else {
            (None, false)
        };
        if let Some(depth) = depth {
            if hammer {
                return Some(Ok(flatten_target(target, Some(depth), true)));
            }
            return Some(Ok(flatten_target(target, Some(depth), false)));
        }
        return None;
    }

    match method {
        "expmod" => Some(crate::builtins::expmod(target, arg1, arg2)),
        "fmt" => {
            if !fmt_joinable_target(target) {
                return Some(Err(RuntimeError::new(
                    "Too many positionals passed; expected 1 or 2 arguments but got 3",
                )));
            }
            let fmt = arg1.to_string_value();
            let sep = arg2.to_string_value();
            let rendered = runtime::value_to_list(target)
                .into_iter()
                .map(|item| runtime::format_sprintf(&fmt, Some(&item)))
                .collect::<Vec<_>>()
                .join(&sep);
            Some(Ok(Value::str(rendered)))
        }
        "substr" => {
            let start = match arg1 {
                Value::Int(i) if *i >= 0 => *i as usize,
                Value::Int(_) => return None, // negative: let runtime handle
                _ => return None,
            };
            let len = match arg2 {
                Value::Int(i) if *i >= 0 => *i as usize,
                Value::Int(_) => return None,
                _ => return None,
            };
            let s = target.to_string_value();
            let chars: Vec<char> = s.chars().collect();
            if start > chars.len() {
                return None; // out-of-range: let runtime handle (returns Failure)
            }
            let end = (start + len).min(chars.len());
            Some(Ok(Value::str(chars[start..end].iter().collect())))
        }
        "base" => {
            let radix = match arg1 {
                Value::Int(r) if (2..=36).contains(r) => *r as u32,
                Value::Str(s) => match s.parse::<u32>() {
                    Ok(r) if (2..=36).contains(&r) => r,
                    _ => {
                        return Some(Err(RuntimeError::new(
                            "X::OutOfRange: base requires radix 2..36",
                        )));
                    }
                },
                _ => {
                    return Some(Err(RuntimeError::new(
                        "X::OutOfRange: base requires radix 2..36",
                    )));
                }
            };
            let digits_mode = match arg2 {
                Value::Int(d) if *d < 0 => {
                    return Some(Err(RuntimeError::new(
                        "X::OutOfRange: digits must be non-negative",
                    )));
                }
                Value::Int(d) => BaseDigits::Fixed(*d as u32),
                Value::Whatever => BaseDigits::Whatever,
                _ => None?,
            };
            match target {
                Value::Int(i) => Some(Ok(Value::str(rat_to_base(*i, 1, radix, digits_mode)))),
                Value::Num(f) => {
                    let (n, d) = f64_to_rat(*f);
                    Some(Ok(Value::str(rat_to_base(n, d, radix, digits_mode))))
                }
                Value::Rat(n, d) | Value::FatRat(n, d) => {
                    Some(Ok(Value::str(rat_to_base(*n, *d, radix, digits_mode))))
                }
                Value::Instance { attributes, .. } => {
                    if let Some(val) = attributes.get("value") {
                        match val {
                            Value::Int(i) => {
                                Some(Ok(Value::str(rat_to_base(*i, 1, radix, digits_mode))))
                            }
                            Value::Rat(n, d) | Value::FatRat(n, d) => {
                                Some(Ok(Value::str(rat_to_base(*n, *d, radix, digits_mode))))
                            }
                            Value::Num(f) => {
                                let (n, d) = f64_to_rat(*f);
                                Some(Ok(Value::str(rat_to_base(n, d, radix, digits_mode))))
                            }
                            _ => None,
                        }
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }
        "read-ubits" | "read-bits" => {
            let bytes = buf_get_bytes(target)?;
            let from = runtime::to_int(arg1);
            let bits = runtime::to_int(arg2);
            if from < 0 || bits < 0 {
                return Some(Err(RuntimeError::new(
                    "bit offset/length must be non-negative",
                )));
            }
            let from = from as usize;
            let bits = bits as usize;
            let total_bits = bytes.len().saturating_mul(8);
            if from.checked_add(bits).is_none_or(|end| end > total_bits) {
                return Some(Err(RuntimeError::new(format!(
                    "read from out of range. Is: {}, should be in 0..{}",
                    from, total_bits
                ))));
            }
            let unsigned = read_ubits_from_bytes(&bytes, from, bits);
            if method == "read-ubits" || bits == 0 {
                return Some(Ok(bigint_to_value(unsigned)));
            }
            let sign_bit = BigInt::from(1u8) << (bits - 1);
            let signed = if (&unsigned & &sign_bit).is_zero() {
                unsigned
            } else {
                unsigned - (BigInt::from(1u8) << bits)
            };
            Some(Ok(bigint_to_value(signed)))
        }
        // Buf/Blob read-num methods (2 args: offset + endian)
        "read-num32" | "read-num64" => {
            if let Value::Package(type_name) = target {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller {}({}:U)",
                    method, type_name,
                ))));
            }
            let bytes = buf_get_bytes(target)?;
            let offset_i64 = to_int_val(arg1);
            let endian_val = match arg2 {
                Value::Enum { value, .. } => *value,
                Value::Int(i) => *i,
                _ => 0, // NativeEndian
            };
            let size: usize = if method == "read-num32" { 4 } else { 8 };
            if offset_i64 < 0
                || (offset_i64 as usize)
                    .checked_add(size)
                    .is_none_or(|end| end > bytes.len())
            {
                return Some(Err(RuntimeError::new(format!(
                    "read from out of range. Is: {}, should be in 0..{}",
                    offset_i64,
                    bytes.len()
                ))));
            }
            let offset = offset_i64 as usize;
            let result = if size == 4 {
                read_f32_endian(&bytes[offset..offset + 4], endian_val)
            } else {
                read_f64_endian(&bytes[offset..offset + 8], endian_val)
            };
            Some(Ok(Value::Num(result)))
        }
        "subbuf" => {
            if !is_buf_like(target) {
                return None;
            }
            let items = buf_get_int_items(target)?;
            let cn = buf_class_name(target);
            let len = items.len();
            let start = resolve_buf_index(arg1, len);
            if start < 0 {
                return Some(Err(out_of_range_error(start, 0, len as i64)));
            }
            if start as usize > len {
                return Some(Err(out_of_range_error(start, 0, len as i64)));
            }
            // arg2 is the length
            let sub_len = resolve_buf_len(arg2, len, start as usize);
            if sub_len < 0 {
                return Some(Err(out_of_range_error(sub_len, 0, len as i64)));
            }
            let s = start as usize;
            let available = len - s;
            let take = (sub_len as usize).min(available);
            Some(Ok(make_buf_from_int_items(&cn, &items[s..s + take])))
        }
        "subbuf-rw" => {
            if !is_buf_like(target) {
                return None;
            }
            let items = buf_get_int_items(target)?;
            let cn = buf_class_name(target);
            let len = items.len();
            let start = resolve_buf_index(arg1, len);
            if start < 0 || start as usize > len {
                return Some(Err(out_of_range_error(start, 0, len as i64)));
            }
            let sub_len = resolve_buf_len(arg2, len, start as usize);
            if sub_len < 0 {
                return Some(Err(out_of_range_error(sub_len, 0, len as i64)));
            }
            let s = start as usize;
            let available = len - s;
            let take = (sub_len as usize).min(available);
            Some(Ok(make_buf_from_int_items(&cn, &items[s..s + take])))
        }
        _ => None,
    }
}

fn parse_radix_checked(arg: &Value) -> Option<Result<u32, RuntimeError>> {
    match arg {
        Value::Int(r) => {
            if (2..=36).contains(r) {
                Some(Ok(*r as u32))
            } else {
                Some(Err(RuntimeError::new(
                    "X::OutOfRange: base requires radix 2..36",
                )))
            }
        }
        Value::Str(s) => match s.parse::<u32>() {
            Ok(r) if (2..=36).contains(&r) => Some(Ok(r)),
            Ok(_) => Some(Err(RuntimeError::new(
                "X::OutOfRange: base requires radix 2..36",
            ))),
            _ => None,
        },
        _ => None,
    }
}

/// Specifies how many fractional digits to produce in base conversion.
enum BaseDigits {
    /// Auto-detect: scale to denominator size, minimum 6 (default for 1-arg .base)
    Auto,
    /// Exact number of digits (from explicit integer argument)
    Fixed(u32),
    /// Whatever: produce digits until remainder is 0 (no rounding)
    Whatever,
}

/// Convert a rational number n/d to a string in the given base.
fn rat_to_base(n: i64, d: i64, radix: u32, digits_mode: BaseDigits) -> String {
    if d == 0 {
        return if n > 0 {
            "Inf".to_string()
        } else if n < 0 {
            "-Inf".to_string()
        } else {
            "NaN".to_string()
        };
    }
    let negative = (n < 0) != (d < 0);
    let mut num = (n as i128).unsigned_abs();
    let den = (d as i128).unsigned_abs();
    let radix = radix as u128;

    let int_part = num / den;
    num %= den;

    let mut result = if negative {
        "-".to_string()
    } else {
        String::new()
    };
    result.push_str(&int_to_base(int_part as u64, radix as u32));

    if num == 0 {
        if let BaseDigits::Fixed(digits) = digits_mode
            && digits > 0
        {
            result.push('.');
            for _ in 0..digits {
                result.push('0');
            }
        }
        return result;
    }

    // Whatever mode: produce exact digits until remainder is 0
    if matches!(digits_mode, BaseDigits::Whatever) {
        result.push('.');
        let digit_chars = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        // Safety limit to prevent infinite loops for non-terminating fractions
        for _ in 0..100000 {
            num *= radix;
            let digit = num / den;
            result.push(digit_chars[digit as usize] as char);
            num %= den;
            if num == 0 {
                break;
            }
        }
        return result;
    }

    let limit = match digits_mode {
        BaseDigits::Fixed(d) => d,
        BaseDigits::Auto => {
            // For Rat without explicit digits, scale to denominator size with minimum of 6
            let log_den = (den as f64).log(radix as f64).ceil() as u32;
            std::cmp::max(6, log_den)
        }
        BaseDigits::Whatever => unreachable!(),
    };
    if limit == 0 {
        // Round integer part: if fractional part >= 0.5, round up
        if num * 2 >= den {
            // Increment the integer part and rebuild
            let rounded_int = int_part + 1;
            let mut rounded_result = if negative {
                "-".to_string()
            } else {
                String::new()
            };
            rounded_result.push_str(&int_to_base(rounded_int as u64, radix as u32));
            return rounded_result;
        }
        return result;
    }

    result.push('.');
    let digit_chars = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    let mut frac_digits = Vec::new();
    let is_auto = matches!(digits_mode, BaseDigits::Auto);
    for _ in 0..limit {
        num *= radix;
        let digit = num / den;
        frac_digits.push(digit as u8);
        num %= den;
        if num == 0 && is_auto {
            break;
        }
    }
    // Round last digit if we have a remainder
    let mut int_carry = false;
    if num > 0 && !frac_digits.is_empty() && num * 2 >= den {
        let mut carry = true;
        for d in frac_digits.iter_mut().rev() {
            if carry {
                *d += 1;
                if *d >= radix as u8 {
                    *d = 0;
                } else {
                    carry = false;
                }
            }
        }
        if carry {
            // Carry propagated past all fractional digits (e.g., 0.9 -> 1.0)
            int_carry = true;
        }
    }
    if int_carry {
        // Rebuild result with incremented integer part
        let rounded_int = int_part + 1;
        result.clear();
        if negative {
            result.push('-');
        }
        result.push_str(&int_to_base(rounded_int as u64, radix as u32));
        result.push('.');
        for _ in 0..frac_digits.len() {
            result.push('0');
        }
    } else {
        for d in &frac_digits {
            result.push(digit_chars[*d as usize] as char);
        }
    }
    result
}

fn int_to_base(mut n: u64, radix: u32) -> String {
    if n == 0 {
        return "0".to_string();
    }
    let digits = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    let mut buf = Vec::new();
    while n > 0 {
        buf.push(digits[(n % radix as u64) as usize]);
        n /= radix as u64;
    }
    buf.reverse();
    String::from_utf8(buf).unwrap()
}

/// Convert f64 to a rational approximation (numerator, denominator).
fn f64_to_rat(f: f64) -> (i64, i64) {
    if f.is_nan() {
        return (0, 0);
    }
    if f.is_infinite() {
        return if f > 0.0 { (1, 0) } else { (-1, 0) };
    }
    // Multiply by increasing powers of 10 to find exact fraction
    let negative = f < 0.0;
    let f = f.abs();
    let mut den: i64 = 1;
    let mut num = f;
    for _ in 0..18 {
        if (num - num.round()).abs() < 1e-10 {
            break;
        }
        num *= 10.0;
        den *= 10;
    }
    let n = num.round() as i64;
    // Simplify
    let g = gcd_u64(n.unsigned_abs(), den.unsigned_abs());
    let n = n / g as i64;
    let d = den / g as i64;
    if negative { (-n, d) } else { (n, d) }
}

fn gcd_u64(mut a: u64, mut b: u64) -> u64 {
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

/// Compute base-repeating for a rational number.
/// Returns (non_repeating_part, repeating_part) as strings.
fn rat_base_repeating(n: i64, d: i64, radix: u32) -> (String, String) {
    use std::collections::HashMap;

    if d == 0 {
        return (
            if n > 0 {
                "Inf".to_string()
            } else if n < 0 {
                "-Inf".to_string()
            } else {
                "NaN".to_string()
            },
            String::new(),
        );
    }
    let negative = (n < 0) != (d < 0);
    let mut num = (n as i128).unsigned_abs();
    let den = (d as i128).unsigned_abs();
    let radix128 = radix as u128;

    let int_part = num / den;
    num %= den;

    let mut prefix = if negative {
        "-".to_string()
    } else {
        String::new()
    };
    prefix.push_str(&int_to_base(int_part as u64, radix));

    if num == 0 {
        return (prefix, String::new());
    }

    prefix.push('.');
    let digit_chars = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

    // Track remainders to find the cycle
    let mut remainder_positions: HashMap<u128, usize> = HashMap::new();
    let mut frac_digits = Vec::new();

    loop {
        if num == 0 {
            // Exact representation, no repeating part
            let non_rep: String = frac_digits
                .iter()
                .map(|&d| digit_chars[d] as char)
                .collect();
            return (format!("{}{}", prefix, non_rep), String::new());
        }
        if let Some(&pos) = remainder_positions.get(&num) {
            // Found a cycle
            let non_rep: String = frac_digits[..pos]
                .iter()
                .map(|&d| digit_chars[d] as char)
                .collect();
            let rep: String = frac_digits[pos..]
                .iter()
                .map(|&d| digit_chars[d] as char)
                .collect();
            return (format!("{}{}", prefix, non_rep), rep);
        }
        remainder_positions.insert(num, frac_digits.len());
        num *= radix128;
        let digit = (num / den) as usize;
        frac_digits.push(digit);
        num %= den;
    }
}
