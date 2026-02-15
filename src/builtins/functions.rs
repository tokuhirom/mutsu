#![allow(clippy::result_large_err)]

use crate::runtime;
use crate::value::{RuntimeError, Value};
use unicode_segmentation::UnicodeSegmentation;

use super::rng::{builtin_rand, builtin_srand, builtin_srand_auto};
use super::unicode::{titlecase_string, unicode_char_name};

// ── Built-in function dispatch ───────────────────────────────────────
/// Try to dispatch a built-in function call.
pub(crate) fn native_function(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match args.len() {
        0 => native_function_0arg(name),
        1 => native_function_1arg(name, &args[0]),
        2 => native_function_2arg(name, &args[0], &args[1]),
        3 => native_function_3arg(name, &args[0], &args[1], &args[2]),
        _ => native_function_variadic(name, args),
    }
}

fn native_function_0arg(name: &str) -> Option<Result<Value, RuntimeError>> {
    match name {
        "rand" => Some(Ok(Value::Num(builtin_rand()))),
        "srand" => {
            builtin_srand_auto();
            Some(Ok(Value::Nil))
        }
        _ => None,
    }
}

fn native_function_1arg(name: &str, arg: &Value) -> Option<Result<Value, RuntimeError>> {
    match name {
        "srand" => {
            let seed = match arg {
                Value::Int(n) => *n as u64,
                Value::Num(n) => *n as u64,
                _ => arg.to_string_value().parse::<u64>().unwrap_or(0),
            };
            builtin_srand(seed);
            Some(Ok(Value::Nil))
        }
        "uc" => Some(Ok(Value::Str(arg.to_string_value().to_uppercase()))),
        "lc" => Some(Ok(Value::Str(arg.to_string_value().to_lowercase()))),
        "tc" => Some(Ok(Value::Str(titlecase_string(&arg.to_string_value())))),
        "tclc" => Some(Ok(Value::Str(crate::value::tclc_str(
            &arg.to_string_value(),
        )))),
        "wordcase" => Some(Ok(Value::Str(crate::value::wordcase_str(
            &arg.to_string_value(),
        )))),
        "chomp" => Some(Ok(Value::Str(
            arg.to_string_value().trim_end_matches('\n').to_string(),
        ))),
        "chop" => {
            // Type objects (Package) should throw
            if let Value::Package(type_name) = arg {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller chop({}:U)",
                    type_name,
                ))));
            }
            let mut s = arg.to_string_value();
            s.pop();
            Some(Ok(Value::Str(s)))
        }
        "trim" => Some(Ok(Value::Str(arg.to_string_value().trim().to_string()))),
        "trim-leading" => Some(Ok(Value::Str(
            arg.to_string_value().trim_start().to_string(),
        ))),
        "trim-trailing" => Some(Ok(Value::Str(arg.to_string_value().trim_end().to_string()))),
        "flip" => Some(Ok(Value::Str(
            arg.to_string_value().chars().rev().collect(),
        ))),
        "words" => {
            let s = arg.to_string_value();
            let parts: Vec<Value> = s
                .split_whitespace()
                .map(|p| Value::Str(p.to_string()))
                .collect();
            Some(Ok(Value::Array(parts)))
        }
        "chars" => Some(Ok(Value::Int(
            arg.to_string_value().graphemes(true).count() as i64,
        ))),
        "chr" => {
            if let Value::Int(i) = arg
                && *i >= 0
                && let Some(ch) = std::char::from_u32(*i as u32)
            {
                return Some(Ok(Value::Str(ch.to_string())));
            }
            Some(Ok(Value::Str(String::new())))
        }
        "ord" => {
            if let Some(ch) = arg.to_string_value().chars().next() {
                Some(Ok(Value::Int(ch as u32 as i64)))
            } else {
                Some(Ok(Value::Nil))
            }
        }
        "uniname" => {
            let s = arg.to_string_value();
            if s.is_empty() {
                return Some(Ok(Value::Nil));
            }
            if let Some(ch) = s.chars().next() {
                let name = unicode_char_name(ch);
                Some(Ok(Value::Str(name)))
            } else {
                Some(Ok(Value::Nil))
            }
        }
        "abs" => Some(Ok(match arg {
            Value::Int(i) => Value::Int(i.abs()),
            Value::Num(f) => Value::Num(f.abs()),
            _ => Value::Int(0),
        })),
        "sqrt" => Some(Ok(match arg {
            Value::Int(i) => Value::Num((*i as f64).sqrt()),
            Value::Num(f) => Value::Num(f.sqrt()),
            _ => Value::Num(f64::NAN),
        })),
        "floor" => Some(Ok(match arg {
            Value::Num(f) if f.is_nan() || f.is_infinite() => Value::Num(*f),
            Value::Num(f) => Value::Int(f.floor() as i64),
            Value::Int(i) => Value::Int(*i),
            _ => Value::Int(0),
        })),
        "ceiling" | "ceil" => Some(Ok(match arg {
            Value::Num(f) if f.is_nan() || f.is_infinite() => Value::Num(*f),
            Value::Num(f) => Value::Int(f.ceil() as i64),
            Value::Int(i) => Value::Int(*i),
            _ => Value::Int(0),
        })),
        "round" => Some(Ok(match arg {
            Value::Num(f) if f.is_nan() || f.is_infinite() => Value::Num(*f),
            Value::Num(f) => Value::Int(f.round() as i64),
            Value::Int(i) => Value::Int(*i),
            _ => Value::Int(0),
        })),
        "exp" => Some(Ok(match arg {
            Value::Int(i) => Value::Num((*i as f64).exp()),
            Value::Num(f) => Value::Num(f.exp()),
            _ => Value::Num(f64::NAN),
        })),
        "log" => {
            let x = runtime::to_float_value(arg).unwrap_or(f64::NAN);
            Some(Ok(Value::Num(x.ln())))
        }
        "sin" | "cos" | "tan" | "asin" | "acos" | "atan" => {
            let x = runtime::to_float_value(arg).unwrap_or(0.0);
            let result = match name {
                "sin" => x.sin(),
                "cos" => x.cos(),
                "tan" => x.tan(),
                "asin" => x.asin(),
                "acos" => x.acos(),
                "atan" => x.atan(),
                _ => 0.0,
            };
            Some(Ok(Value::Num(result)))
        }
        "truncate" => {
            if let Some(num) = runtime::to_float_value(arg) {
                if num.is_nan() || num.is_infinite() {
                    Some(Ok(Value::Num(num)))
                } else {
                    Some(Ok(Value::Int(num.trunc() as i64)))
                }
            } else {
                Some(Ok(Value::Int(runtime::to_int(arg))))
            }
        }
        "defined" => Some(Ok(Value::Bool(match arg {
            Value::Nil | Value::Package(_) => false,
            Value::Slip(items) if items.is_empty() => false,
            _ => true,
        }))),
        "elems" => match arg {
            Value::Array(items) => Some(Ok(Value::Int(items.len() as i64))),
            Value::Hash(items) => Some(Ok(Value::Int(items.len() as i64))),
            Value::Str(s) => Some(Ok(Value::Int(s.chars().count() as i64))),
            Value::LazyList(_) => None,
            _ => Some(Ok(Value::Int(0))),
        },
        "reverse" => Some(Ok(match arg {
            Value::Array(items) => {
                let mut reversed = items.clone();
                reversed.reverse();
                Value::Array(reversed)
            }
            Value::Str(s) => Value::Str(s.chars().rev().collect()),
            _ => Value::Nil,
        })),
        "sort" => Some(Ok(match arg {
            Value::Array(items) => {
                let mut sorted = items.clone();
                sorted.sort_by_key(|a| a.to_string_value());
                Value::Array(sorted)
            }
            _ => Value::Nil,
        })),
        "flat" => Some(Ok(match arg {
            Value::Array(items) => {
                let mut flat = Vec::new();
                for item in items {
                    if let Value::Array(sub) = item {
                        flat.extend(sub.clone());
                    } else {
                        flat.push(item.clone());
                    }
                }
                Value::Array(flat)
            }
            other => Value::Array(vec![other.clone()]),
        })),
        "first" => Some(Ok(match arg {
            Value::Array(items) => items.first().cloned().unwrap_or(Value::Nil),
            _ => arg.clone(),
        })),
        "min" => Some(Ok(match arg {
            Value::Array(items) => items
                .iter()
                .cloned()
                .min_by(|a, b| match (a, b) {
                    (Value::Int(x), Value::Int(y)) => x.cmp(y),
                    _ => a.to_string_value().cmp(&b.to_string_value()),
                })
                .unwrap_or(Value::Nil),
            _ => arg.clone(),
        })),
        "max" => Some(Ok(match arg {
            Value::Array(items) => items
                .iter()
                .cloned()
                .max_by(|a, b| match (a, b) {
                    (Value::Int(x), Value::Int(y)) => x.cmp(y),
                    _ => a.to_string_value().cmp(&b.to_string_value()),
                })
                .unwrap_or(Value::Nil),
            _ => arg.clone(),
        })),
        "ords" => {
            let s = arg.to_string_value();
            let codes: Vec<Value> = s.chars().map(|ch| Value::Int(ch as u32 as i64)).collect();
            Some(Ok(Value::Array(codes)))
        }
        "gist" => Some(Ok(Value::Str(arg.to_string_value()))),
        _ => None,
    }
}

fn native_function_2arg(
    name: &str,
    arg1: &Value,
    arg2: &Value,
) -> Option<Result<Value, RuntimeError>> {
    match name {
        "chop" => {
            // Type objects (Package) should throw
            if let Value::Package(type_name) = arg1 {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller chop({}:U)",
                    type_name,
                ))));
            }
            let s = arg1.to_string_value();
            let n = match arg2 {
                Value::Int(i) => (*i).max(0) as usize,
                Value::BigInt(bi) => {
                    use num_traits::ToPrimitive;
                    bi.to_usize().unwrap_or(usize::MAX)
                }
                Value::Num(f) => (*f as i64).max(0) as usize,
                _ => arg2.to_string_value().parse::<usize>().unwrap_or(1),
            };
            let char_count = s.chars().count();
            let keep = char_count.saturating_sub(n);
            let result: String = s.chars().take(keep).collect();
            Some(Ok(Value::Str(result)))
        }
        "join" => {
            let sep = arg1.to_string_value();
            match arg2 {
                Value::Array(items) => {
                    let joined = items
                        .iter()
                        .map(|v| v.to_string_value())
                        .collect::<Vec<_>>()
                        .join(&sep);
                    Some(Ok(Value::Str(joined)))
                }
                _ => Some(Ok(Value::Str(String::new()))),
            }
        }
        "index" => {
            let s = arg1.to_string_value();
            let needle = arg2.to_string_value();
            Some(Ok(match s.find(&needle) {
                Some(pos) => Value::Int(s[..pos].chars().count() as i64),
                None => Value::Nil,
            }))
        }
        "substr" => {
            let s = arg1.to_string_value();
            let start = match arg2 {
                Value::Int(i) => (*i).max(0) as usize,
                _ => return None,
            };
            let chars: Vec<char> = s.chars().collect();
            Some(Ok(Value::Str(
                chars[start.min(chars.len())..].iter().collect(),
            )))
        }
        "atan2" => {
            let y = runtime::to_float_value(arg1).unwrap_or(0.0);
            let x = runtime::to_float_value(arg2).unwrap_or(0.0);
            Some(Ok(Value::Num(y.atan2(x))))
        }
        "log" => {
            let x = runtime::to_float_value(arg1).unwrap_or(f64::NAN);
            let base_val = runtime::to_float_value(arg2).unwrap_or(f64::NAN);
            if base_val.is_finite() && base_val > 0.0 && base_val != 1.0 && x > 0.0 {
                Some(Ok(Value::Num(x.ln() / base_val.ln())))
            } else {
                Some(Ok(Value::Num(f64::NAN)))
            }
        }
        "round" => {
            let x = runtime::to_float_value(arg1)?;
            let scale = runtime::to_float_value(arg2)?;
            if scale == 0.0 {
                Some(Ok(Value::Int(x.round() as i64)))
            } else {
                let factor = (1.0 / scale).abs();
                Some(Ok(Value::Num((x * factor).round() / factor)))
            }
        }
        "min" => {
            let cmp = match (arg1, arg2) {
                (Value::Int(x), Value::Int(y)) => x.cmp(y),
                _ => arg1.to_string_value().cmp(&arg2.to_string_value()),
            };
            Some(Ok(
                if cmp == std::cmp::Ordering::Less || cmp == std::cmp::Ordering::Equal {
                    arg1.clone()
                } else {
                    arg2.clone()
                },
            ))
        }
        "max" => {
            let cmp = match (arg1, arg2) {
                (Value::Int(x), Value::Int(y)) => x.cmp(y),
                _ => arg1.to_string_value().cmp(&arg2.to_string_value()),
            };
            Some(Ok(
                if cmp == std::cmp::Ordering::Greater || cmp == std::cmp::Ordering::Equal {
                    arg1.clone()
                } else {
                    arg2.clone()
                },
            ))
        }
        _ => None,
    }
}

fn native_function_3arg(
    name: &str,
    arg1: &Value,
    arg2: &Value,
    arg3: &Value,
) -> Option<Result<Value, RuntimeError>> {
    match name {
        "substr" => {
            let s = arg1.to_string_value();
            let start = match arg2 {
                Value::Int(i) => (*i).max(0) as usize,
                _ => return None,
            };
            let len = match arg3 {
                Value::Int(i) => (*i).max(0) as usize,
                _ => return None,
            };
            let chars: Vec<char> = s.chars().collect();
            let start = start.min(chars.len());
            let end = (start + len).min(chars.len());
            Some(Ok(Value::Str(chars[start..end].iter().collect())))
        }
        _ => None,
    }
}

fn native_function_variadic(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "min" => {
            if args.is_empty() {
                return Some(Ok(Value::Nil));
            }
            Some(Ok(args
                .iter()
                .cloned()
                .min_by(|a, b| match (a, b) {
                    (Value::Int(x), Value::Int(y)) => x.cmp(y),
                    _ => a.to_string_value().cmp(&b.to_string_value()),
                })
                .unwrap_or(Value::Nil)))
        }
        "max" => {
            if args.is_empty() {
                return Some(Ok(Value::Nil));
            }
            Some(Ok(args
                .iter()
                .cloned()
                .max_by(|a, b| match (a, b) {
                    (Value::Int(x), Value::Int(y)) => x.cmp(y),
                    _ => a.to_string_value().cmp(&b.to_string_value()),
                })
                .unwrap_or(Value::Nil)))
        }
        "chrs" => {
            let mut result = String::new();
            for arg in args {
                match arg {
                    Value::Int(i) => {
                        if *i >= 0
                            && let Some(ch) = std::char::from_u32(*i as u32)
                        {
                            result.push(ch);
                            continue;
                        }
                        result.push_str(&arg.to_string_value());
                    }
                    Value::Array(items) => {
                        for item in items {
                            if let Value::Int(i) = item
                                && *i >= 0
                                && let Some(ch) = std::char::from_u32(*i as u32)
                            {
                                result.push(ch);
                                continue;
                            }
                            result.push_str(&item.to_string_value());
                        }
                    }
                    _ => result.push_str(&arg.to_string_value()),
                }
            }
            Some(Ok(Value::Str(result)))
        }
        "flat" => {
            let mut result = Vec::new();
            for arg in args {
                match arg {
                    Value::Array(items) => {
                        for item in items {
                            if let Value::Array(sub) = item {
                                result.extend(sub.clone());
                            } else {
                                result.push(item.clone());
                            }
                        }
                    }
                    other => result.push(other.clone()),
                }
            }
            Some(Ok(Value::Array(result)))
        }
        _ => None,
    }
}
