#![allow(clippy::result_large_err)]

use crate::interpreter::Interpreter;
use crate::value::{RuntimeError, Value, make_rat};

// ── 0-arg method dispatch ────────────────────────────────────────────
/// Try to dispatch a 0-argument method call on a Value.
/// Returns `Some(Ok(..))` / `Some(Err(..))` when handled, `None` to fall through.
pub(crate) fn native_method_0arg(
    target: &Value,
    method: &str,
) -> Option<Result<Value, RuntimeError>> {
    match method {
        "defined" => Some(Ok(Value::Bool(!matches!(
            target,
            Value::Nil | Value::Package(_)
        )))),
        "Bool" => Some(Ok(Value::Bool(target.truthy()))),
        "Str" => match target {
            Value::Package(_) | Value::Instance { .. } => None,
            Value::Str(s) if s == "IO::Special" => Some(Ok(Value::Str(String::new()))),
            _ => Some(Ok(Value::Str(target.to_string_value()))),
        },
        "Int" => {
            let result = match target {
                Value::Int(i) => Value::Int(*i),
                Value::Num(f) => Value::Int(*f as i64),
                Value::Rat(n, d) if *d != 0 => Value::Int(*n / *d),
                Value::Str(s) => {
                    if let Ok(i) = s.trim().parse::<i64>() {
                        Value::Int(i)
                    } else if let Ok(f) = s.trim().parse::<f64>() {
                        Value::Int(f as i64)
                    } else {
                        return None;
                    }
                }
                Value::Bool(b) => Value::Int(if *b { 1 } else { 0 }),
                _ => return None,
            };
            Some(Ok(result))
        }
        "Numeric" | "Num" => {
            let result = match target {
                Value::Int(i) => Value::Int(*i),
                Value::Num(f) => Value::Num(*f),
                Value::Rat(n, d) if *d != 0 => Value::Num(*n as f64 / *d as f64),
                Value::Str(s) => {
                    if let Ok(i) = s.trim().parse::<i64>() {
                        Value::Int(i)
                    } else if let Ok(f) = s.trim().parse::<f64>() {
                        Value::Num(f)
                    } else {
                        return None;
                    }
                }
                Value::Bool(b) => Value::Int(if *b { 1 } else { 0 }),
                _ => return None,
            };
            Some(Ok(result))
        }
        "chars" => Some(Ok(Value::Int(
            target.to_string_value().chars().count() as i64
        ))),
        "elems" => {
            let result = match target {
                Value::Array(items) => Value::Int(items.len() as i64),
                Value::Hash(items) => Value::Int(items.len() as i64),
                Value::Set(items) => Value::Int(items.len() as i64),
                Value::Bag(items) => Value::Int(items.len() as i64),
                Value::Mix(items) => Value::Int(items.len() as i64),
                _ => return None,
            };
            Some(Ok(result))
        }
        "Complex-i" => {
            let imag = match target {
                Value::Int(i) => *i as f64,
                Value::Num(f) => *f,
                _ => 0.0,
            };
            Some(Ok(Value::Complex(0.0, imag)))
        }
        "abs" => {
            let result = match target {
                Value::Int(i) => Value::Int(i.abs()),
                Value::Num(f) => Value::Num(f.abs()),
                Value::Rat(n, d) => Value::Rat(n.abs(), *d),
                _ => return None,
            };
            Some(Ok(result))
        }
        "uc" => Some(Ok(Value::Str(target.to_string_value().to_uppercase()))),
        "lc" => Some(Ok(Value::Str(target.to_string_value().to_lowercase()))),
        "tc" => {
            let s = target.to_string_value();
            let mut result = String::new();
            let mut capitalize = true;
            for ch in s.chars() {
                if capitalize {
                    for c in ch.to_uppercase() {
                        result.push(c);
                    }
                    capitalize = false;
                } else {
                    result.push(ch);
                }
            }
            Some(Ok(Value::Str(result)))
        }
        "sign" => {
            let result = match target {
                Value::Int(i) => Value::Int(i.signum()),
                Value::Num(f) => {
                    if f.is_nan() {
                        Value::Num(f64::NAN)
                    } else {
                        Value::Int(if *f > 0.0 {
                            1
                        } else if *f < 0.0 {
                            -1
                        } else {
                            0
                        })
                    }
                }
                _ => return None,
            };
            Some(Ok(result))
        }
        "end" => match target {
            Value::Array(items) => Some(Ok(Value::Int(items.len() as i64 - 1))),
            _ => None,
        },
        "flat" => match target {
            Value::Array(items) => {
                let mut result = Vec::new();
                for item in items {
                    match item {
                        Value::Array(inner) => result.extend(inner.iter().cloned()),
                        other => result.push(other.clone()),
                    }
                }
                Some(Ok(Value::Array(result)))
            }
            _ => None,
        },
        "sort" => match target {
            Value::Array(items) => {
                let mut sorted = items.clone();
                sorted.sort_by_key(|a| a.to_string_value());
                Some(Ok(Value::Array(sorted)))
            }
            _ => None,
        },
        "reverse" => match target {
            Value::Array(items) => {
                let mut reversed = items.clone();
                reversed.reverse();
                Some(Ok(Value::Array(reversed)))
            }
            Value::Str(s) => Some(Ok(Value::Str(s.chars().rev().collect()))),
            _ => None,
        },
        "unique" => match target {
            Value::Array(items) => {
                let mut seen = Vec::new();
                let mut result = Vec::new();
                for item in items {
                    let key = item.to_string_value();
                    if !seen.contains(&key) {
                        seen.push(key);
                        result.push(item.clone());
                    }
                }
                Some(Ok(Value::Array(result)))
            }
            _ => None,
        },
        "keys" => match target {
            Value::Hash(map) => {
                let keys: Vec<Value> = map.keys().map(|k| Value::Str(k.clone())).collect();
                Some(Ok(Value::Array(keys)))
            }
            _ => None,
        },
        "values" => match target {
            Value::Hash(map) => {
                let values: Vec<Value> = map.values().cloned().collect();
                Some(Ok(Value::Array(values)))
            }
            _ => None,
        },
        "floor" => match target {
            Value::Num(f) if f.is_nan() || f.is_infinite() => Some(Ok(Value::Num(*f))),
            Value::Num(f) => Some(Ok(Value::Int(f.floor() as i64))),
            Value::Int(i) => Some(Ok(Value::Int(*i))),
            Value::Rat(n, d) if *d != 0 => {
                let q = *n / *d;
                let r = *n % *d;
                if r != 0 && (*n < 0) != (*d < 0) {
                    Some(Ok(Value::Int(q - 1)))
                } else {
                    Some(Ok(Value::Int(q)))
                }
            }
            _ => None,
        },
        "ceiling" | "ceil" => match target {
            Value::Num(f) if f.is_nan() || f.is_infinite() => Some(Ok(Value::Num(*f))),
            Value::Num(f) => Some(Ok(Value::Int(f.ceil() as i64))),
            Value::Int(i) => Some(Ok(Value::Int(*i))),
            Value::Rat(n, d) if *d != 0 => {
                let q = *n / *d;
                let r = *n % *d;
                if r != 0 && (*n < 0) == (*d < 0) {
                    Some(Ok(Value::Int(q + 1)))
                } else {
                    Some(Ok(Value::Int(q)))
                }
            }
            _ => None,
        },
        "round" => match target {
            Value::Num(f) if f.is_nan() || f.is_infinite() => Some(Ok(Value::Num(*f))),
            Value::Num(f) => Some(Ok(Value::Int(f.round() as i64))),
            Value::Int(i) => Some(Ok(Value::Int(*i))),
            _ => None,
        },
        "sqrt" => match target {
            Value::Int(i) => Some(Ok(Value::Num((*i as f64).sqrt()))),
            Value::Num(f) => Some(Ok(Value::Num(f.sqrt()))),
            _ => None,
        },
        "words" => {
            let s = target.to_string_value();
            let words: Vec<Value> = s
                .split_whitespace()
                .map(|w| Value::Str(w.to_string()))
                .collect();
            Some(Ok(Value::Array(words)))
        }
        "lines" => {
            let s = target.to_string_value();
            let lines: Vec<Value> = s.lines().map(|l| Value::Str(l.to_string())).collect();
            Some(Ok(Value::Array(lines)))
        }
        "trim" => Some(Ok(Value::Str(target.to_string_value().trim().to_string()))),
        "trim-leading" => Some(Ok(Value::Str(
            target.to_string_value().trim_start().to_string(),
        ))),
        "trim-trailing" => Some(Ok(Value::Str(
            target.to_string_value().trim_end().to_string(),
        ))),
        "flip" => Some(Ok(Value::Str(
            target.to_string_value().chars().rev().collect(),
        ))),
        "so" => Some(Ok(Value::Bool(target.truthy()))),
        "not" => Some(Ok(Value::Bool(!target.truthy()))),
        "chomp" => Some(Ok(Value::Str(
            target.to_string_value().trim_end_matches('\n').to_string(),
        ))),
        "chop" => {
            // Type objects (Package) should throw
            if let Value::Package(type_name) = target {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller chop({}:U)",
                    type_name,
                ))));
            }
            let mut s = target.to_string_value();
            s.pop();
            Some(Ok(Value::Str(s)))
        }
        "comb" => {
            let s = target.to_string_value();
            let parts: Vec<Value> = s.chars().map(|c| Value::Str(c.to_string())).collect();
            Some(Ok(Value::Array(parts)))
        }
        "gist" | "raku" | "perl" => match target {
            Value::Nil => Some(Ok(Value::Str("(Any)".to_string()))),
            Value::Rat(n, d) => {
                if *d == 0 {
                    if *n == 0 {
                        Some(Ok(Value::Str("NaN".to_string())))
                    } else if *n > 0 {
                        Some(Ok(Value::Str("Inf".to_string())))
                    } else {
                        Some(Ok(Value::Str("-Inf".to_string())))
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
                            Some(Ok(Value::Str(s)))
                        } else {
                            Some(Ok(Value::Str(format!("{}.0", val))))
                        }
                    } else {
                        Some(Ok(Value::Str(format!("<{}/{}>", n, d))))
                    }
                }
            }
            Value::Package(_) | Value::Instance { .. } | Value::Enum { .. } => None,
            Value::Version {
                parts, plus, minus, ..
            } => {
                let s = Value::version_parts_to_string(parts);
                let suffix = if *plus {
                    "+"
                } else if *minus {
                    "-"
                } else {
                    ""
                };
                Some(Ok(Value::Str(format!("v{}{}", s, suffix))))
            }
            _ => Some(Ok(Value::Str(target.to_string_value()))),
        },
        "head" => match target {
            Value::Array(items) => Some(Ok(items.first().cloned().unwrap_or(Value::Nil))),
            _ => None,
        },
        "tail" => match target {
            Value::Array(items) => Some(Ok(items.last().cloned().unwrap_or(Value::Nil))),
            _ => None,
        },
        "first" => match target {
            Value::Array(items) => Some(Ok(items.first().cloned().unwrap_or(Value::Nil))),
            _ => None,
        },
        "min" => match target {
            Value::Array(items) => Some(Ok(items
                .iter()
                .cloned()
                .min_by(|a, b| match (a, b) {
                    (Value::Int(x), Value::Int(y)) => x.cmp(y),
                    _ => a.to_string_value().cmp(&b.to_string_value()),
                })
                .unwrap_or(Value::Nil))),
            _ => Some(Ok(target.clone())),
        },
        "max" => match target {
            Value::Array(items) => Some(Ok(items
                .iter()
                .cloned()
                .max_by(|a, b| match (a, b) {
                    (Value::Int(x), Value::Int(y)) => x.cmp(y),
                    _ => a.to_string_value().cmp(&b.to_string_value()),
                })
                .unwrap_or(Value::Nil))),
            _ => Some(Ok(target.clone())),
        },
        "tclc" => Some(Ok(Value::Str(crate::value::tclc_str(
            &target.to_string_value(),
        )))),
        "wordcase" => Some(Ok(Value::Str(crate::value::wordcase_str(
            &target.to_string_value(),
        )))),
        "succ" => match target {
            Value::Enum { .. } | Value::Instance { .. } => None,
            Value::Int(i) => Some(Ok(Value::Int(i + 1))),
            Value::Str(s) => {
                if s.is_empty() {
                    Some(Ok(Value::Str(String::new())))
                } else {
                    let mut chars: Vec<char> = s.chars().collect();
                    if let Some(last) = chars.last_mut() {
                        *last = char::from_u32(*last as u32 + 1).unwrap_or(*last);
                    }
                    Some(Ok(Value::Str(chars.into_iter().collect())))
                }
            }
            _ => Some(Ok(target.clone())),
        },
        "pred" => match target {
            Value::Enum { .. } | Value::Instance { .. } => None,
            Value::Int(i) => Some(Ok(Value::Int(i - 1))),
            _ => Some(Ok(target.clone())),
        },
        "log" => match target {
            Value::Int(i) => Some(Ok(Value::Num((*i as f64).ln()))),
            Value::Num(f) => Some(Ok(Value::Num(f.ln()))),
            _ => Some(Ok(Value::Num(f64::NAN))),
        },
        "exp" => match target {
            Value::Int(i) => Some(Ok(Value::Num((*i as f64).exp()))),
            Value::Num(f) => Some(Ok(Value::Num(f.exp()))),
            _ => Some(Ok(Value::Num(f64::NAN))),
        },
        "Rat" => match target {
            Value::Rat(_, _) => Some(Ok(target.clone())),
            Value::Int(i) => Some(Ok(make_rat(*i, 1))),
            Value::Num(f) => {
                let denom = 1_000_000i64;
                let numer = (f * denom as f64).round() as i64;
                Some(Ok(make_rat(numer, denom)))
            }
            Value::FatRat(n, d) => Some(Ok(make_rat(*n, *d))),
            Value::Package(_) => Some(Ok(make_rat(0, 1))),
            _ => Some(Ok(make_rat(0, 1))),
        },
        _ => None,
    }
}

// ── 1-arg method dispatch ────────────────────────────────────────────
/// Try to dispatch a 1-argument method call on a Value.
pub(crate) fn native_method_1arg(
    target: &Value,
    method: &str,
    arg: &Value,
) -> Option<Result<Value, RuntimeError>> {
    match method {
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
                Value::Num(f) => (*f as i64).max(0) as usize,
                _ => arg.to_string_value().parse::<usize>().unwrap_or(1),
            };
            let char_count = s.chars().count();
            let keep = char_count.saturating_sub(n);
            let result: String = s.chars().take(keep).collect();
            Some(Ok(Value::Str(result)))
        }
        "contains" => {
            if let Value::Package(type_name) = arg {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller contains({}:U)",
                    type_name,
                ))));
            }
            let s = target.to_string_value();
            let needle = arg.to_string_value();
            Some(Ok(Value::Bool(s.contains(&needle))))
        }
        "starts-with" => {
            let s = target.to_string_value();
            let prefix = arg.to_string_value();
            Some(Ok(Value::Bool(s.starts_with(&prefix))))
        }
        "ends-with" => {
            let s = target.to_string_value();
            let suffix = arg.to_string_value();
            Some(Ok(Value::Bool(s.ends_with(&suffix))))
        }
        "index" => {
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
            let s = target.to_string_value();
            let start = match arg {
                Value::Int(i) => (*i).max(0) as usize,
                _ => return None,
            };
            let result: String = s.chars().skip(start).collect();
            Some(Ok(Value::Str(result)))
        }
        "split" => {
            let s = target.to_string_value();
            let sep = arg.to_string_value();
            let parts: Vec<Value> = s.split(&sep).map(|p| Value::Str(p.to_string())).collect();
            Some(Ok(Value::Array(parts)))
        }
        "join" => match target {
            Value::Array(items) => {
                let sep = arg.to_string_value();
                let joined = items
                    .iter()
                    .map(|v| v.to_string_value())
                    .collect::<Vec<_>>()
                    .join(&sep);
                Some(Ok(Value::Str(joined)))
            }
            _ => None,
        },
        "head" => match target {
            Value::Array(items) => {
                let n = match arg {
                    Value::Int(i) => *i as usize,
                    _ => return None,
                };
                Some(Ok(Value::Array(items[..n.min(items.len())].to_vec())))
            }
            _ => None,
        },
        "tail" => match target {
            Value::Array(items) => {
                let n = match arg {
                    Value::Int(i) => *i as usize,
                    _ => return None,
                };
                let start = items.len().saturating_sub(n);
                Some(Ok(Value::Array(items[start..].to_vec())))
            }
            _ => None,
        },
        "rindex" => {
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
            let rendered = Interpreter::format_sprintf(&fmt, Some(target));
            Some(Ok(Value::Str(rendered)))
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
                    Value::Int(r) => *r,
                    _ => return None,
                };
                let s = match radix {
                    2 => format!("{:b}", i),
                    8 => format!("{:o}", i),
                    16 => format!("{:X}", i),
                    _ => format!("{}", i),
                };
                Some(Ok(Value::Str(s)))
            }
            _ => None,
        },
        "round" => {
            let scale = match arg {
                Value::Int(i) => *i as f64,
                Value::Num(f) => *f,
                _ => return None,
            };
            let x = match target {
                Value::Int(i) => *i as f64,
                Value::Num(f) => *f,
                _ => return None,
            };
            if scale == 0.0 {
                return Some(Ok(Value::Int(x.round() as i64)));
            }
            let factor = (1.0 / scale).abs();
            Some(Ok(Value::Num((x * factor).round() / factor)))
        }
        "log" => {
            let base_val = match arg {
                Value::Int(i) => *i as f64,
                Value::Num(f) => *f,
                _ => return None,
            };
            let x = match target {
                Value::Int(i) => *i as f64,
                Value::Num(f) => *f,
                _ => return None,
            };
            if base_val.is_finite() && base_val > 0.0 && base_val != 1.0 && x > 0.0 {
                Some(Ok(Value::Num(x.ln() / base_val.ln())))
            } else {
                Some(Ok(Value::Num(f64::NAN)))
            }
        }
        _ => None,
    }
}

// ── 2-arg method dispatch ────────────────────────────────────────────
/// Try to dispatch a 2-argument method call on a Value.
pub(crate) fn native_method_2arg(
    target: &Value,
    method: &str,
    arg1: &Value,
    arg2: &Value,
) -> Option<Result<Value, RuntimeError>> {
    match method {
        "substr" => {
            let s = target.to_string_value();
            let start = match arg1 {
                Value::Int(i) => (*i).max(0) as usize,
                _ => return None,
            };
            let len = match arg2 {
                Value::Int(i) => (*i).max(0) as usize,
                _ => return None,
            };
            let chars: Vec<char> = s.chars().collect();
            let end = (start + len).min(chars.len());
            let start = start.min(chars.len());
            Some(Ok(Value::Str(chars[start..end].iter().collect())))
        }
        _ => None,
    }
}

// ── Built-in function dispatch ───────────────────────────────────────
/// Try to dispatch a built-in function call.
pub(crate) fn native_function(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match args.len() {
        1 => native_function_1arg(name, &args[0]),
        2 => native_function_2arg(name, &args[0], &args[1]),
        3 => native_function_3arg(name, &args[0], &args[1], &args[2]),
        _ => native_function_variadic(name, args),
    }
}

fn native_function_1arg(name: &str, arg: &Value) -> Option<Result<Value, RuntimeError>> {
    match name {
        "uc" => Some(Ok(Value::Str(arg.to_string_value().to_uppercase()))),
        "lc" => Some(Ok(Value::Str(arg.to_string_value().to_lowercase()))),
        "tc" => {
            let s = arg.to_string_value();
            let mut result = String::new();
            let mut capitalize = true;
            for ch in s.chars() {
                if capitalize {
                    for c in ch.to_uppercase() {
                        result.push(c);
                    }
                    capitalize = false;
                } else {
                    result.push(ch);
                }
            }
            Some(Ok(Value::Str(result)))
        }
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
        "chars" => Some(Ok(Value::Int(arg.to_string_value().chars().count() as i64))),
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
            let x = Interpreter::to_float_value(arg).unwrap_or(f64::NAN);
            Some(Ok(Value::Num(x.ln())))
        }
        "sin" | "cos" | "tan" | "asin" | "acos" | "atan" => {
            let x = Interpreter::to_float_value(arg).unwrap_or(0.0);
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
            if let Some(num) = Interpreter::to_float_value(arg) {
                if num.is_nan() || num.is_infinite() {
                    Some(Ok(Value::Num(num)))
                } else {
                    Some(Ok(Value::Int(num.trunc() as i64)))
                }
            } else {
                Some(Ok(Value::Int(Interpreter::to_int(arg))))
            }
        }
        "defined" => Some(Ok(Value::Bool(!matches!(
            arg,
            Value::Nil | Value::Package(_)
        )))),
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
            let y = Interpreter::to_float_value(arg1).unwrap_or(0.0);
            let x = Interpreter::to_float_value(arg2).unwrap_or(0.0);
            Some(Ok(Value::Num(y.atan2(x))))
        }
        "log" => {
            let x = Interpreter::to_float_value(arg1).unwrap_or(f64::NAN);
            let base_val = Interpreter::to_float_value(arg2).unwrap_or(f64::NAN);
            if base_val.is_finite() && base_val > 0.0 && base_val != 1.0 && x > 0.0 {
                Some(Ok(Value::Num(x.ln() / base_val.ln())))
            } else {
                Some(Ok(Value::Num(f64::NAN)))
            }
        }
        "round" => {
            let x = Interpreter::to_float_value(arg1)?;
            let scale = Interpreter::to_float_value(arg2)?;
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

// ── Arithmetic operators ─────────────────────────────────────────────
pub(crate) fn arith_add(left: Value, right: Value) -> Result<Value, RuntimeError> {
    // Range + Int: shift both bounds
    match (&left, &right) {
        (Value::Range(a, b), Value::Int(n)) => return Ok(Value::Range(a + n, b + n)),
        (Value::RangeExcl(a, b), Value::Int(n)) => return Ok(Value::RangeExcl(a + n, b + n)),
        (Value::RangeExclStart(a, b), Value::Int(n)) => {
            return Ok(Value::RangeExclStart(a + n, b + n));
        }
        (Value::RangeExclBoth(a, b), Value::Int(n)) => {
            return Ok(Value::RangeExclBoth(a + n, b + n));
        }
        (Value::Int(n), Value::Range(a, b)) => return Ok(Value::Range(a + n, b + n)),
        (Value::Int(n), Value::RangeExcl(a, b)) => return Ok(Value::RangeExcl(a + n, b + n)),
        (Value::Int(n), Value::RangeExclStart(a, b)) => {
            return Ok(Value::RangeExclStart(a + n, b + n));
        }
        (Value::Int(n), Value::RangeExclBoth(a, b)) => {
            return Ok(Value::RangeExclBoth(a + n, b + n));
        }
        _ => {}
    }
    let (l, r) = Interpreter::coerce_numeric(left, right);
    Ok(arith_add_coerced(l, r))
}

fn arith_add_coerced(l: Value, r: Value) -> Value {
    if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
        let (ar, ai) = Interpreter::to_complex_parts(&l).unwrap_or((0.0, 0.0));
        let (br, bi) = Interpreter::to_complex_parts(&r).unwrap_or((0.0, 0.0));
        Value::Complex(ar + br, ai + bi)
    } else if let (Some((an, ad)), Some((bn, bd))) =
        (Interpreter::to_rat_parts(&l), Interpreter::to_rat_parts(&r))
    {
        if matches!(l, Value::Rat(_, _)) || matches!(r, Value::Rat(_, _)) {
            make_rat(an * bd + bn * ad, ad * bd)
        } else {
            match (l, r) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_add(b)),
                (Value::Num(a), Value::Num(b)) => Value::Num(a + b),
                (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 + b),
                (Value::Num(a), Value::Int(b)) => Value::Num(a + b as f64),
                _ => Value::Int(0),
            }
        }
    } else {
        match (l, r) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_add(b)),
            (Value::Num(a), Value::Num(b)) => Value::Num(a + b),
            (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 + b),
            (Value::Num(a), Value::Int(b)) => Value::Num(a + b as f64),
            _ => Value::Int(0),
        }
    }
}

pub(crate) fn arith_sub(left: Value, right: Value) -> Value {
    let (l, r) = Interpreter::coerce_numeric(left, right);
    if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
        let (ar, ai) = Interpreter::to_complex_parts(&l).unwrap_or((0.0, 0.0));
        let (br, bi) = Interpreter::to_complex_parts(&r).unwrap_or((0.0, 0.0));
        Value::Complex(ar - br, ai - bi)
    } else if let (Some((an, ad)), Some((bn, bd))) =
        (Interpreter::to_rat_parts(&l), Interpreter::to_rat_parts(&r))
    {
        if matches!(l, Value::Rat(_, _)) || matches!(r, Value::Rat(_, _)) {
            make_rat(an * bd - bn * ad, ad * bd)
        } else {
            match (l, r) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_sub(b)),
                (Value::Num(a), Value::Num(b)) => Value::Num(a - b),
                (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 - b),
                (Value::Num(a), Value::Int(b)) => Value::Num(a - b as f64),
                _ => Value::Int(0),
            }
        }
    } else {
        match (l, r) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_sub(b)),
            (Value::Num(a), Value::Num(b)) => Value::Num(a - b),
            (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 - b),
            (Value::Num(a), Value::Int(b)) => Value::Num(a - b as f64),
            _ => Value::Int(0),
        }
    }
}

pub(crate) fn arith_mul(left: Value, right: Value) -> Value {
    let (l, r) = Interpreter::coerce_numeric(left, right);
    if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
        let (ar, ai) = Interpreter::to_complex_parts(&l).unwrap_or((0.0, 0.0));
        let (br, bi) = Interpreter::to_complex_parts(&r).unwrap_or((0.0, 0.0));
        Value::Complex(ar * br - ai * bi, ar * bi + ai * br)
    } else if let (Some((an, ad)), Some((bn, bd))) =
        (Interpreter::to_rat_parts(&l), Interpreter::to_rat_parts(&r))
    {
        if matches!(l, Value::Rat(_, _)) || matches!(r, Value::Rat(_, _)) {
            make_rat(an * bn, ad * bd)
        } else {
            match (l, r) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_mul(b)),
                (Value::Num(a), Value::Num(b)) => Value::Num(a * b),
                (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 * b),
                (Value::Num(a), Value::Int(b)) => Value::Num(a * b as f64),
                _ => Value::Int(0),
            }
        }
    } else {
        match (l, r) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_mul(b)),
            (Value::Num(a), Value::Num(b)) => Value::Num(a * b),
            (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 * b),
            (Value::Num(a), Value::Int(b)) => Value::Num(a * b as f64),
            _ => Value::Int(0),
        }
    }
}

pub(crate) fn arith_div(left: Value, right: Value) -> Result<Value, RuntimeError> {
    let (l, r) = Interpreter::coerce_numeric(left, right);
    if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
        let (ar, ai) = Interpreter::to_complex_parts(&l).unwrap_or((0.0, 0.0));
        let (br, bi) = Interpreter::to_complex_parts(&r).unwrap_or((0.0, 0.0));
        let denom = br * br + bi * bi;
        if denom == 0.0 {
            return Err(RuntimeError::new("Division by zero"));
        }
        Ok(Value::Complex(
            (ar * br + ai * bi) / denom,
            (ai * br - ar * bi) / denom,
        ))
    } else {
        Ok(match (&l, &r) {
            (Value::Rat(_, _), _) | (_, Value::Rat(_, _)) | (Value::Int(_), Value::Int(_)) => {
                let (an, ad) = Interpreter::to_rat_parts(&l).unwrap_or((0, 1));
                let (bn, bd) = Interpreter::to_rat_parts(&r).unwrap_or((0, 1));
                if bn == 0 {
                    return Err(RuntimeError::new("Division by zero"));
                }
                make_rat(an * bd, ad * bn)
            }
            (Value::Num(a), Value::Num(b)) => Value::Num(a / b),
            (Value::Int(a), Value::Num(b)) => Value::Num(*a as f64 / b),
            (Value::Num(a), Value::Int(b)) => Value::Num(a / *b as f64),
            _ => Value::Int(0),
        })
    }
}

pub(crate) fn arith_mod(left: Value, right: Value) -> Result<Value, RuntimeError> {
    let (l, r) = Interpreter::coerce_numeric(left, right);
    if let (Some((an, ad)), Some((bn, bd))) =
        (Interpreter::to_rat_parts(&l), Interpreter::to_rat_parts(&r))
    {
        if matches!(l, Value::Rat(_, _)) || matches!(r, Value::Rat(_, _)) {
            if bn == 0 {
                return Err(RuntimeError::new("Modulo by zero"));
            }
            let lf = an as f64 / ad as f64;
            let rf = bn as f64 / bd as f64;
            Ok(Value::Num(lf % rf))
        } else {
            Ok(match (l, r) {
                (Value::Int(_), Value::Int(0)) => {
                    return Err(RuntimeError::new("Modulo by zero"));
                }
                (Value::Int(a), Value::Int(b)) => Value::Int(a % b),
                (Value::Num(a), Value::Num(b)) => Value::Num(a % b),
                (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 % b),
                (Value::Num(a), Value::Int(b)) => Value::Num(a % b as f64),
                _ => Value::Int(0),
            })
        }
    } else {
        Ok(match (l, r) {
            (Value::Int(_), Value::Int(0)) => {
                return Err(RuntimeError::new("Modulo by zero"));
            }
            (Value::Int(a), Value::Int(b)) => Value::Int(a % b),
            (Value::Num(a), Value::Num(b)) => Value::Num(a % b),
            (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 % b),
            (Value::Num(a), Value::Int(b)) => Value::Num(a % b as f64),
            _ => Value::Int(0),
        })
    }
}

pub(crate) fn arith_pow(left: Value, right: Value) -> Value {
    let (l, r) = Interpreter::coerce_numeric(left, right);
    if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
        let (ar, ai) = Interpreter::to_complex_parts(&l).unwrap_or((0.0, 0.0));
        let (br, bi) = Interpreter::to_complex_parts(&r).unwrap_or((0.0, 0.0));
        let ln_r = (ar * ar + ai * ai).sqrt().ln();
        let ln_i = ai.atan2(ar);
        let wr = br * ln_r - bi * ln_i;
        let wi = br * ln_i + bi * ln_r;
        let mag = wr.exp();
        Value::Complex(mag * wi.cos(), mag * wi.sin())
    } else {
        match (l, r) {
            (Value::Int(a), Value::Int(b)) if b >= 0 => Value::Int(a.pow(b as u32)),
            (Value::Int(a), Value::Int(b)) => {
                let pos = (-b) as u32;
                let base = a.pow(pos);
                make_rat(1, base)
            }
            (Value::Rat(n, d), Value::Int(b)) if b >= 0 => {
                let p = b as u32;
                make_rat(n.pow(p), d.pow(p))
            }
            (Value::Rat(n, d), Value::Int(b)) => {
                let p = (-b) as u32;
                make_rat(d.pow(p), n.pow(p))
            }
            (Value::Num(a), Value::Int(b)) => Value::Num(a.powi(b as i32)),
            (Value::Int(a), Value::Num(b)) => Value::Num((a as f64).powf(b)),
            (Value::Num(a), Value::Num(b)) => Value::Num(a.powf(b)),
            _ => Value::Int(0),
        }
    }
}

pub(crate) fn arith_negate(val: Value) -> Result<Value, RuntimeError> {
    match val {
        Value::Int(i) => Ok(Value::Int(-i)),
        Value::Num(f) => Ok(Value::Num(-f)),
        Value::Rat(n, d) => Ok(Value::Rat(-n, d)),
        Value::Complex(r, i) => Ok(Value::Complex(-r, -i)),
        Value::Str(ref s) => {
            if let Ok(i) = s.trim().parse::<i64>() {
                Ok(Value::Int(-i))
            } else if let Ok(f) = s.trim().parse::<f64>() {
                Ok(Value::Num(-f))
            } else {
                Err(RuntimeError::new("Unary - expects numeric"))
            }
        }
        _ => Err(RuntimeError::new("Unary - expects numeric")),
    }
}
