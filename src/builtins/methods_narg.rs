#![allow(clippy::result_large_err)]

use crate::runtime;
use crate::value::{RuntimeError, Value};

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
            let rendered = runtime::format_sprintf(&fmt, Some(target));
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
        "roll" => {
            let count = match arg {
                Value::Int(i) if *i > 0 => *i as usize,
                Value::Int(_) => 0,
                _ => return None,
            };
            let items = runtime::value_to_list(target);
            if items.is_empty() || count == 0 {
                return Some(Ok(Value::Array(Vec::new())));
            }
            let mut result = Vec::with_capacity(count);
            for _ in 0..count {
                let mut idx = (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize;
                if idx >= items.len() {
                    idx = items.len() - 1;
                }
                result.push(items[idx].clone());
            }
            Some(Ok(Value::Array(result)))
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
