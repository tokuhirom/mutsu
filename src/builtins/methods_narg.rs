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
        "Rat" => {
            // .Rat(epsilon) — just ignore epsilon and convert like .Rat
            let result = match target {
                Value::Rat(_, _) => target.clone(),
                Value::Int(i) => Value::Rat(*i, 1),
                Value::Num(f) => {
                    let denom = 1_000_000i64;
                    let numer = (f * denom as f64).round() as i64;
                    Value::Rat(numer, denom)
                }
                Value::FatRat(n, d) => Value::Rat(*n, *d),
                Value::Str(s) => {
                    if let Ok(f) = s.parse::<f64>() {
                        let denom = 1_000_000i64;
                        let numer = (f * denom as f64).round() as i64;
                        Value::Rat(numer, denom)
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
            Some(Ok(Value::array(parts)))
        }
        "join" => match target {
            Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items) => {
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
                    Value::Str(format!("batch size must be at least 1, got {}", n)),
                );
                let ex = Value::make_instance("X::OutOfRange".to_string(), attrs);
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
                    return Some(Ok(Value::Str("0".to_string())));
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
                Some(Ok(Value::Str(String::from_utf8(buf).unwrap())))
            }
            Value::Num(f) => {
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
                Some(Ok(Value::Str(rat_to_base(n, d, radix, None))))
            }
            Value::Rat(n, d) => {
                let radix = parse_radix(arg)?;
                // For Rat, use rational arithmetic for precision
                Some(Ok(Value::Str(rat_to_base(*n, *d, radix, None))))
            }
            _ => None,
        },
        "round" => {
            let scale = match arg {
                Value::Int(i) => *i as f64,
                Value::Num(f) => *f,
                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                _ => return None,
            };
            let x = match target {
                Value::Int(i) => *i as f64,
                Value::Num(f) => *f,
                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                _ => return None,
            };
            if scale == 0.0 {
                return Some(Ok(Value::Int(x.round() as i64)));
            }
            let factor = (1.0 / scale).abs();
            // Return Rat when the result has a fractional part matching the scale
            let result = (x * factor).round() / factor;
            if result == result.floor() {
                Some(Ok(Value::Int(result as i64)))
            } else {
                let (n, d) = f64_to_rat(result);
                Some(Ok(Value::Rat(n, d)))
            }
        }
        "pick" => {
            let mut items = runtime::value_to_list(target);
            Some(Ok(match arg {
                Value::Num(f) if f.is_infinite() && f.is_sign_positive() => {
                    // .pick(*) — Fisher-Yates shuffle
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
            let count = match arg {
                Value::Int(i) if *i > 0 => *i as usize,
                Value::Int(_) => 0,
                _ => return None,
            };
            let items = runtime::value_to_list(target);
            if items.is_empty() || count == 0 {
                return Some(Ok(Value::array(Vec::new())));
            }
            let mut result = Vec::with_capacity(count);
            for _ in 0..count {
                let mut idx = (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize;
                if idx >= items.len() {
                    idx = items.len() - 1;
                }
                result.push(items[idx].clone());
            }
            Some(Ok(Value::array(result)))
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
        _ => None,
    }
}

/// Extract byte array from a Buf/Blob instance.
fn buf_get_bytes(target: &Value) -> Option<Vec<u8>> {
    if let Value::Instance {
        class_name,
        attributes,
        ..
    } = target
        && (class_name == "Buf" || class_name == "Blob")
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
            let digits = match arg2 {
                Value::Int(d) if *d < 0 => {
                    return Some(Err(RuntimeError::new(
                        "X::OutOfRange: digits must be non-negative",
                    )));
                }
                Value::Int(d) => *d as u32,
                _ => None?,
            };
            match target {
                Value::Int(i) => Some(Ok(Value::Str(rat_to_base(*i, 1, radix, Some(digits))))),
                Value::Num(f) => Some(Ok(Value::Str(format_base_with_digits(*f, radix, digits)))),
                Value::Rat(n, d) => Some(Ok(Value::Str(rat_to_base(*n, *d, radix, Some(digits))))),
                _ => None,
            }
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
        _ => None,
    }
}

fn parse_radix(arg: &Value) -> Option<u32> {
    match arg {
        Value::Int(r) if *r >= 2 && *r <= 36 => Some(*r as u32),
        Value::Str(s) => s.parse::<u32>().ok().filter(|r| *r >= 2 && *r <= 36),
        _ => None,
    }
}

/// Convert a rational number n/d to a string in the given base.
/// If `max_digits` is None, auto-detect precision (up to 256 digits).
fn rat_to_base(n: i64, d: i64, radix: u32, max_digits: Option<u32>) -> String {
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
        if let Some(digits) = max_digits
            && digits > 0
        {
            result.push('.');
            for _ in 0..digits {
                result.push('0');
            }
        }
        return result;
    }

    let limit = max_digits.unwrap_or(256);
    if limit == 0 {
        // Round integer part
        if num * 2 >= den {
            // Would need carry propagation; for now just return as-is
        }
        return result;
    }

    result.push('.');
    let digit_chars = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    let mut frac_digits = Vec::new();
    for _ in 0..limit {
        num *= radix;
        let digit = num / den;
        frac_digits.push(digit as u8);
        num %= den;
        if num == 0 && max_digits.is_none() {
            break;
        }
    }
    // Round last digit if we have a remainder
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
    }
    for d in &frac_digits {
        result.push(digit_chars[*d as usize] as char);
    }
    result
}

fn format_base_with_digits(val: f64, radix: u32, digits: u32) -> String {
    let negative = val < 0.0;
    let val = val.abs();
    let int_part = val.floor() as u64;
    let frac_part = val - int_part as f64;
    let mut result = if negative {
        "-".to_string()
    } else {
        String::new()
    };
    result.push_str(&int_to_base(int_part, radix));
    if digits > 0 {
        result.push('.');
        let mut frac = frac_part;
        let digit_chars = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        for i in 0..digits {
            frac *= radix as f64;
            let mut d = frac.floor() as u64;
            if i == digits - 1 {
                let remainder = frac - d as f64;
                if remainder >= 0.5 {
                    d += 1;
                }
            }
            if d >= radix as u64 {
                d = 0;
            }
            result.push(digit_chars[d as usize] as char);
            frac -= frac.floor();
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
