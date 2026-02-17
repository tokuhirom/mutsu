#![allow(clippy::result_large_err)]

use crate::runtime;
use crate::value::{RuntimeError, Value, make_rat};
use num_traits::Signed;
use unicode_segmentation::UnicodeSegmentation;

use super::rng::builtin_rand;
use super::unicode::titlecase_string;

mod coercion;
mod collection;

// ── 0-arg method dispatch ────────────────────────────────────────────
/// Try to dispatch a 0-argument method call on a Value.
/// Returns `Some(Ok(..))` / `Some(Err(..))` when handled, `None` to fall through.
pub(crate) fn native_method_0arg(
    target: &Value,
    method: &str,
) -> Option<Result<Value, RuntimeError>> {
    // Try core string/numeric/array methods first
    if let result @ Some(_) = dispatch_core(target, method) {
        return result;
    }
    // Then collection methods (keys, values, kv, pairs, etc.)
    if let result @ Some(_) = collection::dispatch(target, method) {
        return result;
    }
    // Then type coercion and specialized methods
    coercion::dispatch(target, method)
}

fn dispatch_core(target: &Value, method: &str) -> Option<Result<Value, RuntimeError>> {
    match method {
        "defined" => Some(Ok(Value::Bool(match target {
            Value::Nil | Value::Package(_) => false,
            Value::Slip(items) if items.is_empty() => false,
            _ => true,
        }))),
        "Bool" => Some(Ok(Value::Bool(target.truthy()))),
        "Str" => match target {
            Value::Package(_) | Value::Instance { .. } => None,
            Value::Str(s) if s == "IO::Special" => Some(Ok(Value::Str(String::new()))),
            Value::Array(items) if items.iter().all(|v| matches!(v, Value::Int(_))) => {
                // Uni-like array: convert codepoints to string
                let s: String = items
                    .iter()
                    .filter_map(|v| match v {
                        Value::Int(cp) => char::from_u32(*cp as u32),
                        _ => None,
                    })
                    .collect();
                Some(Ok(Value::Str(s)))
            }
            _ => Some(Ok(Value::Str(target.to_string_value()))),
        },
        "Int" => {
            let result = match target {
                Value::Int(i) => Value::Int(*i),
                Value::BigInt(_) => target.clone(),
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
                Value::Complex(r, _) => Value::Int(*r as i64),
                _ => return None,
            };
            Some(Ok(result))
        }
        "Numeric" | "Num" => {
            let result = match target {
                Value::Int(i) => Value::Int(*i),
                Value::BigInt(_) => target.clone(),
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
                Value::Complex(r, _) => Value::Num(*r),
                _ => return None,
            };
            Some(Ok(result))
        }
        "chars" => Some(Ok(Value::Int(
            target.to_string_value().graphemes(true).count() as i64,
        ))),
        "ord" => {
            let s = target.to_string_value();
            if let Some(ch) = s.chars().next() {
                Some(Ok(Value::Int(ch as u32 as i64)))
            } else {
                Some(Ok(Value::Nil))
            }
        }
        "ords" => {
            let s = target.to_string_value();
            let ords: Vec<Value> = s.chars().map(|c| Value::Int(c as u32 as i64)).collect();
            Some(Ok(Value::Array(ords)))
        }
        "chr" => {
            let code = match target {
                Value::Int(i) => *i,
                Value::Num(f) => *f as i64,
                _ => {
                    let s = target.to_string_value();
                    s.parse::<i64>().unwrap_or(0)
                }
            };
            if let Some(ch) = char::from_u32(code as u32) {
                Some(Ok(Value::Str(ch.to_string())))
            } else {
                Some(Err(RuntimeError::new(format!(
                    "chr({}) does not map to a valid Unicode character",
                    code
                ))))
            }
        }
        "chrs" => {
            // .chrs on a list/array of ints or a range
            let val_to_i64 = |v: &Value| -> i64 {
                match v {
                    Value::Int(i) => *i,
                    Value::Num(f) => *f as i64,
                    _ => v.to_string_value().parse::<i64>().unwrap_or(0),
                }
            };
            let items: Vec<i64> = match target {
                Value::Array(items) => items.iter().map(&val_to_i64).collect(),
                Value::Range(a, b) => (*a..=*b).collect(),
                Value::RangeExcl(a, b) => (*a..*b).collect(),
                _ => vec![val_to_i64(target)],
            };
            let s: String = items
                .iter()
                .filter_map(|&code| char::from_u32(code as u32))
                .collect();
            Some(Ok(Value::Str(s)))
        }
        "elems" => {
            let result = match target {
                Value::Array(items) => Value::Int(items.len() as i64),
                Value::Hash(items) => Value::Int(items.len() as i64),
                Value::Set(items) => Value::Int(items.len() as i64),
                Value::Bag(items) => Value::Int(items.len() as i64),
                Value::Mix(items) => Value::Int(items.len() as i64),
                Value::Junction { values, .. } => Value::Int(values.len() as i64),
                _ => Value::Int(1),
            };
            Some(Ok(result))
        }
        "Complex-i" | "i" => {
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
                Value::BigInt(n) => Value::BigInt(n.abs()),
                Value::Num(f) => Value::Num(f.abs()),
                Value::Rat(n, d) => Value::Rat(n.abs(), *d),
                Value::Complex(r, i) => Value::Num((r * r + i * i).sqrt()),
                _ => return None,
            };
            Some(Ok(result))
        }
        "rand" => {
            let max = match target {
                Value::Int(n) => *n as f64,
                Value::Num(n) => *n,
                Value::Rat(n, d) => *n as f64 / *d as f64,
                _ => return None,
            };
            Some(Ok(Value::Num(builtin_rand() * max)))
        }
        "uc" => Some(Ok(Value::Str(target.to_string_value().to_uppercase()))),
        "lc" => Some(Ok(Value::Str(target.to_string_value().to_lowercase()))),
        "tc" => Some(Ok(Value::Str(titlecase_string(&target.to_string_value())))),
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
            Value::Rat(n, d) if *d != 0 => Some(Ok(Value::Num((*n as f64 / *d as f64).sqrt()))),
            Value::Complex(r, i) => {
                let mag = (r * r + i * i).sqrt();
                let re = ((mag + r) / 2.0).sqrt();
                let im = i.signum() * ((mag - r) / 2.0).sqrt();
                Some(Ok(Value::Complex(re, im)))
            }
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
        "codes" => {
            let s = target.to_string_value();
            Some(Ok(Value::Int(s.chars().count() as i64)))
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
        "flip" => {
            let s = target.to_string_value();
            let reversed: String = s.graphemes(true).rev().collect();
            Some(Ok(Value::Str(reversed)))
        }
        "so" => Some(Ok(Value::Bool(target.truthy()))),
        "not" => Some(Ok(Value::Bool(!target.truthy()))),
        "chomp" => Some(Ok(Value::Str(
            target.to_string_value().trim_end_matches('\n').to_string(),
        ))),
        "chop" => {
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
            Value::Bool(b) => {
                if method == "gist" {
                    Some(Ok(Value::Str(
                        if *b { "True" } else { "False" }.to_string(),
                    )))
                } else {
                    Some(Ok(Value::Str(
                        if *b { "Bool::True" } else { "Bool::False" }.to_string(),
                    )))
                }
            }
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
                } else if *n % *d == 0 {
                    // Exact integer: Rat(10, 2) => "5"
                    Some(Ok(Value::Str(format!("{}", *n / *d))))
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
                        Some(Ok(Value::Str(format!("{}", val))))
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
            _ => {
                let items = runtime::value_to_list(target);
                Some(Ok(items.first().cloned().unwrap_or(Value::Nil)))
            }
        },
        "tail" => match target {
            Value::Array(items) => Some(Ok(items.last().cloned().unwrap_or(Value::Nil))),
            _ => {
                let items = runtime::value_to_list(target);
                Some(Ok(items.last().cloned().unwrap_or(Value::Nil)))
            }
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
            Value::Rat(n, d) if *d != 0 => Some(Ok(Value::Num((*n as f64 / *d as f64).ln()))),
            Value::Complex(r, i) => {
                let mag = (r * r + i * i).sqrt().ln();
                let arg = i.atan2(*r);
                Some(Ok(Value::Complex(mag, arg)))
            }
            _ => Some(Ok(Value::Num(f64::NAN))),
        },
        "log2" => match target {
            Value::Int(i) => Some(Ok(Value::Num((*i as f64).log2()))),
            Value::Num(f) => Some(Ok(Value::Num(f.log2()))),
            Value::Complex(r, i) => {
                let mag = (r * r + i * i).sqrt().ln();
                let arg = i.atan2(*r);
                let ln2 = 2.0f64.ln();
                Some(Ok(Value::Complex(mag / ln2, arg / ln2)))
            }
            _ => Some(Ok(Value::Num(f64::NAN))),
        },
        "log10" => match target {
            Value::Int(i) => Some(Ok(Value::Num((*i as f64).log10()))),
            Value::Num(f) => Some(Ok(Value::Num(f.log10()))),
            Value::Complex(r, i) => {
                let mag = (r * r + i * i).sqrt().ln();
                let arg = i.atan2(*r);
                let ln10 = 10.0f64.ln();
                Some(Ok(Value::Complex(mag / ln10, arg / ln10)))
            }
            _ => Some(Ok(Value::Num(f64::NAN))),
        },
        "exp" => match target {
            Value::Int(i) => Some(Ok(Value::Num((*i as f64).exp()))),
            Value::Num(f) => Some(Ok(Value::Num(f.exp()))),
            Value::Rat(n, d) if *d != 0 => Some(Ok(Value::Num((*n as f64 / *d as f64).exp()))),
            Value::Complex(r, i) => {
                // exp(a+bi) = exp(a) * (cos(b) + i*sin(b))
                let ea = r.exp();
                Some(Ok(Value::Complex(ea * i.cos(), ea * i.sin())))
            }
            _ => Some(Ok(Value::Num(f64::NAN))),
        },
        "sin" | "cos" | "tan" | "asin" | "acos" | "atan" | "sec" | "cosec" | "cotan" | "asec"
        | "acosec" | "acotan" | "sinh" | "cosh" | "tanh" | "sech" | "cosech" | "cotanh"
        | "asinh" | "acosh" | "atanh" => {
            let x = match target {
                Value::Int(i) => *i as f64,
                Value::Num(f) => *f,
                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                _ => return Some(Ok(Value::Num(f64::NAN))),
            };
            let result = match method {
                "sin" => x.sin(),
                "cos" => x.cos(),
                "tan" => x.tan(),
                "asin" => x.asin(),
                "acos" => x.acos(),
                "atan" => x.atan(),
                "sec" => 1.0 / x.cos(),
                "cosec" => 1.0 / x.sin(),
                "cotan" => 1.0 / x.tan(),
                "asec" => (1.0 / x).acos(),
                "acosec" => (1.0 / x).asin(),
                "acotan" => (1.0 / x).atan(),
                "sinh" => x.sinh(),
                "cosh" => x.cosh(),
                "tanh" => x.tanh(),
                "sech" => 1.0 / x.cosh(),
                "cosech" => 1.0 / x.sinh(),
                "cotanh" => 1.0 / x.tanh(),
                "asinh" => x.asinh(),
                "acosh" => x.acosh(),
                "atanh" => x.atanh(),
                _ => f64::NAN,
            };
            Some(Ok(Value::Num(result)))
        }
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
        "sink" => Some(Ok(Value::Nil)),
        "NFC" | "NFD" | "NFKC" | "NFKD" => {
            use unicode_normalization::UnicodeNormalization;
            let s = uni_or_str(target);
            let normalized: String = match method {
                "NFC" => s.nfc().collect(),
                "NFD" => s.nfd().collect(),
                "NFKC" => s.nfkc().collect(),
                _ => s.nfkd().collect(),
            };
            let codepoints: Vec<Value> = normalized.chars().map(|c| Value::Int(c as i64)).collect();
            Some(Ok(Value::Array(codepoints)))
        }
        _ => None,
    }
}

/// Convert a Value to a string for Unicode normalization.
/// If the value is an Array of Int (Uni-like), convert codepoints to a string.
fn uni_or_str(target: &Value) -> String {
    match target {
        Value::Array(items) if items.iter().all(|v| matches!(v, Value::Int(_))) => items
            .iter()
            .filter_map(|v| match v {
                Value::Int(cp) => char::from_u32(*cp as u32),
                _ => None,
            })
            .collect(),
        _ => target.to_string_value(),
    }
}
