/// Math and miscellaneous methods: tclc, wordcase, succ, pred, log, log2, log10,
/// exp, atan2, trig functions, Rat, FatRat, tree, encode, sink, item, race/hyper,
/// NFC/NFD/NFKC/NFKD
use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, make_big_rat, make_rat};
use num_traits::ToPrimitive;

use super::complex_math::complex_trig;

/// Recursively apply `.tree` to nested arrays.
fn tree_recursive(items: &[Value]) -> Vec<Value> {
    items
        .iter()
        .map(|v| match v {
            Value::Array(inner, ..) => Value::array(tree_recursive(inner)),
            other => other.clone(),
        })
        .collect()
}

/// Convert a Value to a string for Unicode normalization.
/// If the value is an Array of Int (Uni-like), convert codepoints to a string.
fn uni_or_str(target: &Value) -> String {
    match target {
        Value::Array(items, ..) if items.iter().all(|v| matches!(v, Value::Int(_))) => items
            .iter()
            .filter_map(|v| match v {
                Value::Int(cp) => char::from_u32(*cp as u32),
                _ => None,
            })
            .collect(),
        _ => target.to_string_value(),
    }
}

pub(super) fn dispatch(
    target: &Value,
    method: &str,
) -> Option<Option<Result<Value, RuntimeError>>> {
    match method {
        "tclc" => Some(Some(Ok(Value::str(crate::value::tclc_str(
            &target.to_string_value(),
        ))))),
        "wordcase" => Some(Some(Ok(Value::str(crate::value::wordcase_str(
            &target.to_string_value(),
        ))))),
        "succ" => Some(match target {
            Value::Enum { .. } | Value::Instance { .. } => None,
            Value::Int(i) => Some(Ok(Value::Int(i + 1))),
            Value::Num(f) => Some(Ok(Value::Num(f + 1.0))),
            Value::Complex(r, i) => Some(Ok(Value::Complex(r + 1.0, *i))),
            Value::Rat(n, d) => Some(Ok(make_rat(n + d, *d))),
            Value::FatRat(n, d) => Some(Ok(Value::FatRat(n + d, *d))),
            Value::BigRat(n, d) => Some(Ok(Value::BigRat(n + d, d.clone()))),
            Value::Bool(_) => Some(Ok(Value::Bool(true))),
            Value::Str(s) => Some(Ok(Value::str(crate::builtins::str_increment::string_succ(
                s,
            )))),
            _ => Some(Ok(target.clone())),
        }),
        "pred" => Some(match target {
            Value::Enum { .. } | Value::Instance { .. } => None,
            Value::Int(i) => Some(Ok(Value::Int(i - 1))),
            Value::Num(f) => Some(Ok(Value::Num(f - 1.0))),
            Value::Complex(r, i) => Some(Ok(Value::Complex(r - 1.0, *i))),
            Value::Rat(n, d) => Some(Ok(make_rat(n - d, *d))),
            Value::FatRat(n, d) => Some(Ok(Value::FatRat(n - d, *d))),
            Value::BigRat(n, d) => Some(Ok(Value::BigRat(n - d, d.clone()))),
            Value::Bool(_) => Some(Ok(Value::Bool(false))),
            Value::Str(s) => Some(Ok(Value::str(crate::builtins::str_increment::string_pred(
                s,
            )))),
            _ => Some(Ok(target.clone())),
        }),
        "log" => Some(match target {
            Value::Int(i) => Some(Ok(Value::Num((*i as f64).ln()))),
            Value::BigInt(i) => Some(Ok(Value::Num(i.to_f64().unwrap_or(f64::INFINITY).ln()))),
            Value::Num(f) => Some(Ok(Value::Num(f.ln()))),
            Value::Rat(n, d) if *d != 0 => Some(Ok(Value::Num((*n as f64 / *d as f64).ln()))),
            Value::BigRat(n, d) if d != &num_bigint::BigInt::from(0) => Some(Ok(Value::Num(
                (n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0)).ln(),
            ))),
            Value::Complex(r, i) => {
                let mag = (r * r + i * i).sqrt().ln();
                let arg = i.atan2(*r);
                Some(Ok(Value::Complex(mag, arg)))
            }
            _ => None,
        }),
        "log2" => Some(match target {
            Value::Int(i) => Some(Ok(Value::Num((*i as f64).log2()))),
            Value::BigInt(i) => Some(Ok(Value::Num(i.to_f64().unwrap_or(f64::INFINITY).log2()))),
            Value::Num(f) => Some(Ok(Value::Num(f.log2()))),
            Value::Rat(n, d) if *d != 0 => Some(Ok(Value::Num((*n as f64 / *d as f64).log2()))),
            Value::BigRat(n, d) if d != &num_bigint::BigInt::from(0) => Some(Ok(Value::Num(
                (n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0)).log2(),
            ))),
            Value::Complex(r, i) => {
                let mag = (r * r + i * i).sqrt().ln();
                let arg = i.atan2(*r);
                let ln2 = 2.0f64.ln();
                Some(Ok(Value::Complex(mag / ln2, arg / ln2)))
            }
            _ => None,
        }),
        "log10" => Some(match target {
            Value::Int(i) => Some(Ok(Value::Num((*i as f64).log10()))),
            Value::BigInt(i) => Some(Ok(Value::Num(i.to_f64().unwrap_or(f64::INFINITY).log10()))),
            Value::Num(f) => Some(Ok(Value::Num(f.log10()))),
            Value::Rat(n, d) if *d != 0 => Some(Ok(Value::Num((*n as f64 / *d as f64).log10()))),
            Value::BigRat(n, d) if d != &num_bigint::BigInt::from(0) => Some(Ok(Value::Num(
                (n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0)).log10(),
            ))),
            Value::Complex(r, i) => {
                let mag = (r * r + i * i).sqrt().ln();
                let arg = i.atan2(*r);
                let ln10 = 10.0f64.ln();
                Some(Ok(Value::Complex(mag / ln10, arg / ln10)))
            }
            _ => None,
        }),
        "exp" => Some(match target {
            Value::Int(i) => Some(Ok(Value::Num((*i as f64).exp()))),
            Value::BigInt(i) => Some(Ok(Value::Num(i.to_f64().unwrap_or(f64::INFINITY).exp()))),
            Value::Num(f) => Some(Ok(Value::Num(f.exp()))),
            Value::Rat(n, d) if *d != 0 => Some(Ok(Value::Num((*n as f64 / *d as f64).exp()))),
            Value::BigRat(n, d) if d != &num_bigint::BigInt::from(0) => Some(Ok(Value::Num(
                (n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0)).exp(),
            ))),
            Value::Complex(r, i) => {
                // exp(a+bi) = exp(a) * (cos(b) + i*sin(b))
                let ea = r.exp();
                Some(Ok(Value::Complex(ea * i.cos(), ea * i.sin())))
            }
            _ => None,
        }),
        "atan2" => {
            // .atan2 with no args defaults to x=1
            let y = match target {
                Value::Int(i) => *i as f64,
                Value::BigInt(i) => i.to_f64().unwrap_or(f64::INFINITY),
                Value::Num(f) => *f,
                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                Value::FatRat(n, d) if *d != 0 => *n as f64 / *d as f64,
                Value::BigRat(n, d) if d != &num_bigint::BigInt::from(0) => {
                    n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0)
                }
                Value::Str(s) => match s.parse::<f64>() {
                    Ok(f) => f,
                    Err(_) => return Some(Some(Ok(Value::Num(f64::NAN)))),
                },
                _ => return Some(None), // fall through to runtime for user types
            };
            Some(Some(Ok(Value::Num(y.atan2(1.0)))))
        }
        "sin" | "cos" | "tan" | "asin" | "acos" | "atan" | "sec" | "cosec" | "cotan" | "asec"
        | "acosec" | "acotan" | "sinh" | "cosh" | "tanh" | "sech" | "cosech" | "cotanh"
        | "asinh" | "acosh" | "atanh" | "asech" | "acosech" | "acotanh" => {
            // Complex: dispatch to complex trig
            if let Value::Complex(re, im) = target {
                let result = complex_trig(method, *re, *im);
                return Some(Some(Ok(Value::Complex(result.0, result.1))));
            }
            let x = match target {
                Value::Int(i) => *i as f64,
                Value::BigInt(i) => i.to_f64().unwrap_or(f64::INFINITY),
                Value::Num(f) => *f,
                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                Value::FatRat(n, d) if *d != 0 => *n as f64 / *d as f64,
                Value::BigRat(n, d) if d != &num_bigint::BigInt::from(0) => {
                    n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0)
                }
                Value::Str(s) => match s.parse::<f64>() {
                    Ok(f) => f,
                    Err(_) => return Some(Some(Ok(Value::Num(f64::NAN)))),
                },
                _ => return Some(None), // fall through to runtime for user types
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
                "asinh" => {
                    let sign = x.signum();
                    let ax = x.abs();
                    sign * (ax + (ax * ax + 1.0).sqrt()).ln()
                }
                "acosh" => {
                    if x < 1.0 {
                        f64::NAN
                    } else {
                        (x + (x * x - 1.0).sqrt()).ln()
                    }
                }
                "atanh" => x.atanh(),
                "asech" => {
                    let y = 1.0 / x;
                    (y + (y * y - 1.0).sqrt()).ln()
                }
                "acosech" => {
                    let y = 1.0 / x;
                    (y + (y * y + 1.0).sqrt()).ln()
                }
                "acotanh" => (1.0 / x).atanh(),
                _ => f64::NAN,
            };
            Some(Some(Ok(Value::Num(result))))
        }
        "Rat" => Some(match target {
            Value::Rat(_, _) => Some(Ok(target.clone())),
            Value::BigRat(_, _) => Some(Ok(target.clone())),
            Value::Int(i) => Some(Ok(make_rat(*i, 1))),
            Value::Num(f) => {
                if f.is_nan() {
                    Some(Ok(Value::Rat(0, 0)))
                } else if f.is_infinite() {
                    if f.is_sign_positive() {
                        Some(Ok(Value::Rat(1, 0)))
                    } else {
                        Some(Ok(Value::Rat(-1, 0)))
                    }
                } else {
                    Some(Ok(crate::builtins::num_to_rat_with_epsilon(*f, 1e-6)))
                }
            }
            Value::FatRat(n, d) => Some(Ok(make_rat(*n, *d))),
            Value::Str(s) => {
                if let Some((n_str, d_str)) = s.split_once('/') {
                    let n = n_str.trim().parse::<i64>().unwrap_or(0);
                    let d = d_str.trim().parse::<i64>().unwrap_or(1);
                    Some(Ok(make_rat(n, if d == 0 { 1 } else { d })))
                } else if s.contains('.') {
                    let trimmed = s.trim();
                    let negative = trimmed.starts_with('-');
                    let abs_str = if negative { &trimmed[1..] } else { trimmed };
                    if let Some((int_part, frac_part)) = abs_str.split_once('.') {
                        let frac_digits = frac_part.len() as u32;
                        let denom = 10i64.pow(frac_digits);
                        let int_val = int_part.parse::<i64>().unwrap_or(0);
                        let frac_val = frac_part.parse::<i64>().unwrap_or(0);
                        let numer = int_val * denom + frac_val;
                        let numer = if negative { -numer } else { numer };
                        Some(Ok(make_rat(numer, denom)))
                    } else {
                        Some(Ok(make_rat(0, 1)))
                    }
                } else {
                    let n = s.trim().parse::<i64>().unwrap_or(0);
                    Some(Ok(make_rat(n, 1)))
                }
            }
            Value::Complex(r, im) => {
                if im.abs() <= 1e-15 {
                    let denom = 1_000_000i64;
                    let numer = (r * denom as f64).round() as i64;
                    Some(Ok(make_rat(numer, denom)))
                } else {
                    Some(Err(RuntimeError::new(
                        "Cannot convert Complex to Real: imaginary part not zero",
                    )))
                }
            }
            Value::Instance { .. } => None,
            Value::Package(_) => Some(Ok(make_rat(0, 1))),
            _ => Some(Ok(make_rat(0, 1))),
        }),
        "FatRat" => Some(match target {
            Value::FatRat(_, _) => Some(Ok(target.clone())),
            Value::Rat(n, d) => Some(Ok(Value::FatRat(*n, *d))),
            Value::BigRat(_, _) => Some(Ok(target.clone())),
            Value::Int(i) => Some(Ok(Value::FatRat(*i, 1))),
            Value::BigInt(i) => Some(Ok(make_big_rat((**i).clone(), num_bigint::BigInt::from(1)))),
            Value::Num(f) => {
                if f.is_nan() {
                    Some(Ok(Value::FatRat(0, 0)))
                } else if f.is_infinite() {
                    if f.is_sign_positive() {
                        Some(Ok(Value::FatRat(1, 0)))
                    } else {
                        Some(Ok(Value::FatRat(-1, 0)))
                    }
                } else {
                    let rat = crate::builtins::num_to_rat_with_epsilon(*f, 1e-6);
                    match rat {
                        Value::Rat(n, d) => Some(Ok(Value::FatRat(n, d))),
                        _ => Some(Ok(Value::FatRat(0, 1))),
                    }
                }
            }
            Value::Str(s) => {
                if let Ok(f) = s.parse::<f64>() {
                    let rat = crate::builtins::num_to_rat_with_epsilon(f, 1e-6);
                    match rat {
                        Value::Rat(n, d) => Some(Ok(Value::FatRat(n, d))),
                        _ => Some(Ok(Value::FatRat(0, 1))),
                    }
                } else {
                    Some(Ok(Value::FatRat(0, 1)))
                }
            }
            Value::Package(_) => Some(Ok(Value::FatRat(0, 1))),
            _ => Some(Ok(Value::FatRat(0, 1))),
        }),
        "tree" => Some(match target {
            Value::Array(items, ..) => Some(Ok(Value::array(tree_recursive(items)))),
            _ => Some(Ok(target.clone())),
        }),
        "encode" => {
            let s = target.to_string_value();
            let bytes: Vec<Value> = s.as_bytes().iter().map(|&b| Value::Int(b as i64)).collect();
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("bytes".to_string(), Value::array(bytes));
            Some(Some(Ok(Value::make_instance(
                Symbol::intern("utf8"),
                attrs,
            ))))
        }
        "sink" => Some(match target {
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Failure" => {
                if let Some(ex) = attributes.get("exception") {
                    let mut err = RuntimeError::new(ex.to_string_value());
                    err.exception = Some(Box::new(ex.clone()));
                    Some(Err(err))
                } else {
                    Some(Ok(Value::Nil))
                }
            }
            _ => Some(Ok(Value::Nil)),
        }),
        "item" => Some(match target {
            Value::Array(items, kind) => Some(Ok(Value::Array(items.clone(), kind.itemize()))),
            Value::LazyList(_) => None, // fall through to runtime to force
            other => Some(Ok(Value::Scalar(Box::new(other.clone())))),
        }),
        "race" | "hyper" => {
            if matches!(target, Value::LazyList(_)) {
                return Some(None);
            }
            // Single-threaded: just materialize into an array
            let items = runtime::value_to_list(target);
            Some(Some(Ok(Value::array(items))))
        }
        "NFC" | "NFD" | "NFKC" | "NFKD" => {
            use unicode_normalization::UnicodeNormalization;
            let s = uni_or_str(target);
            let normalized: String = match method {
                "NFC" => s.nfc().collect(),
                "NFD" => s.nfd().collect(),
                "NFKC" => s.nfkc().collect(),
                _ => s.nfkd().collect(),
            };
            Some(Some(Ok(Value::Uni {
                form: method.to_string(),
                text: normalized,
            })))
        }
        _ => None,
    }
}
