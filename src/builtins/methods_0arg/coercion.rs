use crate::builtins::primality::{is_prime_bigint, is_prime_i64};
use crate::value::{RuntimeError, Value};
use std::collections::HashMap;

/// Type coercion and specialized 0-arg methods: numerator, denominator, nude,
/// is-prime, isNaN, re, im, conj, reals, Complex, key, value, Slip, list/Array, Range
pub(super) fn dispatch(target: &Value, method: &str) -> Option<Result<Value, RuntimeError>> {
    match method {
        "numerator" => match target {
            Value::Rat(n, _) => Some(Ok(Value::Int(*n))),
            Value::FatRat(n, _) => Some(Ok(Value::Int(*n))),
            Value::Int(i) => Some(Ok(Value::Int(*i))),
            _ => Some(Ok(Value::Int(0))),
        },
        "denominator" => match target {
            Value::Rat(_, d) => Some(Ok(Value::Int(*d))),
            Value::FatRat(_, d) => Some(Ok(Value::Int(*d))),
            Value::Int(_) => Some(Ok(Value::Int(1))),
            _ => Some(Ok(Value::Int(1))),
        },
        "isNaN" => match target {
            Value::Rat(0, 0) => Some(Ok(Value::Bool(true))),
            Value::Num(f) => Some(Ok(Value::Bool(f.is_nan()))),
            _ => Some(Ok(Value::Bool(false))),
        },
        "nude" => match target {
            Value::Rat(n, d) => Some(Ok(Value::array(vec![Value::Int(*n), Value::Int(*d)]))),
            Value::FatRat(n, d) => Some(Ok(Value::array(vec![Value::Int(*n), Value::Int(*d)]))),
            Value::Int(i) => Some(Ok(Value::array(vec![Value::Int(*i), Value::Int(1)]))),
            _ => Some(Ok(Value::array(vec![Value::Int(0), Value::Int(1)]))),
        },
        "is-prime" => Some(value_is_prime(target)),
        "re" => match target {
            Value::Complex(r, _) => Some(Ok(Value::Num(*r))),
            Value::Int(i) => Some(Ok(Value::Num(*i as f64))),
            Value::Num(f) => Some(Ok(Value::Num(*f))),
            _ => Some(Ok(Value::Num(0.0))),
        },
        "im" => match target {
            Value::Complex(_, i) => Some(Ok(Value::Num(*i))),
            _ => Some(Ok(Value::Num(0.0))),
        },
        "conj" => match target {
            Value::Complex(r, i) => Some(Ok(Value::Complex(*r, -i))),
            Value::Int(i) => Some(Ok(Value::Complex(*i as f64, 0.0))),
            Value::Num(f) => Some(Ok(Value::Complex(*f, 0.0))),
            _ => Some(Ok(Value::Complex(0.0, 0.0))),
        },
        "reals" => match target {
            Value::Complex(r, i) => Some(Ok(Value::array(vec![Value::Num(*r), Value::Num(*i)]))),
            _ => None,
        },
        "polar" => match target {
            Value::Complex(r, i) => {
                let mag = (r * r + i * i).sqrt();
                let angle = i.atan2(*r);
                Some(Ok(Value::array(vec![Value::Num(mag), Value::Num(angle)])))
            }
            Value::Int(i) => {
                let f = *i as f64;
                let mag = f.abs();
                let angle = if f < 0.0 { std::f64::consts::PI } else { 0.0 };
                Some(Ok(Value::array(vec![Value::Num(mag), Value::Num(angle)])))
            }
            Value::Num(f) => {
                let mag = f.abs();
                let angle = if *f < 0.0 { std::f64::consts::PI } else { 0.0 };
                Some(Ok(Value::array(vec![Value::Num(mag), Value::Num(angle)])))
            }
            _ => None,
        },
        "cis" => match target {
            Value::Int(i) => {
                let x = *i as f64;
                Some(Ok(Value::Complex(x.cos(), x.sin())))
            }
            Value::Num(f) => Some(Ok(Value::Complex(f.cos(), f.sin()))),
            Value::Rat(n, d) if *d != 0 => {
                let x = *n as f64 / *d as f64;
                Some(Ok(Value::Complex(x.cos(), x.sin())))
            }
            _ => None,
        },
        "Complex" => match target {
            Value::Complex(_, _) => Some(Ok(target.clone())),
            Value::Int(i) => Some(Ok(Value::Complex(*i as f64, 0.0))),
            Value::Num(f) => Some(Ok(Value::Complex(*f, 0.0))),
            _ => Some(Ok(Value::Complex(0.0, 0.0))),
        },
        "key" => match target {
            Value::Pair(k, _) => Some(Ok(Value::Str(k.clone()))),
            Value::ValuePair(k, _) => Some(Ok(*k.clone())),
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Pair" => {
                Some(Ok(attributes.get("key").cloned().unwrap_or(Value::Nil)))
            }
            Value::Bool(true) => Some(Ok(Value::Str("True".to_string()))),
            Value::Bool(false) => Some(Ok(Value::Str("False".to_string()))),
            _ => None,
        },
        "value" => match target {
            Value::Pair(_, v) | Value::ValuePair(_, v) => Some(Ok(*v.clone())),
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Pair" => {
                if let (Some(Value::Hash(hash)), Some(Value::Str(key))) =
                    (attributes.get("__mutsu_hash_ref"), attributes.get("key"))
                {
                    Some(Ok(hash.get(key).cloned().unwrap_or(Value::Nil)))
                } else {
                    Some(Ok(attributes.get("value").cloned().unwrap_or(Value::Nil)))
                }
            }
            Value::Bool(b) => Some(Ok(Value::Int(if *b { 1 } else { 0 }))),
            _ => None,
        },
        "antipair" => match target {
            Value::Pair(k, v) => Some(Ok(match v.as_ref() {
                Value::Str(s) => Value::Pair(s.clone(), Box::new(Value::Str(k.clone()))),
                _ => Value::ValuePair(v.clone(), Box::new(Value::Str(k.clone()))),
            })),
            Value::ValuePair(k, v) => Some(Ok(Value::ValuePair(
                Box::new(*v.clone()),
                Box::new(*k.clone()),
            ))),
            _ => None,
        },
        "Slip" => match target {
            Value::Array(items, ..) | Value::Seq(items) => Some(Ok(Value::Slip(items.clone()))),
            Value::Slip(_) => Some(Ok(target.clone())),
            Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. } => Some(Ok(Value::slip(
                crate::runtime::utils::value_to_list(target),
            ))),
            _ => Some(Ok(Value::slip(vec![target.clone()]))),
        },
        "List" => match target {
            Value::Range(a, b) => {
                if *b == i64::MAX || *a == i64::MIN {
                    Some(Ok(target.clone()))
                } else {
                    Some(Ok(Value::array((*a..=*b).map(Value::Int).collect())))
                }
            }
            Value::RangeExcl(a, b) => {
                if *b == i64::MAX || *a == i64::MIN {
                    Some(Ok(target.clone()))
                } else {
                    Some(Ok(Value::array((*a..*b).map(Value::Int).collect())))
                }
            }
            Value::Array(items, _) | Value::Seq(items) => Some(Ok(Value::array(items.to_vec()))),
            Value::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                // String range: expand using codepoint succession
                if let (Value::Str(s), Value::Str(e)) = (start.as_ref(), end.as_ref()) {
                    if s.chars().count() == 1 && e.chars().count() == 1 {
                        let sc = s.chars().next().unwrap() as u32;
                        let ec = e.chars().next().unwrap() as u32;
                        let start_cp = if *excl_start { sc + 1 } else { sc };
                        let items: Vec<Value> = if sc <= ec {
                            let end_cp = if *excl_end { ec } else { ec + 1 };
                            (start_cp..end_cp)
                                .filter_map(char::from_u32)
                                .map(|c| Value::Str(c.to_string()))
                                .collect()
                        } else {
                            let end_cp = if *excl_end { ec } else { ec.saturating_sub(1) };
                            (end_cp + 1..=start_cp)
                                .rev()
                                .filter_map(char::from_u32)
                                .map(|c| Value::Str(c.to_string()))
                                .collect()
                        };
                        Some(Ok(Value::array(items)))
                    } else {
                        Some(Ok(Value::array(vec![target.clone()])))
                    }
                } else {
                    // Numeric generic range
                    let items = crate::runtime::utils::value_to_list(target);
                    Some(Ok(Value::array(items)))
                }
            }
            Value::RangeExclStart(a, b) => {
                if *b == i64::MAX {
                    Some(Ok(target.clone()))
                } else {
                    Some(Ok(Value::array((*a + 1..=*b).map(Value::Int).collect())))
                }
            }
            Value::RangeExclBoth(a, b) => {
                if *b == i64::MAX {
                    Some(Ok(target.clone()))
                } else {
                    Some(Ok(Value::array((*a + 1..*b).map(Value::Int).collect())))
                }
            }
            _ => Some(Ok(Value::array(vec![target.clone()]))),
        },
        "list" | "Array" => match target {
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Supply" => {
                let items = match attributes.get("values") {
                    Some(Value::Array(items, ..)) => items.to_vec(),
                    _ => Vec::new(),
                };
                Some(Ok(Value::array(items)))
            }
            Value::Range(a, b) => {
                if *b == i64::MAX || *a == i64::MIN {
                    Some(Ok(target.clone()))
                } else {
                    Some(Ok(Value::array((*a..=*b).map(Value::Int).collect())))
                }
            }
            Value::RangeExcl(a, b) => {
                if *b == i64::MAX || *a == i64::MIN {
                    Some(Ok(target.clone()))
                } else {
                    Some(Ok(Value::array((*a..*b).map(Value::Int).collect())))
                }
            }
            Value::RangeExclStart(a, b) => {
                if *b == i64::MAX || *a == i64::MIN {
                    Some(Ok(target.clone()))
                } else {
                    Some(Ok(Value::array((a + 1..=*b).map(Value::Int).collect())))
                }
            }
            Value::RangeExclBoth(a, b) => {
                if *b == i64::MAX || *a == i64::MIN {
                    Some(Ok(target.clone()))
                } else {
                    Some(Ok(Value::array((a + 1..*b).map(Value::Int).collect())))
                }
            }
            Value::GenericRange { .. } => {
                let items = crate::runtime::utils::value_to_list(target);
                Some(Ok(Value::array(items)))
            }
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Buf" || class_name == "Blob" => {
                let bytes = match attributes.get("bytes") {
                    Some(Value::Array(items, ..)) => items.to_vec(),
                    _ => Vec::new(),
                };
                Some(Ok(Value::array(bytes)))
            }
            Value::Array(..) => Some(Ok(target.clone())),
            Value::Channel(_) => None, // fall through to runtime for drain
            _ => Some(Ok(Value::array(vec![target.clone()]))),
        },
        "Range" => match target {
            Value::Array(items, ..) => Some(Ok(Value::RangeExcl(0, items.len() as i64))),
            Value::Str(s) => Some(Ok(Value::RangeExcl(0, s.chars().count() as i64))),
            Value::Range(_, _)
            | Value::RangeExcl(_, _)
            | Value::RangeExclStart(_, _)
            | Value::RangeExclBoth(_, _)
            | Value::GenericRange { .. } => Some(Ok(target.clone())),
            _ => None,
        },
        "Supply" => {
            if let Value::Instance { class_name, .. } = target
                && class_name == "Supplier"
            {
                // Supplier.Supply has runtime behavior (live stream), not generic coercion.
                return None;
            }
            // .Supply on an existing Supply is a noop — return self
            if let Value::Instance { class_name, .. } = target
                && class_name == "Supply"
            {
                return Some(Ok(target.clone()));
            }
            // Supplier.Supply must be handled by runtime native methods
            // so the returned Supply remains linked to Supplier.emit/.done.
            if let Value::Instance { class_name, .. } = target
                && class_name == "Supplier"
            {
                return None;
            }
            let values = match target {
                Value::Array(items, ..) => items.to_vec(),
                Value::Range(a, b) => (*a..=*b).map(Value::Int).collect(),
                Value::RangeExcl(a, b) => (*a..*b).map(Value::Int).collect(),
                _ => vec![target.clone()],
            };
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("values".to_string(), Value::array(values));
            attrs.insert("taps".to_string(), Value::array(Vec::new()));
            attrs.insert("live".to_string(), Value::Bool(false));
            Some(Ok(Value::make_instance("Supply".to_string(), attrs)))
        }
        _ => None,
    }
}

/// Implement is-prime for any Value, with type coercion.
/// Negative numbers are not prime.
/// Complex with non-zero imaginary throws X::Numeric::Real.
/// Str, Num, Rat, FatRat coerce to Int first.
pub(crate) fn value_is_prime(target: &Value) -> Result<Value, RuntimeError> {
    // Unwrap allomorphic types (Mixin) to the inner numeric value
    if let Value::Mixin(inner, _) = target {
        return value_is_prime(inner);
    }
    match target {
        Value::Int(n) => {
            if *n < 0 {
                return Ok(Value::Bool(false));
            }
            Ok(Value::Bool(is_prime_i64(*n)))
        }
        Value::BigInt(n) => {
            if n.sign() == num_bigint::Sign::Minus {
                return Ok(Value::Bool(false));
            }
            Ok(Value::Bool(is_prime_bigint(n)))
        }
        Value::Num(f) => {
            if *f < 0.0 || f.fract() != 0.0 {
                return Ok(Value::Bool(false));
            }
            let n = *f as i64;
            Ok(Value::Bool(is_prime_i64(n)))
        }
        Value::Rat(n, d) | Value::FatRat(n, d) => {
            if *d == 0 {
                return Ok(Value::Bool(false));
            }
            if *n < 0 {
                return Ok(Value::Bool(false));
            }
            if n % d != 0 {
                return Ok(Value::Bool(false));
            }
            let int_val = n / d;
            Ok(Value::Bool(is_prime_i64(int_val)))
        }
        Value::BigRat(n, d) => {
            use num_traits::Zero;
            if d.is_zero() {
                return Ok(Value::Bool(false));
            }
            if n.sign() == num_bigint::Sign::Minus {
                return Ok(Value::Bool(false));
            }
            let (quot, rem) = num_integer::Integer::div_rem(n, d);
            if !rem.is_zero() {
                return Ok(Value::Bool(false));
            }
            Ok(Value::Bool(is_prime_bigint(&quot)))
        }
        Value::Complex(_, i) if *i != 0.0 => {
            let mut attrs = HashMap::new();
            attrs.insert(
                "message".to_string(),
                Value::Str(format!(
                    "Cannot convert {} to Real: imaginary part not zero",
                    match target {
                        Value::Complex(r, i) => {
                            if *i >= 0.0 {
                                format!("{}+{}i", r, i)
                            } else {
                                format!("{}{}i", r, i)
                            }
                        }
                        _ => format!("{:?}", target),
                    }
                )),
            );
            attrs.insert("target".to_string(), Value::Str("Real".to_string()));
            attrs.insert("source".to_string(), target.clone());
            let ex = Value::make_instance("X::Numeric::Real".to_string(), attrs);
            let mut err =
                RuntimeError::new("Cannot convert Complex to Real: imaginary part not zero");
            err.exception = Some(Box::new(ex));
            Err(err)
        }
        Value::Complex(r, _) => {
            // imaginary is 0, treat as real
            if *r < 0.0 || r.fract() != 0.0 {
                return Ok(Value::Bool(false));
            }
            let n = *r as i64;
            Ok(Value::Bool(is_prime_i64(n)))
        }
        Value::Str(s) => {
            // Try to parse as a number
            // Handle Unicode minus sign
            let s = s.replace('\u{2212}', "-");
            if let Ok(n) = s.parse::<i64>() {
                if n < 0 {
                    return Ok(Value::Bool(false));
                }
                return Ok(Value::Bool(is_prime_i64(n)));
            }
            if let Ok(f) = s.parse::<f64>() {
                if f < 0.0 || f.fract() != 0.0 {
                    return Ok(Value::Bool(false));
                }
                let n = f as i64;
                return Ok(Value::Bool(is_prime_i64(n)));
            }
            Ok(Value::Bool(false))
        }
        _ => Ok(Value::Bool(false)),
    }
}
