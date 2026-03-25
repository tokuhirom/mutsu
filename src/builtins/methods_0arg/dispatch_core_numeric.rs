/// Numeric and element methods: elems, default, Complex-i/i, abs, lsb, msb, rand,
/// uc, lc, fc, tc, sign
use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};
use num_traits::{Signed, ToPrimitive, Zero};
use unicode_normalization::UnicodeNormalization;

use super::{
    int_lsb_value, int_msb_value, is_infinite_range, range_elems_lazy_failure, unicode_foldcase,
};
use crate::builtins::rng::builtin_rand;

pub(super) fn dispatch(
    target: &Value,
    method: &str,
) -> Option<Option<Result<Value, RuntimeError>>> {
    match method {
        "elems" => {
            if let Value::LazyList(list) = target {
                if matches!(
                    list.env.get("__mutsu_lazylist_from_gather"),
                    Some(Value::Bool(true))
                ) {
                    return Some(None);
                }
                let mut ex_attrs = std::collections::HashMap::new();
                ex_attrs.insert(
                    "message".to_string(),
                    Value::str("Cannot .elems a lazy list".to_string()),
                );
                let exception = Value::make_instance(Symbol::intern("X::Cannot::Lazy"), ex_attrs);
                let mut failure_attrs = std::collections::HashMap::new();
                failure_attrs.insert("exception".to_string(), exception);
                failure_attrs.insert("handled".to_string(), Value::Bool(false));
                return Some(Some(Ok(Value::make_instance(
                    Symbol::intern("Failure"),
                    failure_attrs,
                ))));
            }
            if let Some(items) = target.as_list_items() {
                return Some(Some(Ok(Value::Int(items.len() as i64))));
            }
            let result = match target {
                Value::Hash(items) => Value::Int(items.len() as i64),
                Value::Set(items, _) => Value::Int(items.len() as i64),
                Value::Bag(items, _) => Value::Int(items.len() as i64),
                Value::Mix(items, _) => Value::Int(items.len() as i64),
                Value::Junction { values, .. } => Value::Int(values.len() as i64),
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) => {
                    if let Some(Value::Array(bytes, ..)) = attributes.get("bytes") {
                        Value::Int(bytes.len() as i64)
                    } else {
                        Value::Int(0)
                    }
                }
                Value::Channel(_) => {
                    return Some(Some(Err(RuntimeError::new(
                        "Cannot call '.elems' on a Channel instance".to_string(),
                    ))));
                }
                Value::Range(start, end) if *start == i64::MIN || *end == i64::MAX => {
                    return Some(range_elems_lazy_failure("elems"));
                }
                Value::Range(start, end) => Value::Int((*end - *start + 1).max(0)),
                Value::RangeExcl(start, end) if *start == i64::MIN || *end == i64::MAX => {
                    return Some(range_elems_lazy_failure("elems"));
                }
                Value::RangeExcl(start, end) => Value::Int((*end - *start).max(0)),
                Value::RangeExclStart(start, end) if *start == i64::MIN || *end == i64::MAX => {
                    return Some(range_elems_lazy_failure("elems"));
                }
                Value::RangeExclStart(start, end) => Value::Int((*end - *start).max(0)),
                Value::RangeExclBoth(start, end) if *start == i64::MIN || *end == i64::MAX => {
                    return Some(range_elems_lazy_failure("elems"));
                }
                Value::RangeExclBoth(start, end) => Value::Int((*end - *start - 1).max(0)),
                Value::GenericRange { .. } if is_infinite_range(target) => {
                    return Some(range_elems_lazy_failure("elems"));
                }
                Value::GenericRange { .. } => {
                    let list = crate::runtime::utils::value_to_list(target);
                    Value::Int(list.len() as i64)
                }
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Stash" => match attributes.get("symbols") {
                    Some(Value::Hash(map)) => Value::Int(map.len() as i64),
                    _ => Value::Int(0),
                },
                _ => Value::Int(1),
            };
            Some(Some(Ok(result)))
        }
        "default" => {
            let result = match target {
                Value::Array(..) | Value::Hash(..) => Value::Package(Symbol::intern("Any")),
                Value::Set(..) => Value::Bool(false),
                Value::Bag(..) | Value::Mix(..) => Value::Int(0),
                _ => return Some(None),
            };
            Some(Some(Ok(result)))
        }
        "Complex-i" | "i" => {
            let imag = match target {
                Value::Int(i) => *i as f64,
                Value::Num(f) => *f,
                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                Value::FatRat(n, d) if *d != 0 => *n as f64 / *d as f64,
                Value::BigInt(n) => n.to_f64().unwrap_or(0.0),
                Value::BigRat(n, d) if !d.is_zero() => {
                    n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0)
                }
                Value::Complex(r, i) => return Some(Some(Ok(Value::Complex(-*i, *r)))),
                _ => return Some(None),
            };
            Some(Some(Ok(Value::Complex(0.0, imag))))
        }
        "abs" => {
            let result = match target {
                Value::Int(i) => Value::Int(i.abs()),
                Value::BigInt(n) => Value::bigint(n.as_ref().abs()),
                Value::Num(f) => Value::Num(f.abs()),
                Value::Rat(n, d) => Value::Rat(n.abs(), *d),
                Value::FatRat(n, d) => Value::FatRat(n.abs(), *d),
                Value::Complex(r, i) => Value::Num((r * r + i * i).sqrt()),
                Value::Bool(b) => Value::Int(if *b { 1 } else { 0 }),
                _ => return Some(None),
            };
            Some(Some(Ok(result)))
        }
        "lsb" => Some(int_lsb_value(target).map(Ok)),
        "msb" => Some(int_msb_value(target).map(Ok)),
        "rand" => {
            let max = match target {
                Value::Int(n) => *n as f64,
                Value::Num(n) => *n,
                Value::Rat(n, d) => *n as f64 / *d as f64,
                Value::Range(start, end) => {
                    let from = *start as f64;
                    let to = *end as f64;
                    let v = from + builtin_rand() * (to - from);
                    return Some(Some(Ok(Value::Num(v))));
                }
                Value::RangeExcl(start, end) => {
                    let from = *start as f64;
                    let to = *end as f64;
                    if from >= to {
                        return Some(Some(Ok(Value::Nil)));
                    }
                    let v = from + builtin_rand() * (to - from);
                    // Ensure we don't generate the excluded endpoint
                    let v = if v >= to {
                        f64::from_bits(to.to_bits().saturating_sub(1))
                    } else {
                        v
                    };
                    return Some(Some(Ok(Value::Num(v))));
                }
                Value::RangeExclStart(start, end) => {
                    let from = *start as f64;
                    let to = *end as f64;
                    if from >= to {
                        return Some(Some(Ok(Value::Nil)));
                    }
                    let v = from + builtin_rand() * (to - from);
                    // Ensure we don't generate the excluded endpoint
                    let v = if v <= from {
                        f64::from_bits(from.to_bits().saturating_add(1))
                    } else {
                        v
                    };
                    return Some(Some(Ok(Value::Num(v))));
                }
                Value::RangeExclBoth(start, end) => {
                    let from = *start as f64;
                    let to = *end as f64;
                    if from >= to {
                        return Some(Some(Ok(Value::Nil)));
                    }
                    let v = from + builtin_rand() * (to - from);
                    // Ensure we don't generate either excluded endpoint
                    let v = if v <= from {
                        f64::from_bits(from.to_bits().saturating_add(1))
                    } else {
                        v
                    };
                    let v = if v >= to {
                        f64::from_bits(to.to_bits().saturating_sub(1))
                    } else {
                        v
                    };
                    return Some(Some(Ok(Value::Num(v))));
                }
                Value::GenericRange {
                    start,
                    end,
                    excl_start,
                    excl_end,
                } => {
                    let make_rand_failure = || -> Value {
                        let mut ex_attrs = std::collections::HashMap::new();
                        ex_attrs.insert(
                            "message".to_string(),
                            Value::str(
                                "Cannot get a random value from a non-numeric Range".to_string(),
                            ),
                        );
                        let ex = Value::make_instance(
                            crate::symbol::Symbol::intern("X::AdHoc"),
                            ex_attrs,
                        );
                        let mut failure_attrs = std::collections::HashMap::new();
                        failure_attrs.insert("exception".to_string(), ex);
                        Value::make_instance(
                            crate::symbol::Symbol::intern("Failure"),
                            failure_attrs,
                        )
                    };
                    let Some(mut from) = runtime::to_float_value(start) else {
                        return Some(Some(Ok(make_rand_failure())));
                    };
                    let Some(mut to) = runtime::to_float_value(end) else {
                        return Some(Some(Ok(make_rand_failure())));
                    };
                    if *excl_start {
                        from = f64::from_bits(from.to_bits().saturating_add(1));
                    }
                    if *excl_end {
                        to = f64::from_bits(to.to_bits().saturating_sub(1));
                    }
                    if !from.is_finite() || !to.is_finite() || from > to {
                        return Some(Some(Ok(Value::Nil)));
                    }
                    let v = from + builtin_rand() * (to - from);
                    return Some(Some(Ok(Value::Num(v))));
                }
                // Cool types: numify first (e.g., List.rand returns rand in 0..^elems)
                Value::Array(items, ..) => items.len() as f64,
                Value::Seq(items) => items.len() as f64,
                Value::Str(s) => s.parse::<f64>().unwrap_or(0.0),
                Value::Bool(b) => {
                    if *b {
                        1.0
                    } else {
                        0.0
                    }
                }
                _ => return Some(None),
            };
            Some(Some(Ok(Value::Num(builtin_rand() * max))))
        }
        "uc" => Some(Some(Ok(Value::str(
            target
                .to_string_value()
                .to_uppercase()
                .nfc()
                .collect::<String>(),
        )))),
        "lc" => Some(Some(Ok(Value::str(
            target
                .to_string_value()
                .to_lowercase()
                .nfc()
                .collect::<String>(),
        )))),
        "fc" => Some(Some(Ok(Value::str(unicode_foldcase(
            &target.to_string_value(),
        ))))),
        "tc" => Some(Some(Ok(Value::str(
            crate::builtins::unicode::titlecase_string(&target.to_string_value()),
        )))),
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
                Value::Rat(n, d) => {
                    if *d == 0 {
                        if *n > 0 {
                            Value::Int(1)
                        } else if *n < 0 {
                            Value::Int(-1)
                        } else {
                            Value::Num(f64::NAN)
                        }
                    } else {
                        // sign is determined by n/d
                        let sign = n.signum() * d.signum();
                        Value::Int(sign)
                    }
                }
                Value::FatRat(n, d) => {
                    if *d == 0 {
                        if *n > 0 {
                            Value::Int(1)
                        } else if *n < 0 {
                            Value::Int(-1)
                        } else {
                            Value::Num(f64::NAN)
                        }
                    } else {
                        let sign = n.signum() * d.signum();
                        Value::Int(sign)
                    }
                }
                Value::BigInt(n) => {
                    use num_bigint::Sign;
                    Value::Int(match n.sign() {
                        Sign::Plus => 1,
                        Sign::Minus => -1,
                        Sign::NoSign => 0,
                    })
                }
                Value::Complex(re, im) => {
                    // sign of a Complex number is the number divided by its absolute value
                    let abs = (re * re + im * im).sqrt();
                    if abs == 0.0 {
                        Value::Complex(0.0, 0.0)
                    } else {
                        Value::Complex(re / abs, im / abs)
                    }
                }
                Value::Enum { value, .. } => Value::Int(value.as_i64().signum()),
                _ => return Some(None),
            };
            Some(Some(Ok(result)))
        }
        _ => None,
    }
}
