/// Type coercion methods: self, clone, defined, DEFINITE, WHICH, Bool, Str, Int, UInt,
/// Num, Real, Numeric, Bridge
use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};
use num_traits::{ToPrimitive, Zero};
use std::sync::Arc;

use super::{normalize_unicode_digits, parse_raku_int_from_str};

pub(super) fn dispatch(
    target: &Value,
    method: &str,
) -> Option<Option<Result<Value, RuntimeError>>> {
    match method {
        "self" => {
            // For unhandled Failures, .self throws the exception
            if let Value::Instance {
                class_name,
                attributes,
                ..
            } = target
                && class_name == "Failure"
                && !target.is_failure_handled()
                && let Some(ex) = attributes.get("exception")
            {
                let msg = ex.to_string_value();
                let mut err = crate::value::RuntimeError::new(msg);
                err.exception = Some(Box::new(ex.clone()));
                return Some(Some(Err(err)));
            }
            Some(Some(Ok(target.clone())))
        }
        "clone" => {
            match target {
                Value::Package(_) | Value::Nil => Some(Some(Ok(target.clone()))),
                Value::Array(items, kind) => {
                    Some(Some(Ok(Value::Array(Arc::new(items.to_vec()), *kind))))
                }
                Value::Hash(map) => Some(Some(Ok(Value::Hash(Arc::new((**map).clone()))))),
                Value::Set(data, mutable) => {
                    Some(Some(Ok(Value::Set(Arc::new((**data).clone()), *mutable))))
                }
                Value::Bag(data, mutable) => {
                    Some(Some(Ok(Value::Bag(Arc::new((**data).clone()), *mutable))))
                }
                Value::Mix(data, mutable) => {
                    Some(Some(Ok(Value::Mix(Arc::new((**data).clone()), *mutable))))
                }
                Value::Sub(data) => {
                    // Clone the sub with a new id so state variables are independent
                    let mut new_data = (**data).clone();
                    new_data.id = crate::value::next_instance_id();
                    Some(Some(Ok(Value::Sub(Arc::new(new_data)))))
                }
                _ => Some(None), // fall through to slow path for instances etc.
            }
        }
        "defined" => {
            // Calling .defined on a Failure marks it as handled
            if let Value::Instance { class_name, .. } = target
                && class_name == "Failure"
            {
                target.mark_failure_handled();
            }
            // For junctions, autothread .defined over eigenstates and collapse
            if let Value::Junction { kind, values } = target {
                fn value_defined(v: &Value) -> bool {
                    match v {
                        Value::Nil | Value::Package(_) => false,
                        Value::Slip(items) if items.is_empty() => false,
                        Value::Instance { class_name, .. } if class_name == "Failure" => false,
                        Value::Junction { kind, values } => {
                            let results: Vec<bool> = values.iter().map(value_defined).collect();
                            collapse_junction(kind, &results)
                        }
                        _ => true,
                    }
                }
                fn collapse_junction(kind: &crate::value::JunctionKind, results: &[bool]) -> bool {
                    use crate::value::JunctionKind;
                    match kind {
                        JunctionKind::Any => results.iter().any(|&b| b),
                        JunctionKind::All => results.iter().all(|&b| b),
                        JunctionKind::One => results.iter().filter(|&&b| b).count() == 1,
                        JunctionKind::None => results.iter().all(|&b| !b),
                    }
                }
                let results: Vec<bool> = values.iter().map(value_defined).collect();
                let collapsed = collapse_junction(kind, &results);
                Some(Some(Ok(Value::Bool(collapsed))))
            } else {
                Some(Some(Ok(Value::Bool(match target {
                    Value::Nil | Value::Package(_) => false,
                    Value::Slip(items) if items.is_empty() => false,
                    Value::Instance { class_name, .. } if class_name == "Failure" => false,
                    _ => true,
                }))))
            }
        }
        "DEFINITE" => Some(Some(Ok(Value::Bool(match target {
            Value::Nil | Value::Package(_) | Value::CustomType { .. } => false,
            Value::Slip(items) if items.is_empty() => false,
            Value::Instance { class_name, .. } if class_name == "Failure" => false,
            _ => true,
        })))),
        "WHICH" => {
            // Determine if this is a value type (ValueObjAt) or reference type (ObjAt)
            let is_value_type = matches!(
                target,
                Value::Int(_)
                    | Value::BigInt(_)
                    | Value::Num(_)
                    | Value::Str(_)
                    | Value::Bool(_)
                    | Value::Rat(_, _)
                    | Value::BigRat(_, _)
                    | Value::FatRat(_, _)
                    | Value::Complex(_, _)
                    | Value::Set(_, _)
                    | Value::Bag(_, _)
                    | Value::Mix(_, _)
                    | Value::Junction { .. }
                    | Value::Nil
            );
            let which_str = match target {
                Value::Package(name) => format!("{}|U{}", name.resolve(), name.id()),
                Value::CustomType { name, id, .. } => {
                    format!("{}|U{}", name.resolve(), id)
                }
                Value::Int(n) => format!("Int|{}", n),
                Value::BigInt(n) => format!("Int|{}", n),
                Value::Num(n) => format!("Num|{}", n),
                Value::Str(s) => format!("Str|{}", s),
                Value::Bool(b) => format!("Bool|{}", if *b { 1 } else { 0 }),
                Value::Rat(n, d) => format!("Rat|{}/{}", n, d),
                Value::FatRat(n, d) => format!("FatRat|{}/{}", n, d),
                Value::Complex(r, i) => format!("Complex|{}+{}i", r, i),
                Value::Nil => format!("Nil|U{}", Symbol::intern("Nil").id()),
                Value::Set(set, _) => {
                    let mut keys: Vec<&String> = set.iter().collect();
                    keys.sort();
                    use std::hash::{Hash, Hasher};
                    let mut hasher = std::collections::hash_map::DefaultHasher::new();
                    for k in &keys {
                        k.hash(&mut hasher);
                    }
                    format!("Set|{:016X}", hasher.finish())
                }
                Value::Bag(bag, _) => {
                    let mut pairs: Vec<(&String, &i64)> = bag.iter().collect();
                    pairs.sort_by(|a, b| a.0.cmp(b.0));
                    use std::hash::{Hash, Hasher};
                    let mut hasher = std::collections::hash_map::DefaultHasher::new();
                    for (k, v) in &pairs {
                        k.hash(&mut hasher);
                        v.hash(&mut hasher);
                    }
                    format!("Bag|{:016X}", hasher.finish())
                }
                Value::Mix(mix, _) => {
                    let mut pairs: Vec<(&String, &f64)> = mix.iter().collect();
                    pairs.sort_by(|a, b| a.0.cmp(b.0));
                    use std::hash::{Hash, Hasher};
                    let mut hasher = std::collections::hash_map::DefaultHasher::new();
                    for (k, v) in &pairs {
                        k.hash(&mut hasher);
                        v.to_bits().hash(&mut hasher);
                    }
                    format!("Mix|{:016X}", hasher.finish())
                }
                Value::Sub(sub_data) => {
                    format!(
                        "{}|{}",
                        runtime::utils::value_type_name(target),
                        sub_data.id
                    )
                }
                Value::Regex(pattern) => {
                    format!("Regex|{:p}", Arc::as_ptr(pattern))
                }
                Value::RegexWithAdverbs { pattern, .. } => {
                    format!("Regex|{:p}", Arc::as_ptr(pattern))
                }
                Value::Instance { id, .. } => {
                    format!("{}|{}", runtime::utils::value_type_name(target), id)
                }
                Value::Junction { kind, values } => {
                    use std::hash::{Hash, Hasher};
                    let mut hasher = std::collections::hash_map::DefaultHasher::new();
                    // Hash the kind
                    match kind {
                        crate::value::JunctionKind::Any => 0u8.hash(&mut hasher),
                        crate::value::JunctionKind::All => 1u8.hash(&mut hasher),
                        crate::value::JunctionKind::One => 2u8.hash(&mut hasher),
                        crate::value::JunctionKind::None => 3u8.hash(&mut hasher),
                    }
                    // Hash each eigenstate's string representation
                    for v in values.iter() {
                        v.to_string_value().hash(&mut hasher);
                    }
                    format!("Junction|{:016X}", hasher.finish())
                }
                Value::Seq(items) => {
                    format!("Seq|{:p}", Arc::as_ptr(items))
                }
                Value::Slip(items) => {
                    format!("Slip|{:p}", Arc::as_ptr(items))
                }
                Value::Array(items, ..) => {
                    format!("Array|{:p}", Arc::as_ptr(items))
                }
                Value::Hash(map) => {
                    format!("Hash|{:p}", Arc::as_ptr(map))
                }
                Value::Promise(p) => {
                    format!("Promise|{:p}", p.arc_ptr())
                }
                Value::Channel(c) => {
                    format!("Channel|{:p}", c.arc_ptr())
                }
                Value::Whatever => {
                    use std::sync::atomic::{AtomicU64, Ordering};
                    static WHATEVER_ID: AtomicU64 = AtomicU64::new(0);
                    // Whatever is a type object singleton
                    let id = WHATEVER_ID.load(Ordering::Relaxed);
                    if id == 0 {
                        WHATEVER_ID.store(1, Ordering::Relaxed);
                    }
                    format!("Whatever|U{}", WHATEVER_ID.load(Ordering::Relaxed))
                }
                _ => {
                    // Fallback: use a global counter to ensure uniqueness
                    // This is not ideal since the same value will get different IDs
                    // on repeated calls, but it prevents false identity collisions.
                    use std::sync::atomic::{AtomicU64, Ordering};
                    static COUNTER: AtomicU64 = AtomicU64::new(1);
                    format!(
                        "{}|{}",
                        runtime::utils::value_type_name(target),
                        COUNTER.fetch_add(1, Ordering::Relaxed)
                    )
                }
            };
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("WHICH".to_string(), Value::str(which_str));
            let objat_class = if is_value_type { "ValueObjAt" } else { "ObjAt" };
            Some(Some(Ok(Value::make_instance(
                Symbol::intern(objat_class),
                attrs,
            ))))
        }
        "Bool" => {
            if matches!(target, Value::Instance { .. })
                && (target.does_check("Real") || target.does_check("Numeric"))
            {
                Some(None)
            } else if matches!(
                target,
                Value::Regex(_)
                    | Value::RegexWithAdverbs { .. }
                    | Value::Routine { is_regex: true, .. }
            ) {
                // Regex.Bool needs to smartmatch against $_, which requires
                // runtime context. Fall through to the runtime handler.
                Some(None)
            } else {
                // Calling .Bool on a Failure marks it as handled
                if let Value::Instance { class_name, .. } = target
                    && class_name == "Failure"
                {
                    target.mark_failure_handled();
                }
                Some(Some(Ok(Value::Bool(target.truthy()))))
            }
        }
        "Str" | "Stringy" => Some(match target {
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Failure" => {
                // Using a Failure in string context throws the wrapped exception.
                if let Some(ex) = attributes.get("exception") {
                    Some(Err(RuntimeError::from_exception_value(ex.clone())))
                } else {
                    Some(Err(RuntimeError::new("Failed")))
                }
            }
            Value::Package(_) | Value::Instance { .. } => None,
            Value::LazyList(_) => None, // fall through to runtime to force the list
            Value::Enum { .. } => None, // fall through to enum dispatch for string enum support
            Value::Str(s) if s.as_str() == "IO::Special" => Some(Ok(Value::str_from(""))),
            Value::Rat(_, 0) | Value::FatRat(_, 0) => {
                // Zero-denominator Rat/FatRat .Str throws X::Numeric::DivideByZero
                None // fall through to runtime for exception with proper context
            }
            _ => Some(Ok(Value::str(target.to_string_value()))),
        }),
        "Int" => {
            let result = match target {
                Value::Int(i) => Value::Int(*i),
                Value::BigInt(_) => target.clone(),
                Value::Num(f) => {
                    if f.is_nan() || f.is_infinite() {
                        let msg = format!(
                            "Cannot convert {} to Int: not a finite number",
                            if f.is_nan() {
                                "NaN".to_string()
                            } else if f.is_sign_positive() {
                                "Inf".to_string()
                            } else {
                                "-Inf".to_string()
                            }
                        );
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("message".to_string(), Value::str(msg));
                        attrs.insert("source".to_string(), target.clone());
                        attrs.insert("target".to_string(), Value::str_from("Int"));
                        let ex = Value::make_instance(
                            crate::symbol::Symbol::intern("X::Numeric::CannotConvert"),
                            attrs,
                        );
                        let mut failure_attrs = std::collections::HashMap::new();
                        failure_attrs.insert("exception".to_string(), ex);
                        return Some(Some(Ok(Value::make_instance(
                            crate::symbol::Symbol::intern("Failure"),
                            failure_attrs,
                        ))));
                    }
                    Value::Int(*f as i64)
                }
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Instant" || class_name == "Duration" => {
                    let numeric = attributes
                        .get("value")
                        .and_then(|v| match v {
                            Value::Int(i) => Some(*i as f64),
                            Value::BigInt(n) => Some(n.to_f64().unwrap_or(f64::INFINITY)),
                            Value::Num(f) => Some(*f),
                            Value::Rat(n, d) if *d != 0 => Some(*n as f64 / *d as f64),
                            Value::FatRat(n, d) if *d != 0 => Some(*n as f64 / *d as f64),
                            Value::BigRat(n, d) if !d.is_zero() => {
                                Some(n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0))
                            }
                            _ => None,
                        })
                        .unwrap_or(0.0);
                    Value::Int(numeric as i64)
                }
                Value::Rat(_, d) if *d == 0 => {
                    return Some(Some(Ok(RuntimeError::divide_by_zero_failure_for_method(
                        "Int", "Rational",
                    ))));
                }
                Value::FatRat(_, d) if *d == 0 => {
                    return Some(Some(Ok(RuntimeError::divide_by_zero_failure_for_method(
                        "Int", "Rational",
                    ))));
                }
                Value::Rat(n, d) if *d != 0 => Value::Int(*n / *d),
                Value::FatRat(n, d) if *d != 0 => Value::Int(*n / *d),
                Value::BigRat(_, d) if d.is_zero() => {
                    return Some(Some(Ok(RuntimeError::divide_by_zero_failure_for_method(
                        "Int", "Rational",
                    ))));
                }
                Value::BigRat(n, d) if !d.is_zero() => {
                    use num_traits::ToPrimitive;
                    Value::Int((n / d).to_i64().unwrap_or(i64::MAX))
                }
                Value::Str(s) => {
                    if let Some(v) = parse_raku_int_from_str(s) {
                        v
                    } else {
                        // Return a Failure (lazy exception) instead of throwing
                        let mut ex_attrs = std::collections::HashMap::new();
                        ex_attrs.insert("source".to_string(), Value::str(s.to_string()));
                        ex_attrs.insert(
                            "reason".to_string(),
                            Value::str(
                                "base-10 number must begin with valid digits or '.'".to_string(),
                            ),
                        );
                        ex_attrs.insert("pos".to_string(), Value::Int(0));
                        ex_attrs.insert(
                            "message".to_string(),
                            Value::str(format!(
                                "Cannot convert string to number: base-10 number must begin with valid digits or '.' in '{}'",
                                s
                            )),
                        );
                        let ex = Value::make_instance(
                            crate::symbol::Symbol::intern("X::Str::Numeric"),
                            ex_attrs,
                        );
                        let mut failure_attrs = std::collections::HashMap::new();
                        failure_attrs.insert("exception".to_string(), ex);
                        failure_attrs.insert("handled".to_string(), Value::Bool(false));
                        return Some(Some(Ok(Value::make_instance(
                            crate::symbol::Symbol::intern("Failure"),
                            failure_attrs,
                        ))));
                    }
                }
                Value::Bool(b) => Value::Int(if *b { 1 } else { 0 }),
                Value::Complex(r, _) => Value::Int(*r as i64),
                Value::Hash(h) => Value::Int(h.len() as i64),
                Value::Array(items, ..) => Value::Int(items.len() as i64),
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
                _ => return Some(None),
            };
            Some(Some(Ok(result)))
        }
        "UInt" => {
            // First coerce to Int, then check non-negative
            let int_result = match target {
                Value::Int(i) => Some(Value::Int(*i)),
                Value::BigInt(_) => Some(target.clone()),
                Value::Num(f) if f.is_finite() => Some(Value::Int(f.trunc() as i64)),
                Value::Rat(n, d) if *d != 0 => Some(Value::Int(*n / *d)),
                Value::Bool(b) => Some(Value::Int(if *b { 1 } else { 0 })),
                Value::Str(s) => {
                    if let Some(v) = parse_raku_int_from_str(s) {
                        Some(v)
                    } else {
                        // Return Failure for invalid string
                        let mut ex_attrs = std::collections::HashMap::new();
                        ex_attrs.insert("source".to_string(), Value::str(s.to_string()));
                        ex_attrs.insert(
                            "reason".to_string(),
                            Value::str(
                                "base-10 number must begin with valid digits or '.'".to_string(),
                            ),
                        );
                        ex_attrs.insert("pos".to_string(), Value::Int(0));
                        ex_attrs.insert(
                            "message".to_string(),
                            Value::str(format!(
                                "Cannot convert string to number: base-10 number must begin with valid digits or '.' in '{}'",
                                s
                            )),
                        );
                        let ex = Value::make_instance(
                            crate::symbol::Symbol::intern("X::Str::Numeric"),
                            ex_attrs,
                        );
                        let mut failure_attrs = std::collections::HashMap::new();
                        failure_attrs.insert("exception".to_string(), ex);
                        failure_attrs.insert("handled".to_string(), Value::Bool(false));
                        return Some(Some(Ok(Value::make_instance(
                            crate::symbol::Symbol::intern("Failure"),
                            failure_attrs,
                        ))));
                    }
                }
                _ => None,
            };
            if let Some(int_val) = int_result {
                // Check non-negative
                let is_neg = match &int_val {
                    Value::Int(i) => *i < 0,
                    Value::BigInt(n) => n.sign() == num_bigint::Sign::Minus,
                    _ => false,
                };
                if is_neg {
                    return Some(Some(Err(RuntimeError::new(format!(
                        "Coercion to UInt out of range. Is: {}, should be in 0..^Inf",
                        int_val.to_string_value()
                    )))));
                }
                Some(Some(Ok(int_val)))
            } else {
                Some(None)
            }
        }
        "Num" => {
            let result = match target {
                Value::Int(i) => Value::Num(*i as f64),
                Value::BigInt(n) => {
                    use num_traits::ToPrimitive;
                    Value::Num(n.to_f64().unwrap_or(f64::INFINITY))
                }
                Value::Num(f) => Value::Num(*f),
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Instant" || class_name == "Duration" => {
                    let numeric = attributes
                        .get("value")
                        .and_then(|v| match v {
                            Value::Int(i) => Some(*i as f64),
                            Value::BigInt(n) => Some(n.to_f64().unwrap_or(f64::INFINITY)),
                            Value::Num(f) => Some(*f),
                            Value::Rat(n, d) if *d != 0 => Some(*n as f64 / *d as f64),
                            Value::FatRat(n, d) if *d != 0 => Some(*n as f64 / *d as f64),
                            Value::BigRat(n, d) if !d.is_zero() => {
                                Some(n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0))
                            }
                            _ => None,
                        })
                        .unwrap_or(0.0);
                    Value::Num(numeric)
                }
                Value::Rat(n, d) | Value::FatRat(n, d) if *d == 0 => Value::Num(if *n == 0 {
                    f64::NAN
                } else if *n > 0 {
                    f64::INFINITY
                } else {
                    f64::NEG_INFINITY
                }),
                Value::Rat(n, d) if *d != 0 => Value::Num(*n as f64 / *d as f64),
                Value::FatRat(n, d) if *d != 0 => Value::Num(*n as f64 / *d as f64),
                Value::BigRat(n, d) if !d.is_zero() => {
                    use num_traits::ToPrimitive;
                    let num = n.to_f64().unwrap_or(0.0);
                    let den = d.to_f64().unwrap_or(1.0);
                    Value::Num(num / den)
                }
                Value::Str(s) => {
                    let trimmed = s.trim();
                    // Normalize U+2212 MINUS SIGN to ASCII hyphen-minus
                    let normalized = trimmed.replace('\u{2212}', "-");
                    if let Ok(f) = normalized.parse::<f64>() {
                        Value::Num(f)
                    } else {
                        return Some(Some(Err(RuntimeError::new(format!(
                            "X::Str::Numeric: Cannot convert string '{}' to a number",
                            s
                        )))));
                    }
                }
                Value::Bool(b) => Value::Num(if *b { 1.0 } else { 0.0 }),
                Value::Complex(_, _) => return Some(None), // fall through to runtime for $*TOLERANCE check
                Value::Array(items, ..) => Value::Num(items.len() as f64),
                Value::Seq(items) | Value::Slip(items) => Value::Num(items.len() as f64),
                _ => return Some(None),
            };
            Some(Some(Ok(result)))
        }
        "Real" => {
            let result = match target {
                Value::Int(i) => Value::Int(*i),
                Value::BigInt(_) => target.clone(),
                Value::Num(f) => Value::Num(*f),
                Value::Rat(n, d) => Value::Rat(*n, *d),
                Value::FatRat(n, d) => Value::FatRat(*n, *d),
                Value::BigRat(n, d) => Value::BigRat(n.clone(), d.clone()),
                Value::Bool(b) => Value::Int(if *b { 1 } else { 0 }),
                Value::Complex(r, im) => {
                    if im.abs() <= 1e-15 {
                        Value::Num(*r)
                    } else {
                        let mut ex_attrs = std::collections::HashMap::new();
                        ex_attrs.insert(
                            "message".to_string(),
                            Value::str_from(
                                "Cannot convert Complex to Real: imaginary part not zero",
                            ),
                        );
                        ex_attrs.insert("target".to_string(), Value::str_from("Real"));
                        ex_attrs.insert("source".to_string(), target.clone());
                        let ex = Value::make_instance(Symbol::intern("X::Numeric::Real"), ex_attrs);
                        let mut failure_attrs = std::collections::HashMap::new();
                        failure_attrs.insert("exception".to_string(), ex);
                        return Some(Some(Ok(Value::make_instance(
                            Symbol::intern("Failure"),
                            failure_attrs,
                        ))));
                    }
                }
                Value::Str(s) => {
                    if let Ok(i) = s.trim().parse::<i64>() {
                        Value::Int(i)
                    } else if let Ok(f) = s.trim().parse::<f64>() {
                        Value::Num(f)
                    } else {
                        return Some(Some(Err(RuntimeError::new(format!(
                            "X::Str::Numeric: Cannot convert string '{}' to a number",
                            s
                        )))));
                    }
                }
                _ => return Some(None),
            };
            Some(Some(Ok(result)))
        }
        "Numeric" => {
            let result = match target {
                Value::Int(i) => Value::Int(*i),
                Value::BigInt(_) => target.clone(),
                Value::Num(f) => Value::Num(*f),
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Instant" || class_name == "Duration" => {
                    let numeric = attributes
                        .get("value")
                        .and_then(|v| match v {
                            Value::Int(i) => Some(*i as f64),
                            Value::BigInt(n) => Some(n.to_f64().unwrap_or(f64::INFINITY)),
                            Value::Num(f) => Some(*f),
                            Value::Rat(n, d) if *d != 0 => Some(*n as f64 / *d as f64),
                            Value::FatRat(n, d) if *d != 0 => Some(*n as f64 / *d as f64),
                            Value::BigRat(n, d) if !d.is_zero() => {
                                Some(n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0))
                            }
                            _ => None,
                        })
                        .unwrap_or(0.0);
                    Value::Num(numeric)
                }
                Value::Rat(n, d) if *d != 0 => Value::Num(*n as f64 / *d as f64),
                Value::Str(s) => {
                    let trimmed = s.trim();
                    if let Ok(i) = trimmed.parse::<i64>() {
                        Value::Int(i)
                    } else if let Ok(f) = trimmed.parse::<f64>() {
                        Value::Num(f)
                    } else if let Some(normalized) = normalize_unicode_digits(trimmed) {
                        if let Ok(i) = normalized.parse::<i64>() {
                            Value::Int(i)
                        } else if let Ok(f) = normalized.parse::<f64>() {
                            Value::Num(f)
                        } else {
                            return Some(Some(Err(RuntimeError::new(format!(
                                "X::Str::Numeric: Cannot convert string '{}' to a number",
                                s
                            )))));
                        }
                    } else {
                        return Some(Some(Err(RuntimeError::new(format!(
                            "X::Str::Numeric: Cannot convert string '{}' to a number",
                            s
                        )))));
                    }
                }
                Value::Bool(b) => Value::Int(if *b { 1 } else { 0 }),
                Value::Complex(r, _) => Value::Num(*r),
                Value::Array(items, ..) => Value::Int(items.len() as i64),
                Value::Hash(h) => Value::Int(h.len() as i64),
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
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Stash" => {
                    let count = match attributes.get("symbols") {
                        Some(Value::Hash(map)) => map.len() as i64,
                        _ => 0,
                    };
                    Value::Int(count)
                }
                _ => return Some(None),
            };
            Some(Some(Ok(result)))
        }
        "Bridge" => {
            let result = match target {
                Value::Int(i) => Value::Num(*i as f64),
                Value::BigInt(n) => Value::Num(n.to_f64().unwrap_or(f64::INFINITY)),
                Value::Num(f) => Value::Num(*f),
                Value::Rat(n, d) if *d != 0 => Value::Num(*n as f64 / *d as f64),
                Value::FatRat(n, d) if *d != 0 => {
                    Value::Num(n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0))
                }
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Instant" || class_name == "Duration" => {
                    let bridged = attributes
                        .get("value")
                        .and_then(|v| match v {
                            Value::Int(i) => Some(*i as f64),
                            Value::BigInt(n) => Some(n.to_f64().unwrap_or(f64::INFINITY)),
                            Value::Num(f) => Some(*f),
                            Value::Rat(n, d) if *d != 0 => Some(*n as f64 / *d as f64),
                            Value::FatRat(n, d) if *d != 0 => {
                                Some(n.to_f64().unwrap_or(0.0) / d.to_f64().unwrap_or(1.0))
                            }
                            _ => None,
                        })
                        .unwrap_or(0.0);
                    Value::Num(bridged)
                }
                _ => return Some(None),
            };
            Some(Some(Ok(result)))
        }
        _ => None,
    }
}
