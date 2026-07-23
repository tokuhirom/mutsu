use super::{int_lsb_value, int_msb_value, is_infinite_range, range_elems_lazy_failure};
use crate::builtins::rng::builtin_rand;
/// Numeric and element methods: elems, default, abs, lsb, msb, rand,
/// uc, lc, fc, tc, sign
use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};
use num_traits::Signed;

/// Check if a Package type object is calling a :D-requiring numeric method.
/// Returns X::Parameter::InvalidConcreteness error if so.
fn check_numeric_type_object_method(
    target: &Value,
    method: &str,
) -> Option<Option<Result<Value, RuntimeError>>> {
    if let ValueView::Package(pkg_name) = target.view() {
        let name = pkg_name.resolve();
        // Numeric types whose instances have these methods
        let is_numeric_type = matches!(
            name.as_ref(),
            "Int" | "UInt" | "Num" | "Rat" | "FatRat" | "Complex" | "Cool" | "Numeric"
        );
        if !is_numeric_type {
            return None;
        }
        // Methods that require :D (a concrete instance)
        let is_d_method = matches!(
            method,
            "abs"
                | "sign"
                | "sqrt"
                | "exp"
                | "log"
                | "log2"
                | "log10"
                | "ceiling"
                | "floor"
                | "round"
                | "truncate"
                | "narrow"
                | "lsb"
                | "msb"
                | "base"
                | "polymod"
        );
        if !is_d_method {
            return None;
        }
        // Determine the parent numeric type for the error
        let expected_type = match name.as_ref() {
            "UInt" => "Int",
            _ => &name,
        };
        return Some(Some(Err(RuntimeError::parameter_invalid_concreteness(
            expected_type,
            &name,
            method,
            "self",
            true, // should_be_concrete
            true, // param_is_invocant
        ))));
    }
    None
}

pub(super) fn dispatch(
    target: &Value,
    method: &str,
) -> Option<Option<Result<Value, RuntimeError>>> {
    // Type objects calling :D-requiring numeric methods
    if let result @ Some(_) = check_numeric_type_object_method(target, method) {
        return result;
    }
    match method {
        "elems" => {
            if let ValueView::LazyList(list) = target.view() {
                let from_gather = matches!(
                    list.env
                        .get("__mutsu_lazylist_from_gather")
                        .map(Value::view),
                    Some(ValueView::Bool(true))
                );
                // A plain `gather {...}` Seq is not lazy (raku: `.is-lazy` is
                // False), so `.elems` reifies and counts. A `lazy gather` (or
                // `gather.lazy`) is `lazy`-forced -- it carries the preserve-lazy
                // marker -- and `.elems` must throw X::Cannot::Lazy like any
                // other lazy list, rather than eagerly reifying.
                let forced_lazy = list
                    .env
                    .get("__mutsu_preserve_lazy_on_array_assign")
                    .is_some();
                if from_gather && !forced_lazy {
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
                failure_attrs.insert("handled".to_string(), Value::FALSE);
                return Some(Some(Ok(Value::make_instance(
                    Symbol::intern("Failure"),
                    failure_attrs,
                ))));
            }
            // A lazy (infinite-backed) array cannot report its element count;
            // raku throws X::Cannot::Lazy. Check before `as_list_items`, which
            // would otherwise return the capped backing length.
            if let ValueView::Array(_, kind) = target.view()
                && kind.is_lazy()
            {
                return Some(range_elems_lazy_failure("elems"));
            }
            if let Some(items) = target.as_list_items() {
                return Some(Some(Ok(Value::int(items.len() as i64))));
            }
            let result = match target.view() {
                ValueView::Hash(items) => Value::int(items.len() as i64),
                ValueView::Set(items, _) => Value::int(items.len() as i64),
                ValueView::Bag(items, _) => Value::int(items.len() as i64),
                ValueView::Mix(items, _) => Value::int(items.len() as i64),
                ValueView::Junction { values, .. } => Value::int(values.len() as i64),
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) => {
                    if let Some(ValueView::Array(bytes, ..)) =
                        attributes.as_map().get("bytes").map(Value::view)
                    {
                        Value::int(bytes.len() as i64)
                    } else {
                        Value::int(0)
                    }
                }
                ValueView::Channel(_) => {
                    return Some(Some(Err(RuntimeError::new(
                        "Cannot call '.elems' on a Channel instance".to_string(),
                    ))));
                }
                ValueView::Range(start, end) if start == i64::MIN || end == i64::MAX => {
                    return Some(range_elems_lazy_failure("elems"));
                }
                ValueView::Range(start, end) => Value::int((end - start + 1).max(0)),
                ValueView::RangeExcl(start, end) if start == i64::MIN || end == i64::MAX => {
                    return Some(range_elems_lazy_failure("elems"));
                }
                ValueView::RangeExcl(start, end) => Value::int((end - start).max(0)),
                ValueView::RangeExclStart(start, end) if start == i64::MIN || end == i64::MAX => {
                    return Some(range_elems_lazy_failure("elems"));
                }
                ValueView::RangeExclStart(start, end) => Value::int((end - start).max(0)),
                ValueView::RangeExclBoth(start, end) if start == i64::MIN || end == i64::MAX => {
                    return Some(range_elems_lazy_failure("elems"));
                }
                ValueView::RangeExclBoth(start, end) => Value::int((end - start - 1).max(0)),
                ValueView::GenericRange { .. } if is_infinite_range(target) => {
                    return Some(range_elems_lazy_failure("elems"));
                }
                ValueView::GenericRange { .. } => {
                    let list = crate::runtime::utils::value_to_list(target);
                    Value::int(list.len() as i64)
                }
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Stash" => {
                    match attributes.as_map().get("symbols").map(Value::view) {
                        Some(ValueView::Hash(map)) => Value::int(map.len() as i64),
                        _ => Value::int(0),
                    }
                }
                _ => Value::int(1),
            };
            Some(Some(Ok(result)))
        }
        "default" => {
            let result = match target.view() {
                // A value-carried `is default(...)` (embedded in HashData/
                // ArrayData) takes priority over the type default, so it survives
                // raw-parameter binding and list construction.
                ValueView::Array(a, _) if a.default.is_some() => {
                    a.default.as_deref().cloned().unwrap()
                }
                ValueView::Hash(h) if h.default.is_some() => h.default.as_deref().cloned().unwrap(),
                ValueView::Array(..) | ValueView::Hash(..) => Value::package(Symbol::intern("Any")),
                ValueView::Set(..) => Value::FALSE,
                ValueView::Bag(..) | ValueView::Mix(..) => Value::int(0),
                _ => return Some(None),
            };
            Some(Some(Ok(result)))
        }
        "abs" => {
            let result = match target.view() {
                ValueView::Int(i) => Value::int(i.abs()),
                ValueView::BigInt(n) => Value::bigint(n.as_ref().abs()),
                ValueView::Num(f) => Value::num(f.abs()),
                ValueView::Rat(n, d) => Value::rat_raw(n.abs(), d),
                ValueView::FatRat(n, d) => Value::fat_rat_raw(n.abs(), d),
                ValueView::Complex(r, i) => Value::num((r * r + i * i).sqrt()),
                ValueView::Bool(b) => Value::int(if b { 1 } else { 0 }),
                _ => return Some(None),
            };
            Some(Some(Ok(result)))
        }
        "lsb" => Some(int_lsb_value(target).map(Ok)),
        "msb" => Some(int_msb_value(target).map(Ok)),
        "rand" => {
            let max = match target.view() {
                ValueView::Int(n) => n as f64,
                ValueView::Num(n) => n,
                ValueView::Rat(n, d) => n as f64 / d as f64,
                ValueView::Range(start, end) => {
                    let from = start as f64;
                    let to = end as f64;
                    let v = from + builtin_rand() * (to - from);
                    return Some(Some(Ok(Value::num(v))));
                }
                ValueView::RangeExcl(start, end) => {
                    let from = start as f64;
                    let to = end as f64;
                    if from >= to {
                        return Some(Some(Ok(Value::NIL)));
                    }
                    let v = from + builtin_rand() * (to - from);
                    // Ensure we don't generate the excluded endpoint
                    let v = if v >= to {
                        f64::from_bits(to.to_bits().saturating_sub(1))
                    } else {
                        v
                    };
                    return Some(Some(Ok(Value::num(v))));
                }
                ValueView::RangeExclStart(start, end) => {
                    let from = start as f64;
                    let to = end as f64;
                    if from >= to {
                        return Some(Some(Ok(Value::NIL)));
                    }
                    let v = from + builtin_rand() * (to - from);
                    // Ensure we don't generate the excluded endpoint
                    let v = if v <= from {
                        f64::from_bits(from.to_bits().saturating_add(1))
                    } else {
                        v
                    };
                    return Some(Some(Ok(Value::num(v))));
                }
                ValueView::RangeExclBoth(start, end) => {
                    let from = start as f64;
                    let to = end as f64;
                    if from >= to {
                        return Some(Some(Ok(Value::NIL)));
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
                    return Some(Some(Ok(Value::num(v))));
                }
                ValueView::GenericRange {
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
                    if excl_start {
                        from = f64::from_bits(from.to_bits().saturating_add(1));
                    }
                    if excl_end {
                        to = f64::from_bits(to.to_bits().saturating_sub(1));
                    }
                    if !from.is_finite() || !to.is_finite() || from > to {
                        return Some(Some(Ok(Value::NIL)));
                    }
                    let v = from + builtin_rand() * (to - from);
                    return Some(Some(Ok(Value::num(v))));
                }
                // Cool types: numify first (e.g., List.rand returns rand in 0..^elems)
                ValueView::Array(items, ..) => items.len() as f64,
                ValueView::Seq(items) => items.len() as f64,
                ValueView::Str(s) => s.parse::<f64>().unwrap_or(0.0),
                ValueView::Bool(b) => {
                    if b {
                        1.0
                    } else {
                        0.0
                    }
                }
                _ => return Some(None),
            };
            Some(Some(Ok(Value::num(builtin_rand() * max))))
        }
        "uc" => Some(Some(Ok(Value::str(
            crate::builtins::unicode::grapheme_uppercase(&target.to_string_value()),
        )))),
        "lc" => Some(Some(Ok(Value::str(
            crate::builtins::unicode::grapheme_lowercase(&target.to_string_value()),
        )))),
        "fc" => Some(Some(Ok(Value::str(
            crate::builtins::unicode::grapheme_foldcase(&target.to_string_value()),
        )))),
        "tc" => Some(Some(Ok(Value::str(
            crate::builtins::unicode::titlecase_string(&target.to_string_value()),
        )))),
        "sign" => {
            let result = match target.view() {
                ValueView::Int(i) => Value::int(i.signum()),
                ValueView::Num(f) => {
                    if f.is_nan() {
                        Value::num(f64::NAN)
                    } else {
                        Value::int(if f > 0.0 {
                            1
                        } else if f < 0.0 {
                            -1
                        } else {
                            0
                        })
                    }
                }
                ValueView::Rat(n, d) => {
                    if d == 0 {
                        if n > 0 {
                            Value::int(1)
                        } else if n < 0 {
                            Value::int(-1)
                        } else {
                            Value::num(f64::NAN)
                        }
                    } else {
                        // sign is determined by n/d
                        let sign = n.signum() * d.signum();
                        Value::int(sign)
                    }
                }
                ValueView::FatRat(n, d) => {
                    if d == 0 {
                        if n > 0 {
                            Value::int(1)
                        } else if n < 0 {
                            Value::int(-1)
                        } else {
                            Value::num(f64::NAN)
                        }
                    } else {
                        let sign = n.signum() * d.signum();
                        Value::int(sign)
                    }
                }
                ValueView::BigInt(n) => {
                    use num_bigint::Sign;
                    Value::int(match n.sign() {
                        Sign::Plus => 1,
                        Sign::Minus => -1,
                        Sign::NoSign => 0,
                    })
                }
                ValueView::Complex(re, im) => {
                    // `sign` requires a Real. A Complex with a non-zero imaginary
                    // part cannot be coerced to Real, so it throws X::Numeric::Real
                    // (matching `.Int`/`.Real` coercion). A purely real Complex
                    // yields the Int sign of its real part.
                    if im != 0.0 {
                        let mut attrs = std::collections::HashMap::new();
                        let rendered = if im >= 0.0 {
                            format!("{re}+{im}i")
                        } else {
                            format!("{re}{im}i")
                        };
                        attrs.insert(
                            "message".to_string(),
                            Value::str(format!(
                                "Cannot convert {rendered} to Real: imaginary part not zero"
                            )),
                        );
                        attrs.insert("target".to_string(), Value::package(Symbol::intern("Real")));
                        attrs.insert("source".to_string(), target.clone());
                        let ex = Value::make_instance(Symbol::intern("X::Numeric::Real"), attrs);
                        let mut err = RuntimeError::new(
                            "Cannot convert Complex to Real: imaginary part not zero",
                        );
                        err.exception = Some(Box::new(ex));
                        return Some(Some(Err(err)));
                    }
                    Value::int(if re > 0.0 {
                        1
                    } else if re < 0.0 {
                        -1
                    } else {
                        0
                    })
                }
                ValueView::Enum { value, .. } => Value::int(value.as_i64().signum()),
                _ => return Some(None),
            };
            Some(Some(Ok(result)))
        }
        _ => None,
    }
}
