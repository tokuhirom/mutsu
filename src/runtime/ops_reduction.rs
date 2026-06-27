use super::*;
use crate::symbol::Symbol;
use num_traits::{Signed, ToPrimitive, Zero};

impl Interpreter {
    fn reduction_repeat_error(class_name: &str, message: &str) -> RuntimeError {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(message.to_string()));
        let ex = Value::make_instance(Symbol::intern(class_name), attrs);
        let mut err = RuntimeError::new(message.to_string());
        err.exception = Some(Box::new(ex));
        err
    }

    fn reduction_parse_repeat_count(value: &Value) -> Result<Option<i64>, RuntimeError> {
        let mut current = value;
        while let Value::Mixin(inner, _) = current {
            current = inner;
        }
        match current {
            Value::Whatever => Ok(None),
            Value::Int(i) => Ok(Some(*i)),
            Value::BigInt(n) => Ok(Some(n.to_i64().unwrap_or(i64::MAX))),
            Value::Num(f) => {
                if f.is_nan() {
                    return Err(Self::reduction_repeat_error(
                        "X::Numeric::CannotConvert",
                        "Cannot convert NaN to Int",
                    ));
                }
                if f.is_infinite() {
                    if f.is_sign_positive() {
                        return Ok(None);
                    }
                    return Err(Self::reduction_repeat_error(
                        "X::Numeric::CannotConvert",
                        "Cannot convert -Inf to Int",
                    ));
                }
                Ok(Some(f.trunc() as i64))
            }
            Value::Rat(n, d) => {
                if *d == 0 {
                    if *n > 0 {
                        return Ok(None);
                    }
                    return Err(Self::reduction_repeat_error(
                        "X::Numeric::CannotConvert",
                        if *n < 0 {
                            "Cannot convert -Inf to Int"
                        } else {
                            "Cannot convert NaN to Int"
                        },
                    ));
                }
                Ok(Some(n / d))
            }
            Value::FatRat(n, d) => {
                if d.is_zero() {
                    if n.is_positive() {
                        return Ok(None);
                    }
                    return Err(Self::reduction_repeat_error(
                        "X::Numeric::CannotConvert",
                        if n.is_negative() {
                            "Cannot convert -Inf to Int"
                        } else {
                            "Cannot convert NaN to Int"
                        },
                    ));
                }
                Ok(Some((n / d).to_i64().unwrap_or(i64::MAX)))
            }
            Value::BigRat(n, d) => {
                if d.is_zero() {
                    if n.is_positive() {
                        return Ok(None);
                    }
                    return Err(Self::reduction_repeat_error(
                        "X::Numeric::CannotConvert",
                        if n.is_negative() {
                            "Cannot convert -Inf to Int"
                        } else {
                            "Cannot convert NaN to Int"
                        },
                    ));
                }
                Ok(Some((n.as_ref() / d.as_ref()).to_i64().unwrap_or(i64::MAX)))
            }
            Value::Str(s) => {
                let parsed = s.trim().parse::<f64>().map_err(|_| {
                    Self::reduction_repeat_error(
                        "X::Str::Numeric",
                        &format!("Cannot convert string '{}' to a number", s),
                    )
                })?;
                Self::reduction_parse_repeat_count(&Value::Num(parsed))
            }
            Value::Array(items, ..) => Ok(Some(items.len() as i64)),
            Value::Seq(items) => Ok(Some(items.len() as i64)),
            Value::LazyList(ll) => Ok(Some(
                ll.cache
                    .lock()
                    .unwrap_or_else(|e| e.into_inner())
                    .as_ref()
                    .map_or(0usize, |v| v.len()) as i64,
            )),
            Value::Package(_) => Ok(Some(0)),
            _ => Ok(Some(0)),
        }
    }

    pub(crate) fn apply_reduction_op(
        op: &str,
        left: &Value,
        right: &Value,
    ) -> Result<Value, RuntimeError> {
        let to_bag_counts = |value: &Value| -> Option<std::collections::HashMap<String, i64>> {
            match value {
                Value::Bag(items, _) => {
                    Some(crate::runtime::utils::bag_counts_as_i64(&items.counts))
                }
                Value::Set(items, _) => Some(items.iter().map(|k| (k.clone(), 1)).collect()),
                Value::Hash(items) => Some({
                    let mut counts = std::collections::HashMap::new();
                    for (k, v) in items.iter() {
                        let count = match v {
                            Value::Int(i) => *i,
                            Value::Num(n) => *n as i64,
                            Value::Rat(n, d) if *d != 0 => n / d,
                            Value::FatRat(n, d) if *d != 0 => n / d,
                            Value::Bool(b) => i64::from(*b),
                            _ => return None,
                        };
                        counts.insert(k.clone(), count);
                    }
                    counts
                }),
                _ => None,
            }
        };
        let to_complex = |v: &Value| -> Option<(f64, f64)> {
            let mut cur = v;
            while let Value::Mixin(inner, _) = cur {
                cur = inner;
            }
            match cur {
                Value::Complex(r, i) => Some((*r, *i)),
                _ => None,
            }
        };
        let to_num = |v: &Value| -> f64 {
            let mut cur = v;
            while let Value::Mixin(inner, _) = cur {
                cur = inner;
            }
            match cur {
                Value::Int(i) => *i as f64,
                Value::Num(f) => *f,
                Value::Rat(n, d) => {
                    if *d == 0 {
                        f64::NAN
                    } else {
                        *n as f64 / *d as f64
                    }
                }
                Value::FatRat(n, d) => {
                    if *d == 0 {
                        f64::NAN
                    } else {
                        *n as f64 / *d as f64
                    }
                }
                Value::Str(s) => s.parse::<f64>().unwrap_or(0.0),
                Value::Bool(b) => {
                    if *b {
                        1.0
                    } else {
                        0.0
                    }
                }
                Value::Enum { value, .. } => value.as_i64() as f64,
                Value::Array(items, kind) => {
                    if kind.is_itemized() {
                        0.0
                    } else {
                        items.len() as f64
                    }
                }
                _ => 0.0,
            }
        };
        let to_int = |v: &Value| -> i64 {
            let mut cur = v;
            while let Value::Mixin(inner, _) = cur {
                cur = inner;
            }
            match cur {
                Value::Int(i) => *i,
                Value::BigInt(n) => n
                    .to_i64()
                    .unwrap_or_else(|| if n.is_negative() { i64::MIN } else { i64::MAX }),
                Value::Num(f) => *f as i64,
                Value::Rat(n, d) => {
                    if *d == 0 {
                        0
                    } else {
                        n / d
                    }
                }
                Value::FatRat(n, d) => {
                    if *d == 0 {
                        0
                    } else {
                        n / d
                    }
                }
                Value::Str(s) => s.parse::<i64>().unwrap_or(0),
                Value::Bool(b) => {
                    if *b {
                        1
                    } else {
                        0
                    }
                }
                Value::Array(items, kind) => {
                    if kind.is_itemized() {
                        0
                    } else {
                        items.len() as i64
                    }
                }
                _ => 0,
            }
        };
        // Handle R (reverse) meta-prefix: swap operands and recurse with inner op
        if let Some(inner_op) = op.strip_prefix('R')
            && !inner_op.is_empty()
        {
            return Self::apply_reduction_op(inner_op, right, left);
        }
        if let Some(inner_op) = op.strip_prefix('X')
            && !inner_op.is_empty()
        {
            let left_list = Self::value_to_list(left);
            let right_list = Self::value_to_list(right);
            let mut out = Vec::new();
            for l in &left_list {
                for r in &right_list {
                    out.push(Self::apply_reduction_op(inner_op, l, r)?);
                }
            }
            return Ok(Value::Seq(std::sync::Arc::new(out)));
        }
        if let Some(inner_op) = op.strip_prefix('Z')
            && !inner_op.is_empty()
        {
            let left_list = Self::value_to_list(left);
            let right_list = Self::value_to_list(right);
            let len = left_list.len().min(right_list.len());
            let mut out = Vec::with_capacity(len);
            for i in 0..len {
                out.push(Self::apply_reduction_op(
                    inner_op,
                    &left_list[i],
                    &right_list[i],
                )?);
            }
            return Ok(Value::Seq(std::sync::Arc::new(out)));
        }
        match op {
            "+" => {
                if let (Some(mut left_counts), Some(right_counts)) =
                    (to_bag_counts(left), to_bag_counts(right))
                {
                    for (key, count) in right_counts {
                        *left_counts.entry(key).or_insert(0) += count;
                    }
                    return Ok(Value::bag(left_counts));
                }
                crate::builtins::arith_add(left.clone(), right.clone())
            }
            "-" => Ok(crate::builtins::arith_sub(left.clone(), right.clone())),
            "*" => Ok(crate::builtins::arith_mul(left.clone(), right.clone())),
            "/" => crate::builtins::arith_div(left.clone(), right.clone()),
            "div" => {
                let divisor = to_int(right);
                if divisor == 0 {
                    return Ok(RuntimeError::divide_by_zero_failure(
                        Some(Value::Int(to_int(left))),
                        Some("div"),
                    ));
                }
                Ok(Value::Int(to_int(left).div_euclid(divisor)))
            }
            "%" | "mod" => crate::builtins::arith_mod(left.clone(), right.clone()),
            "**" => Ok(crate::builtins::arith_pow(left.clone(), right.clone())),
            "~" => Ok(crate::runtime::Interpreter::concat_values(
                left.clone(),
                right.clone(),
            )),
            "&&" | "and" => {
                if !left.truthy() {
                    Ok(left.clone())
                } else {
                    Ok(right.clone())
                }
            }
            "||" | "or" => {
                if left.truthy() {
                    Ok(left.clone())
                } else {
                    Ok(right.clone())
                }
            }
            "//" => {
                if crate::runtime::types::value_is_defined(left) {
                    Ok(left.clone())
                } else {
                    Ok(right.clone())
                }
            }
            "orelse" => {
                if crate::runtime::types::value_is_defined(left) {
                    Ok(left.clone())
                } else {
                    Ok(right.clone())
                }
            }
            "andthen" => {
                if crate::runtime::types::value_is_defined(left) {
                    Ok(right.clone())
                } else {
                    // Return Empty (empty Slip) when left is undefined
                    Ok(Value::Slip(std::sync::Arc::new(vec![])))
                }
            }
            "xor" => {
                let lt = left.truthy();
                let rt = right.truthy();
                if lt && !rt {
                    Ok(left.clone())
                } else if !lt && rt {
                    Ok(right.clone())
                } else {
                    Ok(Value::Nil)
                }
            }
            "minmax" => {
                // Extract (lo, hi) bounds from each operand via the single shared
                // helper (handles Range / Array-Seq / scalar, recursing into
                // elements) so this logic is not duplicated with the VM.
                let (left_lo, left_hi) = crate::vm::vm_misc_ops::minmax_bounds_of_value(left);
                let (right_lo, right_hi) = crate::vm::vm_misc_ops::minmax_bounds_of_value(right);
                let lo = if crate::runtime::compare_values(&left_lo, &right_lo) <= 0 {
                    left_lo
                } else {
                    right_lo
                };
                let hi = if crate::runtime::compare_values(&left_hi, &right_hi) >= 0 {
                    left_hi
                } else {
                    right_hi
                };
                // Return Range(lo,hi) when both bounds are integers (matches
                // Raku's behavior where `1..3 eqv 1..3` uses the integer Range
                // type), otherwise use GenericRange for non-integer bounds.
                Ok(match (&lo, &hi) {
                    (Value::Int(l), Value::Int(h)) => Value::Range(*l, *h),
                    _ => Value::GenericRange {
                        start: std::sync::Arc::new(lo),
                        end: std::sync::Arc::new(hi),
                        excl_start: false,
                        excl_end: false,
                    },
                })
            }
            "min" => {
                // Undefined (Any/Nil/type-object) acts as Inf for min
                let left_undef = !crate::runtime::types::value_is_defined(left);
                let right_undef = !crate::runtime::types::value_is_defined(right);
                if left_undef && right_undef {
                    Ok(Value::Num(f64::INFINITY))
                } else if left_undef {
                    Ok(right.clone())
                } else if right_undef || crate::runtime::compare_values(left, right) <= 0 {
                    Ok(left.clone())
                } else {
                    Ok(right.clone())
                }
            }
            "max" => {
                // Undefined (Any/Nil/type-object) acts as -Inf for max
                let left_undef = !crate::runtime::types::value_is_defined(left);
                let right_undef = !crate::runtime::types::value_is_defined(right);
                if left_undef && right_undef {
                    Ok(Value::Num(f64::NEG_INFINITY))
                } else if left_undef {
                    Ok(right.clone())
                } else if right_undef || crate::runtime::compare_values(left, right) >= 0 {
                    Ok(left.clone())
                } else {
                    Ok(right.clone())
                }
            }
            "+&" => {
                let a = left.to_bigint();
                let b = right.to_bigint();
                Ok(Value::from_bigint(a & b))
            }
            "+|" => {
                let a = left.to_bigint();
                let b = right.to_bigint();
                Ok(Value::from_bigint(a | b))
            }
            "+^" => {
                let a = left.to_bigint();
                let b = right.to_bigint();
                Ok(Value::from_bigint(a ^ b))
            }
            "==" => {
                if let (Some(a), Some(b)) = (
                    super::to_big_rat_parts(left),
                    super::to_big_rat_parts(right),
                ) {
                    Ok(Value::Bool(super::big_rat_parts_equal(a, b)))
                } else if let (Some(l), Some(r)) = (to_complex(left), to_complex(right)) {
                    Ok(Value::Bool(l.0 == r.0 && l.1 == r.1))
                } else {
                    Ok(Value::Bool(to_num(left) == to_num(right)))
                }
            }
            "=" => Ok(right.clone()),
            "!=" | "!==" => {
                if let (Some(a), Some(b)) = (
                    super::to_big_rat_parts(left),
                    super::to_big_rat_parts(right),
                ) {
                    Ok(Value::Bool(!super::big_rat_parts_equal(a, b)))
                } else if let (Some(l), Some(r)) = (to_complex(left), to_complex(right)) {
                    Ok(Value::Bool(l.0 != r.0 || l.1 != r.1))
                } else {
                    Ok(Value::Bool(to_num(left) != to_num(right)))
                }
            }
            "<" => Ok(Value::Bool(to_num(left) < to_num(right))),
            ">" => Ok(Value::Bool(to_num(left) > to_num(right))),
            "<=" => Ok(Value::Bool(to_num(left) <= to_num(right))),
            ">=" => Ok(Value::Bool(to_num(left) >= to_num(right))),
            "eq" => Ok(Value::Bool(
                left.to_string_value() == right.to_string_value(),
            )),
            "ne" => Ok(Value::Bool(
                left.to_string_value() != right.to_string_value(),
            )),
            "lt" => Ok(Value::Bool(
                left.to_string_value() < right.to_string_value(),
            )),
            "gt" => Ok(Value::Bool(
                left.to_string_value() > right.to_string_value(),
            )),
            "le" => Ok(Value::Bool(
                left.to_string_value() <= right.to_string_value(),
            )),
            "ge" => Ok(Value::Bool(
                left.to_string_value() >= right.to_string_value(),
            )),
            "after" => Ok(Value::Bool(
                left.to_string_value() > right.to_string_value(),
            )),
            "before" => Ok(Value::Bool(
                left.to_string_value() < right.to_string_value(),
            )),
            "leg" => {
                let ord = left.to_string_value().cmp(&right.to_string_value());
                Ok(super::make_order(ord))
            }
            "cmp" => {
                let ord = match (left, right) {
                    (Value::Int(a), Value::Int(b)) => a.cmp(b),
                    (
                        Value::Rat(_, _) | Value::FatRat(_, _) | Value::BigRat(_, _),
                        Value::Rat(_, _) | Value::FatRat(_, _) | Value::BigRat(_, _),
                    ) => super::to_big_rat_parts(left)
                        .zip(super::to_big_rat_parts(right))
                        .and_then(|(a, b)| super::compare_big_rat_parts(a, b))
                        .unwrap_or(std::cmp::Ordering::Equal),
                    (Value::Num(a), Value::Num(b)) => {
                        a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal)
                    }
                    (Value::Int(a), Value::Num(b)) => (*a as f64)
                        .partial_cmp(b)
                        .unwrap_or(std::cmp::Ordering::Equal),
                    (Value::Num(a), Value::Int(b)) => a
                        .partial_cmp(&(*b as f64))
                        .unwrap_or(std::cmp::Ordering::Equal),
                    (
                        Value::Instance {
                            attributes: left_attrs,
                            ..
                        },
                        Value::Instance {
                            attributes: right_attrs,
                            ..
                        },
                    ) if left_attrs.contains_key("year")
                        && left_attrs.contains_key("month")
                        && left_attrs.contains_key("day")
                        && left_attrs.contains_key("hour")
                        && left_attrs.contains_key("minute")
                        && left_attrs.contains_key("second")
                        && left_attrs.contains_key("timezone")
                        && right_attrs.contains_key("year")
                        && right_attrs.contains_key("month")
                        && right_attrs.contains_key("day")
                        && right_attrs.contains_key("hour")
                        && right_attrs.contains_key("minute")
                        && right_attrs.contains_key("second")
                        && right_attrs.contains_key("timezone") =>
                    {
                        let (ly, lm, ld, lh, lmin, ls, ltz) =
                            crate::builtins::methods_0arg::temporal::datetime_attrs(
                                &(left_attrs).as_map(),
                            );
                        let (ry, rm, rd, rh, rmin, rs, rtz) =
                            crate::builtins::methods_0arg::temporal::datetime_attrs(
                                &(right_attrs).as_map(),
                            );
                        let left_instant =
                            crate::builtins::methods_0arg::temporal::datetime_to_instant_leap_aware(
                                ly, lm, ld, lh, lmin, ls, ltz,
                            );
                        let right_instant =
                            crate::builtins::methods_0arg::temporal::datetime_to_instant_leap_aware(
                                ry, rm, rd, rh, rmin, rs, rtz,
                            );
                        left_instant
                            .partial_cmp(&right_instant)
                            .unwrap_or(std::cmp::Ordering::Equal)
                    }
                    (l, r)
                        if matches!(
                            l,
                            Value::Int(_)
                                | Value::BigInt(_)
                                | Value::Num(_)
                                | Value::Rat(_, _)
                                | Value::FatRat(_, _)
                                | Value::BigRat(_, _)
                        ) && matches!(
                            r,
                            Value::Int(_)
                                | Value::BigInt(_)
                                | Value::Num(_)
                                | Value::Rat(_, _)
                                | Value::FatRat(_, _)
                                | Value::BigRat(_, _)
                        ) =>
                    {
                        let lf = super::to_float_value(l).unwrap_or(0.0);
                        let rf = super::to_float_value(r).unwrap_or(0.0);
                        lf.partial_cmp(&rf).unwrap_or(std::cmp::Ordering::Equal)
                    }
                    (Value::Version { parts: ap, .. }, Value::Version { parts: bp, .. }) => {
                        super::version_cmp_parts(ap, bp)
                    }
                    _ => left.to_string_value().cmp(&right.to_string_value()),
                };
                Ok(super::make_order(ord))
            }
            "gcd" => {
                use num_bigint::BigInt;
                use num_traits::Zero;
                let mut a: BigInt = left.to_bigint().abs();
                let mut b: BigInt = right.to_bigint().abs();
                while !b.is_zero() {
                    let t = b.clone();
                    b = &a % &b;
                    a = t;
                }
                Ok(Value::from_bigint(a))
            }
            "lcm" => {
                use num_bigint::BigInt;
                use num_traits::Zero;
                let a: BigInt = left.to_bigint().abs();
                let b: BigInt = right.to_bigint().abs();
                if a.is_zero() && b.is_zero() {
                    Ok(Value::Int(0))
                } else {
                    let mut ga = a.clone();
                    let mut gb = b.clone();
                    while !gb.is_zero() {
                        let t = gb.clone();
                        gb = &ga % &gb;
                        ga = t;
                    }
                    Ok(Value::from_bigint((&a / &ga) * &b))
                }
            }
            "^^" => {
                let lt = left.truthy();
                let rt = right.truthy();
                if lt && !rt {
                    Ok(left.clone())
                } else if !lt && rt {
                    Ok(right.clone())
                } else if lt && rt {
                    Ok(Value::Nil)
                } else {
                    // both falsy: return the last falsy value
                    Ok(right.clone())
                }
            }
            "~~" => {
                if let (
                    Value::Instance {
                        class_name: dt_class,
                        attributes: dt_attrs,
                        ..
                    },
                    Value::Instance {
                        class_name: d_class,
                        attributes: d_attrs,
                        ..
                    },
                ) = (left, right)
                    && dt_class == "DateTime"
                    && d_class == "Date"
                {
                    let (y, m, d, _, _, _, _) =
                        crate::builtins::methods_0arg::temporal::datetime_attrs(
                            &(dt_attrs).as_map(),
                        );
                    let (dy, dm, dd) =
                        crate::builtins::methods_0arg::temporal::date_attrs(&(d_attrs).as_map());
                    return Ok(Value::Bool(y == dy && m == dm && d == dd));
                }
                // Basic smartmatch fallback: value equality
                Ok(Value::Bool(left == right))
            }
            "eqv" => Ok(Value::Bool(left.eqv(right))),
            "=:=" => Ok(Value::Bool(super::values_identical(left, right))),
            "!=:=" => Ok(Value::Bool(!super::values_identical(left, right))),
            "===" => Ok(Value::Bool(super::values_identical(left, right))),
            "!===" => Ok(Value::Bool(!super::values_identical(left, right))),
            "=>" => match left {
                Value::Str(_) => Ok(Value::Pair(left.to_string_value(), Box::new(right.clone()))),
                _ => Ok(Value::ValuePair(
                    Box::new(left.clone()),
                    Box::new(right.clone()),
                )),
            },
            "&" => {
                let mut vals = match left {
                    Value::Junction {
                        kind: crate::value::JunctionKind::All,
                        values,
                    } => values.as_ref().clone(),
                    _ => vec![left.clone()],
                };
                vals.push(right.clone());
                Ok(Value::Junction {
                    kind: crate::value::JunctionKind::All,
                    values: std::sync::Arc::new(vals),
                })
            }
            "|" => {
                let mut vals = match left {
                    Value::Junction {
                        kind: crate::value::JunctionKind::Any,
                        values,
                    } => values.as_ref().clone(),
                    _ => vec![left.clone()],
                };
                vals.push(right.clone());
                Ok(Value::Junction {
                    kind: crate::value::JunctionKind::Any,
                    values: std::sync::Arc::new(vals),
                })
            }
            "^" => {
                let mut vals = match left {
                    Value::Junction {
                        kind: crate::value::JunctionKind::One,
                        values,
                    } => values.as_ref().clone(),
                    _ => vec![left.clone()],
                };
                vals.push(right.clone());
                Ok(Value::Junction {
                    kind: crate::value::JunctionKind::One,
                    values: std::sync::Arc::new(vals),
                })
            }
            "~|" => Self::str_bitwise_op(left, right, |a, b| a | b, true),
            "~^" => Self::str_bitwise_op(left, right, |a, b| a ^ b, true),
            "~&" => Self::str_bitwise_op(left, right, |a, b| a & b, false),
            "+<" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Self::shift_left_i64(*a, *b)),
                _ => Ok(Self::shift_left_bigint(&left.to_bigint(), to_int(right))),
            },
            "+>" => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Self::shift_right_i64(*a, *b)),
                _ => Ok(Self::shift_right_bigint(&left.to_bigint(), to_int(right))),
            },
            "x" => {
                if matches!(right, Value::Whatever) {
                    let mut env = crate::env::Env::new();
                    env.insert(
                        "__mutsu_callable_type".to_string(),
                        Value::str_from("WhateverCode"),
                    );
                    let param = "__wc_0".to_string();
                    let body = vec![Stmt::Expr(Expr::Binary {
                        left: Box::new(Expr::Literal(left.clone())),
                        op: crate::token_kind::TokenKind::Ident("x".to_string()),
                        right: Box::new(Expr::Var(param.clone())),
                    })];
                    return Ok(Value::make_sub(
                        Symbol::intern("GLOBAL"),
                        Symbol::intern("<whatevercode-x>"),
                        vec![param],
                        Vec::new(),
                        body,
                        false,
                        env,
                    ));
                }
                let Some(n_raw) = Self::reduction_parse_repeat_count(right)? else {
                    return Err(Self::reduction_repeat_error(
                        "X::Numeric::CannotConvert",
                        "Cannot convert Inf to Int",
                    ));
                };
                let n = n_raw.max(0) as usize;
                {
                    let repeated = crate::runtime::utils::coerce_to_str(left).repeat(n);
                    use unicode_normalization::UnicodeNormalization;
                    Ok(Value::str(repeated.nfc().collect::<String>()))
                }
            }
            "X" => {
                let left_list = Self::value_to_list(left);
                let right_list = Self::value_to_list(right);
                let mut results = Vec::new();
                for l in &left_list {
                    for r in &right_list {
                        let mut tuple = match l {
                            Value::Array(items, ..) => items.to_vec(),
                            _ => vec![l.clone()],
                        };
                        tuple.push(r.clone());
                        results.push(Value::array(tuple));
                    }
                }
                Ok(Value::array(results))
            }
            "xx" => {
                const EAGER_LIMIT: usize = 10_000;
                const LAZY_CACHE: usize = 4_096;
                let (repeat, lazy) = match Self::reduction_parse_repeat_count(right)? {
                    Some(n) if n <= 0 => (0usize, false),
                    Some(n) if (n as usize) <= EAGER_LIMIT => (n as usize, false),
                    Some(n) => ((n as usize).min(LAZY_CACHE), true),
                    None => (LAZY_CACHE, true),
                };
                let items: Vec<Value> = std::iter::repeat_n(left.clone(), repeat).collect();
                if lazy {
                    Ok(Value::LazyList(std::sync::Arc::new(
                        crate::value::LazyList::new_cached(items),
                    )))
                } else {
                    Ok(Value::Seq(std::sync::Arc::new(items)))
                }
            }
            "," => {
                let mut items = match left {
                    Value::Array(values, kind) if !kind.is_itemized() => values.to_vec(),
                    Value::Seq(values) | Value::Slip(values) => values.to_vec(),
                    other => vec![other.clone()],
                };
                match right {
                    Value::Array(values, kind) if !kind.is_itemized() => {
                        items.extend(values.iter().cloned());
                    }
                    Value::Seq(values) | Value::Slip(values) => {
                        items.extend(values.iter().cloned());
                    }
                    other => items.push(other.clone()),
                }
                Ok(Value::array(items))
            }
            // Set operators take their result mutability from the first operand.
            "(|)" | "∪" => Self::apply_set_union(left, right)
                .map(|r| with_set_mutability(r, set_result_mutability(left))),
            "(+)" | "⊎" => Self::apply_set_addition(left, right)
                .map(|r| with_set_mutability(r, set_result_mutability(left))),
            "(.)" | "⊍" => Self::apply_set_multiply(left, right)
                .map(|r| with_set_mutability(r, set_result_mutability(left))),
            "(-)" | "∖" => Ok(with_set_mutability(
                set_diff_values(left, right),
                set_result_mutability(left),
            )),
            "(&)" | "∩" => Ok(with_set_mutability(
                set_intersect_values(left, right),
                set_result_mutability(left),
            )),
            // Symmetric difference demotes to an immutable Set when the right
            // operand is not a QuantHash, but only at the Set level.
            "(^)" | "⊖" => Ok(with_set_mutability(
                set_sym_diff_values(left, right),
                set_sym_diff_mutability(left, right),
            )),
            "(==)" | "≡" => Ok(Value::Bool(Self::apply_set_equality(left, right)?)),
            "≢" => Ok(Value::Bool(!Self::apply_set_equality(left, right)?)),
            _ if op.ends_with('=') && op.len() > 1 => {
                // Compound assignment operator (e.g., "~=", "+=", "-=", "*=")
                // Apply the base operator and return the result.
                let base_op = &op[..op.len() - 1];
                Self::apply_reduction_op(base_op, left, right)
            }
            _ => Err(RuntimeError::new(format!(
                "Unsupported reduction operator: {}",
                op
            ))),
        }
    }
}
