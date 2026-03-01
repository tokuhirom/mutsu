use super::*;
use num_traits::{Signed, ToPrimitive, Zero};

impl Interpreter {
    fn union_is_infinite_bound(value: &Value) -> bool {
        match value {
            Value::Num(n) => n.is_infinite(),
            Value::Rat(_, d) | Value::FatRat(_, d) => *d == 0,
            Value::Mixin(inner, _) => Self::union_is_infinite_bound(inner),
            _ => false,
        }
    }

    fn union_is_lazy_input(value: &Value) -> bool {
        match value {
            Value::LazyList(_) => true,
            Value::GenericRange { start, end, .. } => {
                Self::union_is_infinite_bound(start) || Self::union_is_infinite_bound(end)
            }
            _ => false,
        }
    }

    fn union_insert_set_elem(elems: &mut std::collections::HashSet<String>, value: &Value) {
        let pair_selected = |weight: &Value| weight.truthy() || matches!(weight, Value::Nil);
        match value {
            Value::Set(items) => {
                elems.extend(items.iter().cloned());
            }
            Value::Bag(items) => {
                for (k, v) in items.iter() {
                    if *v > 0 {
                        elems.insert(k.clone());
                    }
                }
            }
            Value::Mix(items) => {
                for (k, v) in items.iter() {
                    if *v != 0.0 {
                        elems.insert(k.clone());
                    }
                }
            }
            Value::Hash(items) => {
                for (k, v) in items.iter() {
                    if v.truthy() || matches!(v, Value::Nil) {
                        elems.insert(k.clone());
                    }
                }
            }
            Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items) => {
                for item in items.iter() {
                    Self::union_insert_set_elem(elems, item);
                }
            }
            range if range.is_range() => {
                for item in Self::value_to_list(range) {
                    Self::union_insert_set_elem(elems, &item);
                }
            }
            Value::Pair(key, weight) => {
                if pair_selected(weight) {
                    elems.insert(key.clone());
                }
            }
            Value::ValuePair(key, weight) => {
                if pair_selected(weight) {
                    elems.insert(key.to_string_value());
                }
            }
            other => {
                let sv = other.to_string_value();
                if !sv.is_empty() {
                    elems.insert(sv);
                }
            }
        }
    }

    fn union_set_keys(value: &Value) -> Result<std::collections::HashSet<String>, RuntimeError> {
        if Self::union_is_lazy_input(value) {
            return Err(RuntimeError::new("X::Cannot::Lazy"));
        }
        match value {
            Value::Set(s) => Ok((**s).clone()),
            Value::Bag(b) => Ok(b.keys().cloned().collect()),
            Value::Mix(m) => Ok(m.keys().cloned().collect()),
            Value::Hash(h) => Ok(h
                .iter()
                .filter_map(|(k, v)| {
                    if v.truthy() || matches!(v, Value::Nil) {
                        Some(k.clone())
                    } else {
                        None
                    }
                })
                .collect()),
            Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items) => {
                let mut elems = std::collections::HashSet::new();
                for item in items.iter() {
                    Self::union_insert_set_elem(&mut elems, item);
                }
                Ok(elems)
            }
            range if range.is_range() => {
                let mut elems = std::collections::HashSet::new();
                for item in Self::value_to_list(range) {
                    Self::union_insert_set_elem(&mut elems, &item);
                }
                Ok(elems)
            }
            Value::Pair(_, _) | Value::ValuePair(_, _) => {
                let mut elems = std::collections::HashSet::new();
                Self::union_insert_set_elem(&mut elems, value);
                Ok(elems)
            }
            other => {
                let mut elems = std::collections::HashSet::new();
                let sv = other.to_string_value();
                if !sv.is_empty() {
                    elems.insert(sv);
                }
                Ok(elems)
            }
        }
    }

    fn union_bag_counts(
        value: &Value,
    ) -> Result<std::collections::HashMap<String, i64>, RuntimeError> {
        if Self::union_is_lazy_input(value) {
            return Err(RuntimeError::new("X::Cannot::Lazy"));
        }
        match value {
            Value::Bag(b) => Ok((**b).clone()),
            Value::Mix(m) => Ok(m
                .iter()
                .filter_map(|(k, w)| {
                    if *w != 0.0 {
                        Some((k.clone(), 1))
                    } else {
                        None
                    }
                })
                .collect()),
            other => {
                let set = Self::union_set_keys(other)?;
                Ok(set.into_iter().map(|k| (k, 1)).collect())
            }
        }
    }

    fn union_mix_weights(
        value: &Value,
    ) -> Result<std::collections::HashMap<String, f64>, RuntimeError> {
        if Self::union_is_lazy_input(value) {
            return Err(RuntimeError::new("X::Cannot::Lazy"));
        }
        match value {
            Value::Mix(m) => Ok((**m).clone()),
            Value::Bag(b) => Ok(b.iter().map(|(k, v)| (k.clone(), *v as f64)).collect()),
            other => {
                let set = Self::union_set_keys(other)?;
                Ok(set.into_iter().map(|k| (k, 1.0)).collect())
            }
        }
    }

    fn apply_set_union(left: &Value, right: &Value) -> Result<Value, RuntimeError> {
        if matches!(left, Value::Instance { class_name, .. } if class_name == "Failure")
            || matches!(right, Value::Instance { class_name, .. } if class_name == "Failure")
        {
            return Err(RuntimeError::new("Exception"));
        }
        if matches!(left, Value::Mix(_)) || matches!(right, Value::Mix(_)) {
            let mut l = Self::union_mix_weights(left)?;
            let r = Self::union_mix_weights(right)?;
            for (k, v) in r {
                let e = l.entry(k).or_insert(0.0);
                *e = e.max(v);
            }
            return Ok(Value::mix(l));
        }
        if matches!(left, Value::Bag(_)) || matches!(right, Value::Bag(_)) {
            let mut l = Self::union_bag_counts(left)?;
            let r = Self::union_bag_counts(right)?;
            for (k, v) in r {
                let e = l.entry(k).or_insert(0);
                *e = (*e).max(v);
            }
            return Ok(Value::bag(l));
        }
        let mut l = Self::union_set_keys(left)?;
        let r = Self::union_set_keys(right)?;
        l.extend(r);
        Ok(Value::set(l))
    }

    fn reduction_repeat_error(class_name: &str, message: &str) -> RuntimeError {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::Str(message.to_string()));
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
                Ok(Some((n / d).to_i64().unwrap_or(i64::MAX)))
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

    fn shift_left_i64(a: i64, b: i64) -> Value {
        if b < 0 {
            let shift = b.unsigned_abs();
            let shifted = if shift >= i64::BITS as u64 {
                if a < 0 { -1 } else { 0 }
            } else {
                a >> (shift as u32)
            };
            return Value::Int(shifted);
        }
        let shift = b as u64;
        if shift >= i64::BITS as u64 {
            return Value::from_bigint(num_bigint::BigInt::from(a) << (shift as usize));
        }
        // Use BigInt for the shift to avoid i64 overflow (Raku integers are arbitrary precision)
        Value::from_bigint(num_bigint::BigInt::from(a) << (shift as usize))
    }

    fn shift_right_i64(a: i64, b: i64) -> Value {
        if b < 0 {
            let shift = b.unsigned_abs();
            if shift >= i64::BITS as u64 {
                return Value::from_bigint(num_bigint::BigInt::from(a) << (shift as usize));
            }
            if let Some(v) = a.checked_shl(shift as u32) {
                Value::Int(v)
            } else {
                Value::from_bigint(num_bigint::BigInt::from(a) << (shift as usize))
            }
        } else {
            let shift = b as u64;
            let shifted = if shift >= i64::BITS as u64 {
                if a < 0 { -1 } else { 0 }
            } else {
                a >> (shift as u32)
            };
            Value::Int(shifted)
        }
    }

    pub(crate) fn apply_reduction_op(
        op: &str,
        left: &Value,
        right: &Value,
    ) -> Result<Value, RuntimeError> {
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
                _ => 0,
            }
        };
        let is_fractional =
            |v: &Value| matches!(v, Value::Num(_) | Value::Rat(_, _) | Value::FatRat(_, _));
        // Handle R (reverse) meta-prefix: swap operands and recurse with inner op
        if let Some(inner_op) = op.strip_prefix('R')
            && !inner_op.is_empty()
        {
            return Self::apply_reduction_op(inner_op, right, left);
        }
        match op {
            "+" => {
                if is_fractional(left) || is_fractional(right) {
                    Ok(Value::Num(to_num(left) + to_num(right)))
                } else {
                    Ok(Value::Int(to_int(left) + to_int(right)))
                }
            }
            "-" => {
                if is_fractional(left) || is_fractional(right) {
                    Ok(Value::Num(to_num(left) - to_num(right)))
                } else {
                    Ok(Value::Int(to_int(left) - to_int(right)))
                }
            }
            "*" => {
                if is_fractional(left) || is_fractional(right) {
                    Ok(Value::Num(to_num(left) * to_num(right)))
                } else {
                    Ok(Value::Int(to_int(left) * to_int(right)))
                }
            }
            "/" => crate::builtins::arith_div(left.clone(), right.clone()),
            "div" => {
                let divisor = to_int(right);
                if divisor == 0 {
                    return Err(RuntimeError::numeric_divide_by_zero());
                }
                Ok(Value::Int(to_int(left).div_euclid(divisor)))
            }
            "%" => {
                if is_fractional(left) || is_fractional(right) {
                    Ok(Value::Num(to_num(left) % to_num(right)))
                } else {
                    Ok(Value::Int(to_int(left) % to_int(right)))
                }
            }
            "**" => Ok(crate::builtins::arith_pow(left.clone(), right.clone())),
            "~" => Ok(Value::Str(format!(
                "{}{}",
                crate::runtime::utils::coerce_to_str(left),
                crate::runtime::utils::coerce_to_str(right)
            ))),
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
                if !matches!(left, Value::Nil) {
                    Ok(left.clone())
                } else {
                    Ok(right.clone())
                }
            }
            "min" => {
                if to_num(left) <= to_num(right) {
                    Ok(left.clone())
                } else {
                    Ok(right.clone())
                }
            }
            "max" => {
                if to_num(left) >= to_num(right) {
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
            "==" => Ok(Value::Bool(to_num(left) == to_num(right))),
            "!=" => Ok(Value::Bool(to_num(left) != to_num(right))),
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
                    (Value::Rat(_, _), _)
                    | (_, Value::Rat(_, _))
                    | (Value::FatRat(_, _), _)
                    | (_, Value::FatRat(_, _)) => {
                        if let (Some((an, ad)), Some((bn, bd))) =
                            (super::to_rat_parts(left), super::to_rat_parts(right))
                        {
                            super::compare_rat_parts((an, ad), (bn, bd))
                        } else {
                            left.to_string_value().cmp(&right.to_string_value())
                        }
                    }
                    (Value::Num(a), Value::Num(b)) => {
                        a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal)
                    }
                    (Value::Int(a), Value::Num(b)) => (*a as f64)
                        .partial_cmp(b)
                        .unwrap_or(std::cmp::Ordering::Equal),
                    (Value::Num(a), Value::Int(b)) => a
                        .partial_cmp(&(*b as f64))
                        .unwrap_or(std::cmp::Ordering::Equal),
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
                // Basic smartmatch for reduction context: value equality
                Ok(Value::Bool(left == right))
            }
            "eqv" => Ok(Value::Bool(left.eqv(right))),
            "=:=" => Ok(Value::Bool(super::values_identical(left, right))),
            "===" => Ok(Value::Bool(super::values_identical(left, right))),
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
            "~|" => {
                let ls = crate::runtime::utils::coerce_to_str(left);
                let rs = crate::runtime::utils::coerce_to_str(right);
                let max_len = ls.len().max(rs.len());
                let mut out = Vec::with_capacity(max_len);
                for i in 0..max_len {
                    let lb = ls.as_bytes().get(i).copied().unwrap_or(0);
                    let rb = rs.as_bytes().get(i).copied().unwrap_or(0);
                    out.push(lb | rb);
                }
                Ok(Value::Str(String::from_utf8_lossy(&out).into_owned()))
            }
            "~^" => {
                let ls = crate::runtime::utils::coerce_to_str(left);
                let rs = crate::runtime::utils::coerce_to_str(right);
                let max_len = ls.len().max(rs.len());
                let mut out = Vec::with_capacity(max_len);
                for i in 0..max_len {
                    let lb = ls.as_bytes().get(i).copied().unwrap_or(0);
                    let rb = rs.as_bytes().get(i).copied().unwrap_or(0);
                    out.push(lb ^ rb);
                }
                Ok(Value::Str(String::from_utf8_lossy(&out).into_owned()))
            }
            "~&" => {
                let ls = crate::runtime::utils::coerce_to_str(left);
                let rs = crate::runtime::utils::coerce_to_str(right);
                let min_len = ls.len().min(rs.len());
                let mut out = Vec::with_capacity(min_len);
                for i in 0..min_len {
                    out.push(ls.as_bytes()[i] & rs.as_bytes()[i]);
                }
                Ok(Value::Str(String::from_utf8_lossy(&out).into_owned()))
            }
            "+<" => Ok(Self::shift_left_i64(to_int(left), to_int(right))),
            "+>" => Ok(Self::shift_right_i64(to_int(left), to_int(right))),
            "x" => {
                if matches!(right, Value::Whatever) {
                    let mut env = std::collections::HashMap::new();
                    env.insert(
                        "__mutsu_callable_type".to_string(),
                        Value::Str("WhateverCode".to_string()),
                    );
                    let param = "__wc_0".to_string();
                    let body = vec![Stmt::Expr(Expr::Binary {
                        left: Box::new(Expr::Literal(left.clone())),
                        op: crate::token_kind::TokenKind::Ident("x".to_string()),
                        right: Box::new(Expr::Var(param.clone())),
                    })];
                    return Ok(Value::make_sub(
                        "GLOBAL".to_string(),
                        "<whatevercode-x>".to_string(),
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
                Ok(Value::Str(
                    crate::runtime::utils::coerce_to_str(left).repeat(n),
                ))
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
                        crate::value::LazyList {
                            body: Vec::new(),
                            env: std::collections::HashMap::new(),
                            cache: std::sync::Mutex::new(Some(items)),
                        },
                    )))
                } else {
                    Ok(Value::Seq(std::sync::Arc::new(items)))
                }
            }
            "," => {
                let mut items = match left {
                    Value::Array(values, is_itemized) if !*is_itemized => values.to_vec(),
                    Value::Seq(values) | Value::Slip(values) => values.to_vec(),
                    other => vec![other.clone()],
                };
                match right {
                    Value::Array(values, is_itemized) if !*is_itemized => {
                        items.extend(values.iter().cloned());
                    }
                    Value::Seq(values) | Value::Slip(values) => {
                        items.extend(values.iter().cloned());
                    }
                    other => items.push(other.clone()),
                }
                Ok(Value::array(items))
            }
            "(|)" | "∪" => Self::apply_set_union(left, right),
            "(-)" | "∖" => Ok(set_diff_values(left, right)),
            "(&)" | "∩" => Ok(set_intersect_values(left, right)),
            "(^)" | "⊖" => Ok(set_sym_diff_values(left, right)),
            _ => Err(RuntimeError::new(format!(
                "Unsupported reduction operator: {}",
                op
            ))),
        }
    }

    pub(crate) fn value_to_list(val: &Value) -> Vec<Value> {
        match val {
            Value::Array(items, ..) => items.to_vec(),
            Value::Seq(items) => items.to_vec(),
            Value::LazyList(ll) => ll.cache.lock().unwrap().clone().unwrap_or_default(),
            Value::Hash(items) => items
                .iter()
                .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.clone())))
                .collect(),
            Value::Range(a, b) => {
                let end = (*b).min(*a + 1_000_000);
                (*a..=end).map(Value::Int).collect()
            }
            Value::RangeExcl(a, b) => {
                let end = (*b).min(*a + 1_000_000);
                (*a..end).map(Value::Int).collect()
            }
            Value::RangeExclStart(a, b) => {
                let start = *a + 1;
                let end = (*b).min(start + 1_000_000);
                (start..=end).map(Value::Int).collect()
            }
            Value::RangeExclBoth(a, b) => {
                let start = *a + 1;
                let end = (*b).min(start + 1_000_000);
                (start..end).map(Value::Int).collect()
            }
            Value::GenericRange { .. } => crate::runtime::utils::value_to_list(val),
            Value::Set(items) => items.iter().map(|s| Value::Str(s.clone())).collect(),
            Value::Bag(items) => items
                .iter()
                .map(|(k, v)| Value::Pair(k.clone(), Box::new(Value::Int(*v))))
                .collect(),
            Value::Mix(items) => items
                .iter()
                .map(|(k, v)| Value::Pair(k.clone(), Box::new(Value::Num(*v))))
                .collect(),
            Value::Slip(items) => items.to_vec(),
            Value::Nil => vec![],
            other => vec![other.clone()],
        }
    }

    pub(crate) fn compare(
        left: Value,
        right: Value,
        f: fn(i32) -> bool,
    ) -> Result<Value, RuntimeError> {
        if matches!(left, Value::Pair(..) | Value::ValuePair(..))
            || matches!(right, Value::Pair(..) | Value::ValuePair(..))
        {
            return Err(RuntimeError::new("X::Multi::NoMatch"));
        }
        // Version-vs-Version comparison: use version_cmp_parts directly
        if let (Value::Version { parts: ap, .. }, Value::Version { parts: bp, .. }) =
            (&left, &right)
        {
            let ord = super::version_cmp_parts(ap, bp) as i32;
            return Ok(Value::Bool(f(ord)));
        }
        let (l, r) = super::coerce_numeric(left, right);
        // Complex numbers cannot be ordered; throw if either operand has non-zero imaginary part
        if let Value::Complex(_, im) = &l
            && *im != 0.0
        {
            return Err(RuntimeError::new(
                "Cannot convert Complex to Real: imaginary part not zero",
            ));
        }
        if let Value::Complex(_, im) = &r
            && *im != 0.0
        {
            return Err(RuntimeError::new(
                "Cannot convert Complex to Real: imaginary part not zero",
            ));
        }
        if let (Some((an, ad)), Some((bn, bd))) = (super::to_rat_parts(&l), super::to_rat_parts(&r))
            && (matches!(l, Value::Rat(_, _)) || matches!(r, Value::Rat(_, _)))
        {
            return Ok(Value::Bool(f(
                super::compare_rat_parts((an, ad), (bn, bd)) as i32
            )));
        }
        match (l, r) {
            (Value::Int(a), Value::Int(b)) => {
                let ord = a.cmp(&b) as i32;
                Ok(Value::Bool(f(ord)))
            }
            (Value::Num(a), Value::Num(b)) => {
                let ord = a.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal) as i32;
                Ok(Value::Bool(f(ord)))
            }
            (Value::Int(a), Value::Num(b)) => {
                let a = a as f64;
                let ord = a.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal) as i32;
                Ok(Value::Bool(f(ord)))
            }
            (Value::Num(a), Value::Int(b)) => {
                let b = b as f64;
                let ord = a.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal) as i32;
                Ok(Value::Bool(f(ord)))
            }
            (ref left_val, ref right_val) => {
                // Fallback: convert both to f64 for cross-type comparisons (e.g. Num vs Rat)
                let a = super::to_float_value(left_val).unwrap_or(0.0);
                let b = super::to_float_value(right_val).unwrap_or(0.0);
                let ord = a.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal) as i32;
                Ok(Value::Bool(f(ord)))
            }
        }
    }
}
