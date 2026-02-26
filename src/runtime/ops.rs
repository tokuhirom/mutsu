use super::*;

impl Interpreter {
    pub(crate) fn apply_reduction_op(
        op: &str,
        left: &Value,
        right: &Value,
    ) -> Result<Value, RuntimeError> {
        let to_num = |v: &Value| -> f64 {
            match v {
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
            match v {
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
            "/" => {
                if let (Value::Int(a), Value::Int(b)) = (left, right) {
                    if *b == 0 {
                        Ok(crate::value::make_rat(*a, 0))
                    } else {
                        Ok(crate::value::make_rat(*a, *b))
                    }
                } else {
                    let denom = to_num(right);
                    if denom == 0.0 {
                        Err(RuntimeError::numeric_divide_by_zero())
                    } else {
                        Ok(Value::Num(to_num(left) / denom))
                    }
                }
            }
            "%" => {
                if is_fractional(left) || is_fractional(right) {
                    Ok(Value::Num(to_num(left) % to_num(right)))
                } else {
                    Ok(Value::Int(to_int(left) % to_int(right)))
                }
            }
            "**" => Ok(Value::Num(to_num(left).powf(to_num(right)))),
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
            "+&" => Ok(Value::Int(to_int(left) & to_int(right))),
            "+|" => Ok(Value::Int(to_int(left) | to_int(right))),
            "+^" => Ok(Value::Int(to_int(left) ^ to_int(right))),
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
            "+<" => Ok(Value::Int(to_int(left) << (to_int(right) as u32))),
            "+>" => Ok(Value::Int(to_int(left) >> (to_int(right) as u32))),
            "x" => {
                let s = crate::runtime::utils::coerce_to_str(left);
                let n = to_int(right).max(0) as usize;
                Ok(Value::Str(s.repeat(n)))
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
                let n = to_int(right).max(0) as usize;
                let items: Vec<Value> = std::iter::repeat_n(left.clone(), n).collect();
                Ok(Value::Seq(std::sync::Arc::new(items)))
            }
            "," => {
                let mut items = match left {
                    Value::Array(values, ..) => values.to_vec(),
                    other => vec![other.clone()],
                };
                items.push(right.clone());
                Ok(Value::array(items))
            }
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
        // Version-vs-Version comparison: use version_cmp_parts directly
        if let (Value::Version { parts: ap, .. }, Value::Version { parts: bp, .. }) =
            (&left, &right)
        {
            let ord = super::version_cmp_parts(ap, bp) as i32;
            return Ok(Value::Bool(f(ord)));
        }
        let (l, r) = super::coerce_numeric(left, right);
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
