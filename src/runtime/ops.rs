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
        match op {
            "+" => {
                if matches!(left, Value::Num(_)) || matches!(right, Value::Num(_)) {
                    Ok(Value::Num(to_num(left) + to_num(right)))
                } else {
                    Ok(Value::Int(to_int(left) + to_int(right)))
                }
            }
            "-" => {
                if matches!(left, Value::Num(_)) || matches!(right, Value::Num(_)) {
                    Ok(Value::Num(to_num(left) - to_num(right)))
                } else {
                    Ok(Value::Int(to_int(left) - to_int(right)))
                }
            }
            "*" => {
                if matches!(left, Value::Num(_)) || matches!(right, Value::Num(_)) {
                    Ok(Value::Num(to_num(left) * to_num(right)))
                } else {
                    Ok(Value::Int(to_int(left) * to_int(right)))
                }
            }
            "/" => {
                if let (Value::Int(a), Value::Int(b)) = (left, right) {
                    if *b == 0 {
                        // Rat with zero denominator (numifies to Inf/-Inf/NaN)
                        Ok(Value::Rat(*a, 0))
                    } else {
                        Ok(crate::value::make_rat(*a, *b))
                    }
                } else {
                    Ok(Value::Num(to_num(left) / to_num(right)))
                }
            }
            "%" => {
                if matches!(left, Value::Num(_)) || matches!(right, Value::Num(_)) {
                    Ok(Value::Num(to_num(left) % to_num(right)))
                } else {
                    Ok(Value::Int(to_int(left) % to_int(right)))
                }
            }
            "**" => Ok(Value::Num(to_num(left).powf(to_num(right)))),
            "~" => Ok(Value::Str(format!(
                "{}{}",
                left.to_string_value(),
                right.to_string_value()
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
                            (an * bd).cmp(&(bn * ad))
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
            _ => Err(RuntimeError::new(format!(
                "Unsupported reduction operator: {}",
                op
            ))),
        }
    }

    pub(crate) fn value_to_list(val: &Value) -> Vec<Value> {
        match val {
            Value::Array(items) => items.to_vec(),
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
            Value::GenericRange { .. } => vec![val.clone()],
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

    pub(crate) fn eval_hyper_op(
        op: &str,
        left: &Value,
        right: &Value,
        dwim_left: bool,
        dwim_right: bool,
    ) -> Result<Value, RuntimeError> {
        let left_list = Self::value_to_list(left);
        let right_list = Self::value_to_list(right);
        let left_len = left_list.len();
        let right_len = right_list.len();

        if left_len == 0 && right_len == 0 {
            return Ok(Value::array(Vec::new()));
        }

        let result_len = if !dwim_left && !dwim_right {
            if left_len != right_len {
                return Err(RuntimeError::new(format!(
                    "Non-dwimmy hyper operator: left has {} elements, right has {}",
                    left_len, right_len
                )));
            }
            left_len
        } else if dwim_left && dwim_right {
            std::cmp::max(left_len, right_len)
        } else if dwim_right {
            left_len
        } else {
            right_len
        };

        let mut results = Vec::with_capacity(result_len);
        for i in 0..result_len {
            let l = if left_len == 0 {
                &Value::Int(0)
            } else {
                &left_list[i % left_len]
            };
            let r = if right_len == 0 {
                &Value::Int(0)
            } else {
                &right_list[i % right_len]
            };
            results.push(Self::apply_reduction_op(op, l, r)?);
        }
        Ok(Value::array(results))
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
            let lhs = an as i128 * bd as i128;
            let rhs = bn as i128 * ad as i128;
            return Ok(Value::Bool(f(lhs.cmp(&rhs) as i32)));
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
            _ => Ok(Value::Bool(f(0))),
        }
    }
}
