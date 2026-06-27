use super::*;

impl Interpreter {
    pub(crate) fn value_to_list(val: &Value) -> Vec<Value> {
        match val {
            Value::Array(items, kind) if kind.is_itemized() => vec![val.clone()],
            Value::Array(items, ..) => items.to_vec(),
            Value::Seq(items) => items.to_vec(),
            Value::LazyList(ll) => ll.cache.lock().unwrap().clone().unwrap_or_default(),
            Value::Hash(items) => items
                .iter()
                .map(|(k, v)| items.typed_pair(k, v.clone()))
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
            Value::Set(items, _) => items
                .iter()
                .map(|s| Value::Pair(s.clone(), Box::new(Value::Bool(true))))
                .collect(),
            Value::Bag(items, _) => items
                .iter()
                .map(|(k, v)| Value::Pair(k.clone(), Box::new(Value::from_bigint(v.clone()))))
                .collect(),
            Value::Mix(items, _) => items
                .iter()
                .map(|(k, v)| {
                    Value::Pair(k.clone(), Box::new(crate::value::mix_weight_to_value(*v)))
                })
                .collect(),
            Value::Slip(items) => items.to_vec(),
            // A WalkList flattens to its candidate closures in list context.
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name.resolve() == "WalkList" => {
                crate::runtime::utils::walk_list_candidates(attributes)
                    .unwrap_or_else(|| vec![val.clone()])
            }
            // Nil is a single scalar item in list context (e.g. `for Nil { }`
            // does one iteration); it is not an empty list.
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
            return Err(RuntimeError::typed_msg(
                "X::Multi::NoMatch",
                "Cannot resolve caller; none of the candidates match",
            ));
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
        if let (Some(a), Some(b)) = (super::to_big_rat_parts(&l), super::to_big_rat_parts(&r))
            && (matches!(
                l,
                Value::Rat(_, _) | Value::FatRat(_, _) | Value::BigRat(_, _)
            ) || matches!(
                r,
                Value::Rat(_, _) | Value::FatRat(_, _) | Value::BigRat(_, _)
            ))
        {
            if let Some(ord) = super::compare_big_rat_parts(a, b) {
                return Ok(Value::Bool(f(ord as i32)));
            }
            return Ok(Value::Bool(false));
        }
        match (l, r) {
            (Value::Int(a), Value::Int(b)) => {
                let ord = a.cmp(&b) as i32;
                Ok(Value::Bool(f(ord)))
            }
            (Value::Num(a), Value::Num(b)) => {
                // NaN is unordered: all comparisons except != return False
                if let Some(ord) = a.partial_cmp(&b) {
                    Ok(Value::Bool(f(ord as i32)))
                } else {
                    Ok(Value::Bool(false))
                }
            }
            (Value::Int(a), Value::Num(b)) => {
                let a = a as f64;
                if let Some(ord) = a.partial_cmp(&b) {
                    Ok(Value::Bool(f(ord as i32)))
                } else {
                    Ok(Value::Bool(false))
                }
            }
            (Value::Num(a), Value::Int(b)) => {
                let b = b as f64;
                if let Some(ord) = a.partial_cmp(&b) {
                    Ok(Value::Bool(f(ord as i32)))
                } else {
                    Ok(Value::Bool(false))
                }
            }
            // BigInt vs BigInt
            (Value::BigInt(a), Value::BigInt(b)) => {
                let ord = a.cmp(&b) as i32;
                Ok(Value::Bool(f(ord)))
            }
            // BigInt vs Int (and vice versa) — exact comparison
            (Value::BigInt(a), Value::Int(b)) => {
                let ord = a.as_ref().cmp(&num_bigint::BigInt::from(b)) as i32;
                Ok(Value::Bool(f(ord)))
            }
            (Value::Int(a), Value::BigInt(b)) => {
                let ord = num_bigint::BigInt::from(a).cmp(b.as_ref()) as i32;
                Ok(Value::Bool(f(ord)))
            }
            (ref left_val, ref right_val) => {
                // Fallback: convert both to f64 for cross-type comparisons (e.g. Num vs Rat)
                let a = super::to_float_value(left_val).unwrap_or(0.0);
                let b = super::to_float_value(right_val).unwrap_or(0.0);
                if let Some(ord) = a.partial_cmp(&b) {
                    Ok(Value::Bool(f(ord as i32)))
                } else {
                    Ok(Value::Bool(false))
                }
            }
        }
    }
}
