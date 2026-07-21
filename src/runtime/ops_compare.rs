use super::*;

impl Interpreter {
    pub(crate) fn value_to_list(val: &Value) -> Vec<Value> {
        match val.view() {
            ValueView::Array(_, kind) if kind.is_itemized() => vec![val.clone()],
            // A role mixin over a (non-itemized) list-ish value lists as the
            // inner value does; itemized/scalar inners keep one item (and the
            // mixin identity). Mirrors utils::value_to_list.
            ValueView::Mixin(inner, _)
                if matches!(
                    inner.view(),
                    ValueView::Array(_, kind) if !kind.is_itemized()
                ) || matches!(
                    inner.view(),
                    ValueView::Seq(_)
                        | ValueView::Slip(_)
                        | ValueView::LazyList(_)
                        | ValueView::Range(..)
                        | ValueView::RangeExcl(..)
                        | ValueView::RangeExclStart(..)
                        | ValueView::RangeExclBoth(..)
                        | ValueView::GenericRange { .. }
                        | ValueView::Set(..)
                        | ValueView::Bag(..)
                        | ValueView::Mix(..)
                ) || (matches!(inner.view(), ValueView::Hash(_))
                    && !inner.hash_is_itemized()) =>
            {
                Self::value_to_list(inner)
            }
            ValueView::Array(items, ..) => items.to_vec(),
            ValueView::Seq(items) => items.to_vec(),
            ValueView::LazyList(ll) => ll.cache.lock().unwrap().clone().unwrap_or_default(),
            ValueView::Hash(items) => items
                .iter()
                .map(|(k, v)| items.typed_pair(k, v.clone()))
                .collect(),
            ValueView::Range(a, b) => {
                let end = b.min(a + 1_000_000);
                (a..=end).map(Value::int).collect()
            }
            ValueView::RangeExcl(a, b) => {
                let end = b.min(a + 1_000_000);
                (a..end).map(Value::int).collect()
            }
            ValueView::RangeExclStart(a, b) => {
                let start = a + 1;
                let end = b.min(start + 1_000_000);
                (start..=end).map(Value::int).collect()
            }
            ValueView::RangeExclBoth(a, b) => {
                let start = a + 1;
                let end = b.min(start + 1_000_000);
                (start..end).map(Value::int).collect()
            }
            ValueView::GenericRange { .. } => crate::runtime::utils::value_to_list(val),
            ValueView::Set(items, _) => items
                .iter()
                .map(|s| Value::pair(s.clone(), Value::TRUE))
                .collect(),
            ValueView::Bag(items, _) => items
                .iter()
                .map(|(k, v)| Value::pair(k.clone(), Value::from_bigint(v.clone())))
                .collect(),
            ValueView::Mix(items, _) => items
                .iter()
                .map(|(k, v)| Value::pair(k.clone(), crate::value::mix_weight_to_value(*v)))
                .collect(),
            ValueView::Slip(items) => items.to_vec(),
            // Positional-ish instances (WalkList candidates, Backtrace frames,
            // captured __array_items) share the utils implementation.
            ValueView::Instance { .. } => crate::runtime::utils::value_to_list(val),
            // Nil is a single scalar item in list context (e.g. `for Nil { }`
            // does one iteration); it is not an empty list.
            _ => vec![val.clone()],
        }
    }

    pub(crate) fn compare(
        left: Value,
        right: Value,
        f: fn(i32) -> bool,
    ) -> Result<Value, RuntimeError> {
        if matches!(left.view(), ValueView::Pair(..) | ValueView::ValuePair(..))
            || matches!(right.view(), ValueView::Pair(..) | ValueView::ValuePair(..))
        {
            return Err(RuntimeError::typed_msg(
                "X::Multi::NoMatch",
                "Cannot resolve caller; none of the candidates match",
            ));
        }
        // Version-vs-Version comparison: order by parts, then the `+`/`-` flag.
        if let (
            ValueView::Version {
                parts: ap,
                plus: apl,
                minus: ami,
            },
            ValueView::Version {
                parts: bp,
                plus: bpl,
                minus: bmi,
            },
        ) = (left.view(), right.view())
        {
            let ord = super::version_cmp(ap, apl, ami, bp, bpl, bmi) as i32;
            return Ok(Value::truth(f(ord)));
        }
        let (l, r) = super::coerce_numeric(left, right);
        // Complex numbers cannot be ordered; throw if either operand has non-zero imaginary part
        if let ValueView::Complex(_, im) = l.view()
            && im != 0.0
        {
            return Err(RuntimeError::new(
                "Cannot convert Complex to Real: imaginary part not zero",
            ));
        }
        if let ValueView::Complex(_, im) = r.view()
            && im != 0.0
        {
            return Err(RuntimeError::new(
                "Cannot convert Complex to Real: imaginary part not zero",
            ));
        }
        if let (Some(a), Some(b)) = (super::to_big_rat_parts(&l), super::to_big_rat_parts(&r))
            && (matches!(
                l.view(),
                ValueView::Rat(_, _) | ValueView::FatRat(_, _) | ValueView::BigRat(_, _)
            ) || matches!(
                r.view(),
                ValueView::Rat(_, _) | ValueView::FatRat(_, _) | ValueView::BigRat(_, _)
            ))
        {
            if let Some(ord) = super::compare_big_rat_parts(a, b) {
                return Ok(Value::truth(f(ord as i32)));
            }
            return Ok(Value::FALSE);
        }
        match (l.view(), r.view()) {
            (ValueView::Int(a), ValueView::Int(b)) => {
                let ord = a.cmp(&b) as i32;
                Ok(Value::truth(f(ord)))
            }
            (ValueView::Num(a), ValueView::Num(b)) => {
                // NaN is unordered: all comparisons except != return False
                if let Some(ord) = a.partial_cmp(&b) {
                    Ok(Value::truth(f(ord as i32)))
                } else {
                    Ok(Value::FALSE)
                }
            }
            (ValueView::Int(a), ValueView::Num(b)) => {
                let a = a as f64;
                if let Some(ord) = a.partial_cmp(&b) {
                    Ok(Value::truth(f(ord as i32)))
                } else {
                    Ok(Value::FALSE)
                }
            }
            (ValueView::Num(a), ValueView::Int(b)) => {
                let b = b as f64;
                if let Some(ord) = a.partial_cmp(&b) {
                    Ok(Value::truth(f(ord as i32)))
                } else {
                    Ok(Value::FALSE)
                }
            }
            // BigInt vs BigInt
            (ValueView::BigInt(a), ValueView::BigInt(b)) => {
                let ord = a.as_ref().cmp(b.as_ref()) as i32;
                Ok(Value::truth(f(ord)))
            }
            // BigInt vs Int (and vice versa) — exact comparison
            (ValueView::BigInt(a), ValueView::Int(b)) => {
                let ord = a.as_ref().cmp(&num_bigint::BigInt::from(b)) as i32;
                Ok(Value::truth(f(ord)))
            }
            (ValueView::Int(a), ValueView::BigInt(b)) => {
                let ord = num_bigint::BigInt::from(a).cmp(b.as_ref()) as i32;
                Ok(Value::truth(f(ord)))
            }
            (_, _) => {
                // Fallback: convert both to f64 for cross-type comparisons (e.g. Num vs Rat)
                let a = super::to_float_value(&l).unwrap_or(0.0);
                let b = super::to_float_value(&r).unwrap_or(0.0);
                if let Some(ord) = a.partial_cmp(&b) {
                    Ok(Value::truth(f(ord as i32)))
                } else {
                    Ok(Value::FALSE)
                }
            }
        }
    }
}
