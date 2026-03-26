use super::*;
use crate::symbol::Symbol;
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
            Value::Set(items, _) => {
                elems.extend(items.iter().cloned());
            }
            Value::Bag(items, _) => {
                for (k, v) in items.iter() {
                    if *v > 0 {
                        elems.insert(k.clone());
                    }
                }
            }
            Value::Mix(items, _) => {
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
            _ if value.as_list_items().is_some() => {
                for item in value.as_list_items().unwrap().iter() {
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
            Value::Set(s, _) => Ok((**s).clone()),
            Value::Bag(b, _) => Ok(b.keys().cloned().collect()),
            Value::Mix(m, _) => Ok(m.keys().cloned().collect()),
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
            _ if value.as_list_items().is_some() => {
                let mut elems = std::collections::HashSet::new();
                for item in value.as_list_items().unwrap().iter() {
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
            Value::Bag(b, _) => Ok((**b).clone()),
            Value::Mix(m, _) => Ok(m
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
            Value::Mix(m, _) => Ok((**m).clone()),
            Value::Bag(b, _) => Ok(b.iter().map(|(k, v)| (k.clone(), *v as f64)).collect()),
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
        if matches!(left, Value::Mix(_, _)) || matches!(right, Value::Mix(_, _)) {
            let mut l = Self::union_mix_weights(left)?;
            let r = Self::union_mix_weights(right)?;
            for (k, v) in r {
                let e = l.entry(k).or_insert(0.0);
                *e = e.max(v);
            }
            return Ok(Value::mix(l));
        }
        if matches!(left, Value::Bag(_, _)) || matches!(right, Value::Bag(_, _)) {
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

    fn set_equal_bag_counts(
        value: &Value,
    ) -> Result<std::collections::HashMap<String, i64>, RuntimeError> {
        let mut counts = Self::union_bag_counts(value)?;
        counts.retain(|_, v| *v > 0);
        Ok(counts)
    }

    fn set_equal_mix_weights(
        value: &Value,
    ) -> Result<std::collections::HashMap<String, f64>, RuntimeError> {
        let mut weights = Self::union_mix_weights(value)?;
        weights.retain(|_, w| *w != 0.0);
        Ok(weights)
    }

    fn apply_set_equality(left: &Value, right: &Value) -> Result<bool, RuntimeError> {
        if matches!(left, Value::Mix(_, _)) || matches!(right, Value::Mix(_, _)) {
            return Ok(Self::set_equal_mix_weights(left)? == Self::set_equal_mix_weights(right)?);
        }
        if matches!(left, Value::Bag(_, _)) || matches!(right, Value::Bag(_, _)) {
            return Ok(Self::set_equal_bag_counts(left)? == Self::set_equal_bag_counts(right)?);
        }
        Ok(Self::union_set_keys(left)? == Self::union_set_keys(right)?)
    }

    fn multiply_pair_i64(value: &Value) -> i64 {
        match value {
            Value::Int(i) => *i,
            Value::Num(n) => *n as i64,
            Value::Rat(n, d) if *d != 0 => n / d,
            Value::FatRat(n, d) if *d != 0 => n / d,
            Value::Bool(b) => i64::from(*b),
            _ => i64::from(value.truthy()),
        }
    }

    fn multiply_pair_f64(value: &Value) -> f64 {
        match value {
            Value::Int(i) => *i as f64,
            Value::Num(n) => *n,
            Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
            Value::FatRat(n, d) if *d != 0 => *n as f64 / *d as f64,
            _ => 1.0,
        }
    }

    fn multiply_bag_counts(
        value: &Value,
    ) -> Result<std::collections::HashMap<String, (i64, bool)>, RuntimeError> {
        if let Value::Scalar(inner) = value {
            return Self::multiply_bag_counts(inner.as_ref());
        }
        if Self::union_is_lazy_input(value) {
            return Err(RuntimeError::new("X::Cannot::Lazy"));
        }
        match value {
            Value::Bag(b, _) => {
                let resolved = crate::runtime::utils::resolve_bag_tab_keys(b);
                Ok(resolved.into_iter().map(|(k, v)| (k, (v, false))).collect())
            }
            Value::Mix(m, _) => Ok(m
                .iter()
                .map(|(k, v)| (k.clone(), (*v as i64, false)))
                .collect()),
            Value::Set(s, _) => Ok(s.iter().map(|k| (k.clone(), (1, false))).collect()),
            Value::Hash(h) => Ok(h
                .iter()
                .filter_map(|(k, v)| {
                    let c = Self::multiply_pair_i64(v);
                    if c > 0 {
                        Some((k.clone(), (c, c != 1)))
                    } else {
                        None
                    }
                })
                .collect()),
            _ if value.as_list_items().is_some() => {
                let mut counts = std::collections::HashMap::new();
                for item in value.as_list_items().unwrap().iter() {
                    match item {
                        Value::Pair(k, v) => {
                            let c = Self::multiply_pair_i64(v);
                            let entry = counts.entry(k.clone()).or_insert((0, false));
                            entry.0 += c;
                            entry.1 |= c != 1;
                        }
                        Value::ValuePair(k, v) => {
                            let key = k.to_string_value();
                            let c = Self::multiply_pair_i64(v);
                            let entry = counts.entry(key).or_insert((0, false));
                            entry.0 += c;
                            entry.1 |= c != 1;
                        }
                        other => {
                            let entry = counts.entry(other.to_string_value()).or_insert((0, false));
                            entry.0 += 1;
                        }
                    }
                }
                Ok(counts)
            }
            range if range.is_range() => {
                let mut counts = std::collections::HashMap::new();
                for item in Self::value_to_list(range) {
                    let entry = counts.entry(item.to_string_value()).or_insert((0, false));
                    entry.0 += 1;
                }
                Ok(counts)
            }
            Value::Pair(k, v) => {
                let mut counts = std::collections::HashMap::new();
                let c = Self::multiply_pair_i64(v);
                if c > 0 {
                    counts.insert(k.clone(), (c, c != 1));
                }
                Ok(counts)
            }
            Value::ValuePair(k, v) => {
                let mut counts = std::collections::HashMap::new();
                let c = Self::multiply_pair_i64(v);
                if c > 0 {
                    counts.insert(k.to_string_value(), (c, c != 1));
                }
                Ok(counts)
            }
            other => {
                let mut counts = std::collections::HashMap::new();
                counts.insert(other.to_string_value(), (1, false));
                Ok(counts)
            }
        }
    }

    fn multiply_mix_weights(
        value: &Value,
    ) -> Result<std::collections::HashMap<String, f64>, RuntimeError> {
        if let Value::Scalar(inner) = value {
            return Self::multiply_mix_weights(inner.as_ref());
        }
        if Self::union_is_lazy_input(value) {
            return Err(RuntimeError::new("X::Cannot::Lazy"));
        }
        match value {
            Value::Mix(m, _) => Ok((**m).clone()),
            Value::Bag(b, _) => {
                let resolved = crate::runtime::utils::resolve_bag_tab_keys(b);
                Ok(resolved.into_iter().map(|(k, v)| (k, v as f64)).collect())
            }
            Value::Set(s, _) => Ok(s.iter().map(|k| (k.clone(), 1.0)).collect()),
            Value::Hash(h) => Ok(h
                .iter()
                .filter_map(|(k, v)| {
                    let w = match v {
                        Value::Int(i) => *i as f64,
                        Value::Num(n) => *n,
                        Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                        Value::FatRat(n, d) if *d != 0 => *n as f64 / *d as f64,
                        Value::Bool(b) => {
                            if *b {
                                1.0
                            } else {
                                0.0
                            }
                        }
                        _ => {
                            if v.truthy() {
                                1.0
                            } else {
                                0.0
                            }
                        }
                    };
                    if w != 0.0 { Some((k.clone(), w)) } else { None }
                })
                .collect()),
            _ if value.as_list_items().is_some() => {
                let mut weights = std::collections::HashMap::new();
                for item in value.as_list_items().unwrap().iter() {
                    match item {
                        Value::Pair(k, v) => {
                            *weights.entry(k.clone()).or_insert(0.0) += Self::multiply_pair_f64(v);
                        }
                        Value::ValuePair(k, v) => {
                            let key = k.to_string_value();
                            *weights.entry(key).or_insert(0.0) += Self::multiply_pair_f64(v);
                        }
                        other => {
                            *weights.entry(other.to_string_value()).or_insert(0.0) += 1.0;
                        }
                    }
                }
                Ok(weights)
            }
            range if range.is_range() => {
                let mut weights = std::collections::HashMap::new();
                for item in Self::value_to_list(range) {
                    *weights.entry(item.to_string_value()).or_insert(0.0) += 1.0;
                }
                Ok(weights)
            }
            Value::Pair(k, v) => {
                let mut weights = std::collections::HashMap::new();
                let w = Self::multiply_pair_f64(v);
                if w != 0.0 {
                    weights.insert(k.clone(), w);
                }
                Ok(weights)
            }
            Value::ValuePair(k, v) => {
                let mut weights = std::collections::HashMap::new();
                let w = Self::multiply_pair_f64(v);
                if w != 0.0 {
                    weights.insert(k.to_string_value(), w);
                }
                Ok(weights)
            }
            other => {
                let mut weights = std::collections::HashMap::new();
                weights.insert(other.to_string_value(), 1.0);
                Ok(weights)
            }
        }
    }

    fn apply_set_multiply(left: &Value, right: &Value) -> Result<Value, RuntimeError> {
        let left = match left {
            Value::Scalar(inner) => inner.as_ref(),
            other => other,
        };
        let right = match right {
            Value::Scalar(inner) => inner.as_ref(),
            other => other,
        };
        if matches!(left, Value::Instance { class_name, .. } if class_name == "Failure")
            || matches!(right, Value::Instance { class_name, .. } if class_name == "Failure")
        {
            return Err(RuntimeError::new("Exception"));
        }
        if matches!(left, Value::Mix(_, _)) || matches!(right, Value::Mix(_, _)) {
            let l = Self::multiply_mix_weights(left)?;
            let r = Self::multiply_mix_weights(right)?;
            let mut result = std::collections::HashMap::new();
            for (k, lv) in l {
                if let Some(rv) = r.get(&k) {
                    let product = lv * rv;
                    if product != 0.0 {
                        result.insert(k, product);
                    }
                }
            }
            return Ok(Value::mix(result));
        }
        let l = Self::multiply_bag_counts(left)?;
        let r = Self::multiply_bag_counts(right)?;
        let mut result: std::collections::HashMap<String, i64> = std::collections::HashMap::new();
        for (k, (lv, _l_explicit)) in l {
            if let Some((rv, _r_explicit)) = r.get(&k) {
                let product = lv * *rv;
                if product > 0 {
                    result.insert(k, product);
                }
            }
        }
        Ok(Value::bag(result))
    }

    /// Coerce a value to bag-like weights for the (+) operator.
    /// Unlike union_bag_counts, this properly handles Hash and Pair values.
    fn addition_bag_counts(
        value: &Value,
    ) -> Result<std::collections::HashMap<String, i64>, RuntimeError> {
        if Self::union_is_lazy_input(value) {
            return Err(RuntimeError::new("X::Cannot::Lazy"));
        }
        match value {
            Value::Bag(b, _) => Ok((**b).clone()),
            Value::Mix(m, _) => Ok(m
                .iter()
                .filter_map(|(k, w)| {
                    if *w != 0.0 {
                        Some((k.clone(), *w as i64))
                    } else {
                        None
                    }
                })
                .collect()),
            Value::Set(s, _) => Ok(s.iter().map(|k| (k.clone(), 1)).collect()),
            Value::Hash(map) => {
                let mut result = std::collections::HashMap::new();
                for (k, v) in map.iter() {
                    let weight = v.to_f64() as i64;
                    if weight != 0 {
                        result.insert(k.clone(), weight);
                    }
                }
                Ok(result)
            }
            _ if value.as_list_items().is_some() => {
                let mut result = std::collections::HashMap::new();
                for item in value.as_list_items().unwrap().iter() {
                    match item {
                        Value::Pair(k, v) => {
                            let weight = v.to_f64() as i64;
                            *result.entry(k.clone()).or_insert(0) += weight;
                        }
                        Value::ValuePair(k, v) => {
                            let weight = v.to_f64() as i64;
                            *result.entry(k.to_string_value()).or_insert(0) += weight;
                        }
                        other => {
                            let key = other.to_string_value();
                            if !key.is_empty() {
                                *result.entry(key).or_insert(0) += 1;
                            }
                        }
                    }
                }
                result.retain(|_, v| *v != 0);
                Ok(result)
            }
            Value::Pair(k, v) => {
                let mut result = std::collections::HashMap::new();
                let weight = v.to_f64() as i64;
                if weight != 0 {
                    result.insert(k.clone(), weight);
                }
                Ok(result)
            }
            other => {
                let set = Self::union_set_keys(other)?;
                Ok(set.into_iter().map(|k| (k, 1)).collect())
            }
        }
    }

    /// Coerce a value to mix-like weights for the (+) operator.
    fn addition_mix_weights(
        value: &Value,
    ) -> Result<std::collections::HashMap<String, f64>, RuntimeError> {
        if Self::union_is_lazy_input(value) {
            return Err(RuntimeError::new("X::Cannot::Lazy"));
        }
        match value {
            Value::Mix(m, _) => Ok((**m).clone()),
            Value::Bag(b, _) => Ok(b.iter().map(|(k, v)| (k.clone(), *v as f64)).collect()),
            Value::Set(s, _) => Ok(s.iter().map(|k| (k.clone(), 1.0)).collect()),
            Value::Hash(map) => {
                let mut result = std::collections::HashMap::new();
                for (k, v) in map.iter() {
                    let weight = v.to_f64();
                    if weight != 0.0 {
                        result.insert(k.clone(), weight);
                    }
                }
                Ok(result)
            }
            _ if value.as_list_items().is_some() => {
                let mut result = std::collections::HashMap::new();
                for item in value.as_list_items().unwrap().iter() {
                    match item {
                        Value::Pair(k, v) => {
                            let weight = v.to_f64();
                            *result.entry(k.clone()).or_insert(0.0) += weight;
                        }
                        Value::ValuePair(k, v) => {
                            let weight = v.to_f64();
                            *result.entry(k.to_string_value()).or_insert(0.0) += weight;
                        }
                        other => {
                            let key = other.to_string_value();
                            if !key.is_empty() {
                                *result.entry(key).or_insert(0.0) += 1.0;
                            }
                        }
                    }
                }
                result.retain(|_, v| *v != 0.0);
                Ok(result)
            }
            Value::Pair(k, v) => {
                let mut result = std::collections::HashMap::new();
                let weight = v.to_f64();
                if weight != 0.0 {
                    result.insert(k.clone(), weight);
                }
                Ok(result)
            }
            other => {
                let set = Self::union_set_keys(other)?;
                Ok(set.into_iter().map(|k| (k, 1.0)).collect())
            }
        }
    }

    fn apply_set_addition(left: &Value, right: &Value) -> Result<Value, RuntimeError> {
        let left = match left {
            Value::Scalar(inner) => inner.as_ref(),
            other => other,
        };
        let right = match right {
            Value::Scalar(inner) => inner.as_ref(),
            other => other,
        };
        if matches!(left, Value::Instance { class_name, .. } if class_name == "Failure")
            || matches!(right, Value::Instance { class_name, .. } if class_name == "Failure")
        {
            return Err(RuntimeError::new("Exception"));
        }
        // Determine type level: Mix > Bag > Set, minimum is Bag for (+)
        let type_level = |v: &Value| -> u8 {
            match v {
                Value::Mix(_, _) => 2,
                Value::Bag(_, _) => 1,
                Value::Package(sym) => match sym.resolve().as_str() {
                    "Mix" | "MixHash" => 2,
                    "Bag" | "BagHash" => 1,
                    _ => 0,
                },
                _ => 0,
            }
        };
        let result_level = type_level(left).max(type_level(right)).max(1);

        if result_level >= 2 {
            let mut l = Self::addition_mix_weights(left)?;
            let r = Self::addition_mix_weights(right)?;
            for (k, v) in r {
                let e = l.entry(k).or_insert(0.0);
                *e += v;
            }
            return Ok(Value::mix(l));
        }
        let mut l = Self::addition_bag_counts(left)?;
        let r = Self::addition_bag_counts(right)?;
        for (k, v) in r {
            let e = l.entry(k).or_insert(0);
            *e += v;
        }
        Ok(Value::bag(l))
    }

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

    fn is_buf_value(val: &Value) -> bool {
        if let Value::Instance { class_name, .. } = val {
            let cn = class_name.resolve();
            cn == "Buf"
                || cn == "Blob"
                || cn == "utf8"
                || cn == "utf16"
                || cn.starts_with("Buf[")
                || cn.starts_with("Blob[")
                || cn.starts_with("buf")
                || cn.starts_with("blob")
        } else {
            false
        }
    }

    fn extract_buf_bytes(val: &Value) -> Vec<u8> {
        if let Value::Instance { attributes, .. } = val
            && let Some(Value::Array(items, ..)) = attributes.get("bytes")
        {
            items
                .iter()
                .map(|v| match v {
                    Value::Int(n) => *n as u8,
                    _ => 0,
                })
                .collect()
        } else {
            Vec::new()
        }
    }

    fn buf_class_name(val: &Value) -> Option<String> {
        if let Value::Instance { class_name, .. } = val {
            let cn = class_name.resolve();
            if cn == "Buf"
                || cn == "Blob"
                || cn == "utf8"
                || cn == "utf16"
                || cn.starts_with("Buf[")
                || cn.starts_with("Blob[")
                || cn.starts_with("buf")
                || cn.starts_with("blob")
            {
                return Some(cn.to_string());
            }
        }
        None
    }

    fn str_bitwise_op(
        left: &Value,
        right: &Value,
        op: fn(u32, u32) -> u32,
        pad_to_max: bool,
    ) -> Result<Value, RuntimeError> {
        let left_is_buf = Self::is_buf_value(left);
        let right_is_buf = Self::is_buf_value(right);
        let any_buf = left_is_buf || right_is_buf;
        if any_buf {
            // For Buf values, operate on bytes
            let lb = if left_is_buf {
                Self::extract_buf_bytes(left)
            } else {
                crate::runtime::utils::coerce_to_str(left)
                    .as_bytes()
                    .to_vec()
            };
            let rb = if right_is_buf {
                Self::extract_buf_bytes(right)
            } else {
                crate::runtime::utils::coerce_to_str(right)
                    .as_bytes()
                    .to_vec()
            };
            // Buf bitwise ops always extend to the longer operand,
            // padding the shorter one with zeros (even for ~&).
            let len = lb.len().max(rb.len());
            let mut out = Vec::with_capacity(len);
            for i in 0..len {
                let a = lb.get(i).copied().unwrap_or(0) as u32;
                let b = rb.get(i).copied().unwrap_or(0) as u32;
                out.push(op(a, b) as u8);
            }
            let byte_vals: Vec<Value> = out.into_iter().map(|b| Value::Int(b as i64)).collect();
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("bytes".to_string(), Value::array(byte_vals));
            let result_type = match (Self::buf_class_name(left), Self::buf_class_name(right)) {
                (Some(l), Some(r)) if l == r => l,
                _ => "Buf".to_string(),
            };
            Ok(Value::make_instance(
                crate::symbol::Symbol::intern(&result_type),
                attrs,
            ))
        } else {
            // For strings, operate on Unicode codepoints (ordinal values)
            let ls = crate::runtime::utils::coerce_to_str(left);
            let rs = crate::runtime::utils::coerce_to_str(right);
            let lc: Vec<u32> = ls.chars().map(|c| c as u32).collect();
            let rc: Vec<u32> = rs.chars().map(|c| c as u32).collect();
            let len = if pad_to_max {
                lc.len().max(rc.len())
            } else {
                lc.len().min(rc.len())
            };
            let mut out = String::with_capacity(len);
            for i in 0..len {
                let a = lc.get(i).copied().unwrap_or(0);
                let b = rc.get(i).copied().unwrap_or(0);
                let result_cp = op(a, b);
                if let Some(ch) = char::from_u32(result_cp) {
                    out.push(ch);
                }
            }
            // Apply NFC normalization to the result
            use unicode_normalization::UnicodeNormalization;
            Ok(Value::str(out.nfc().collect::<String>()))
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

    fn shift_left_bigint(a: &num_bigint::BigInt, b: i64) -> Value {
        if b < 0 {
            Value::from_bigint(a >> (b.unsigned_abs() as usize))
        } else {
            Value::from_bigint(a << (b as usize))
        }
    }

    fn shift_right_bigint(a: &num_bigint::BigInt, b: i64) -> Value {
        if b < 0 {
            Value::from_bigint(a << (b.unsigned_abs() as usize))
        } else {
            Value::from_bigint(a >> (b as usize))
        }
    }

    pub(crate) fn apply_reduction_op(
        op: &str,
        left: &Value,
        right: &Value,
    ) -> Result<Value, RuntimeError> {
        let to_bag_counts = |value: &Value| -> Option<std::collections::HashMap<String, i64>> {
            match value {
                Value::Bag(items, _) => Some((**items).clone()),
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
        let is_fractional =
            |v: &Value| matches!(v, Value::Num(_) | Value::Rat(_, _) | Value::FatRat(_, _));
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
            return Ok(Value::array(out));
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
            return Ok(Value::array(out));
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
            "%" | "mod" => {
                if is_fractional(left) || is_fractional(right) {
                    let l = to_num(left);
                    let r = to_num(right);
                    if r == 0.0 {
                        return Ok(RuntimeError::divide_by_zero_failure(
                            Some(Value::Num(l)),
                            Some("%"),
                        ));
                    }
                    // Raku uses floored-division modulo (sign follows divisor)
                    Ok(Value::Num(l - (l / r).floor() * r))
                } else {
                    let l = to_int(left);
                    let r = to_int(right);
                    if r == 0 {
                        return Ok(RuntimeError::divide_by_zero_failure(
                            Some(Value::Int(l)),
                            Some("%"),
                        ));
                    }
                    // Raku uses floored-division modulo (sign follows divisor)
                    // a - floor(a/b) * b
                    let rem = l % r;
                    let result = if rem != 0 && (rem ^ r) < 0 {
                        rem + r
                    } else {
                        rem
                    };
                    Ok(Value::Int(result))
                }
            }
            "**" => Ok(crate::builtins::arith_pow(left.clone(), right.clone())),
            "~" => {
                // Buf ~ Buf → Buf (byte concatenation, preserving LHS type)
                if crate::vm::VM::is_buf_value(left) && crate::vm::VM::is_buf_value(right) {
                    let result_class = if let Value::Instance { class_name, .. } = left {
                        *class_name
                    } else {
                        crate::symbol::Symbol::intern("Buf")
                    };
                    let mut bytes = crate::vm::VM::extract_buf_bytes(left);
                    bytes.extend(crate::vm::VM::extract_buf_bytes(right));
                    let byte_vals: Vec<Value> =
                        bytes.into_iter().map(|b| Value::Int(b as i64)).collect();
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("bytes".to_string(), Value::array(byte_vals));
                    return Ok(Value::make_instance(result_class, attrs));
                }
                Ok(Value::str(format!(
                    "{}{}",
                    crate::runtime::utils::coerce_to_str(left),
                    crate::runtime::utils::coerce_to_str(right)
                )))
            }
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
                if crate::runtime::types::value_is_defined(left) && left.truthy() {
                    Ok(right.clone())
                } else {
                    Ok(Value::Nil)
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
                // Check if either operand is an array/list - if so, fall through to
                // builtin_minmax which handles flattening. This path is only for
                // scalar reduction like [minmax] 1, 2, 3.
                if matches!(left, Value::Array(_, _) | Value::Seq(_))
                    || matches!(right, Value::Array(_, _) | Value::Seq(_))
                {
                    return Err(RuntimeError::new(format!(
                        "Unsupported reduction operator: {}",
                        op
                    )));
                }
                // Extract min/max from existing Ranges
                let (left_lo, left_hi) = match left {
                    Value::Range(a, b)
                    | Value::RangeExcl(a, b)
                    | Value::RangeExclStart(a, b)
                    | Value::RangeExclBoth(a, b) => (Value::Int(*a), Value::Int(*b)),
                    Value::GenericRange { start, end, .. } => ((**start).clone(), (**end).clone()),
                    _ => (left.clone(), left.clone()),
                };
                let (right_lo, right_hi) = match right {
                    Value::Range(a, b)
                    | Value::RangeExcl(a, b)
                    | Value::RangeExclStart(a, b)
                    | Value::RangeExclBoth(a, b) => (Value::Int(*a), Value::Int(*b)),
                    Value::GenericRange { start, end, .. } => ((**start).clone(), (**end).clone()),
                    _ => (right.clone(), right.clone()),
                };
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
                Ok(Value::GenericRange {
                    start: std::sync::Arc::new(lo),
                    end: std::sync::Arc::new(hi),
                    excl_start: false,
                    excl_end: false,
                })
            }
            "min" => {
                if crate::runtime::compare_values(left, right) <= 0 {
                    Ok(left.clone())
                } else {
                    Ok(right.clone())
                }
            }
            "max" => {
                if crate::runtime::compare_values(left, right) >= 0 {
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
                } else {
                    Ok(Value::Bool(to_num(left) == to_num(right)))
                }
            }
            "=" => Ok(right.clone()),
            "!=" => {
                if let (Some(a), Some(b)) = (
                    super::to_big_rat_parts(left),
                    super::to_big_rat_parts(right),
                ) {
                    Ok(Value::Bool(!super::big_rat_parts_equal(a, b)))
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
                            crate::builtins::methods_0arg::temporal::datetime_attrs(left_attrs);
                        let (ry, rm, rd, rh, rmin, rs, rtz) =
                            crate::builtins::methods_0arg::temporal::datetime_attrs(right_attrs);
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
                        crate::builtins::methods_0arg::temporal::datetime_attrs(dt_attrs);
                    let (dy, dm, dd) = crate::builtins::methods_0arg::temporal::date_attrs(d_attrs);
                    return Ok(Value::Bool(y == dy && m == dm && d == dd));
                }
                // Basic smartmatch fallback: value equality
                Ok(Value::Bool(left == right))
            }
            "eqv" => Ok(Value::Bool(left.eqv(right))),
            "=:=" => Ok(Value::Bool(super::values_identical(left, right))),
            "!=:=" => Ok(Value::Bool(!super::values_identical(left, right))),
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
            "(|)" | "∪" => Self::apply_set_union(left, right),
            "(+)" | "⊎" => Self::apply_set_addition(left, right),
            "(.)" | "⊍" => Self::apply_set_multiply(left, right),
            "(-)" | "∖" => Ok(set_diff_values(left, right)),
            "(&)" | "∩" => Ok(set_intersect_values(left, right)),
            "(^)" | "⊖" => Ok(set_sym_diff_values(left, right)),
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

    pub(crate) fn value_to_list(val: &Value) -> Vec<Value> {
        match val {
            Value::Array(items, kind) if kind.is_itemized() => vec![val.clone()],
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
            Value::Set(items, _) => items
                .iter()
                .map(|s| Value::Pair(s.clone(), Box::new(Value::Bool(true))))
                .collect(),
            Value::Bag(items, _) => items
                .iter()
                .map(|(k, v)| Value::Pair(k.clone(), Box::new(Value::Int(*v))))
                .collect(),
            Value::Mix(items, _) => items
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
