use super::*;

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

    fn union_set_keys(value: &Value) -> Result<std::collections::HashSet<String>, RuntimeError> {
        if Self::union_is_lazy_input(value) {
            return Err(RuntimeError::new("X::Cannot::Lazy"));
        }
        match value {
            Value::Set(s, _) => Ok(s.elements.clone()),
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
            Value::Bag(b, _) => Ok(crate::runtime::utils::bag_counts_as_i64(&b.counts)),
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
            Value::Mix(m, _) => Ok(m.weights.clone()),
            Value::Bag(b, _) => Ok(b
                .iter()
                .map(|(k, v)| (k.clone(), crate::runtime::utils::bigint_to_f64_sat(v)))
                .collect()),
            other => {
                let set = Self::union_set_keys(other)?;
                Ok(set.into_iter().map(|k| (k, 1.0)).collect())
            }
        }
    }

    pub(crate) fn apply_set_union(left: &Value, right: &Value) -> Result<Value, RuntimeError> {
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

    pub(crate) fn apply_set_equality(left: &Value, right: &Value) -> Result<bool, RuntimeError> {
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
        let value = value.descalarize();
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
        let value = value.descalarize();
        if Self::union_is_lazy_input(value) {
            return Err(RuntimeError::new("X::Cannot::Lazy"));
        }
        match value {
            Value::Mix(m, _) => Ok(m.weights.clone()),
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

    pub(crate) fn apply_set_multiply(left: &Value, right: &Value) -> Result<Value, RuntimeError> {
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

    /// Coerce a value to bag-like count map for the (+) operator.
    /// Unlike union_bag_counts, this properly handles Hash and Pair values.
    fn addition_bag_counts(
        value: &Value,
    ) -> Result<std::collections::HashMap<String, i64>, RuntimeError> {
        if Self::union_is_lazy_input(value) {
            return Err(RuntimeError::new("X::Cannot::Lazy"));
        }
        match value {
            Value::Bag(b, _) => Ok(crate::runtime::utils::bag_counts_as_i64(&b.counts)),
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
            Value::Mix(m, _) => Ok(m.weights.clone()),
            Value::Bag(b, _) => Ok(b
                .iter()
                .map(|(k, v)| (k.clone(), crate::runtime::utils::bigint_to_f64_sat(v)))
                .collect()),
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

    pub(crate) fn apply_set_addition(left: &Value, right: &Value) -> Result<Value, RuntimeError> {
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
}
