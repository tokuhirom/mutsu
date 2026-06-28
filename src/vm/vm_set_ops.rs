use super::*;

impl Interpreter {
    fn integral_bigint(value: &Value) -> Option<num_bigint::BigInt> {
        match value {
            Value::Int(i) => Some(num_bigint::BigInt::from(*i)),
            Value::BigInt(i) => Some((**i).clone()),
            Value::Num(n)
                if n.fract() == 0.0
                    && n.is_finite()
                    && *n >= i64::MIN as f64
                    && *n <= i64::MAX as f64 =>
            {
                Some(num_bigint::BigInt::from(*n as i64))
            }
            Value::Rat(n, d) if *d != 0 && *n % *d == 0 => Some(num_bigint::BigInt::from(*n / *d)),
            _ => None,
        }
    }

    pub(crate) fn is_failure_value(value: &Value) -> bool {
        matches!(value, Value::Instance { class_name, .. } if class_name == "Failure")
    }

    fn value_matches_key(value: &Value, key: &str) -> bool {
        value.to_string_value() == key
    }

    fn range_contains(range: &Value, needle: &Value) -> bool {
        match range {
            Value::Range(start, end) => {
                if let Some(v) = Self::integral_bigint(needle) {
                    let min = num_bigint::BigInt::from(*start);
                    let max = num_bigint::BigInt::from(*end);
                    v >= min && v <= max
                } else {
                    false
                }
            }
            Value::RangeExcl(start, end) => {
                if let Some(v) = Self::integral_bigint(needle) {
                    let min = num_bigint::BigInt::from(*start);
                    let max = num_bigint::BigInt::from(*end);
                    v >= min && v < max
                } else {
                    false
                }
            }
            Value::RangeExclStart(start, end) => {
                if let Some(v) = Self::integral_bigint(needle) {
                    let min = num_bigint::BigInt::from(*start);
                    let max = num_bigint::BigInt::from(*end);
                    v > min && v <= max
                } else {
                    false
                }
            }
            Value::RangeExclBoth(start, end) => {
                if let Some(v) = Self::integral_bigint(needle) {
                    let min = num_bigint::BigInt::from(*start);
                    let max = num_bigint::BigInt::from(*end);
                    v > min && v < max
                } else {
                    false
                }
            }
            Value::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                if matches!(start.as_ref(), Value::Str(_)) || matches!(end.as_ref(), Value::Str(_))
                {
                    let v = needle.to_string_value();
                    let min = start.to_string_value();
                    let max = end.to_string_value();
                    let min_ok = if *excl_start { v > min } else { v >= min };
                    let max_ok = if *excl_end { v < max } else { v <= max };
                    min_ok && max_ok
                } else if matches!(
                    (start.as_ref(), end.as_ref()),
                    (
                        Value::Int(_) | Value::BigInt(_),
                        Value::Int(_) | Value::BigInt(_)
                    )
                ) {
                    let Some(v) = Self::integral_bigint(needle) else {
                        return false;
                    };
                    let min = start.to_bigint();
                    let max = end.to_bigint();
                    let min_ok = if *excl_start { v > min } else { v >= min };
                    let max_ok = if *excl_end { v < max } else { v <= max };
                    min_ok && max_ok
                } else {
                    let v = needle.to_f64();
                    let min = start.to_f64();
                    let max = end.to_f64();
                    let min_ok = if *excl_start { v > min } else { v >= min };
                    let max_ok = if *excl_end { v < max } else { v <= max };
                    min_ok && max_ok
                }
            }
            _ => false,
        }
    }

    fn hash_contains(
        &mut self,
        hash: &HashMap<String, Value>,
        needle: &Value,
        whole: &Value,
    ) -> bool {
        if let Some(info) = self.container_type_metadata(whole) {
            if let Some(key_type) = info.key_type {
                if (key_type == "Any" || key_type == "Mu")
                    && matches!(needle, Value::Str(s) if s.parse::<i128>().is_ok())
                {
                    // Heuristic for typed hashes: numeric-looking string keys should
                    // not alias numeric keys.
                    return false;
                }
                if key_type != "Any"
                    && key_type != "Mu"
                    && !self.type_matches_value(&key_type, needle)
                {
                    return false;
                }
            }
        } else if !matches!(needle, Value::Str(_)) {
            // Plain Hash keys are Str by default.
            return false;
        }
        hash.contains_key(&needle.to_string_value())
    }

    fn set_contains(&mut self, container: &Value, needle: &Value) -> bool {
        let key = needle.to_string_value();
        match container {
            Value::Set(s, _) => s.contains(&key),
            Value::Bag(b, _) => b.get(&key).is_some_and(num_traits::Signed::is_positive),
            Value::Mix(m, _) => m.get(&key).is_some_and(|weight| *weight != 0.0),
            Value::Hash(h) => self.hash_contains(h, needle, container),
            v if v.as_list_items().is_some() => v
                .as_list_items()
                .unwrap()
                .iter()
                .any(|item| Self::value_matches_key(item, &key)),
            range if range.is_range() => Self::range_contains(range, needle),
            _ => false,
        }
    }

    fn quant_hash_weights(value: &Value) -> HashMap<String, f64> {
        match runtime::coerce_value_to_quanthash(value) {
            Value::Set(items, _) => items.iter().map(|k| (k.clone(), 1.0)).collect(),
            Value::Bag(items, _) => items
                .iter()
                .map(|(k, v)| (k.clone(), crate::runtime::utils::bigint_to_f64_sat(v)))
                .collect(),
            Value::Mix(items, _) => items.iter().map(|(k, v)| (k.clone(), *v)).collect(),
            _ => HashMap::new(),
        }
    }

    pub(super) fn quant_hash_subset(left: &Value, right: &Value) -> bool {
        let left_map = Self::quant_hash_weights(left);
        let right_map = Self::quant_hash_weights(right);
        let keys: std::collections::HashSet<String> =
            left_map.keys().chain(right_map.keys()).cloned().collect();
        keys.into_iter().all(|key| {
            let lv = left_map.get(&key).copied().unwrap_or(0.0);
            let rv = right_map.get(&key).copied().unwrap_or(0.0);
            lv <= rv
        })
    }

    pub(super) fn quant_hash_strict_subset(left: &Value, right: &Value) -> bool {
        let left_map = Self::quant_hash_weights(left);
        let right_map = Self::quant_hash_weights(right);
        let keys: std::collections::HashSet<String> =
            left_map.keys().chain(right_map.keys()).cloned().collect();
        let mut strictly_less = false;
        for key in keys {
            let lv = left_map.get(&key).copied().unwrap_or(0.0);
            let rv = right_map.get(&key).copied().unwrap_or(0.0);
            if lv > rv {
                return false;
            }
            if lv < rv {
                strictly_less = true;
            }
        }
        strictly_less
    }

    pub(super) fn lazy_list_error() -> RuntimeError {
        RuntimeError::cannot_lazy("elems")
    }

    fn is_infinite_bound(value: &Value) -> bool {
        match value {
            Value::Num(n) => n.is_infinite(),
            Value::Rat(_, d) | Value::FatRat(_, d) => *d == 0,
            Value::Mixin(inner, _) => Self::is_infinite_bound(inner),
            _ => false,
        }
    }

    fn is_lazy_union_input(value: &Value) -> bool {
        match value {
            Value::LazyList(_) => true,
            Value::GenericRange { start, end, .. } => {
                Self::is_infinite_bound(start) || Self::is_infinite_bound(end)
            }
            _ => false,
        }
    }

    pub(crate) fn union_insert_set_elem(elems: &mut HashSet<String>, value: &Value) {
        let pair_selected = |weight: &Value| weight.truthy() || matches!(weight, Value::Nil);
        match value {
            Value::Set(items, _) => {
                elems.extend(items.iter().cloned());
            }
            Value::Bag(items, _) => {
                for (k, v) in items.iter() {
                    if num_traits::Signed::is_positive(v) {
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
            v if v.as_list_items().is_some() => {
                for item in v.as_list_items().unwrap().iter() {
                    Self::union_insert_set_elem(elems, item);
                }
            }
            range if range.is_range() => {
                for item in runtime::value_to_list(range) {
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

    fn value_to_set_keys(value: &Value) -> Result<HashSet<String>, RuntimeError> {
        if Self::is_lazy_union_input(value) {
            return Err(Self::lazy_list_error());
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
            v if v.as_list_items().is_some() => {
                let mut elems = HashSet::new();
                for item in v.as_list_items().unwrap().iter() {
                    Self::union_insert_set_elem(&mut elems, item);
                }
                Ok(elems)
            }
            range if range.is_range() => {
                let mut elems = HashSet::new();
                for item in runtime::value_to_list(range) {
                    Self::union_insert_set_elem(&mut elems, &item);
                }
                Ok(elems)
            }
            Value::Pair(_, _) | Value::ValuePair(_, _) => {
                let mut elems = HashSet::new();
                Self::union_insert_set_elem(&mut elems, value);
                Ok(elems)
            }
            other => {
                let mut elems = HashSet::new();
                let sv = other.to_string_value();
                if !sv.is_empty() {
                    elems.insert(sv);
                }
                Ok(elems)
            }
        }
    }

    fn value_to_bag_counts(value: &Value) -> Result<HashMap<String, i64>, RuntimeError> {
        if Self::is_lazy_union_input(value) {
            return Err(Self::lazy_list_error());
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
                let set = Self::value_to_set_keys(other)?;
                Ok(set.into_iter().map(|k| (k, 1)).collect())
            }
        }
    }

    fn value_to_mix_weights(value: &Value) -> Result<HashMap<String, f64>, RuntimeError> {
        if Self::is_lazy_union_input(value) {
            return Err(Self::lazy_list_error());
        }
        match value {
            Value::Mix(m, _) => Ok(m.weights.clone()),
            Value::Bag(b, _) => Ok(b
                .iter()
                .map(|(k, v)| (k.clone(), crate::runtime::utils::bigint_to_f64_sat(v)))
                .collect()),
            other => {
                let set = Self::value_to_set_keys(other)?;
                Ok(set.into_iter().map(|k| (k, 1.0)).collect())
            }
        }
    }

    pub(super) fn exec_set_elem_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        if Self::is_failure_value(&left) || Self::is_failure_value(&right) {
            return Err(RuntimeError::new("Exception"));
        }
        let result = self.set_contains(&right, &left);
        self.stack.push(Value::Bool(result));
        Ok(())
    }

    pub(super) fn exec_set_cont_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        if Self::is_failure_value(&left) || Self::is_failure_value(&right) {
            return Err(RuntimeError::new("Exception"));
        }
        let result = self.set_contains(&left, &right);
        self.stack.push(Value::Bool(result));
        Ok(())
    }

    pub(super) fn exec_set_union_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        if Self::is_failure_value(&left) || Self::is_failure_value(&right) {
            return Err(RuntimeError::new("Exception"));
        }
        let result_mutable = runtime::set_result_mutability(&left);

        let result = match (left, right) {
            (Value::Mix(a, _), Value::Mix(b, _)) => {
                let mut result = a.weights.clone();
                for (k, v) in b.iter() {
                    // For union: if key exists in both, take max; if only in one side, take that value
                    if let Some(e) = result.get_mut(k) {
                        *e = e.max(*v);
                    } else {
                        result.insert(k.clone(), *v);
                    }
                }
                Value::mix(result)
            }
            (Value::Bag(a, _), Value::Bag(b, _)) => {
                let mut result = a.counts.clone();
                for (k, v) in b.iter() {
                    let e = result.entry(k.clone()).or_default();
                    if *v > *e {
                        *e = v.clone();
                    }
                }
                Value::bag_big(result)
            }
            (Value::Set(a, _), Value::Set(b, _)) => {
                let mut result = a.elements.clone();
                for elem in b.iter() {
                    result.insert(elem.clone());
                }
                Value::set(result)
            }
            (l, r) if matches!(l, Value::Mix(_, _)) || matches!(r, Value::Mix(_, _)) => {
                let mut left_mix = Self::value_to_mix_weights(&l)?;
                let right_mix = Self::value_to_mix_weights(&r)?;
                for (k, v) in right_mix {
                    if let Some(e) = left_mix.get_mut(&k) {
                        *e = e.max(v);
                    } else {
                        left_mix.insert(k, v);
                    }
                }
                Value::mix(left_mix)
            }
            (l, r) if matches!(l, Value::Bag(_, _)) || matches!(r, Value::Bag(_, _)) => {
                let mut left_bag = Self::value_to_bag_counts(&l)?;
                let right_bag = Self::value_to_bag_counts(&r)?;
                for (k, v) in right_bag {
                    let e = left_bag.entry(k).or_insert(0);
                    *e = (*e).max(v);
                }
                Value::bag(left_bag)
            }
            (l, r) => {
                let mut left_set = Self::value_to_set_keys(&l)?;
                let right_set = Self::value_to_set_keys(&r)?;
                for elem in right_set {
                    left_set.insert(elem);
                }
                Value::set(left_set)
            }
        };
        self.stack
            .push(runtime::with_set_mutability(result, result_mutable));
        Ok(())
    }
}
