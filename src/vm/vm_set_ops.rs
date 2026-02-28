use super::*;

impl VM {
    fn integral_bigint(value: &Value) -> Option<num_bigint::BigInt> {
        match value {
            Value::Int(i) => Some(num_bigint::BigInt::from(*i)),
            Value::BigInt(i) => Some(i.clone()),
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

    fn is_failure_value(value: &Value) -> bool {
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
        if let Some(info) = self.interpreter.container_type_metadata(whole) {
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
                    && !self.interpreter.type_matches_value(&key_type, needle)
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
            Value::Set(s) => s.contains(&key),
            Value::Bag(b) => b.get(&key).is_some_and(|count| *count > 0),
            Value::Mix(m) => m.get(&key).is_some_and(|weight| *weight != 0.0),
            Value::Hash(h) => self.hash_contains(h, needle, container),
            Value::Array(items, _) | Value::Seq(items) | Value::Slip(items) => {
                items.iter().any(|item| Self::value_matches_key(item, &key))
            }
            range if range.is_range() => Self::range_contains(range, needle),
            _ => false,
        }
    }

    fn lazy_list_error() -> RuntimeError {
        RuntimeError::new("X::Cannot::Lazy")
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

    fn union_insert_set_elem(elems: &mut HashSet<String>, value: &Value) {
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
                let mut elems = HashSet::new();
                for item in items.iter() {
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
            Value::Mix(m) => Ok((**m).clone()),
            Value::Bag(b) => Ok(b.iter().map(|(k, v)| (k.clone(), *v as f64)).collect()),
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

        let result = match (left, right) {
            (Value::Mix(a), Value::Mix(b)) => {
                let mut result = (*a).clone();
                for (k, v) in b.iter() {
                    let e = result.entry(k.clone()).or_insert(0.0);
                    *e = e.max(*v);
                }
                Value::mix(result)
            }
            (Value::Bag(a), Value::Bag(b)) => {
                let mut result = (*a).clone();
                for (k, v) in b.iter() {
                    let e = result.entry(k.clone()).or_insert(0);
                    *e = (*e).max(*v);
                }
                Value::bag(result)
            }
            (Value::Set(a), Value::Set(b)) => {
                let mut result = (*a).clone();
                for elem in b.iter() {
                    result.insert(elem.clone());
                }
                Value::set(result)
            }
            (l, r) if matches!(l, Value::Mix(_)) || matches!(r, Value::Mix(_)) => {
                let mut left_mix = Self::value_to_mix_weights(&l)?;
                let right_mix = Self::value_to_mix_weights(&r)?;
                for (k, v) in right_mix {
                    let e = left_mix.entry(k).or_insert(0.0);
                    *e = e.max(v);
                }
                Value::mix(left_mix)
            }
            (l, r) if matches!(l, Value::Bag(_)) || matches!(r, Value::Bag(_)) => {
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
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_set_intersect_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = match (left, right) {
            (Value::Set(a), Value::Set(b)) => Value::set(a.intersection(&b).cloned().collect()),
            (Value::Bag(a), Value::Bag(b)) => {
                let mut result = HashMap::new();
                for (k, v) in a.iter() {
                    if let Some(bv) = b.get(k) {
                        result.insert(k.clone(), (*v).min(*bv));
                    }
                }
                Value::bag(result)
            }
            (Value::Mix(a), Value::Mix(b)) => {
                let mut result = HashMap::new();
                for (k, v) in a.iter() {
                    if let Some(bv) = b.get(k) {
                        result.insert(k.clone(), v.min(*bv));
                    }
                }
                Value::mix(result)
            }
            (l, r) => {
                let a = runtime::coerce_to_set(&l);
                let b = runtime::coerce_to_set(&r);
                Value::set(a.intersection(&b).cloned().collect())
            }
        };
        self.stack.push(result);
    }

    pub(super) fn exec_set_diff_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = runtime::set_diff_values(&left, &right);
        self.stack.push(result);
    }

    pub(super) fn exec_set_sym_diff_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = match (left, right) {
            (Value::Set(a), Value::Set(b)) => {
                Value::set(a.symmetric_difference(&b).cloned().collect())
            }
            (l, r) => {
                let a = runtime::coerce_to_set(&l);
                let b = runtime::coerce_to_set(&r);
                Value::set(a.symmetric_difference(&b).cloned().collect())
            }
        };
        self.stack.push(result);
    }

    pub(super) fn exec_set_subset_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let a = runtime::coerce_to_set(&left);
        let b = runtime::coerce_to_set(&right);
        self.stack.push(Value::Bool(a.is_subset(&b)));
    }

    pub(super) fn exec_set_superset_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let a = runtime::coerce_to_set(&left);
        let b = runtime::coerce_to_set(&right);
        self.stack.push(Value::Bool(a.is_superset(&b)));
    }

    pub(super) fn exec_set_strict_subset_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let a = runtime::coerce_to_set(&left);
        let b = runtime::coerce_to_set(&right);
        self.stack
            .push(Value::Bool(a.is_subset(&b) && a.len() < b.len()));
    }

    pub(super) fn exec_set_strict_superset_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let a = runtime::coerce_to_set(&left);
        let b = runtime::coerce_to_set(&right);
        self.stack
            .push(Value::Bool(a.is_superset(&b) && a.len() > b.len()));
    }

    pub(super) fn exec_junction_any_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack
            .push(runtime::merge_junction(JunctionKind::Any, left, right));
    }

    pub(super) fn exec_junction_all_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack
            .push(runtime::merge_junction(JunctionKind::All, left, right));
    }

    pub(super) fn exec_junction_one_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack
            .push(runtime::merge_junction(JunctionKind::One, left, right));
    }
}
