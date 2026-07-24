use super::*;

impl Interpreter {
    fn integral_bigint(value: &Value) -> Option<num_bigint::BigInt> {
        match value.view() {
            ValueView::Int(i) => Some(num_bigint::BigInt::from(i)),
            ValueView::BigInt(i) => Some((**i).clone()),
            ValueView::Num(n)
                if n.fract() == 0.0
                    && n.is_finite()
                    && n >= i64::MIN as f64
                    && n <= i64::MAX as f64 =>
            {
                Some(num_bigint::BigInt::from(n as i64))
            }
            ValueView::Rat(n, d) if d != 0 && n % d == 0 => Some(num_bigint::BigInt::from(n / d)),
            _ => None,
        }
    }

    pub(crate) fn is_failure_value(value: &Value) -> bool {
        matches!(value.view(), ValueView::Instance { class_name, .. } if class_name == "Failure")
    }

    fn range_contains(range: &Value, needle: &Value) -> bool {
        match range.view() {
            ValueView::Range(start, end) => {
                if let Some(v) = Self::integral_bigint(needle) {
                    let min = num_bigint::BigInt::from(start);
                    let max = num_bigint::BigInt::from(end);
                    v >= min && v <= max
                } else {
                    false
                }
            }
            ValueView::RangeExcl(start, end) => {
                if let Some(v) = Self::integral_bigint(needle) {
                    let min = num_bigint::BigInt::from(start);
                    let max = num_bigint::BigInt::from(end);
                    v >= min && v < max
                } else {
                    false
                }
            }
            ValueView::RangeExclStart(start, end) => {
                if let Some(v) = Self::integral_bigint(needle) {
                    let min = num_bigint::BigInt::from(start);
                    let max = num_bigint::BigInt::from(end);
                    v > min && v <= max
                } else {
                    false
                }
            }
            ValueView::RangeExclBoth(start, end) => {
                if let Some(v) = Self::integral_bigint(needle) {
                    let min = num_bigint::BigInt::from(start);
                    let max = num_bigint::BigInt::from(end);
                    v > min && v < max
                } else {
                    false
                }
            }
            ValueView::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                if matches!(start.view(), ValueView::Str(_))
                    || matches!(end.view(), ValueView::Str(_))
                {
                    let v = needle.to_string_value();
                    let min = start.to_string_value();
                    let max = end.to_string_value();
                    let min_ok = if excl_start { v > min } else { v >= min };
                    let max_ok = if excl_end { v < max } else { v <= max };
                    min_ok && max_ok
                } else if matches!(
                    (start.view(), end.view()),
                    (
                        ValueView::Int(_) | ValueView::BigInt(_),
                        ValueView::Int(_) | ValueView::BigInt(_)
                    )
                ) {
                    let Some(v) = Self::integral_bigint(needle) else {
                        return false;
                    };
                    let min = start.to_bigint();
                    let max = end.to_bigint();
                    let min_ok = if excl_start { v > min } else { v >= min };
                    let max_ok = if excl_end { v < max } else { v <= max };
                    min_ok && max_ok
                } else {
                    let v = needle.to_f64();
                    let min = start.to_f64();
                    let max = end.to_f64();
                    let min_ok = if excl_start { v > min } else { v >= min };
                    let max_ok = if excl_end { v < max } else { v <= max };
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
        if let Some(info) = self.container_type_metadata(whole)
            && let Some(key_type) = info.key_type
        {
            if key_type != "Any" && key_type != "Mu" && !self.type_matches_value(&key_type, needle)
            {
                return false;
            }
            // An object hash stores `.WHICH` keys: membership is by key
            // object identity, so `13 ∈ %objh` finds the Int key while
            // `"13"` does not.
            let which = crate::runtime::utils::value_which_key(needle);
            return hash.get(&which).is_some_and(|v| v.truthy());
        }
        if !matches!(needle.view(), ValueView::Str(_)) {
            // Plain Hash keys are Str by default.
            return false;
        }
        // A Hash coerces to a Set of the keys whose values are truthy (that is
        // how `.Set` and set membership treat it), so a key mapped to a falsy
        // value (`0`, `Nil`, `""`) is NOT a member: `%(:a, b => 0) ∋ "b"` is
        // False, matching `enum Foo «a b»; Foo.enums ∋ "a"` (a => 0).
        hash.get(&needle.to_string_value())
            .is_some_and(|v| v.truthy())
    }

    fn set_contains(&mut self, container: &Value, needle: &Value) -> bool {
        // Set/Bag/Mix stores are `.WHICH`-keyed: membership is element
        // identity (`===`), so `<1> ∈ (1,).Set` is False (IntStr vs Int)
        // and `"1" ∈ (1,).Set` is False (Str vs Int) — matching Rakudo.
        let (key, _) = crate::runtime::utils::quanthash_elem_entry(needle);
        match container.view() {
            ValueView::Set(s, _) => s.contains(&key),
            ValueView::Bag(b, _) => b.get(&key).is_some_and(num_traits::Signed::is_positive),
            ValueView::Mix(m, _) => m.get(&key).is_some_and(|weight| *weight != 0.0),
            ValueView::Hash(h) => self.hash_contains(&h, needle, container),
            _ if container.as_list_items().is_some() => container
                .as_list_items()
                .unwrap()
                .iter()
                // `∈`/`(elem)` coerces its RHS to a Set and tests membership by
                // `===` (object identity / `.WHICH`), NOT by stringification.
                // So `42 ∈ < 42 >` is False (Int vs the IntStr allomorph) and
                // `"1" ∈ (1,)` is False (Str vs Int) — matching Rakudo.
                .any(|item| crate::runtime::utils::values_identical(item, needle)),
            _ if container.is_range() => Self::range_contains(container, needle),
            _ => false,
        }
    }

    fn quant_hash_weights(value: &Value) -> HashMap<String, f64> {
        let coerced = runtime::coerce_value_to_quanthash(value);
        match coerced.view() {
            ValueView::Set(items, _) => items.iter().map(|k| (k.clone(), 1.0)).collect(),
            ValueView::Bag(items, _) => items
                .iter()
                .map(|(k, v)| (k.clone(), crate::runtime::utils::bigint_to_f64_sat(v)))
                .collect(),
            ValueView::Mix(items, _) => items.iter().map(|(k, v)| (k.clone(), *v)).collect(),
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
        match value.view() {
            ValueView::Num(n) => n.is_infinite(),
            ValueView::Rat(_, d) | ValueView::FatRat(_, d) => d == 0,
            ValueView::Mixin(inner, _) => Self::is_infinite_bound(inner),
            _ => false,
        }
    }

    fn is_lazy_union_input(value: &Value) -> bool {
        match value.view() {
            ValueView::LazyList(_) => true,
            ValueView::GenericRange { start, end, .. } => {
                Self::is_infinite_bound(start) || Self::is_infinite_bound(end)
            }
            _ => false,
        }
    }

    pub(crate) fn union_insert_set_elem(
        elems: &mut HashSet<String>,
        originals: &mut HashMap<String, Value>,
        value: &Value,
    ) {
        use crate::runtime::utils::{
            extend_quanthash_originals, quanthash_elem_entry, quanthash_insert_set,
            record_quanthash_original, str_elem_key,
        };
        let pair_selected = |weight: &Value| weight.truthy() || weight.is_nil();
        match value.view() {
            ValueView::Set(items, _) => {
                extend_quanthash_originals(originals, &items.original_keys);
                elems.extend(items.iter().cloned());
            }
            ValueView::Bag(items, _) => {
                extend_quanthash_originals(originals, &items.original_keys);
                for (k, v) in items.iter() {
                    if num_traits::Signed::is_positive(v) {
                        elems.insert(k.clone());
                    }
                }
            }
            ValueView::Mix(items, _) => {
                extend_quanthash_originals(originals, &items.original_keys);
                for (k, v) in items.iter() {
                    if *v != 0.0 {
                        elems.insert(k.clone());
                    }
                }
            }
            ValueView::Hash(items) => {
                for (k, v) in items.iter() {
                    if v.truthy() || v.is_nil() {
                        let key = crate::runtime::utils::hash_elem_key(&items, k, originals);
                        elems.insert(key);
                    }
                }
            }
            _ if value.as_list_items().is_some() => {
                for item in value.as_list_items().unwrap().iter() {
                    Self::union_insert_set_elem(elems, originals, item);
                }
            }
            _ if value.is_range() => {
                for item in runtime::value_to_list(value) {
                    Self::union_insert_set_elem(elems, originals, &item);
                }
            }
            ValueView::Pair(key, weight) => {
                if pair_selected(weight) {
                    elems.insert(str_elem_key(key));
                }
            }
            ValueView::ValuePair(key, weight) => {
                if pair_selected(weight) {
                    quanthash_insert_set(elems, originals, key);
                }
            }
            _ => {
                // The Any type object is the uninitialized-scalar seed
                // (PLAN 8.5 step 3): like the old Nil seed (whose empty
                // stringification skipped insertion), it contributes no
                // element — `my $s; $s ∪= 0` unions from the empty set
                // (S04-phasers/enter-leave.t 32).
                if value.is_any_type_object() {
                    return;
                }
                let (key, elem) = quanthash_elem_entry(value);
                if !elem.to_string_value().is_empty() {
                    record_quanthash_original(originals, &key, &elem);
                    elems.insert(key);
                }
            }
        }
    }

    fn value_to_set_keys(
        value: &Value,
        originals: &mut HashMap<String, Value>,
    ) -> Result<HashSet<String>, RuntimeError> {
        use crate::runtime::utils::{
            extend_quanthash_originals, quanthash_elem_entry, record_quanthash_original,
        };
        if Self::is_lazy_union_input(value) {
            return Err(Self::lazy_list_error());
        }
        match value.view() {
            ValueView::Set(s, _) => {
                extend_quanthash_originals(originals, &s.original_keys);
                Ok(s.elements.clone())
            }
            ValueView::Bag(b, _) => {
                extend_quanthash_originals(originals, &b.original_keys);
                Ok(b.keys().cloned().collect())
            }
            ValueView::Mix(m, _) => {
                extend_quanthash_originals(originals, &m.original_keys);
                Ok(m.keys().cloned().collect())
            }
            ValueView::Hash(h) => Ok(h
                .iter()
                .filter_map(|(k, v)| {
                    if v.truthy() || v.is_nil() {
                        Some(crate::runtime::utils::hash_elem_key(&h, k, originals))
                    } else {
                        None
                    }
                })
                .collect()),
            _ if value.as_list_items().is_some() => {
                let mut elems = HashSet::new();
                for item in value.as_list_items().unwrap().iter() {
                    Self::union_insert_set_elem(&mut elems, originals, item);
                }
                Ok(elems)
            }
            _ if value.is_range() => {
                let mut elems = HashSet::new();
                for item in runtime::value_to_list(value) {
                    Self::union_insert_set_elem(&mut elems, originals, &item);
                }
                Ok(elems)
            }
            ValueView::Pair(_, _) | ValueView::ValuePair(_, _) => {
                let mut elems = HashSet::new();
                Self::union_insert_set_elem(&mut elems, originals, value);
                Ok(elems)
            }
            _ => {
                let mut elems = HashSet::new();
                // Any type object = uninitialized-scalar seed → empty set,
                // like the old Nil seed (see union_insert_set_elem).
                if !value.is_any_type_object() {
                    let (key, elem) = quanthash_elem_entry(value);
                    if !elem.to_string_value().is_empty() {
                        record_quanthash_original(originals, &key, &elem);
                        elems.insert(key);
                    }
                }
                Ok(elems)
            }
        }
    }

    fn value_to_bag_counts(
        value: &Value,
        originals: &mut HashMap<String, Value>,
    ) -> Result<HashMap<String, i64>, RuntimeError> {
        use crate::runtime::utils::extend_quanthash_originals;
        if Self::is_lazy_union_input(value) {
            return Err(Self::lazy_list_error());
        }
        match value.view() {
            ValueView::Bag(b, _) => {
                extend_quanthash_originals(originals, &b.original_keys);
                Ok(crate::runtime::utils::bag_counts_as_i64(&b.counts))
            }
            ValueView::Mix(m, _) => {
                extend_quanthash_originals(originals, &m.original_keys);
                Ok(m.iter()
                    .filter_map(|(k, w)| {
                        if *w != 0.0 {
                            Some((k.clone(), 1))
                        } else {
                            None
                        }
                    })
                    .collect())
            }
            _ => {
                let set = Self::value_to_set_keys(value, originals)?;
                Ok(set.into_iter().map(|k| (k, 1)).collect())
            }
        }
    }

    fn value_to_mix_weights(
        value: &Value,
        originals: &mut HashMap<String, Value>,
    ) -> Result<HashMap<String, f64>, RuntimeError> {
        use crate::runtime::utils::extend_quanthash_originals;
        if Self::is_lazy_union_input(value) {
            return Err(Self::lazy_list_error());
        }
        match value.view() {
            ValueView::Mix(m, _) => {
                extend_quanthash_originals(originals, &m.original_keys);
                Ok(m.weights.clone())
            }
            ValueView::Bag(b, _) => {
                extend_quanthash_originals(originals, &b.original_keys);
                Ok(b.iter()
                    .map(|(k, v)| (k.clone(), crate::runtime::utils::bigint_to_f64_sat(v)))
                    .collect())
            }
            _ => {
                let set = Self::value_to_set_keys(value, originals)?;
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
        self.stack.push(Value::truth(result));
        Ok(())
    }

    pub(super) fn exec_set_cont_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        if Self::is_failure_value(&left) || Self::is_failure_value(&right) {
            return Err(RuntimeError::new("Exception"));
        }
        let result = self.set_contains(&left, &right);
        self.stack.push(Value::truth(result));
        Ok(())
    }

    pub(super) fn exec_set_union_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        if Self::is_failure_value(&left) || Self::is_failure_value(&right) {
            return Err(RuntimeError::new("Exception"));
        }
        let result_mutable = runtime::set_result_mutability(&left);

        let mut originals: HashMap<String, Value> = HashMap::new();
        let result = match (left.view(), right.view()) {
            (ValueView::Mix(a, _), ValueView::Mix(b, _)) => {
                crate::runtime::utils::extend_quanthash_originals(&mut originals, &a.original_keys);
                crate::runtime::utils::extend_quanthash_originals(&mut originals, &b.original_keys);
                let mut result = a.weights.clone();
                for (k, v) in b.iter() {
                    // For union: if key exists in both, take max; if only in one side, take that value
                    if let Some(e) = result.get_mut(k) {
                        *e = e.max(*v);
                    } else {
                        result.insert(k.clone(), *v);
                    }
                }
                Value::mix_with_original_keys(result, originals)
            }
            (ValueView::Bag(a, _), ValueView::Bag(b, _)) => {
                crate::runtime::utils::extend_quanthash_originals(&mut originals, &a.original_keys);
                crate::runtime::utils::extend_quanthash_originals(&mut originals, &b.original_keys);
                let mut result = a.counts.clone();
                for (k, v) in b.iter() {
                    let e = result.entry(k.clone()).or_default();
                    if *v > *e {
                        *e = v.clone();
                    }
                }
                Value::bag_typed_big(result, originals)
            }
            (ValueView::Set(a, _), ValueView::Set(b, _)) => {
                crate::runtime::utils::extend_quanthash_originals(&mut originals, &a.original_keys);
                crate::runtime::utils::extend_quanthash_originals(&mut originals, &b.original_keys);
                let mut result = a.elements.clone();
                for elem in b.iter() {
                    result.insert(elem.clone());
                }
                Value::set_typed(result, originals)
            }
            (_, _)
                if matches!(left.view(), ValueView::Mix(_, _))
                    || matches!(right.view(), ValueView::Mix(_, _)) =>
            {
                let mut left_mix = Self::value_to_mix_weights(&left, &mut originals)?;
                let right_mix = Self::value_to_mix_weights(&right, &mut originals)?;
                for (k, v) in right_mix {
                    if let Some(e) = left_mix.get_mut(&k) {
                        *e = e.max(v);
                    } else {
                        left_mix.insert(k, v);
                    }
                }
                Value::mix_with_original_keys(left_mix, originals)
            }
            (_, _)
                if matches!(left.view(), ValueView::Bag(_, _))
                    || matches!(right.view(), ValueView::Bag(_, _)) =>
            {
                let mut left_bag = Self::value_to_bag_counts(&left, &mut originals)?;
                let right_bag = Self::value_to_bag_counts(&right, &mut originals)?;
                for (k, v) in right_bag {
                    let e = left_bag.entry(k).or_insert(0);
                    *e = (*e).max(v);
                }
                Value::bag_typed(left_bag, originals)
            }
            (_, _) => {
                let mut left_set = Self::value_to_set_keys(&left, &mut originals)?;
                let right_set = Self::value_to_set_keys(&right, &mut originals)?;
                for elem in right_set {
                    left_set.insert(elem);
                }
                Value::set_typed(left_set, originals)
            }
        };
        self.stack
            .push(runtime::with_set_mutability(result, result_mutable));
        Ok(())
    }
}
