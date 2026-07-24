use super::*;

impl Interpreter {
    fn union_is_infinite_bound(value: &Value) -> bool {
        match value.view() {
            ValueView::Num(n) => n.is_infinite(),
            ValueView::Rat(_, d) | ValueView::FatRat(_, d) => d == 0,
            ValueView::Mixin(inner, _) => Self::union_is_infinite_bound(inner),
            _ => false,
        }
    }

    fn union_is_lazy_input(value: &Value) -> bool {
        match value.view() {
            ValueView::LazyList(_) => true,
            ValueView::GenericRange { start, end, .. } => {
                Self::union_is_infinite_bound(start) || Self::union_is_infinite_bound(end)
            }
            _ => false,
        }
    }

    fn union_set_keys(
        value: &Value,
        originals: &mut std::collections::HashMap<String, Value>,
    ) -> Result<std::collections::HashSet<String>, RuntimeError> {
        use crate::runtime::utils::{
            extend_quanthash_originals, quanthash_elem_entry, record_quanthash_original,
        };
        if Self::union_is_lazy_input(value) {
            return Err(RuntimeError::new("X::Cannot::Lazy"));
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
                let mut elems = std::collections::HashSet::new();
                for item in value.as_list_items().unwrap().iter() {
                    Self::union_insert_set_elem(&mut elems, originals, item);
                }
                Ok(elems)
            }
            _ if value.is_range() => {
                let mut elems = std::collections::HashSet::new();
                for item in Self::value_to_list(value) {
                    Self::union_insert_set_elem(&mut elems, originals, &item);
                }
                Ok(elems)
            }
            ValueView::Pair(_, _) | ValueView::ValuePair(_, _) => {
                let mut elems = std::collections::HashSet::new();
                Self::union_insert_set_elem(&mut elems, originals, value);
                Ok(elems)
            }
            _ => {
                let mut elems = std::collections::HashSet::new();
                let (key, elem) = quanthash_elem_entry(value);
                if !elem.to_string_value().is_empty() {
                    record_quanthash_original(originals, &key, &elem);
                    elems.insert(key);
                }
                Ok(elems)
            }
        }
    }

    fn union_bag_counts(
        value: &Value,
        originals: &mut std::collections::HashMap<String, Value>,
    ) -> Result<std::collections::HashMap<String, i64>, RuntimeError> {
        use crate::runtime::utils::extend_quanthash_originals;
        if Self::union_is_lazy_input(value) {
            return Err(RuntimeError::new("X::Cannot::Lazy"));
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
                let set = Self::union_set_keys(value, originals)?;
                Ok(set.into_iter().map(|k| (k, 1)).collect())
            }
        }
    }

    fn union_mix_weights(
        value: &Value,
        originals: &mut std::collections::HashMap<String, Value>,
    ) -> Result<std::collections::HashMap<String, f64>, RuntimeError> {
        use crate::runtime::utils::extend_quanthash_originals;
        if Self::union_is_lazy_input(value) {
            return Err(RuntimeError::new("X::Cannot::Lazy"));
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
                let set = Self::union_set_keys(value, originals)?;
                Ok(set.into_iter().map(|k| (k, 1.0)).collect())
            }
        }
    }

    pub(crate) fn apply_set_union(left: &Value, right: &Value) -> Result<Value, RuntimeError> {
        if matches!(left.view(), ValueView::Instance { class_name, .. } if class_name == "Failure")
            || matches!(right.view(), ValueView::Instance { class_name, .. } if class_name == "Failure")
        {
            return Err(RuntimeError::new("Exception"));
        }
        let mut originals = std::collections::HashMap::new();
        if matches!(left.view(), ValueView::Mix(_, _))
            || matches!(right.view(), ValueView::Mix(_, _))
        {
            let mut l = Self::union_mix_weights(left, &mut originals)?;
            let r = Self::union_mix_weights(right, &mut originals)?;
            for (k, v) in r {
                let e = l.entry(k).or_insert(0.0);
                *e = e.max(v);
            }
            return Ok(Value::mix_with_original_keys(l, originals));
        }
        if matches!(left.view(), ValueView::Bag(_, _))
            || matches!(right.view(), ValueView::Bag(_, _))
        {
            let mut l = Self::union_bag_counts(left, &mut originals)?;
            let r = Self::union_bag_counts(right, &mut originals)?;
            for (k, v) in r {
                let e = l.entry(k).or_insert(0);
                *e = (*e).max(v);
            }
            return Ok(Value::bag_typed(l, originals));
        }
        let mut l = Self::union_set_keys(left, &mut originals)?;
        let r = Self::union_set_keys(right, &mut originals)?;
        l.extend(r);
        Ok(Value::set_typed(l, originals))
    }

    fn set_equal_bag_counts(
        value: &Value,
    ) -> Result<std::collections::HashMap<String, i64>, RuntimeError> {
        let mut scratch = std::collections::HashMap::new();
        let mut counts = Self::union_bag_counts(value, &mut scratch)?;
        counts.retain(|_, v| *v > 0);
        Ok(counts)
    }

    fn set_equal_mix_weights(
        value: &Value,
    ) -> Result<std::collections::HashMap<String, f64>, RuntimeError> {
        let mut scratch = std::collections::HashMap::new();
        let mut weights = Self::union_mix_weights(value, &mut scratch)?;
        weights.retain(|_, w| *w != 0.0);
        Ok(weights)
    }

    pub(crate) fn apply_set_equality(left: &Value, right: &Value) -> Result<bool, RuntimeError> {
        if matches!(left.view(), ValueView::Mix(_, _))
            || matches!(right.view(), ValueView::Mix(_, _))
        {
            return Ok(Self::set_equal_mix_weights(left)? == Self::set_equal_mix_weights(right)?);
        }
        if matches!(left.view(), ValueView::Bag(_, _))
            || matches!(right.view(), ValueView::Bag(_, _))
        {
            return Ok(Self::set_equal_bag_counts(left)? == Self::set_equal_bag_counts(right)?);
        }
        let mut scratch = std::collections::HashMap::new();
        Ok(Self::union_set_keys(left, &mut scratch)? == Self::union_set_keys(right, &mut scratch)?)
    }

    fn multiply_pair_i64(value: &Value) -> i64 {
        match value.view() {
            ValueView::Int(i) => i,
            ValueView::Num(n) => n as i64,
            ValueView::Rat(n, d) if d != 0 => n / d,
            ValueView::FatRat(n, d) if d != 0 => n / d,
            ValueView::Bool(b) => i64::from(b),
            _ => i64::from(value.truthy()),
        }
    }

    fn multiply_pair_f64(value: &Value) -> f64 {
        match value.view() {
            ValueView::Int(i) => i as f64,
            ValueView::Num(n) => n,
            ValueView::Rat(n, d) if d != 0 => n as f64 / d as f64,
            ValueView::FatRat(n, d) if d != 0 => n as f64 / d as f64,
            _ => 1.0,
        }
    }

    fn multiply_bag_counts(
        value: &Value,
        originals: &mut std::collections::HashMap<String, Value>,
    ) -> Result<std::collections::HashMap<String, (i64, bool)>, RuntimeError> {
        use crate::runtime::utils::{
            extend_quanthash_originals, quanthash_elem_entry, record_quanthash_original,
            str_elem_key,
        };
        let value = value.descalarize();
        if Self::union_is_lazy_input(value) {
            return Err(RuntimeError::new("X::Cannot::Lazy"));
        }
        match value.view() {
            ValueView::Bag(b, _) => {
                extend_quanthash_originals(originals, &b.original_keys);
                let resolved = crate::runtime::utils::resolve_bag_tab_keys(&b);
                Ok(resolved.into_iter().map(|(k, v)| (k, (v, false))).collect())
            }
            ValueView::Mix(m, _) => {
                extend_quanthash_originals(originals, &m.original_keys);
                Ok(m.iter()
                    .map(|(k, v)| (k.clone(), (*v as i64, false)))
                    .collect())
            }
            ValueView::Set(s, _) => {
                extend_quanthash_originals(originals, &s.original_keys);
                Ok(s.iter().map(|k| (k.clone(), (1, false))).collect())
            }
            ValueView::Hash(h) => Ok(h
                .iter()
                .filter_map(|(k, v)| {
                    let c = Self::multiply_pair_i64(v);
                    if c > 0 {
                        Some((
                            crate::runtime::utils::hash_elem_key(&h, k, originals),
                            (c, c != 1),
                        ))
                    } else {
                        None
                    }
                })
                .collect()),
            _ if value.as_list_items().is_some() => {
                let mut counts = std::collections::HashMap::new();
                for item in value.as_list_items().unwrap().iter() {
                    match item.view() {
                        ValueView::Pair(k, v) => {
                            let c = Self::multiply_pair_i64(v);
                            let entry = counts.entry(str_elem_key(k)).or_insert((0, false));
                            entry.0 += c;
                            entry.1 |= c != 1;
                        }
                        ValueView::ValuePair(k, v) => {
                            let (key, elem) = quanthash_elem_entry(k);
                            record_quanthash_original(originals, &key, &elem);
                            let c = Self::multiply_pair_i64(v);
                            let entry = counts.entry(key).or_insert((0, false));
                            entry.0 += c;
                            entry.1 |= c != 1;
                        }
                        _ => {
                            let (key, elem) = quanthash_elem_entry(item);
                            record_quanthash_original(originals, &key, &elem);
                            let entry = counts.entry(key).or_insert((0, false));
                            entry.0 += 1;
                        }
                    }
                }
                Ok(counts)
            }
            _ if value.is_range() => {
                let mut counts = std::collections::HashMap::new();
                for item in Self::value_to_list(value) {
                    let (key, elem) = quanthash_elem_entry(&item);
                    record_quanthash_original(originals, &key, &elem);
                    let entry = counts.entry(key).or_insert((0, false));
                    entry.0 += 1;
                }
                Ok(counts)
            }
            ValueView::Pair(k, v) => {
                let mut counts = std::collections::HashMap::new();
                let c = Self::multiply_pair_i64(v);
                if c > 0 {
                    counts.insert(str_elem_key(k), (c, c != 1));
                }
                Ok(counts)
            }
            ValueView::ValuePair(k, v) => {
                let mut counts = std::collections::HashMap::new();
                let c = Self::multiply_pair_i64(v);
                if c > 0 {
                    let (key, elem) = quanthash_elem_entry(k);
                    record_quanthash_original(originals, &key, &elem);
                    counts.insert(key, (c, c != 1));
                }
                Ok(counts)
            }
            _ => {
                let mut counts = std::collections::HashMap::new();
                let (key, elem) = quanthash_elem_entry(value);
                record_quanthash_original(originals, &key, &elem);
                counts.insert(key, (1, false));
                Ok(counts)
            }
        }
    }

    fn multiply_mix_weights(
        value: &Value,
        originals: &mut std::collections::HashMap<String, Value>,
    ) -> Result<std::collections::HashMap<String, f64>, RuntimeError> {
        use crate::runtime::utils::{
            extend_quanthash_originals, quanthash_elem_entry, record_quanthash_original,
            str_elem_key,
        };
        let value = value.descalarize();
        if Self::union_is_lazy_input(value) {
            return Err(RuntimeError::new("X::Cannot::Lazy"));
        }
        match value.view() {
            ValueView::Mix(m, _) => {
                extend_quanthash_originals(originals, &m.original_keys);
                Ok(m.weights.clone())
            }
            ValueView::Bag(b, _) => {
                extend_quanthash_originals(originals, &b.original_keys);
                let resolved = crate::runtime::utils::resolve_bag_tab_keys(&b);
                Ok(resolved.into_iter().map(|(k, v)| (k, v as f64)).collect())
            }
            ValueView::Set(s, _) => {
                extend_quanthash_originals(originals, &s.original_keys);
                Ok(s.iter().map(|k| (k.clone(), 1.0)).collect())
            }
            ValueView::Hash(h) => Ok(h
                .iter()
                .filter_map(|(k, v)| {
                    let w = match v.view() {
                        ValueView::Int(i) => i as f64,
                        ValueView::Num(n) => n,
                        ValueView::Rat(n, d) if d != 0 => n as f64 / d as f64,
                        ValueView::FatRat(n, d) if d != 0 => n as f64 / d as f64,
                        ValueView::Bool(b) => {
                            if b {
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
                    if w != 0.0 {
                        Some((crate::runtime::utils::hash_elem_key(&h, k, originals), w))
                    } else {
                        None
                    }
                })
                .collect()),
            _ if value.as_list_items().is_some() => {
                let mut weights = std::collections::HashMap::new();
                for item in value.as_list_items().unwrap().iter() {
                    match item.view() {
                        ValueView::Pair(k, v) => {
                            *weights.entry(str_elem_key(k)).or_insert(0.0) +=
                                Self::multiply_pair_f64(v);
                        }
                        ValueView::ValuePair(k, v) => {
                            let (key, elem) = quanthash_elem_entry(k);
                            record_quanthash_original(originals, &key, &elem);
                            *weights.entry(key).or_insert(0.0) += Self::multiply_pair_f64(v);
                        }
                        _ => {
                            let (key, elem) = quanthash_elem_entry(item);
                            record_quanthash_original(originals, &key, &elem);
                            *weights.entry(key).or_insert(0.0) += 1.0;
                        }
                    }
                }
                Ok(weights)
            }
            _ if value.is_range() => {
                let mut weights = std::collections::HashMap::new();
                for item in Self::value_to_list(value) {
                    let (key, elem) = quanthash_elem_entry(&item);
                    record_quanthash_original(originals, &key, &elem);
                    *weights.entry(key).or_insert(0.0) += 1.0;
                }
                Ok(weights)
            }
            ValueView::Pair(k, v) => {
                let mut weights = std::collections::HashMap::new();
                let w = Self::multiply_pair_f64(v);
                if w != 0.0 {
                    weights.insert(str_elem_key(k), w);
                }
                Ok(weights)
            }
            ValueView::ValuePair(k, v) => {
                let mut weights = std::collections::HashMap::new();
                let w = Self::multiply_pair_f64(v);
                if w != 0.0 {
                    let (key, elem) = quanthash_elem_entry(k);
                    record_quanthash_original(originals, &key, &elem);
                    weights.insert(key, w);
                }
                Ok(weights)
            }
            _ => {
                let mut weights = std::collections::HashMap::new();
                let (key, elem) = quanthash_elem_entry(value);
                record_quanthash_original(originals, &key, &elem);
                weights.insert(key, 1.0);
                Ok(weights)
            }
        }
    }

    pub(crate) fn apply_set_multiply(left: &Value, right: &Value) -> Result<Value, RuntimeError> {
        let left = match left.view() {
            ValueView::Scalar(inner) => inner,
            _ => left,
        };
        let right = match right.view() {
            ValueView::Scalar(inner) => inner,
            _ => right,
        };
        if matches!(left.view(), ValueView::Instance { class_name, .. } if class_name == "Failure")
            || matches!(right.view(), ValueView::Instance { class_name, .. } if class_name == "Failure")
        {
            return Err(RuntimeError::new("Exception"));
        }
        let mut originals = std::collections::HashMap::new();
        if matches!(left.view(), ValueView::Mix(_, _))
            || matches!(right.view(), ValueView::Mix(_, _))
        {
            let l = Self::multiply_mix_weights(left, &mut originals)?;
            let r = Self::multiply_mix_weights(right, &mut originals)?;
            let mut result = std::collections::HashMap::new();
            for (k, lv) in l {
                if let Some(rv) = r.get(&k) {
                    let product = lv * rv;
                    if product != 0.0 {
                        result.insert(k, product);
                    }
                }
            }
            return Ok(Value::mix_with_original_keys(result, originals));
        }
        let l = Self::multiply_bag_counts(left, &mut originals)?;
        let r = Self::multiply_bag_counts(right, &mut originals)?;
        let mut result: std::collections::HashMap<String, i64> = std::collections::HashMap::new();
        for (k, (lv, _l_explicit)) in l {
            if let Some((rv, _r_explicit)) = r.get(&k) {
                let product = lv * *rv;
                if product > 0 {
                    result.insert(k, product);
                }
            }
        }
        Ok(Value::bag_typed(result, originals))
    }

    /// Coerce a value to bag-like count map for the (+) operator.
    /// Unlike union_bag_counts, this properly handles Hash and Pair values.
    fn addition_bag_counts(
        value: &Value,
        originals: &mut std::collections::HashMap<String, Value>,
    ) -> Result<std::collections::HashMap<String, i64>, RuntimeError> {
        use crate::runtime::utils::{
            extend_quanthash_originals, quanthash_elem_entry, record_quanthash_original,
            str_elem_key,
        };
        if Self::union_is_lazy_input(value) {
            return Err(RuntimeError::new("X::Cannot::Lazy"));
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
                            Some((k.clone(), *w as i64))
                        } else {
                            None
                        }
                    })
                    .collect())
            }
            ValueView::Set(s, _) => {
                extend_quanthash_originals(originals, &s.original_keys);
                Ok(s.iter().map(|k| (k.clone(), 1)).collect())
            }
            ValueView::Hash(map) => {
                let mut result = std::collections::HashMap::new();
                for (k, v) in map.iter() {
                    let weight = v.to_f64() as i64;
                    if weight != 0 {
                        result.insert(
                            crate::runtime::utils::hash_elem_key(&map, k, originals),
                            weight,
                        );
                    }
                }
                Ok(result)
            }
            _ if value.as_list_items().is_some() => {
                let mut result = std::collections::HashMap::new();
                for item in value.as_list_items().unwrap().iter() {
                    match item.view() {
                        ValueView::Pair(k, v) => {
                            let weight = v.to_f64() as i64;
                            *result.entry(str_elem_key(k)).or_insert(0) += weight;
                        }
                        ValueView::ValuePair(k, v) => {
                            let weight = v.to_f64() as i64;
                            let (key, elem) = quanthash_elem_entry(k);
                            record_quanthash_original(originals, &key, &elem);
                            *result.entry(key).or_insert(0) += weight;
                        }
                        _ => {
                            let (key, elem) = quanthash_elem_entry(item);
                            if !elem.to_string_value().is_empty() {
                                record_quanthash_original(originals, &key, &elem);
                                *result.entry(key).or_insert(0) += 1;
                            }
                        }
                    }
                }
                result.retain(|_, v| *v != 0);
                Ok(result)
            }
            ValueView::Pair(k, v) => {
                let mut result = std::collections::HashMap::new();
                let weight = v.to_f64() as i64;
                if weight != 0 {
                    result.insert(str_elem_key(k), weight);
                }
                Ok(result)
            }
            _ => {
                let set = Self::union_set_keys(value, originals)?;
                Ok(set.into_iter().map(|k| (k, 1)).collect())
            }
        }
    }

    /// Coerce a value to mix-like weights for the (+) operator.
    fn addition_mix_weights(
        value: &Value,
        originals: &mut std::collections::HashMap<String, Value>,
    ) -> Result<std::collections::HashMap<String, f64>, RuntimeError> {
        use crate::runtime::utils::{
            extend_quanthash_originals, quanthash_elem_entry, record_quanthash_original,
            str_elem_key,
        };
        if Self::union_is_lazy_input(value) {
            return Err(RuntimeError::new("X::Cannot::Lazy"));
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
            ValueView::Set(s, _) => {
                extend_quanthash_originals(originals, &s.original_keys);
                Ok(s.iter().map(|k| (k.clone(), 1.0)).collect())
            }
            ValueView::Hash(map) => {
                let mut result = std::collections::HashMap::new();
                for (k, v) in map.iter() {
                    let weight = v.to_f64();
                    if weight != 0.0 {
                        result.insert(
                            crate::runtime::utils::hash_elem_key(&map, k, originals),
                            weight,
                        );
                    }
                }
                Ok(result)
            }
            _ if value.as_list_items().is_some() => {
                let mut result = std::collections::HashMap::new();
                for item in value.as_list_items().unwrap().iter() {
                    match item.view() {
                        ValueView::Pair(k, v) => {
                            let weight = v.to_f64();
                            *result.entry(str_elem_key(k)).or_insert(0.0) += weight;
                        }
                        ValueView::ValuePair(k, v) => {
                            let weight = v.to_f64();
                            let (key, elem) = quanthash_elem_entry(k);
                            record_quanthash_original(originals, &key, &elem);
                            *result.entry(key).or_insert(0.0) += weight;
                        }
                        _ => {
                            let (key, elem) = quanthash_elem_entry(item);
                            if !elem.to_string_value().is_empty() {
                                record_quanthash_original(originals, &key, &elem);
                                *result.entry(key).or_insert(0.0) += 1.0;
                            }
                        }
                    }
                }
                result.retain(|_, v| *v != 0.0);
                Ok(result)
            }
            ValueView::Pair(k, v) => {
                let mut result = std::collections::HashMap::new();
                let weight = v.to_f64();
                if weight != 0.0 {
                    result.insert(str_elem_key(k), weight);
                }
                Ok(result)
            }
            _ => {
                let set = Self::union_set_keys(value, originals)?;
                Ok(set.into_iter().map(|k| (k, 1.0)).collect())
            }
        }
    }

    pub(crate) fn apply_set_addition(left: &Value, right: &Value) -> Result<Value, RuntimeError> {
        let left = match left.view() {
            ValueView::Scalar(inner) => inner,
            _ => left,
        };
        let right = match right.view() {
            ValueView::Scalar(inner) => inner,
            _ => right,
        };
        if matches!(left.view(), ValueView::Instance { class_name, .. } if class_name == "Failure")
            || matches!(right.view(), ValueView::Instance { class_name, .. } if class_name == "Failure")
        {
            return Err(RuntimeError::new("Exception"));
        }
        // Determine type level: Mix > Bag > Set, minimum is Bag for (+)
        let type_level = |v: &Value| -> u8 {
            match v.view() {
                ValueView::Mix(_, _) => 2,
                ValueView::Bag(_, _) => 1,
                ValueView::Package(sym) => match sym.resolve().as_str() {
                    "Mix" | "MixHash" => 2,
                    "Bag" | "BagHash" => 1,
                    _ => 0,
                },
                _ => 0,
            }
        };
        let result_level = type_level(left).max(type_level(right)).max(1);

        let mut originals = std::collections::HashMap::new();
        if result_level >= 2 {
            let mut l = Self::addition_mix_weights(left, &mut originals)?;
            let r = Self::addition_mix_weights(right, &mut originals)?;
            for (k, v) in r {
                let e = l.entry(k).or_insert(0.0);
                *e += v;
            }
            return Ok(Value::mix_with_original_keys(l, originals));
        }
        let mut l = Self::addition_bag_counts(left, &mut originals)?;
        let r = Self::addition_bag_counts(right, &mut originals)?;
        for (k, v) in r {
            let e = l.entry(k).or_insert(0);
            *e += v;
        }
        Ok(Value::bag_typed(l, originals))
    }
}
