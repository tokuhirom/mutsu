use super::*;

impl Interpreter {
    /// Rich :exists adverb handler supporting negation, parameterized arg,
    /// zen slice, and secondary adverbs (:kv, :!kv, :p, :!p, :!v).
    pub(super) fn exec_exists_index_adv_op(
        &mut self,
        flags: u32,
        array_var_name: Option<String>,
    ) -> Result<(), crate::value::RuntimeError> {
        let negated_flag = flags & 1 != 0;
        let has_arg = flags & 2 != 0;
        let is_zen = flags & 4 != 0;
        let adverb_bits = (flags >> 4) & 0xF;

        // Pop arg if present (it's on top of stack)
        let arg_val = if has_arg {
            self.stack.pop().unwrap_or(Value::NIL)
        } else {
            Value::NIL
        };

        // Determine effective negation
        let effective_negated = if has_arg {
            !arg_val.truthy() // falsy arg => negate
        } else {
            negated_flag
        };

        // Check for invalid adverb combos — die at runtime
        match adverb_bits {
            6 | 7 => {
                // InvalidK, InvalidNotK
                return Err(crate::value::RuntimeError::new(
                    "Unsupported combination of :exists and :k adverbs".to_string(),
                ));
            }
            8 => {
                // InvalidV
                return Err(crate::value::RuntimeError::new(
                    "Unsupported combination of :exists and :v adverbs".to_string(),
                ));
            }
            _ => {}
        }

        let (target, indices) = if is_zen {
            let target = self.stack.pop().unwrap_or(Value::NIL);
            Self::throw_if_failure(&target)?;
            let len = match target.view() {
                ValueView::Array(items, ..) => items.len(),
                _ => 0,
            };
            let idxs: Vec<i64> = (0..len as i64).collect();
            (target, idxs)
        } else {
            let mut idx = self.stack.pop().unwrap_or(Value::NIL);
            // An *itemized* list subscript (`@a[$(7,8,9)]:exists`) is a SINGLE
            // index (its `.Int`, the element count), not a slice.
            match idx.view() {
                ValueView::Array(items, crate::value::ArrayKind::ItemList) => {
                    idx = Value::int(items.len() as i64);
                }
                ValueView::Scalar(inner)
                    if inner.is_range() || matches!(inner.view(), ValueView::Array(..)) =>
                {
                    idx = Value::int(crate::runtime::utils::value_to_list(inner).len() as i64);
                }
                _ => {}
            }
            let target = self.stack.pop().unwrap_or(Value::NIL);
            Self::throw_if_failure(&target)?;
            // A nested single-dim slice (`@a[(3, (30, (5,)))]:exists`) preserves
            // its index-tree shape in the result, so it recurses rather than
            // walking a flat pair list. Distinct from a multidim `@a[1;2]` walk.
            if let Some(inner) = Self::nested_index_elements(&idx)
                && inner
                    .iter()
                    .any(|e| Self::nested_index_elements(e).is_some())
                && let Some(items) = Self::positional_exists_items(&target)
            {
                let adverb = match adverb_bits {
                    1 => "kv",
                    2 => "not-kv",
                    3 => "p",
                    4 => "not-p",
                    _ => "none",
                };
                let out = Self::nested_exists_slice(&items, &inner, effective_negated, adverb);
                self.stack.push(Value::array(out));
                return Ok(());
            }
            if let Some(map) = match target.view() {
                ValueView::Hash(map) => Some(map.clone()),
                // A Pair answers `<key>:exists` for its own key: treat it as a
                // single-entry map (`(a => 5)<a>:exists` is True, `<b>` False).
                ValueView::Pair(key, value) => {
                    let mut m = std::collections::HashMap::new();
                    m.insert((*key).clone(), (*value).clone());
                    match Value::hash(m).view() {
                        ValueView::Hash(map) => Some(map.clone()),
                        _ => None,
                    }
                }
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Stash" => {
                    match attributes.as_map().get("symbols").map(Value::view) {
                        Some(ValueView::Hash(map)) => Some(map.clone()),
                        _ => None,
                    }
                }
                _ => None,
            } {
                if adverb_bits == 5 {
                    return Err(crate::value::RuntimeError::new(
                        "Unsupported combination of :exists and :v adverbs".to_string(),
                    ));
                }
                match idx.view() {
                    ValueView::Array(items, ..) => {
                        // Multi-dimensional hash path (%h{a;b;c}:exists): if we can
                        // traverse through nested hashes, treat this as a single exists.
                        if items.len() > 1 {
                            // `cur` is owned: a `&Value` obtained through a view
                            // guard could not outlive the guard's borrow.
                            let mut cur: Value = target.clone();
                            let mut traversed_nested = false;
                            let mut path_exists: Option<bool> = None;
                            for (i, key) in items.iter().enumerate() {
                                let next = if let ValueView::Hash(cur_map) = cur.view() {
                                    let key_s = key.to_string_value();
                                    cur_map.get(&key_s).cloned()
                                } else if traversed_nested {
                                    path_exists = Some(false);
                                    break;
                                } else {
                                    break;
                                };
                                if let Some(next) = next {
                                    if i + 1 == items.len() {
                                        path_exists = Some(!matches!(next.view(), ValueView::Nil));
                                    } else if matches!(next.view(), ValueView::Hash(_)) {
                                        traversed_nested = true;
                                        cur = next;
                                    } else if traversed_nested {
                                        path_exists = Some(false);
                                        break;
                                    } else {
                                        break;
                                    }
                                } else {
                                    if traversed_nested {
                                        path_exists = Some(false);
                                    }
                                    break;
                                }
                            }
                            if traversed_nested {
                                let exists = path_exists.unwrap_or(false);
                                self.stack.push(Value::truth(exists ^ effective_negated));
                                return Ok(());
                            }
                        }
                        let is_obj_hash = self.is_object_hash(&target);
                        let pairs: Vec<(Value, bool)> = items
                            .iter()
                            .map(|k| {
                                let key = if is_obj_hash {
                                    let which = crate::runtime::utils::value_which_key(k);
                                    if map.contains_key(&which) {
                                        which
                                    } else {
                                        Value::hash_key_encode(k)
                                    }
                                } else {
                                    k.to_string_value()
                                };
                                (k.clone(), map.contains_key(&key))
                            })
                            .collect();
                        let result = match adverb_bits {
                            0 => Value::array(
                                pairs
                                    .iter()
                                    .map(|(_, exists)| Value::truth(*exists ^ effective_negated))
                                    .collect(),
                            ),
                            1 => {
                                let mut vals = Vec::new();
                                for (key, exists) in &pairs {
                                    if *exists {
                                        vals.push(key.clone());
                                        vals.push(Value::truth(*exists ^ effective_negated));
                                    }
                                }
                                Value::array(vals)
                            }
                            2 => {
                                let mut vals = Vec::new();
                                for (key, exists) in &pairs {
                                    vals.push(key.clone());
                                    vals.push(Value::truth(*exists ^ effective_negated));
                                }
                                Value::array(vals)
                            }
                            3 => {
                                let mut vals = Vec::new();
                                for (key, exists) in &pairs {
                                    if *exists {
                                        vals.push(Value::value_pair(
                                            key.clone(),
                                            Value::truth(*exists ^ effective_negated),
                                        ));
                                    }
                                }
                                Value::array(vals)
                            }
                            4 => {
                                let mut vals = Vec::new();
                                for (key, exists) in &pairs {
                                    vals.push(Value::value_pair(
                                        key.clone(),
                                        Value::truth(*exists ^ effective_negated),
                                    ));
                                }
                                Value::array(vals)
                            }
                            5 => Value::array(
                                pairs
                                    .iter()
                                    .map(|(_, exists)| Value::truth(*exists ^ effective_negated))
                                    .collect(),
                            ),
                            _ => Value::NIL,
                        };
                        self.stack.push(result);
                        return Ok(());
                    }
                    // Whatever (*) — all hash keys.
                    ValueView::Whatever => {
                        let pairs: Vec<(Value, bool)> =
                            map.keys().map(|k| (Value::str(k.clone()), true)).collect();
                        let result = match adverb_bits {
                            0 => Value::array(
                                pairs
                                    .iter()
                                    .map(|(_, exists)| Value::truth(*exists ^ effective_negated))
                                    .collect(),
                            ),
                            1 => {
                                let mut vals = Vec::new();
                                for (key, exists) in &pairs {
                                    if *exists {
                                        vals.push(key.clone());
                                        vals.push(Value::truth(*exists ^ effective_negated));
                                    }
                                }
                                Value::array(vals)
                            }
                            2 => {
                                let mut vals = Vec::new();
                                for (key, exists) in &pairs {
                                    vals.push(key.clone());
                                    vals.push(Value::truth(*exists ^ effective_negated));
                                }
                                Value::array(vals)
                            }
                            3 => {
                                let mut vals = Vec::new();
                                for (key, exists) in &pairs {
                                    if *exists {
                                        vals.push(Value::value_pair(
                                            key.clone(),
                                            Value::truth(*exists ^ effective_negated),
                                        ));
                                    }
                                }
                                Value::array(vals)
                            }
                            4 => {
                                let mut vals = Vec::new();
                                for (key, exists) in &pairs {
                                    vals.push(Value::value_pair(
                                        key.clone(),
                                        Value::truth(*exists ^ effective_negated),
                                    ));
                                }
                                Value::array(vals)
                            }
                            5 => Value::array(
                                pairs
                                    .iter()
                                    .map(|(_, exists)| Value::truth(*exists ^ effective_negated))
                                    .collect(),
                            ),
                            _ => Value::NIL,
                        };
                        self.stack.push(result);
                        return Ok(());
                    }
                    ValueView::Num(f) if f.is_infinite() && f.is_sign_positive() => {
                        let pairs: Vec<(Value, bool)> =
                            map.keys().map(|k| (Value::str(k.clone()), true)).collect();
                        let result = match adverb_bits {
                            0 => Value::array(
                                pairs
                                    .iter()
                                    .map(|(_, exists)| Value::truth(*exists ^ effective_negated))
                                    .collect(),
                            ),
                            1 => {
                                let mut vals = Vec::new();
                                for (key, exists) in &pairs {
                                    if *exists {
                                        vals.push(key.clone());
                                        vals.push(Value::truth(*exists ^ effective_negated));
                                    }
                                }
                                Value::array(vals)
                            }
                            2 => {
                                let mut vals = Vec::new();
                                for (key, exists) in &pairs {
                                    vals.push(key.clone());
                                    vals.push(Value::truth(*exists ^ effective_negated));
                                }
                                Value::array(vals)
                            }
                            3 => {
                                let mut vals = Vec::new();
                                for (key, exists) in &pairs {
                                    if *exists {
                                        vals.push(Value::value_pair(
                                            key.clone(),
                                            Value::truth(*exists ^ effective_negated),
                                        ));
                                    }
                                }
                                Value::array(vals)
                            }
                            4 => {
                                let mut vals = Vec::new();
                                for (key, exists) in &pairs {
                                    vals.push(Value::value_pair(
                                        key.clone(),
                                        Value::truth(*exists ^ effective_negated),
                                    ));
                                }
                                Value::array(vals)
                            }
                            5 => Value::array(
                                pairs
                                    .iter()
                                    .map(|(_, exists)| Value::truth(*exists ^ effective_negated))
                                    .collect(),
                            ),
                            _ => Value::NIL,
                        };
                        self.stack.push(result);
                        return Ok(());
                    }
                    _ => {
                        // For object hashes, use WHICH for lookup, fallback to hash_key_encode
                        let lookup_key = if self.is_object_hash(&target) {
                            let which = crate::runtime::utils::value_which_key(&idx);
                            if map.contains_key(&which) {
                                which
                            } else {
                                Value::hash_key_encode(&idx)
                            }
                        } else if matches!(idx.view(), ValueView::Package(_)) {
                            // A bare type object key coerces to "" (warning /
                            // user .Str), matching the store/read paths.
                            self.coerce_type_object_hash_key(&idx)?
                        } else {
                            idx.to_string_value()
                        };
                        let exists = map.contains_key(&lookup_key);
                        let result_bool = exists ^ effective_negated;
                        let key = idx.clone();
                        let result = match adverb_bits {
                            0 | 5 => Value::truth(result_bool),
                            // :kv — filter by actual existence, not negated result
                            1 => {
                                if exists {
                                    Value::array(vec![key, Value::truth(result_bool)])
                                } else {
                                    Value::array(Vec::new())
                                }
                            }
                            2 => Value::array(vec![key, Value::truth(result_bool)]),
                            // :p — filter by actual existence, not negated result
                            3 => {
                                if exists {
                                    Value::value_pair(key, Value::truth(result_bool))
                                } else {
                                    Value::array(Vec::new())
                                }
                            }
                            4 => Value::value_pair(key, Value::truth(result_bool)),
                            _ => Value::truth(result_bool),
                        };
                        self.stack.push(result);
                        return Ok(());
                    }
                }
            }
            if let ValueView::Set(set, _) = target.view() {
                let exists_for_key = |key: &Value| set.contains(&key.to_string_value());
                let result = match idx.view() {
                    ValueView::Array(items, ..) => Value::array(
                        items
                            .iter()
                            .map(|k| Value::truth(exists_for_key(k) ^ effective_negated))
                            .collect(),
                    ),
                    _ => Value::truth(exists_for_key(&idx) ^ effective_negated),
                };
                self.stack.push(result);
                return Ok(());
            }
            if let ValueView::Bag(bag, _) = target.view() {
                let exists_for_key = |key: &Value| bag.contains_key(&key.to_string_value());
                let result = match idx.view() {
                    ValueView::Array(items, ..) => Value::array(
                        items
                            .iter()
                            .map(|k| Value::truth(exists_for_key(k) ^ effective_negated))
                            .collect(),
                    ),
                    _ => Value::truth(exists_for_key(&idx) ^ effective_negated),
                };
                self.stack.push(result);
                return Ok(());
            }
            if let ValueView::Mix(mix, _) = target.view() {
                let exists_for_key = |key: &Value| mix.contains_key(&key.to_string_value());
                let result = match idx.view() {
                    ValueView::Array(items, ..) => Value::array(
                        items
                            .iter()
                            .map(|k| Value::truth(exists_for_key(k) ^ effective_negated))
                            .collect(),
                    ),
                    _ => Value::truth(exists_for_key(&idx) ^ effective_negated),
                };
                self.stack.push(result);
                return Ok(());
            }
            if let ValueView::Instance {
                class_name,
                attributes,
                ..
            } = target.view()
            {
                let is_stash = class_name == "Stash" && attributes.contains_key("symbols");
                if !is_stash
                    && let Some(result) = self.instance_exists_pos_result(
                        &target,
                        &idx,
                        effective_negated,
                        adverb_bits,
                    )?
                {
                    self.stack.push(result);
                    return Ok(());
                }
            }
            let idxs = match idx.view() {
                ValueView::Int(i) => vec![i],
                ValueView::Array(items, ..) if crate::runtime::utils::is_shaped_array(&target) => {
                    // Shaped array: multi-dimensional exists (e.g. @arr[0;0]:exists).
                    // An unassigned cell holds its unset seed — Nil or the Any
                    // type object — and does not exist yet.
                    let exists = Self::index_array_multidim(&target, items.as_ref(), false)
                        .ok()
                        .is_some_and(|v| !v.is_nil() && !v.is_any_type_object());
                    let result = Value::truth(exists ^ effective_negated);
                    self.stack.push(result);
                    return Ok(());
                }
                ValueView::Array(items, ..) => items
                    .iter()
                    .map(|v| match v.view() {
                        ValueView::Int(i) => i,
                        _ => {
                            let n = v.to_bigint();
                            n.try_into().unwrap_or(0)
                        }
                    })
                    .collect(),
                // Whatever (*) — treat as zen slice (all elements)
                ValueView::Whatever => {
                    let len = match target.view() {
                        ValueView::Array(items, ..) => items.len(),
                        _ => 0,
                    };
                    (0..len as i64).collect()
                }
                ValueView::Num(f) if f.is_infinite() && f.is_sign_positive() => {
                    let len = match target.view() {
                        ValueView::Array(items, ..) => items.len(),
                        _ => 0,
                    };
                    (0..len as i64).collect()
                }
                _ => {
                    // A bare type object key coerces to "" (warning / user .Str)
                    // before lookup, matching the store/read paths.
                    let pkg_key = if matches!(idx.view(), ValueView::Package(_)) {
                        Some(self.coerce_type_object_hash_key(&idx)?)
                    } else {
                        None
                    };
                    // For hash access, delegate to single key exists
                    let exists = match (target.view(), idx.view()) {
                        (ValueView::Hash(map), ValueView::Str(key)) => {
                            map.contains_key(key.as_str())
                        }
                        (ValueView::Hash(map), _) => map.contains_key(
                            &pkg_key.clone().unwrap_or_else(|| idx.to_string_value()),
                        ),
                        (ValueView::Set(set, _), ValueView::Str(key)) => set.contains(key.as_str()),
                        (ValueView::Set(set, _), _) => set.contains(&idx.to_string_value()),
                        (ValueView::Bag(bag, _), ValueView::Str(key)) => {
                            bag.contains_key(key.as_str())
                        }
                        (ValueView::Bag(bag, _), _) => bag.contains_key(&idx.to_string_value()),
                        (ValueView::Mix(mix, _), ValueView::Str(key)) => {
                            mix.contains_key(key.as_str())
                        }
                        (ValueView::Mix(mix, _), _) => mix.contains_key(&idx.to_string_value()),
                        (
                            ValueView::Instance {
                                class_name,
                                attributes,
                                ..
                            },
                            ValueView::Str(key),
                        ) if class_name == "Stash" => {
                            if let Some(ValueView::Hash(symbols)) =
                                attributes.as_map().get("symbols").map(Value::view)
                            {
                                symbols.contains_key(key.as_str())
                            } else {
                                false
                            }
                        }
                        (
                            ValueView::Instance {
                                class_name,
                                attributes,
                                ..
                            },
                            _,
                        ) if class_name == "Stash" => {
                            if let Some(ValueView::Hash(symbols)) =
                                attributes.as_map().get("symbols").map(Value::view)
                            {
                                symbols.contains_key(&idx.to_string_value())
                            } else {
                                false
                            }
                        }
                        _ => false,
                    };
                    let result = Value::truth(exists ^ effective_negated);
                    self.stack.push(result);
                    return Ok(());
                }
            };
            (target, idxs)
        };

        // A lazy `@`-array (infinite source) is conceptually unbounded: any
        // non-negative index exists, without forcing the list (raku). Only the
        // plain `:exists` form (no :kv/:p adverbs) is special-cased here. (L2)
        if let ValueView::LazyList(ll) = target.view()
            && ll.in_array_context()
            && ll.is_genuinely_lazy()
            && matches!(adverb_bits, 0 | 5)
            && !is_zen
        {
            let vals: Vec<Value> = indices
                .iter()
                .map(|&i| Value::truth((i >= 0) ^ effective_negated))
                .collect();
            self.stack.push(if vals.len() == 1 {
                vals.into_iter().next().unwrap()
            } else {
                Value::array(vals)
            });
            return Ok(());
        }

        // Uni: check exists based on codepoint count
        if let ValueView::Uni(u) = target.view() {
            let len = u.text.chars().count() as i64;
            if indices.len() == 1 && !is_zen {
                let i = indices[0];
                let exists = i >= 0 && i < len;
                let result = exists ^ effective_negated;
                self.stack.push(Value::truth(result));
                return Ok(());
            }
            let vals: Vec<Value> = indices
                .iter()
                .map(|&i| {
                    let exists = i >= 0 && i < len;
                    Value::truth(exists ^ effective_negated)
                })
                .collect();
            self.stack.push(Value::array(vals));
            return Ok(());
        }

        // Owned clone of the array backing (a view guard's borrow cannot
        // outlive the match), so the slice stays valid below.
        let arr_data: Option<(crate::gc::Gc<crate::value::ArrayData>, bool)> = match target.view() {
            ValueView::Array(items, kind) => {
                Some((items.clone(), kind == crate::value::ArrayKind::Shaped))
            }
            _ => None,
        };
        let (items, is_shaped, arr_initialized): (
            &[Value],
            bool,
            Option<&std::collections::HashSet<usize>>,
        ) = match &arr_data {
            Some((data, shaped)) => (data.as_slice(), *shaped, data.initialized.as_ref()),
            None => (&[] as &[Value], false, None),
        };
        // An in-range slot exists unless it is a `Nil` (deleted) or an
        // autovivification gap (`Package("Any")` not in the embedded
        // `initialized` set). Mirrors the `:k`/`:p` predicate in
        // builtins_multidim_subscript so `:exists` agrees with them.
        let slot_present_at = |i: i64| -> bool {
            if is_shaped {
                return i >= 0 && (i as usize) < items.len();
            }
            if i < 0 {
                return false;
            }
            match items.get(i as usize).map(Value::view) {
                None | Some(ValueView::Nil) => false,
                Some(ValueView::Package(name)) if name == "Any" => {
                    arr_initialized.is_none_or(|s| s.contains(&(i as usize)))
                }
                Some(_) => true,
            }
        };

        let is_multi = indices.len() != 1 || is_zen;

        if !is_multi {
            // Single index
            let i = indices[0];
            // Shaped arrays are fixed-size: any in-range index exists,
            // regardless of whether the slot holds the (default) Nil value.
            let slot_present = slot_present_at(i);
            let is_deleted = array_var_name
                .as_deref()
                .is_some_and(|n| self.is_deleted_index(n, i));
            let exists = slot_present && !is_deleted;
            let result_bool = exists ^ effective_negated;
            let key = Value::int(i);
            let result = match adverb_bits {
                0 | 5 => Value::truth(result_bool),
                // :kv — return (index, bool) if exists, else ()
                1 => {
                    if exists {
                        Value::array(vec![key, Value::truth(result_bool)])
                    } else {
                        Value::array(Vec::new())
                    }
                }
                // :!kv — always return (index, bool)
                2 => Value::array(vec![key, Value::truth(result_bool)]),
                // :p — return Pair if exists, else ()
                3 => {
                    if exists {
                        Value::value_pair(key, Value::truth(result_bool))
                    } else {
                        Value::array(Vec::new())
                    }
                }
                // :!p — always return Pair
                4 => Value::value_pair(key, Value::truth(result_bool)),
                _ => Value::truth(result_bool),
            };
            self.stack.push(result);
            return Ok(());
        }

        // Multi-index: compute (index, exists_bool) pairs
        let pairs: Vec<(i64, bool)> = indices
            .iter()
            .map(|&i| {
                let slot_present = slot_present_at(i);
                let is_deleted = array_var_name
                    .as_deref()
                    .is_some_and(|n| self.is_deleted_index(n, i));
                (i, slot_present && !is_deleted)
            })
            .collect();

        let result = match adverb_bits {
            0 => {
                // Plain :exists — list of Bools
                let vals: Vec<Value> = pairs
                    .iter()
                    .map(|(_, e)| Value::truth(*e ^ effective_negated))
                    .collect();
                Value::array(vals)
            }
            1 => {
                // :kv — filter by original exists, interleave (index, bool)
                let mut vals = Vec::new();
                for (i, exists) in &pairs {
                    if *exists {
                        vals.push(Value::int(*i));
                        vals.push(Value::truth(*exists ^ effective_negated));
                    }
                }
                Value::array(vals)
            }
            2 => {
                // :!kv — no filter, interleave all (index, bool)
                let mut vals = Vec::new();
                for (i, exists) in &pairs {
                    vals.push(Value::int(*i));
                    vals.push(Value::truth(*exists ^ effective_negated));
                }
                Value::array(vals)
            }
            3 => {
                // :p — filter by original exists, return Pairs
                let mut vals = Vec::new();
                for (i, exists) in &pairs {
                    if *exists {
                        vals.push(Value::value_pair(
                            Value::int(*i),
                            Value::truth(*exists ^ effective_negated),
                        ));
                    }
                }
                Value::array(vals)
            }
            4 => {
                // :!p — no filter, return all Pairs
                let mut vals = Vec::new();
                for (i, exists) in &pairs {
                    vals.push(Value::value_pair(
                        Value::int(*i),
                        Value::truth(*exists ^ effective_negated),
                    ));
                }
                Value::array(vals)
            }
            5 => {
                // :!v — no filter, return all Bools
                let vals: Vec<Value> = pairs
                    .iter()
                    .map(|(_, e)| Value::truth(*e ^ effective_negated))
                    .collect();
                Value::array(vals)
            }
            _ => Value::NIL,
        };

        self.stack.push(result);
        Ok(())
    }
}
