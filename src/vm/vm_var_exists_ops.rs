use super::*;

impl VM {
    /// Rich :exists adverb handler supporting negation, parameterized arg,
    /// zen slice, and secondary adverbs (:kv, :!kv, :p, :!p, :!v).
    pub(super) fn exec_exists_index_adv_op(
        &mut self,
        flags: u32,
    ) -> Result<(), crate::value::RuntimeError> {
        let negated_flag = flags & 1 != 0;
        let has_arg = flags & 2 != 0;
        let is_zen = flags & 4 != 0;
        let adverb_bits = (flags >> 4) & 0xF;

        // Pop arg if present (it's on top of stack)
        let arg_val = if has_arg {
            self.stack.pop().unwrap_or(Value::Nil)
        } else {
            Value::Nil
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
            let target = self.stack.pop().unwrap_or(Value::Nil);
            Self::throw_if_failure(&target)?;
            let len = match &target {
                Value::Array(items, ..) => items.len(),
                _ => 0,
            };
            let idxs: Vec<i64> = (0..len as i64).collect();
            (target, idxs)
        } else {
            let idx = self.stack.pop().unwrap_or(Value::Nil);
            let target = self.stack.pop().unwrap_or(Value::Nil);
            Self::throw_if_failure(&target)?;
            if let Some(map) = match &target {
                Value::Hash(map) => Some(map.clone()),
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Stash" => match attributes.get("symbols") {
                    Some(Value::Hash(map)) => Some(map.clone()),
                    _ => None,
                },
                _ => None,
            } {
                if adverb_bits == 5 {
                    return Err(crate::value::RuntimeError::new(
                        "Unsupported combination of :exists and :v adverbs".to_string(),
                    ));
                }
                match &idx {
                    Value::Array(items, ..) => {
                        // Multi-dimensional hash path (%h{a;b;c}:exists): if we can
                        // traverse through nested hashes, treat this as a single exists.
                        if items.len() > 1 {
                            let mut cur: &Value = &target;
                            let mut traversed_nested = false;
                            let mut path_exists: Option<bool> = None;
                            for (i, key) in items.iter().enumerate() {
                                if let Value::Hash(cur_map) = cur {
                                    let key_s = key.to_string_value();
                                    if let Some(next) = cur_map.get(&key_s) {
                                        if i + 1 == items.len() {
                                            path_exists = Some(!matches!(next, Value::Nil));
                                        } else if matches!(next, Value::Hash(_)) {
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
                                } else if traversed_nested {
                                    path_exists = Some(false);
                                    break;
                                } else {
                                    break;
                                }
                            }
                            if traversed_nested {
                                let exists = path_exists.unwrap_or(false);
                                self.stack.push(Value::Bool(exists ^ effective_negated));
                                return Ok(());
                            }
                        }
                        let pairs: Vec<(Value, bool)> = items
                            .iter()
                            .map(|k| {
                                let key = k.to_string_value();
                                (k.clone(), map.contains_key(&key))
                            })
                            .collect();
                        let result = match adverb_bits {
                            0 => Value::array(
                                pairs
                                    .iter()
                                    .map(|(_, exists)| Value::Bool(*exists ^ effective_negated))
                                    .collect(),
                            ),
                            1 => {
                                let mut vals = Vec::new();
                                for (key, exists) in &pairs {
                                    if *exists {
                                        vals.push(key.clone());
                                        vals.push(Value::Bool(*exists ^ effective_negated));
                                    }
                                }
                                Value::array(vals)
                            }
                            2 => {
                                let mut vals = Vec::new();
                                for (key, exists) in &pairs {
                                    vals.push(key.clone());
                                    vals.push(Value::Bool(*exists ^ effective_negated));
                                }
                                Value::array(vals)
                            }
                            3 => {
                                let mut vals = Vec::new();
                                for (key, exists) in &pairs {
                                    if *exists {
                                        vals.push(Value::ValuePair(
                                            Box::new(key.clone()),
                                            Box::new(Value::Bool(*exists ^ effective_negated)),
                                        ));
                                    }
                                }
                                Value::array(vals)
                            }
                            4 => {
                                let mut vals = Vec::new();
                                for (key, exists) in &pairs {
                                    vals.push(Value::ValuePair(
                                        Box::new(key.clone()),
                                        Box::new(Value::Bool(*exists ^ effective_negated)),
                                    ));
                                }
                                Value::array(vals)
                            }
                            5 => Value::array(
                                pairs
                                    .iter()
                                    .map(|(_, exists)| Value::Bool(*exists ^ effective_negated))
                                    .collect(),
                            ),
                            _ => Value::Nil,
                        };
                        self.stack.push(result);
                        return Ok(());
                    }
                    // Whatever (*) — all hash keys.
                    Value::Whatever => {
                        let pairs: Vec<(Value, bool)> =
                            map.keys().map(|k| (Value::str(k.clone()), true)).collect();
                        let result = match adverb_bits {
                            0 => Value::array(
                                pairs
                                    .iter()
                                    .map(|(_, exists)| Value::Bool(*exists ^ effective_negated))
                                    .collect(),
                            ),
                            1 => {
                                let mut vals = Vec::new();
                                for (key, exists) in &pairs {
                                    if *exists {
                                        vals.push(key.clone());
                                        vals.push(Value::Bool(*exists ^ effective_negated));
                                    }
                                }
                                Value::array(vals)
                            }
                            2 => {
                                let mut vals = Vec::new();
                                for (key, exists) in &pairs {
                                    vals.push(key.clone());
                                    vals.push(Value::Bool(*exists ^ effective_negated));
                                }
                                Value::array(vals)
                            }
                            3 => {
                                let mut vals = Vec::new();
                                for (key, exists) in &pairs {
                                    if *exists {
                                        vals.push(Value::ValuePair(
                                            Box::new(key.clone()),
                                            Box::new(Value::Bool(*exists ^ effective_negated)),
                                        ));
                                    }
                                }
                                Value::array(vals)
                            }
                            4 => {
                                let mut vals = Vec::new();
                                for (key, exists) in &pairs {
                                    vals.push(Value::ValuePair(
                                        Box::new(key.clone()),
                                        Box::new(Value::Bool(*exists ^ effective_negated)),
                                    ));
                                }
                                Value::array(vals)
                            }
                            5 => Value::array(
                                pairs
                                    .iter()
                                    .map(|(_, exists)| Value::Bool(*exists ^ effective_negated))
                                    .collect(),
                            ),
                            _ => Value::Nil,
                        };
                        self.stack.push(result);
                        return Ok(());
                    }
                    Value::Num(f) if f.is_infinite() && f.is_sign_positive() => {
                        let pairs: Vec<(Value, bool)> =
                            map.keys().map(|k| (Value::str(k.clone()), true)).collect();
                        let result = match adverb_bits {
                            0 => Value::array(
                                pairs
                                    .iter()
                                    .map(|(_, exists)| Value::Bool(*exists ^ effective_negated))
                                    .collect(),
                            ),
                            1 => {
                                let mut vals = Vec::new();
                                for (key, exists) in &pairs {
                                    if *exists {
                                        vals.push(key.clone());
                                        vals.push(Value::Bool(*exists ^ effective_negated));
                                    }
                                }
                                Value::array(vals)
                            }
                            2 => {
                                let mut vals = Vec::new();
                                for (key, exists) in &pairs {
                                    vals.push(key.clone());
                                    vals.push(Value::Bool(*exists ^ effective_negated));
                                }
                                Value::array(vals)
                            }
                            3 => {
                                let mut vals = Vec::new();
                                for (key, exists) in &pairs {
                                    if *exists {
                                        vals.push(Value::ValuePair(
                                            Box::new(key.clone()),
                                            Box::new(Value::Bool(*exists ^ effective_negated)),
                                        ));
                                    }
                                }
                                Value::array(vals)
                            }
                            4 => {
                                let mut vals = Vec::new();
                                for (key, exists) in &pairs {
                                    vals.push(Value::ValuePair(
                                        Box::new(key.clone()),
                                        Box::new(Value::Bool(*exists ^ effective_negated)),
                                    ));
                                }
                                Value::array(vals)
                            }
                            5 => Value::array(
                                pairs
                                    .iter()
                                    .map(|(_, exists)| Value::Bool(*exists ^ effective_negated))
                                    .collect(),
                            ),
                            _ => Value::Nil,
                        };
                        self.stack.push(result);
                        return Ok(());
                    }
                    _ => {
                        let exists = map.contains_key(&idx.to_string_value());
                        let result_bool = exists ^ effective_negated;
                        let key = idx.clone();
                        let result = match adverb_bits {
                            0 | 5 => Value::Bool(result_bool),
                            // :kv — filter by actual existence, not negated result
                            1 => {
                                if exists {
                                    Value::array(vec![key, Value::Bool(result_bool)])
                                } else {
                                    Value::array(Vec::new())
                                }
                            }
                            2 => Value::array(vec![key, Value::Bool(result_bool)]),
                            // :p — filter by actual existence, not negated result
                            3 => {
                                if exists {
                                    Value::ValuePair(
                                        Box::new(key),
                                        Box::new(Value::Bool(result_bool)),
                                    )
                                } else {
                                    Value::array(Vec::new())
                                }
                            }
                            4 => {
                                Value::ValuePair(Box::new(key), Box::new(Value::Bool(result_bool)))
                            }
                            _ => Value::Bool(result_bool),
                        };
                        self.stack.push(result);
                        return Ok(());
                    }
                }
            }
            if let Value::Set(set) = &target {
                let exists_for_key = |key: &Value| set.contains(&key.to_string_value());
                let result = match &idx {
                    Value::Array(items, ..) => Value::array(
                        items
                            .iter()
                            .map(|k| Value::Bool(exists_for_key(k) ^ effective_negated))
                            .collect(),
                    ),
                    _ => Value::Bool(exists_for_key(&idx) ^ effective_negated),
                };
                self.stack.push(result);
                return Ok(());
            }
            if let Value::Bag(bag) = &target {
                let exists_for_key = |key: &Value| bag.contains_key(&key.to_string_value());
                let result = match &idx {
                    Value::Array(items, ..) => Value::array(
                        items
                            .iter()
                            .map(|k| Value::Bool(exists_for_key(k) ^ effective_negated))
                            .collect(),
                    ),
                    _ => Value::Bool(exists_for_key(&idx) ^ effective_negated),
                };
                self.stack.push(result);
                return Ok(());
            }
            if let Value::Mix(mix) = &target {
                let exists_for_key = |key: &Value| mix.contains_key(&key.to_string_value());
                let result = match &idx {
                    Value::Array(items, ..) => Value::array(
                        items
                            .iter()
                            .map(|k| Value::Bool(exists_for_key(k) ^ effective_negated))
                            .collect(),
                    ),
                    _ => Value::Bool(exists_for_key(&idx) ^ effective_negated),
                };
                self.stack.push(result);
                return Ok(());
            }
            if let Value::Instance {
                class_name,
                attributes,
                ..
            } = &target
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
            let idxs = match &idx {
                Value::Int(i) => vec![*i],
                Value::Array(items, ..) if crate::runtime::utils::is_shaped_array(&target) => {
                    // Shaped array: multi-dimensional exists (e.g. @arr[0;0]:exists)
                    let exists = Self::index_array_multidim(&target, items.as_ref(), false)
                        .ok()
                        .is_some_and(|v| !matches!(v, Value::Nil));
                    let result = Value::Bool(exists ^ effective_negated);
                    self.stack.push(result);
                    return Ok(());
                }
                Value::Array(items, ..) => items
                    .iter()
                    .map(|v| match v {
                        Value::Int(i) => *i,
                        _ => {
                            let n = v.to_bigint();
                            n.try_into().unwrap_or(0)
                        }
                    })
                    .collect(),
                // Whatever (*) — treat as zen slice (all elements)
                Value::Whatever => {
                    let len = match &target {
                        Value::Array(items, ..) => items.len(),
                        _ => 0,
                    };
                    (0..len as i64).collect()
                }
                Value::Num(f) if f.is_infinite() && f.is_sign_positive() => {
                    let len = match &target {
                        Value::Array(items, ..) => items.len(),
                        _ => 0,
                    };
                    (0..len as i64).collect()
                }
                _ => {
                    // For hash access, delegate to single key exists
                    let exists = match (&target, &idx) {
                        (Value::Hash(map), Value::Str(key)) => map.contains_key(key.as_str()),
                        (Value::Hash(map), _) => map.contains_key(&idx.to_string_value()),
                        (Value::Set(set), Value::Str(key)) => set.contains(key.as_str()),
                        (Value::Set(set), other) => set.contains(&other.to_string_value()),
                        (Value::Bag(bag), Value::Str(key)) => bag.contains_key(key.as_str()),
                        (Value::Bag(bag), other) => bag.contains_key(&other.to_string_value()),
                        (Value::Mix(mix), Value::Str(key)) => mix.contains_key(key.as_str()),
                        (Value::Mix(mix), other) => mix.contains_key(&other.to_string_value()),
                        (
                            Value::Instance {
                                class_name,
                                attributes,
                                ..
                            },
                            Value::Str(key),
                        ) if class_name == "Stash" => {
                            if let Some(Value::Hash(symbols)) = attributes.get("symbols") {
                                symbols.contains_key(key.as_str())
                            } else {
                                false
                            }
                        }
                        (
                            Value::Instance {
                                class_name,
                                attributes,
                                ..
                            },
                            other,
                        ) if class_name == "Stash" => {
                            if let Some(Value::Hash(symbols)) = attributes.get("symbols") {
                                symbols.contains_key(&other.to_string_value())
                            } else {
                                false
                            }
                        }
                        _ => false,
                    };
                    let result = Value::Bool(exists ^ effective_negated);
                    self.stack.push(result);
                    return Ok(());
                }
            };
            (target, idxs)
        };

        // Uni: check exists based on codepoint count
        if let Value::Uni { text, .. } = &target {
            let len = text.chars().count() as i64;
            if indices.len() == 1 && !is_zen {
                let i = indices[0];
                let exists = i >= 0 && i < len;
                let result = exists ^ effective_negated;
                self.stack.push(Value::Bool(result));
                return Ok(());
            }
            let vals: Vec<Value> = indices
                .iter()
                .map(|&i| {
                    let exists = i >= 0 && i < len;
                    Value::Bool(exists ^ effective_negated)
                })
                .collect();
            self.stack.push(Value::array(vals));
            return Ok(());
        }

        let items = match &target {
            Value::Array(items, ..) => items.as_ref(),
            _ => &[] as &[Value],
        };

        let is_multi = indices.len() != 1 || is_zen;

        if !is_multi {
            // Single index
            let i = indices[0];
            let exists = i >= 0
                && items
                    .get(i as usize)
                    .is_some_and(|v| !matches!(v, Value::Nil));
            let result_bool = exists ^ effective_negated;
            let key = Value::Int(i);
            let result = match adverb_bits {
                0 | 5 => Value::Bool(result_bool),
                // :kv — return (index, bool) if exists, else ()
                1 => {
                    if exists {
                        Value::array(vec![key, Value::Bool(result_bool)])
                    } else {
                        Value::array(Vec::new())
                    }
                }
                // :!kv — always return (index, bool)
                2 => Value::array(vec![key, Value::Bool(result_bool)]),
                // :p — return Pair if exists, else ()
                3 => {
                    if exists {
                        Value::ValuePair(Box::new(key), Box::new(Value::Bool(result_bool)))
                    } else {
                        Value::array(Vec::new())
                    }
                }
                // :!p — always return Pair
                4 => Value::ValuePair(Box::new(key), Box::new(Value::Bool(result_bool))),
                _ => Value::Bool(result_bool),
            };
            self.stack.push(result);
            return Ok(());
        }

        // Multi-index: compute (index, exists_bool) pairs
        let pairs: Vec<(i64, bool)> = indices
            .iter()
            .map(|&i| {
                let exists = i >= 0
                    && items
                        .get(i as usize)
                        .is_some_and(|v| !matches!(v, Value::Nil));
                (i, exists)
            })
            .collect();

        let result = match adverb_bits {
            0 => {
                // Plain :exists — list of Bools
                let vals: Vec<Value> = pairs
                    .iter()
                    .map(|(_, e)| Value::Bool(*e ^ effective_negated))
                    .collect();
                Value::array(vals)
            }
            1 => {
                // :kv — filter by original exists, interleave (index, bool)
                let mut vals = Vec::new();
                for (i, exists) in &pairs {
                    if *exists {
                        vals.push(Value::Int(*i));
                        vals.push(Value::Bool(*exists ^ effective_negated));
                    }
                }
                Value::array(vals)
            }
            2 => {
                // :!kv — no filter, interleave all (index, bool)
                let mut vals = Vec::new();
                for (i, exists) in &pairs {
                    vals.push(Value::Int(*i));
                    vals.push(Value::Bool(*exists ^ effective_negated));
                }
                Value::array(vals)
            }
            3 => {
                // :p — filter by original exists, return Pairs
                let mut vals = Vec::new();
                for (i, exists) in &pairs {
                    if *exists {
                        vals.push(Value::ValuePair(
                            Box::new(Value::Int(*i)),
                            Box::new(Value::Bool(*exists ^ effective_negated)),
                        ));
                    }
                }
                Value::array(vals)
            }
            4 => {
                // :!p — no filter, return all Pairs
                let mut vals = Vec::new();
                for (i, exists) in &pairs {
                    vals.push(Value::ValuePair(
                        Box::new(Value::Int(*i)),
                        Box::new(Value::Bool(*exists ^ effective_negated)),
                    ));
                }
                Value::array(vals)
            }
            5 => {
                // :!v — no filter, return all Bools
                let vals: Vec<Value> = pairs
                    .iter()
                    .map(|(_, e)| Value::Bool(*e ^ effective_negated))
                    .collect();
                Value::array(vals)
            }
            _ => Value::Nil,
        };

        self.stack.push(result);
        Ok(())
    }
}
