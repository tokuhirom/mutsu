use super::*;

impl Interpreter {
    pub(crate) fn dispatch_sort(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Extract :k adverb and callable from args
        let mut return_indices = false;
        let mut callable: Option<Value> = None;
        for arg in args {
            match arg {
                Value::Pair(key, val) if key == "k" => {
                    return_indices = val.truthy();
                }
                Value::Pair(key, val) if key == "by" => {
                    callable = Some(val.as_ref().clone());
                }
                _ => {
                    callable = Some(arg.clone());
                }
            }
        }

        // Determine callable arity
        let callable_arity = if let Some(ref c) = callable {
            match c {
                Value::Sub(data) => {
                    if data.empty_sig {
                        // Reject explicitly 0-arity named subs (e.g., `sub foo () { ... }`)
                        return Err(RuntimeError::new(
                            "sort requires a callable with at least 1 parameter".to_string(),
                        ));
                    }
                    // For blocks/closures, 0 params means implicit $_ (treat as 1-arity mapper)
                    if data.params.is_empty() {
                        1
                    } else {
                        data.params.len()
                    }
                }
                Value::Routine { .. } => {
                    let (params, _) = self.callable_signature(c);
                    if params.is_empty() {
                        // Reject explicitly 0-arity named subs
                        return Err(RuntimeError::new(
                            "sort requires a callable with at least 1 parameter".to_string(),
                        ));
                    }
                    params.len()
                }
                _ => 1,
            }
        } else {
            0
        };

        let callable_args = if let Some(c) = callable {
            vec![c]
        } else {
            vec![]
        };

        /// Stable merge sort that always calls the comparator with (left, right) ordering.
        /// This ensures consistent semantics with Raku where the comparator receives
        /// elements in their relative position order.
        fn merge_sort_with_cmp<F>(items: &mut [Value], cmp: &mut F)
        where
            F: FnMut(&Value, &Value) -> std::cmp::Ordering,
        {
            let len = items.len();
            if len <= 1 {
                return;
            }
            let mid = len / 2;
            merge_sort_with_cmp(&mut items[..mid], cmp);
            merge_sort_with_cmp(&mut items[mid..], cmp);
            // Merge
            let left = items[..mid].to_vec();
            let right = items[mid..].to_vec();
            let (mut i, mut j, mut k) = (0, 0, 0);
            while i < left.len() && j < right.len() {
                // Always compare (left, right) to maintain Raku semantics
                if cmp(&left[i], &right[j]) != std::cmp::Ordering::Greater {
                    items[k] = left[i].clone();
                    i += 1;
                } else {
                    items[k] = right[j].clone();
                    j += 1;
                }
                k += 1;
            }
            while i < left.len() {
                items[k] = left[i].clone();
                i += 1;
                k += 1;
            }
            while j < right.len() {
                items[k] = right[j].clone();
                j += 1;
                k += 1;
            }
        }

        fn sort_items(
            interp: &mut Interpreter,
            items: &mut [Value],
            callable: Option<&Value>,
            arity: usize,
        ) {
            match callable {
                Some(Value::Sub(data)) if arity >= 2 => {
                    // Sub comparator: use merge sort for correct (left, right) ordering
                    let data = data.clone();
                    merge_sort_with_cmp(items, &mut |a: &Value, b: &Value| {
                        let saved = interp.env.clone();
                        for (k, v) in &data.env {
                            interp.env.insert(k.clone(), v.clone());
                        }
                        if data.params.len() >= 2 {
                            interp.env.insert(data.params[0].clone(), a.clone());
                            interp.env.insert(data.params[1].clone(), b.clone());
                        }
                        interp.env.insert("_".to_string(), a.clone());
                        let result = interp.eval_block_value(&data.body).unwrap_or(Value::Int(0));
                        interp.env = saved;
                        sort_result_to_ordering(&result)
                    });
                }
                Some(Value::Sub(data)) if arity <= 1 => {
                    // Sub mapper: manual env to handle $_ and Pair values correctly
                    items.sort_by(|a, b| {
                        let saved = interp.env.clone();
                        for (k, v) in &data.env {
                            interp.env.insert(k.clone(), v.clone());
                        }
                        if let Some(p) = data.params.first() {
                            interp.env.insert(p.clone(), a.clone());
                        }
                        interp.env.insert("_".to_string(), a.clone());
                        let key_a = interp.eval_block_value(&data.body).unwrap_or(Value::Nil);
                        interp.env = saved.clone();
                        for (k, v) in &data.env {
                            interp.env.insert(k.clone(), v.clone());
                        }
                        if let Some(p) = data.params.first() {
                            interp.env.insert(p.clone(), b.clone());
                        }
                        interp.env.insert("_".to_string(), b.clone());
                        let key_b = interp.eval_block_value(&data.body).unwrap_or(Value::Nil);
                        interp.env = saved;
                        compare_values(&key_a, &key_b).cmp(&0)
                    });
                }
                Some(c) if arity >= 2 => {
                    // Routine comparator: use merge sort for correct (left, right) ordering
                    let c = c.clone();
                    merge_sort_with_cmp(items, &mut |a: &Value, b: &Value| {
                        let call_args = vec![a.clone(), b.clone()];
                        match interp.eval_call_on_value(c.clone(), call_args) {
                            Ok(result) => sort_result_to_ordering(&result),
                            Err(_) => std::cmp::Ordering::Equal,
                        }
                    });
                }
                Some(c) if arity == 1 => {
                    // Routine mapper: call with 1 arg
                    items.sort_by(|a, b| {
                        let key_a = interp
                            .eval_call_on_value(c.clone(), vec![a.clone()])
                            .unwrap_or(Value::Nil);
                        let key_b = interp
                            .eval_call_on_value(c.clone(), vec![b.clone()])
                            .unwrap_or(Value::Nil);
                        compare_values(&key_a, &key_b).cmp(&0)
                    });
                }
                _ => {
                    items.sort_by(|a, b| compare_values(a, b).cmp(&0));
                }
            }
        }

        fn sort_result_to_ordering(result: &Value) -> std::cmp::Ordering {
            match result {
                Value::Int(n) => n.cmp(&0),
                // Bool comparators: True means "swap" (a > b), False means "don't swap" (a <= b)
                Value::Bool(true) => std::cmp::Ordering::Greater,
                Value::Bool(false) => std::cmp::Ordering::Less,
                Value::Enum {
                    enum_type, value, ..
                } if enum_type == "Order" => value.as_i64().cmp(&0),
                _ => std::cmp::Ordering::Equal,
            }
        }

        let callable_ref = callable_args.first();

        match target {
            Value::Array(ref items_arc, ref kind) => {
                // For multi-dim shaped arrays, sort over leaves
                let use_leaves = *kind == crate::value::ArrayKind::Shaped
                    && items_arc.iter().any(|v| matches!(v, Value::Array(..)));
                if use_leaves {
                    let mut leaves = crate::runtime::utils::shaped_array_leaves(&target);
                    if return_indices {
                        let indices = sort_indices(self, &leaves, callable_ref, callable_arity);
                        Ok(Value::array(indices))
                    } else {
                        sort_items(self, &mut leaves, callable_ref, callable_arity);
                        Ok(Value::Seq(Arc::new(leaves)))
                    }
                } else {
                    let Value::Array(mut items, ..) = target else {
                        unreachable!()
                    };
                    let items_mut = Arc::make_mut(&mut items);
                    if return_indices {
                        let indices = sort_indices(self, items_mut, callable_ref, callable_arity);
                        Ok(Value::array(indices))
                    } else {
                        sort_items(self, items_mut, callable_ref, callable_arity);
                        Ok(Value::Seq(Arc::new(items_mut.to_vec())))
                    }
                }
            }
            Value::Seq(items) | Value::Slip(items) => {
                let mut sorted = items.as_ref().clone();
                if return_indices {
                    let indices = sort_indices(self, &sorted, callable_ref, callable_arity);
                    Ok(Value::array(indices))
                } else {
                    sort_items(self, &mut sorted, callable_ref, callable_arity);
                    Ok(Value::Seq(Arc::new(sorted)))
                }
            }
            Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. } => {
                let mut sorted = Self::value_to_list(&target);
                if return_indices {
                    let indices = sort_indices(self, &sorted, callable_ref, callable_arity);
                    Ok(Value::array(indices))
                } else {
                    sort_items(self, &mut sorted, callable_ref, callable_arity);
                    Ok(Value::Seq(Arc::new(sorted)))
                }
            }
            Value::Hash(map) => {
                let items: Vec<Value> = map
                    .iter()
                    .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.clone())))
                    .collect();
                self.dispatch_sort(Value::array(items), args)
            }
            other => Ok(other),
        }
    }
}

/// Sort items and return the original indices in sorted order.
fn sort_indices(
    interp: &mut Interpreter,
    items: &[Value],
    callable: Option<&Value>,
    arity: usize,
) -> Vec<Value> {
    use crate::runtime::utils::compare_values;
    let mut indexed: Vec<(usize, &Value)> = items.iter().enumerate().collect();

    fn idx_sort_result(result: &Value) -> std::cmp::Ordering {
        match result {
            Value::Int(n) => n.cmp(&0),
            Value::Bool(true) => std::cmp::Ordering::Greater,
            Value::Bool(false) => std::cmp::Ordering::Less,
            Value::Enum {
                enum_type, value, ..
            } if enum_type == "Order" => value.as_i64().cmp(&0),
            _ => std::cmp::Ordering::Equal,
        }
    }

    match callable {
        Some(Value::Sub(data)) if arity >= 2 => {
            indexed.sort_by(|(_, a), (_, b)| {
                let saved = interp.env.clone();
                for (k, v) in &data.env {
                    interp.env.insert(k.clone(), v.clone());
                }
                if data.params.len() >= 2 {
                    interp.env.insert(data.params[0].clone(), (*a).clone());
                    interp.env.insert(data.params[1].clone(), (*b).clone());
                }
                interp.env.insert("_".to_string(), (*a).clone());
                let result = interp.eval_block_value(&data.body).unwrap_or(Value::Int(0));
                interp.env = saved;
                idx_sort_result(&result)
            });
        }
        Some(Value::Sub(data)) if arity <= 1 => {
            indexed.sort_by(|(_, a), (_, b)| {
                let saved = interp.env.clone();
                for (k, v) in &data.env {
                    interp.env.insert(k.clone(), v.clone());
                }
                if let Some(p) = data.params.first() {
                    interp.env.insert(p.clone(), (*a).clone());
                }
                interp.env.insert("_".to_string(), (*a).clone());
                let key_a = interp.eval_block_value(&data.body).unwrap_or(Value::Nil);
                interp.env = saved.clone();
                for (k, v) in &data.env {
                    interp.env.insert(k.clone(), v.clone());
                }
                if let Some(p) = data.params.first() {
                    interp.env.insert(p.clone(), (*b).clone());
                }
                interp.env.insert("_".to_string(), (*b).clone());
                let key_b = interp.eval_block_value(&data.body).unwrap_or(Value::Nil);
                interp.env = saved;
                compare_values(&key_a, &key_b).cmp(&0)
            });
        }
        Some(c) if arity >= 2 => {
            indexed.sort_by(|(_, a), (_, b)| {
                let call_args = vec![(*a).clone(), (*b).clone()];
                match interp.eval_call_on_value(c.clone(), call_args) {
                    Ok(result) => idx_sort_result(&result),
                    Err(_) => std::cmp::Ordering::Equal,
                }
            });
        }
        Some(c) if arity == 1 => {
            indexed.sort_by(|(_, a), (_, b)| {
                let key_a = interp
                    .eval_call_on_value(c.clone(), vec![(*a).clone()])
                    .unwrap_or(Value::Nil);
                let key_b = interp
                    .eval_call_on_value(c.clone(), vec![(*b).clone()])
                    .unwrap_or(Value::Nil);
                compare_values(&key_a, &key_b).cmp(&0)
            });
        }
        _ => {
            indexed.sort_by(|(_, a), (_, b)| compare_values(a, b).cmp(&0));
        }
    }
    indexed
        .into_iter()
        .map(|(i, _)| Value::Int(i as i64))
        .collect()
}
