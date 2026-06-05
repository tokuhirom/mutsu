use super::*;
use crate::ast::{Expr, Stmt};
use crate::token_kind::TokenKind;

pub(crate) fn inline_numeric_cmp(a: &Value, b: &Value) -> std::cmp::Ordering {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => x.cmp(y),
        (Value::Num(x), Value::Num(y)) => x.partial_cmp(y).unwrap_or(std::cmp::Ordering::Equal),
        (Value::Int(x), Value::Num(y)) => (*x as f64)
            .partial_cmp(y)
            .unwrap_or(std::cmp::Ordering::Equal),
        (Value::Num(x), Value::Int(y)) => x
            .partial_cmp(&(*y as f64))
            .unwrap_or(std::cmp::Ordering::Equal),
        _ => compare_values(a, b).cmp(&0),
    }
}

/// Detect simple comparison patterns in sort blocks and return an inline comparator.
/// Handles `{ $^a <=> $^b }`, `{ $^b <=> $^a }`, `{ $^a cmp $^b }`, `{ $^b cmp $^a }`,
/// and also negated (reversed) variants.
/// Returns Some((reverse, is_string_cmp)) if the block is a simple two-var comparison.
/// Detect simple mapper blocks like `{ .method }` (method call on $_ with no args).
/// Returns Some(method_name) if detected.
fn detect_simple_mapper_block(data: &crate::value::SubData) -> Option<String> {
    let body = &data.body;
    let expr = body.iter().find_map(|s| match s {
        Stmt::Expr(e) => Some(e),
        _ => None,
    })?;
    match expr {
        Expr::MethodCall {
            target,
            name,
            args,
            modifier: None,
            ..
        } if args.is_empty() => {
            if matches!(target.as_ref(), Expr::Var(v) if v == "_") {
                return Some(name.to_string());
            }
            if let Some(p) = data.params.first()
                && matches!(target.as_ref(), Expr::Var(v) if v == p)
            {
                return Some(name.to_string());
            }
            None
        }
        _ => None,
    }
}

/// Detect `{ $^a.method <=> $^b.method }` or `{ $^a.method cmp $^b.method }` patterns.
/// Returns Some((method_name, reverse, is_string_cmp)) if detected.
fn detect_method_cmp_block(data: &crate::value::SubData) -> Option<(String, bool, bool)> {
    if data.params.len() < 2 {
        return None;
    }
    let body = &data.body;
    let expr = body.iter().find_map(|s| match s {
        Stmt::Expr(e) => Some(e),
        _ => None,
    })?;
    match expr {
        Expr::Binary { left, op, right } => {
            let is_string_cmp = matches!(op, TokenKind::Ident(s) if s == "cmp");
            let is_numeric_cmp = matches!(op, TokenKind::LtEqGt);
            if !is_string_cmp && !is_numeric_cmp {
                return None;
            }
            let param_a = &data.params[0];
            let param_b = &data.params[1];
            // Extract method calls on both sides
            let (left_var, left_method) = match left.as_ref() {
                Expr::MethodCall {
                    target,
                    name,
                    args,
                    modifier: None,
                    ..
                } if args.is_empty() => {
                    if let Expr::Var(v) = target.as_ref() {
                        Some((v.as_str(), name.to_string()))
                    } else {
                        None
                    }
                }
                _ => None,
            }?;
            let (right_var, right_method) = match right.as_ref() {
                Expr::MethodCall {
                    target,
                    name,
                    args,
                    modifier: None,
                    ..
                } if args.is_empty() => {
                    if let Expr::Var(v) = target.as_ref() {
                        Some((v.as_str(), name.to_string()))
                    } else {
                        None
                    }
                }
                _ => None,
            }?;
            if left_method != right_method {
                return None;
            }
            // { $^a.method <=> $^b.method } — normal order
            if left_var == param_a && right_var == param_b {
                return Some((left_method, false, is_string_cmp));
            }
            // { $^b.method <=> $^a.method } — reversed
            if left_var == param_b && right_var == param_a {
                return Some((left_method, true, is_string_cmp));
            }
            None
        }
        _ => None,
    }
}

pub(crate) fn detect_simple_cmp_block(data: &crate::value::SubData) -> Option<(bool, bool)> {
    if data.params.len() < 2 {
        return None;
    }
    let body = &data.body;
    // Find the expression statement (skip SetLine)
    let expr = body.iter().find_map(|s| match s {
        Stmt::Expr(e) => Some(e),
        _ => None,
    })?;
    match expr {
        Expr::Binary { left, op, right } => {
            let is_string_cmp = matches!(op, TokenKind::Ident(s) if s == "cmp");
            let is_numeric_cmp = matches!(op, TokenKind::LtEqGt);
            if !is_string_cmp && !is_numeric_cmp {
                return None;
            }
            let param_a = &data.params[0];
            let param_b = &data.params[1];
            // { $^a <=> $^b } — normal order
            if matches!(left.as_ref(), Expr::Var(v) if v == param_a)
                && matches!(right.as_ref(), Expr::Var(v) if v == param_b)
            {
                return Some((false, is_string_cmp));
            }
            // { $^b <=> $^a } — reversed
            if matches!(left.as_ref(), Expr::Var(v) if v == param_b)
                && matches!(right.as_ref(), Expr::Var(v) if v == param_a)
            {
                return Some((true, is_string_cmp));
            }
            None
        }
        _ => None,
    }
}

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
                    // Fast path: detect simple { $^a <=> $^b } or { $^b <=> $^a } patterns
                    if let Some((reverse, is_string_cmp)) = detect_simple_cmp_block(data) {
                        if is_string_cmp {
                            merge_sort_with_cmp(items, &mut |a: &Value, b: &Value| {
                                let (l, r) = if reverse { (b, a) } else { (a, b) };
                                l.to_str_context().cmp(&r.to_str_context())
                            });
                        } else {
                            merge_sort_with_cmp(items, &mut |a: &Value, b: &Value| {
                                let (l, r) = if reverse { (b, a) } else { (a, b) };
                                inline_numeric_cmp(l, r)
                            });
                        }
                        return;
                    }
                    // Fast path: detect { $^a.method <=> $^b.method } pattern
                    // Uses Schwartzian transform: pre-compute keys, then sort by keys
                    if let Some((method_name, reverse, is_string_cmp)) =
                        detect_method_cmp_block(data)
                    {
                        let keys: Vec<Value> = items
                            .iter()
                            .map(|item| {
                                interp
                                    .call_method_with_values(item.clone(), &method_name, vec![])
                                    .unwrap_or(Value::Nil)
                            })
                            .collect();
                        let mut indices: Vec<usize> = (0..items.len()).collect();
                        indices.sort_by(|&i, &j| {
                            let (l, r) = if reverse { (j, i) } else { (i, j) };
                            if is_string_cmp {
                                keys[l].to_str_context().cmp(&keys[r].to_str_context())
                            } else {
                                inline_numeric_cmp(&keys[l], &keys[r])
                            }
                        });
                        let sorted: Vec<Value> =
                            indices.iter().map(|&i| items[i].clone()).collect();
                        items.clone_from_slice(&sorted);
                        return;
                    }
                    // Sub comparator: use merge sort for correct (left, right) ordering
                    let data = data.clone();
                    // Pre-collect keys we'll modify to avoid full env clone
                    let touched_keys: Vec<String> = {
                        let mut keys: Vec<String> = data.env.keys().map(|k| k.resolve()).collect();
                        if data.params.len() >= 2 {
                            keys.push(data.params[0].clone());
                            keys.push(data.params[1].clone());
                        }
                        keys.push("_".to_string());
                        keys.sort();
                        keys.dedup();
                        keys
                    };
                    merge_sort_with_cmp(items, &mut |a: &Value, b: &Value| {
                        // Save only touched keys
                        let saved: Vec<(String, Option<Value>)> = touched_keys
                            .iter()
                            .map(|k| (k.clone(), interp.env.get(k).cloned()))
                            .collect();
                        for (k, v) in &data.env {
                            interp.env.insert_sym(*k, v.clone());
                        }
                        if data.params.len() >= 2 {
                            interp.env.insert(data.params[0].clone(), a.clone());
                            interp.env.insert(data.params[1].clone(), b.clone());
                        }
                        interp.env.insert("_".to_string(), a.clone());
                        let result = interp.eval_block_value(&data.body).unwrap_or(Value::Int(0));
                        // Restore only touched keys
                        for (k, v) in saved {
                            if let Some(val) = v {
                                interp.env.insert(k, val);
                            } else {
                                interp.env.remove(&k);
                            }
                        }
                        sort_result_to_ordering(&result)
                    });
                }
                Some(Value::Sub(data)) if arity <= 1 => {
                    // Fast path: detect { .method } pattern and use Schwartzian transform
                    if let Some(method_name) = detect_simple_mapper_block(data) {
                        // Pre-compute keys (N calls instead of N*log(N))
                        let keys: Vec<Value> = items
                            .iter()
                            .map(|item| {
                                interp
                                    .call_method_with_values(item.clone(), &method_name, vec![])
                                    .unwrap_or(Value::Nil)
                            })
                            .collect();
                        // Build index array and sort by pre-computed keys
                        let mut indices: Vec<usize> = (0..items.len()).collect();
                        indices.sort_by(|&i, &j| compare_values(&keys[i], &keys[j]).cmp(&0));
                        // Rearrange items in-place by sorted indices
                        let sorted: Vec<Value> =
                            indices.iter().map(|&i| items[i].clone()).collect();
                        items.clone_from_slice(&sorted);
                        return;
                    }
                    // Sub mapper: save/restore only touched keys
                    let touched_keys: Vec<String> = {
                        let mut keys: Vec<String> = data.env.keys().map(|k| k.resolve()).collect();
                        if let Some(p) = data.params.first() {
                            keys.push(p.clone());
                        }
                        keys.push("_".to_string());
                        keys.sort();
                        keys.dedup();
                        keys
                    };
                    let data = data.clone();
                    items.sort_by(|a, b| {
                        let saved: Vec<(String, Option<Value>)> = touched_keys
                            .iter()
                            .map(|k| (k.clone(), interp.env.get(k).cloned()))
                            .collect();
                        for (k, v) in &data.env {
                            interp.env.insert_sym(*k, v.clone());
                        }
                        if let Some(p) = data.params.first() {
                            interp.env.insert(p.clone(), a.clone());
                        }
                        interp.env.insert("_".to_string(), a.clone());
                        let key_a = interp.eval_block_value(&data.body).unwrap_or(Value::Nil);
                        for (k, v) in &data.env {
                            interp.env.insert_sym(*k, v.clone());
                        }
                        if let Some(p) = data.params.first() {
                            interp.env.insert(p.clone(), b.clone());
                        }
                        interp.env.insert("_".to_string(), b.clone());
                        let key_b = interp.eval_block_value(&data.body).unwrap_or(Value::Nil);
                        for (k, v) in &saved {
                            if let Some(val) = v {
                                interp.env.insert(k.clone(), val.clone());
                            } else {
                                interp.env.remove(k);
                            }
                        }
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
            let touched_keys: Vec<String> = {
                let mut keys: Vec<String> = data.env.keys().map(|k| k.resolve()).collect();
                if data.params.len() >= 2 {
                    keys.push(data.params[0].clone());
                    keys.push(data.params[1].clone());
                }
                keys.push("_".to_string());
                keys.sort();
                keys.dedup();
                keys
            };
            indexed.sort_by(|(_, a), (_, b)| {
                let saved: Vec<(String, Option<Value>)> = touched_keys
                    .iter()
                    .map(|k| (k.clone(), interp.env.get(k).cloned()))
                    .collect();
                for (k, v) in &data.env {
                    interp.env.insert_sym(*k, v.clone());
                }
                if data.params.len() >= 2 {
                    interp.env.insert(data.params[0].clone(), (*a).clone());
                    interp.env.insert(data.params[1].clone(), (*b).clone());
                }
                interp.env.insert("_".to_string(), (*a).clone());
                let result = interp.eval_block_value(&data.body).unwrap_or(Value::Int(0));
                for (k, v) in saved {
                    if let Some(val) = v {
                        interp.env.insert(k, val);
                    } else {
                        interp.env.remove(&k);
                    }
                }
                idx_sort_result(&result)
            });
        }
        Some(Value::Sub(data)) if arity <= 1 => {
            let touched_keys: Vec<String> = {
                let mut keys: Vec<String> = data.env.keys().map(|k| k.resolve()).collect();
                if let Some(p) = data.params.first() {
                    keys.push(p.clone());
                }
                keys.push("_".to_string());
                keys.sort();
                keys.dedup();
                keys
            };
            indexed.sort_by(|(_, a), (_, b)| {
                let saved: Vec<(String, Option<Value>)> = touched_keys
                    .iter()
                    .map(|k| (k.clone(), interp.env.get(k).cloned()))
                    .collect();
                for (k, v) in &data.env {
                    interp.env.insert_sym(*k, v.clone());
                }
                if let Some(p) = data.params.first() {
                    interp.env.insert(p.clone(), (*a).clone());
                }
                interp.env.insert("_".to_string(), (*a).clone());
                let key_a = interp.eval_block_value(&data.body).unwrap_or(Value::Nil);
                for (k, v) in &data.env {
                    interp.env.insert_sym(*k, v.clone());
                }
                if let Some(p) = data.params.first() {
                    interp.env.insert(p.clone(), (*b).clone());
                }
                interp.env.insert("_".to_string(), (*b).clone());
                let key_b = interp.eval_block_value(&data.body).unwrap_or(Value::Nil);
                for (k, v) in &saved {
                    if let Some(val) = v {
                        interp.env.insert(k.clone(), val.clone());
                    } else {
                        interp.env.remove(k);
                    }
                }
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
