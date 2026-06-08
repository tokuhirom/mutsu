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

/// Engine-agnostic callback interface for the shared sort orchestration.
///
/// `.sort` orchestration (arity detection, merge sort, Schwartzian transform,
/// simple-comparator inlining, `:k` index mode) lives once in
/// [`sort_items_generic`] / [`sort_indices_generic`]. The two engines — the
/// tree-walking `Interpreter` and the bytecode `VM` — only differ in *how they
/// invoke a user callable / method*, which they supply through this trait. This
/// is what lets the VM run `@a.sort({ $^a.foo <=> $^b.foo })` natively (via
/// `vm_call_on_value`) instead of falling back to the interpreter, while keeping
/// a single sort implementation (no duplicate orchestration).
pub(crate) trait SortCaller {
    /// Call a callable (Sub / Routine / WhateverCode) with positional `args`.
    fn call_callable(&mut self, callable: &Value, args: Vec<Value>) -> Value;
    /// Call the 0-arg method `name` on `recv` (Schwartzian key extraction).
    fn call_method(&mut self, recv: Value, name: &str) -> Value;
    /// Resolve the arity of the sort callable (0 = none, 1 = mapper, >=2 =
    /// comparator). Delegates to the single shared `Interpreter::sort_callable_arity`.
    fn callable_arity(&self, callable: Option<&Value>) -> Result<usize, RuntimeError>;
}

/// [`SortCaller`] backed by the tree-walking interpreter.
pub(crate) struct InterpCaller<'a>(pub(crate) &'a mut Interpreter);

impl SortCaller for InterpCaller<'_> {
    fn call_callable(&mut self, callable: &Value, args: Vec<Value>) -> Value {
        // Hash iteration yields `Value::Pair` elements; bind them positionally so
        // a comparator/mapper block sees `$^a`/`$_` (not a named argument that
        // leaves the block with zero positionals). See `pair_as_positional`.
        let args = args
            .iter()
            .map(crate::runtime::utils::pair_as_positional)
            .collect();
        self.0
            .eval_call_on_value(callable.clone(), args)
            .unwrap_or(Value::Nil)
    }

    fn call_method(&mut self, recv: Value, name: &str) -> Value {
        self.0
            .call_method_with_values(recv, name, vec![])
            .unwrap_or(Value::Nil)
    }

    fn callable_arity(&self, callable: Option<&Value>) -> Result<usize, RuntimeError> {
        self.0.sort_callable_arity(callable)
    }
}

/// Stable merge sort that always calls the comparator with (left, right)
/// ordering, matching Raku's requirement that the comparator sees elements in
/// their relative position order.
pub(crate) fn merge_sort_with_cmp<F>(items: &mut [Value], cmp: &mut F)
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
    let left = items[..mid].to_vec();
    let right = items[mid..].to_vec();
    let (mut i, mut j, mut k) = (0, 0, 0);
    while i < left.len() && j < right.len() {
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

/// Map a comparator block's return value to an `Ordering`.
pub(crate) fn sort_result_to_ordering(result: &Value) -> std::cmp::Ordering {
    match result {
        Value::Int(n) => n.cmp(&0),
        // Bool comparators: True means "swap" (a > b), False means "don't swap".
        Value::Bool(true) => std::cmp::Ordering::Greater,
        Value::Bool(false) => std::cmp::Ordering::Less,
        Value::Enum {
            enum_type, value, ..
        } if enum_type == "Order" => value.as_i64().cmp(&0),
        _ => std::cmp::Ordering::Equal,
    }
}

/// Reorder `items` in place by `keys` (Schwartzian transform), preserving
/// stability via an index sort.
fn schwartzian_by_keys(items: &mut [Value], keys: &[Value]) {
    let mut indices: Vec<usize> = (0..items.len()).collect();
    indices.sort_by(|&i, &j| compare_values(&keys[i], &keys[j]).cmp(&0));
    let sorted: Vec<Value> = indices.iter().map(|&i| items[i].clone()).collect();
    items.clone_from_slice(&sorted);
}

/// Shared `.sort` orchestration. `arity` is the resolved callable arity (0 = no
/// callable, 1 = mapper, >=2 = comparator). User callables / methods are invoked
/// through `caller`, so the same logic runs under both engines.
pub(crate) fn sort_items_generic(
    caller: &mut dyn SortCaller,
    items: &mut [Value],
    callable: Option<&Value>,
    arity: usize,
) {
    match callable {
        Some(c) if arity >= 2 => {
            if let Value::Sub(data) = c {
                // `{ $^a <=> $^b }` and friends: compare inline, no call at all.
                if let Some((reverse, is_string_cmp)) = detect_simple_cmp_block(data) {
                    merge_sort_with_cmp(items, &mut |a: &Value, b: &Value| {
                        let (l, r) = if reverse { (b, a) } else { (a, b) };
                        if is_string_cmp {
                            l.to_str_context().cmp(&r.to_str_context())
                        } else {
                            inline_numeric_cmp(l, r)
                        }
                    });
                    return;
                }
                // `{ $^a.method <=> $^b.method }`: Schwartzian over the keys.
                if let Some((method_name, reverse, is_string_cmp)) = detect_method_cmp_block(data) {
                    let keys: Vec<Value> = items
                        .iter()
                        .map(|item| caller.call_method(item.clone(), &method_name))
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
                    let sorted: Vec<Value> = indices.iter().map(|&i| items[i].clone()).collect();
                    items.clone_from_slice(&sorted);
                    return;
                }
            }
            // General comparator (Sub / Routine): merge sort calling it per pair.
            let c = c.clone();
            merge_sort_with_cmp(items, &mut |a: &Value, b: &Value| {
                let result = caller.call_callable(&c, vec![a.clone(), b.clone()]);
                sort_result_to_ordering(&result)
            });
        }
        Some(c) => {
            // arity <= 1: mapper. `{ .method }` is a Schwartzian over a 0-arg
            // method; any other 1-arity callable is a Schwartzian over the block.
            if let Value::Sub(data) = c
                && let Some(method_name) = detect_simple_mapper_block(data)
            {
                let keys: Vec<Value> = items
                    .iter()
                    .map(|item| caller.call_method(item.clone(), &method_name))
                    .collect();
                schwartzian_by_keys(items, &keys);
                return;
            }
            let c = c.clone();
            let keys: Vec<Value> = items
                .iter()
                .map(|item| caller.call_callable(&c, vec![item.clone()]))
                .collect();
            schwartzian_by_keys(items, &keys);
        }
        None => items.sort_by(|a, b| compare_values(a, b).cmp(&0)),
    }
}

/// Like [`sort_items_generic`] but returns the original indices in sorted order
/// (the `:k` adverb).
pub(crate) fn sort_indices_generic(
    caller: &mut dyn SortCaller,
    items: &[Value],
    callable: Option<&Value>,
    arity: usize,
) -> Vec<Value> {
    let mut perm: Vec<usize> = (0..items.len()).collect();
    match callable {
        Some(c) if arity >= 2 => {
            if let Value::Sub(data) = c {
                if let Some((reverse, is_string_cmp)) = detect_simple_cmp_block(data) {
                    merge_sort_indices(&mut perm, items, &mut |a, b| {
                        let (l, r) = if reverse { (b, a) } else { (a, b) };
                        if is_string_cmp {
                            l.to_str_context().cmp(&r.to_str_context())
                        } else {
                            inline_numeric_cmp(l, r)
                        }
                    });
                    return perm.into_iter().map(|i| Value::Int(i as i64)).collect();
                }
                if let Some((method_name, reverse, is_string_cmp)) = detect_method_cmp_block(data) {
                    let keys: Vec<Value> = items
                        .iter()
                        .map(|item| caller.call_method(item.clone(), &method_name))
                        .collect();
                    perm.sort_by(|&i, &j| {
                        let (l, r) = if reverse { (j, i) } else { (i, j) };
                        if is_string_cmp {
                            keys[l].to_str_context().cmp(&keys[r].to_str_context())
                        } else {
                            inline_numeric_cmp(&keys[l], &keys[r])
                        }
                    });
                    return perm.into_iter().map(|i| Value::Int(i as i64)).collect();
                }
            }
            let c = c.clone();
            merge_sort_indices(&mut perm, items, &mut |a, b| {
                let result = caller.call_callable(&c, vec![a.clone(), b.clone()]);
                sort_result_to_ordering(&result)
            });
        }
        Some(c) => {
            let keys: Vec<Value> = if let Value::Sub(data) = c
                && let Some(method_name) = detect_simple_mapper_block(data)
            {
                items
                    .iter()
                    .map(|item| caller.call_method(item.clone(), &method_name))
                    .collect()
            } else {
                let c = c.clone();
                items
                    .iter()
                    .map(|item| caller.call_callable(&c, vec![item.clone()]))
                    .collect()
            };
            perm.sort_by(|&i, &j| compare_values(&keys[i], &keys[j]).cmp(&0));
        }
        None => perm.sort_by(|&i, &j| compare_values(&items[i], &items[j]).cmp(&0)),
    }
    perm.into_iter().map(|i| Value::Int(i as i64)).collect()
}

/// Stable merge sort over a permutation array, comparing the underlying values.
fn merge_sort_indices<F>(perm: &mut [usize], items: &[Value], cmp: &mut F)
where
    F: FnMut(&Value, &Value) -> std::cmp::Ordering,
{
    let len = perm.len();
    if len <= 1 {
        return;
    }
    let mid = len / 2;
    merge_sort_indices(&mut perm[..mid], items, cmp);
    merge_sort_indices(&mut perm[mid..], items, cmp);
    let left = perm[..mid].to_vec();
    let right = perm[mid..].to_vec();
    let (mut i, mut j, mut k) = (0, 0, 0);
    while i < left.len() && j < right.len() {
        if cmp(&items[left[i]], &items[right[j]]) != std::cmp::Ordering::Greater {
            perm[k] = left[i];
            i += 1;
        } else {
            perm[k] = right[j];
            j += 1;
        }
        k += 1;
    }
    while i < left.len() {
        perm[k] = left[i];
        i += 1;
        k += 1;
    }
    while j < right.len() {
        perm[k] = right[j];
        j += 1;
        k += 1;
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
    /// Resolve the arity of a `.sort` callable: 0 = none, 1 = mapper, >=2 =
    /// comparator. Rejects explicitly 0-arity named subs. Shared by both engines
    /// (the VM calls it via `self.interpreter`).
    pub(crate) fn sort_callable_arity(
        &self,
        callable: Option<&Value>,
    ) -> Result<usize, RuntimeError> {
        let Some(c) = callable else { return Ok(0) };
        match c {
            Value::Sub(data) => {
                if data.empty_sig {
                    // Reject explicitly 0-arity named subs (e.g., `sub foo () { ... }`)
                    return Err(RuntimeError::new(
                        "sort requires a callable with at least 1 parameter".to_string(),
                    ));
                }
                // For blocks/closures, 0 params means implicit $_ (1-arity mapper).
                Ok(if data.params.is_empty() {
                    1
                } else {
                    data.params.len()
                })
            }
            Value::Routine { .. } => {
                let (params, _) = self.callable_signature(c);
                if params.is_empty() {
                    return Err(RuntimeError::new(
                        "sort requires a callable with at least 1 parameter".to_string(),
                    ));
                }
                Ok(params.len())
            }
            _ => Ok(1),
        }
    }
}

/// Shared `.sort` entry point — engine-agnostic argument parsing + target-shape
/// dispatch, on top of the shared [`sort_items_generic`] / [`sort_indices_generic`]
/// orchestration. Both engines call this with their own [`SortCaller`]:
/// `InterpCaller` for the tree-walking interpreter (EVAL / regex carrier / `sort()`
/// builtin / top-level `.sort` method) and `VmSortCaller` for the bytecode VM
/// (`try_native_sort`). This removes the duplicate arg-parse + shape-dispatch
/// wrapper that previously lived once per engine.
///
/// Behavior is identical to the former `Interpreter::dispatch_sort`: `:k` selects
/// index mode, `:by` names the callable, and any other arg (including stray
/// adverbs) is taken as the callable. Shaped multi-dim arrays sort over leaves;
/// non-collection targets pass through unchanged. The VM gates which target shapes
/// it accepts before calling here (Shaped / Instance / Supply still fall back).
pub(crate) fn sort_value_generic(
    caller: &mut dyn SortCaller,
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

    let arity = caller.callable_arity(callable.as_ref())?;
    let callable_ref = callable.as_ref();

    match target {
        Value::Array(ref items_arc, ref kind) => {
            // For multi-dim shaped arrays, sort over leaves
            let use_leaves = *kind == crate::value::ArrayKind::Shaped
                && items_arc.iter().any(|v| matches!(v, Value::Array(..)));
            if use_leaves {
                let mut leaves = crate::runtime::utils::shaped_array_leaves(&target);
                if return_indices {
                    Ok(Value::array(sort_indices_generic(
                        caller,
                        &leaves,
                        callable_ref,
                        arity,
                    )))
                } else {
                    sort_items_generic(caller, &mut leaves, callable_ref, arity);
                    Ok(Value::Seq(Arc::new(leaves)))
                }
            } else {
                let Value::Array(mut items, ..) = target else {
                    unreachable!()
                };
                let items_mut = Arc::make_mut(&mut items);
                if return_indices {
                    Ok(Value::array(sort_indices_generic(
                        caller,
                        items_mut,
                        callable_ref,
                        arity,
                    )))
                } else {
                    sort_items_generic(caller, items_mut, callable_ref, arity);
                    Ok(Value::Seq(Arc::new(items_mut.to_vec())))
                }
            }
        }
        Value::Seq(items) | Value::Slip(items) => {
            let mut sorted = items.as_ref().clone();
            if return_indices {
                Ok(Value::array(sort_indices_generic(
                    caller,
                    &sorted,
                    callable_ref,
                    arity,
                )))
            } else {
                sort_items_generic(caller, &mut sorted, callable_ref, arity);
                Ok(Value::Seq(Arc::new(sorted)))
            }
        }
        Value::Range(..)
        | Value::RangeExcl(..)
        | Value::RangeExclStart(..)
        | Value::RangeExclBoth(..)
        | Value::GenericRange { .. } => {
            let mut sorted = Interpreter::value_to_list(&target);
            if return_indices {
                Ok(Value::array(sort_indices_generic(
                    caller,
                    &sorted,
                    callable_ref,
                    arity,
                )))
            } else {
                sort_items_generic(caller, &mut sorted, callable_ref, arity);
                Ok(Value::Seq(Arc::new(sorted)))
            }
        }
        Value::Hash(map) => {
            let items: Vec<Value> = map
                .iter()
                .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.clone())))
                .collect();
            sort_value_generic(caller, Value::array(items), args)
        }
        other => Ok(other),
    }
}
