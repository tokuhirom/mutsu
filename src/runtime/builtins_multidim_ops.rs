use super::*;
use crate::value::ArrayKind;

use super::builtins_multidim::{
    array_to_list, has_multi_indices, leaf_key_tuple, make_key_tuple, multidim_collect_leaves,
    multidim_delete, multidim_index,
};

impl Interpreter {
    /// Handle dynamic adverbs on multidim index: @array[$a;$b;$c]:$delete
    /// Args: [inner_expr_result, adverb_name, adverb_value]
    pub(super) fn builtin_multidim_adverb(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 3 {
            return Err(RuntimeError::new(
                "__mutsu_multidim_adverb expects value, adverb_name, and adverb_value",
            ));
        }
        let value = args[0].clone();
        let _adverb_name = args[1].to_string_value();
        let adverb_value = &args[2];

        // If the adverb is False, just return the value unchanged
        if !adverb_value.truthy() {
            return Ok(value);
        }

        // Adverb is True — currently only "delete" is supported.
        // When the inner expression is a MultiDimIndex result, we need to
        // delete the element from the array. However, the value has already
        // been evaluated, so we return it as-is for now.
        // TODO: Implement actual delete by restructuring the parser to
        // pass target array info.
        Ok(value)
    }

    /// Handle subscript adverbs (:kv, :k, :v, :p, etc.) on multidim index.
    /// Args: [target_array, adverb_name, dim0, dim1, dim2, ...]
    pub(super) fn builtin_multidim_subscript_adverb(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 3 {
            return Err(RuntimeError::new(
                "__mutsu_multidim_subscript_adverb expects target, adverb, and indices",
            ));
        }
        let target = &args[0];
        let adverb = args[1].to_string_value();
        let raw_indices = &args[2..];
        let indices = self.resolve_multidim_indices(target, raw_indices)?;

        // Check if we need multi-result mode
        if has_multi_indices(&indices) {
            return self.multidim_subscript_adverb_multi(target, &adverb, &indices);
        }

        let value = multidim_index(target, &indices);
        let key = make_key_tuple(&indices);
        let exists = !value.is_nil();

        match adverb.as_str() {
            "k" => Ok(if exists { key } else { Value::NIL }),
            "kv" => {
                if exists {
                    let v = array_to_list(value);
                    Ok(Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(vec![key, v])),
                        ArrayKind::List,
                    ))
                } else {
                    Ok(Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(vec![])),
                        ArrayKind::List,
                    ))
                }
            }
            "p" => {
                if exists {
                    let v = array_to_list(value);
                    Ok(Value::value_pair(key, v))
                } else {
                    Ok(Value::NIL)
                }
            }
            "v" => {
                if exists {
                    Ok(array_to_list(value))
                } else {
                    Ok(Value::NIL)
                }
            }
            "not-k" => Ok(if !exists { key } else { Value::NIL }),
            "not-kv" => {
                if !exists {
                    let v = array_to_list(value);
                    Ok(Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(vec![key, v])),
                        ArrayKind::List,
                    ))
                } else {
                    Ok(Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(vec![])),
                        ArrayKind::List,
                    ))
                }
            }
            "not-p" => {
                if !exists {
                    let v = array_to_list(value);
                    Ok(Value::value_pair(key, v))
                } else {
                    Ok(Value::NIL)
                }
            }
            "not-v" => {
                if !exists {
                    Ok(array_to_list(value))
                } else {
                    Ok(Value::NIL)
                }
            }
            _ => Ok(value),
        }
    }

    /// Multi-result adverb handler for Whatever/list indices.
    fn multidim_subscript_adverb_multi(
        &mut self,
        target: &Value,
        adverb: &str,
        indices: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut leaves = Vec::new();
        multidim_collect_leaves(target, indices, &[], &mut leaves);

        let mut out = Vec::new();
        for (path, value) in leaves {
            let exists = !value.is_nil();
            let key = leaf_key_tuple(path);
            match adverb {
                "k" => {
                    if exists {
                        out.push(key);
                    }
                }
                "kv" => {
                    if exists {
                        out.push(key);
                        out.push(array_to_list(value));
                    }
                }
                "p" => {
                    if exists {
                        out.push(Value::value_pair(key, array_to_list(value)));
                    }
                }
                "v" => {
                    if exists {
                        out.push(array_to_list(value));
                    }
                }
                "not-k" => {
                    if !exists {
                        out.push(key);
                    }
                }
                "not-kv" => {
                    if !exists {
                        out.push(key);
                        out.push(array_to_list(value));
                    }
                }
                "not-p" => {
                    if !exists {
                        out.push(Value::value_pair(key, array_to_list(value)));
                    }
                }
                "not-v" => {
                    if !exists {
                        out.push(array_to_list(value));
                    }
                }
                _ => out.push(value),
            }
        }
        Ok(Value::array(out))
    }

    /// Handle :exists with secondary adverbs on multidim index.
    /// Args: [target_array, negated_bool, adverb_name, dim0, dim1, ...]
    pub(super) fn builtin_multidim_exists_adverb(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 4 {
            return Err(RuntimeError::new(
                "__mutsu_multidim_exists_adverb expects target, negated, adverb, and indices",
            ));
        }
        let target = &args[0];
        let negated = args[1].truthy();
        let adverb = args[2].to_string_value();
        let raw_indices = &args[3..];
        let indices = self.resolve_multidim_indices(target, raw_indices)?;
        // A nested single-dimension slice (`@a[(3, (30, (5,)))]:exists`) is a
        // structure-preserving slice, NOT a multidim coordinate walk: recurse the
        // index tree and report an existence Bool per leaf, keeping the nesting.
        if indices.len() == 1
            && let Some(inner) = Self::nested_index_elements(&indices[0])
            && inner
                .iter()
                .any(|e| Self::nested_index_elements(e).is_some())
            && let Some(items) = Self::positional_exists_items(target)
        {
            let out = Self::nested_exists_slice(&items, &inner, negated, &adverb);
            return Ok(Value::array(out));
        }
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
            && class_name == "Stash"
            && let Some(ValueView::Hash(symbols)) =
                attributes.as_map().get("symbols").map(Value::view)
        {
            let stash_exists = |idx: &Value| {
                let key = idx.to_string_value();
                if symbols.contains_key(&key) {
                    return true;
                }
                if !key.starts_with('$')
                    && !key.starts_with('@')
                    && !key.starts_with('%')
                    && !key.starts_with('&')
                {
                    return symbols.contains_key(&format!("${key}"));
                }
                false
            };
            let stash_indices: Vec<Value> = if indices.len() > 1 {
                indices.clone()
            } else {
                match indices[0].view() {
                    ValueView::Array(items, ..) => items.to_vec(),
                    _ => vec![indices[0].clone()],
                }
            };
            let exists_vals: Vec<bool> = stash_indices.iter().map(stash_exists).collect();
            let exists_vals: Vec<bool> = if negated {
                exists_vals.into_iter().map(|v| !v).collect()
            } else {
                exists_vals
            };
            if stash_indices.len() > 1 {
                return Ok(Value::array(
                    exists_vals
                        .into_iter()
                        .map(Value::truth)
                        .collect::<Vec<_>>(),
                ));
            }
            return Ok(Value::truth(*exists_vals.first().unwrap_or(&false)));
        }

        // Multi-result mode for Whatever/list indices
        if has_multi_indices(&indices) {
            return self.multidim_exists_adverb_multi(target, negated, &adverb, &indices);
        }

        let value = multidim_index(target, &indices);
        let raw_exists = !value.is_nil();
        let exists = if negated { !raw_exists } else { raw_exists };
        let key = make_key_tuple(&indices);

        match adverb.as_str() {
            "none" => Ok(Value::truth(exists)),
            "kv" => {
                if raw_exists {
                    Ok(Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(vec![
                            key,
                            Value::truth(exists),
                        ])),
                        ArrayKind::List,
                    ))
                } else {
                    Ok(Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(vec![])),
                        ArrayKind::List,
                    ))
                }
            }
            "p" => {
                if raw_exists {
                    Ok(Value::value_pair(key, Value::truth(exists)))
                } else {
                    Ok(Value::NIL)
                }
            }
            "k" => {
                if raw_exists {
                    Ok(key)
                } else {
                    Ok(Value::NIL)
                }
            }
            "v" => Ok(Value::truth(exists)),
            _ => Ok(Value::truth(exists)),
        }
    }

    /// The positional element view (with unfilled-slot info folded into the
    /// values) for a nested `:exists` slice, or `None` for a non-positional
    /// target. A real array keeps its hole marks (`Package("Any")` in an
    /// uninitialized slot); a Range/List is fully filled.
    pub(crate) fn positional_exists_items(target: &Value) -> Option<Vec<Value>> {
        match target.view() {
            ValueView::Array(items, ..) => Some(items.to_vec()),
            _ if target.is_range()
                || matches!(
                    target.view(),
                    ValueView::Seq(_)
                        | ValueView::HyperSeq(_)
                        | ValueView::RaceSeq(_)
                        | ValueView::LazyList(_)
                ) =>
            {
                Some(crate::runtime::utils::value_to_list(target))
            }
            _ => None,
        }
    }

    /// One level of a nested `:exists`/`:!exists` slice: a sub-list index recurses
    /// into ONE nested list element; a scalar index reports whether that slot
    /// exists (negated by `:!exists`), formatted by the companion adverb
    /// (`none`/`k`/`v`/`kv`/`p`).
    pub(crate) fn nested_exists_slice(
        items: &[Value],
        indices: &[Value],
        negated: bool,
        adverb: &str,
    ) -> Vec<Value> {
        let mut out = Vec::new();
        for idx in indices {
            if let Some(sub) = Self::nested_index_elements(idx) {
                out.push(Value::array(Self::nested_exists_slice(
                    items, &sub, negated, adverb,
                )));
                continue;
            }
            let i = match idx.view() {
                ValueView::Int(i) => i,
                ValueView::Num(f) => f as i64,
                _ => idx.to_string_value().parse::<i64>().unwrap_or(-1),
            };
            let raw_exists = i >= 0
                && (i as usize) < items.len()
                && !matches!(items[i as usize].view(), ValueView::Package(name) if name == "Any");
            let exists = if negated { !raw_exists } else { raw_exists };
            let key = Value::int(i);
            match adverb {
                // `:k` / `:kv` / `:p` keep only actually-existing keys.
                "k" => {
                    if raw_exists {
                        out.push(key);
                    }
                }
                "kv" => {
                    if raw_exists {
                        out.push(key);
                        out.push(Value::truth(exists));
                    }
                }
                "p" => {
                    if raw_exists {
                        out.push(Value::value_pair(key, Value::truth(exists)));
                    }
                }
                // `:!kv` / `:!p` keep every attempted key.
                "not-kv" => {
                    out.push(key);
                    out.push(Value::truth(exists));
                }
                "not-p" => {
                    out.push(Value::value_pair(key, Value::truth(exists)));
                }
                // `none` and `v` report a Bool for every index.
                _ => out.push(Value::truth(exists)),
            }
        }
        out
    }

    /// Multi-result :exists adverb handler for Whatever/list indices.
    fn multidim_exists_adverb_multi(
        &mut self,
        target: &Value,
        negated: bool,
        adverb: &str,
        indices: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut leaves = Vec::new();
        multidim_collect_leaves(target, indices, &[], &mut leaves);

        let mut out = Vec::new();
        for (path, value) in leaves {
            let raw_exists = !value.is_nil();
            let exists = if negated { !raw_exists } else { raw_exists };
            let key = leaf_key_tuple(path);
            match adverb {
                "none" => out.push(Value::truth(exists)),
                "kv" => {
                    if raw_exists {
                        out.push(key);
                        out.push(Value::truth(exists));
                    }
                }
                "p" => {
                    if raw_exists {
                        out.push(Value::value_pair(key, Value::truth(exists)));
                    }
                }
                "k" => {
                    if raw_exists {
                        out.push(key);
                    }
                }
                "v" => out.push(Value::truth(exists)),
                _ => out.push(Value::truth(exists)),
            }
        }
        Ok(Value::array(out))
    }

    /// Handle :delete on multidim index.
    /// Args: [var_name_string, dim0, dim1, ...]
    pub(super) fn builtin_multidim_delete(
        &mut self,
        args: &mut [Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new(
                "__mutsu_multidim_delete expects var_name and indices",
            ));
        }
        let var_name = args[0].to_string_value();
        let mut raw_indices = args[1..].to_vec();
        // `@a[|| @list]:delete` passes the `||` operand list as a single
        // dimension; expand its elements into the real dimensions (the delete
        // counterpart of `expand_pipe_multidim_dims`). A single-dimension
        // multidim subscript is only ever produced by `||`.
        if raw_indices.len() == 1
            && let Some(items) = raw_indices[0].as_list_items()
        {
            raw_indices = items.to_vec();
        }
        let target_val = self.env.get(&var_name).cloned().unwrap_or(Value::NIL);
        let indices = self.resolve_multidim_indices(&target_val, &raw_indices)?;
        // A shaped array (`my @a[2;2]`) has fixed dimensions; an out-of-range
        // index in any dimension is an error (raku throws X::AdHoc), not a
        // silent no-op that yields Any.
        Self::check_shaped_index_bounds(&target_val, &indices)?;
        // Multi-result mode for Whatever/list indices: collect each leaf, then
        // delete, returning the deleted values (each decontainerized).
        if has_multi_indices(&indices) {
            let mut leaves = Vec::new();
            multidim_collect_leaves(&target_val, &indices, &[], &mut leaves);
            if let Some(t) = self.env.get_mut(&var_name) {
                multidim_delete(t, &indices);
                self.writeback_multidim_var_to_local(&var_name);
            }
            let values: Vec<Value> = leaves.into_iter().map(|(_, v)| array_to_list(v)).collect();
            return Ok(Value::array(values));
        }
        // A non-existent (out-of-range) element deletes to `Nil`, not the `Any`
        // hole-value that `multidim_delete` returns for a missing slot.
        if multidim_index(&target_val, &indices).is_nil() {
            return Ok(Value::NIL);
        }
        let Some(target) = self.env.get_mut(&var_name) else {
            return Ok(Value::NIL);
        };
        let result = multidim_delete(target, &indices);
        self.writeback_multidim_var_to_local(&var_name);
        // Decontainerize the deleted element (Array leaf → List), matching
        // raku's multi-dim `:delete` return value.
        Ok(array_to_list(result))
    }

    /// Validate multidim indices against a shaped array's fixed dimensions.
    /// Returns an X::AdHoc error (matching raku) for the first out-of-range
    /// dimension. A no-op for non-shaped arrays / hashes, and for `Whatever` or
    /// multi-index (list/range) subscripts which select within bounds.
    fn check_shaped_index_bounds(target: &Value, indices: &[Value]) -> Result<(), RuntimeError> {
        let ValueView::Array(data, ArrayKind::Shaped) = target.view() else {
            return Ok(());
        };
        let Some(shape) = data.shape.as_ref() else {
            return Ok(());
        };
        for (dim, idx) in indices.iter().enumerate() {
            let Some(&size) = shape.get(dim) else { break };
            // Only plain integer indices are bounds-checked here; Whatever and
            // list/range selectors stay within the dimension by construction.
            let n = match idx.view() {
                ValueView::Int(n) => n,
                _ => continue,
            };
            let size = size as i64;
            // Negative indices count from the end; raku rejects those past -size.
            let resolved = if n < 0 { n + size } else { n };
            if resolved < 0 || resolved >= size {
                return Err(RuntimeError::new(format!(
                    "Index {} for dimension {} out of range (must be 0..{})",
                    n,
                    dim + 1,
                    size - 1
                )));
            }
        }
        Ok(())
    }

    /// After a multidim `:delete` mutates the env copy of `@a`/`%h` in place,
    /// mirror the mutated container back into the caller frame's local slot.
    /// `my @a` lives in a local slot (dual store), so mutating only env would
    /// leave the slot stale and `say @a` would read the pre-delete value.
    /// (The single-dim `DeleteIndexNamed` opcode does the same writeback.)
    pub(super) fn writeback_multidim_var_to_local(&mut self, var_name: &str) {
        let caller_code = self.current_code;
        if caller_code == 0 {
            return;
        }
        if let Some(updated) = self.env.get(var_name).cloned() {
            // SAFETY: `current_code` is the address of the live bytecode frame
            // that invoked this builtin (an ancestor on the call stack, valid
            // for the synchronous duration of this call).
            let code = unsafe { &*(caller_code as *const crate::opcode::CompiledCode) };
            // `slot` is a position in the *caller code's* local-name table, but
            // `self.locals` is the currently-executing frame's locals vec. When
            // this builtin runs nested below a different frame, that frame can
            // have fewer locals than the caller code names (the two are not the
            // same length), so the slot index may be out of range — guard it to
            // avoid an index-out-of-bounds panic (regression from #3748 surfaced
            // by S32-array/multislice-6e.t). A missing slot means the local does
            // not exist in this frame, so the env value already holds the result
            // and skipping the mirror is correct.
            if let Some(slot) = code.locals.iter().position(|n| n == var_name)
                && slot < self.locals.len()
            {
                self.locals[slot] = updated;
            }
        }
    }

    /// Resolve WhateverCode indices: if an index is a Sub (WhateverCode),
    /// call it with the current dimension's array length.
    fn resolve_multidim_indices(
        &mut self,
        target: &Value,
        indices: &[Value],
    ) -> Result<Vec<Value>, RuntimeError> {
        // A Range (or Seq) in a subscript dimension is a multi-key slice
        // (`%h{1;1..3}`), so expand it to its element list up front — then the
        // existing multi-index path (`has_multi_indices` / collect-leaves) walks
        // each key, exactly as it already does for a comma list (`%h{1;2,3}`).
        let indices: Vec<Value> = indices
            .iter()
            .map(|idx| {
                if idx.is_range() || matches!(idx.view(), ValueView::Seq(_)) {
                    Value::array(crate::runtime::utils::value_to_list(idx))
                } else {
                    idx.clone()
                }
            })
            .collect();
        let mut resolved = Vec::with_capacity(indices.len());
        // Read through a `ContainerRef`/`Scalar` so the WhateverCode length probe
        // (`match current { Array => len }`) sees the real container.
        let mut current = target.with_deref(|v| v.descalarize().clone());
        for idx in &indices {
            match idx.view() {
                ValueView::Sub(..) => {
                    // WhateverCode: call with array length
                    let len = match current.view() {
                        ValueView::Array(items, ..) => Value::int(items.len() as i64),
                        _ => Value::int(0),
                    };
                    let result = self.call_sub_value(idx.clone(), vec![len], false)?;
                    // Navigate to next dimension
                    let resolved_idx = result.clone();
                    current = multidim_index(&current, std::slice::from_ref(&resolved_idx));
                    resolved.push(result);
                }
                _ => {
                    // Coerce a non-Int scalar array index (`"0"`, `0e0`, `0/1`)
                    // to its Int so the key tuple (`:k`/`:p`) and the element
                    // lookup use `(0,0,0)`, not the raw `("0",0e0,0.0)`. Only do
                    // this when the current level is an array — a Str into a hash
                    // is a genuine key and must stay a string.
                    let coerced = if matches!(current.view(), ValueView::Array(..))
                        && matches!(
                            idx.view(),
                            ValueView::Str(_)
                                | ValueView::Num(_)
                                | ValueView::Rat(..)
                                | ValueView::FatRat(..)
                                | ValueView::BigRat(..)
                        ) {
                        match idx.view() {
                            ValueView::Num(f) if f >= 0.0 => Value::int(f as i64),
                            ValueView::Rat(n, d) if d != 0 => Value::int(n / d),
                            ValueView::Str(s) => s
                                .parse::<i64>()
                                .map(Value::int)
                                .unwrap_or_else(|_| idx.clone()),
                            _ => idx.clone(),
                        }
                    } else {
                        idx.clone()
                    };
                    current = multidim_index(&current, std::slice::from_ref(&coerced));
                    resolved.push(coerced);
                }
            }
        }
        Ok(resolved)
    }

    /// Handle dynamic adverb (:$delete) on multidim index.
    /// Args: [var_name_string, adverb_name, adverb_value, dim0, dim1, ...]
    pub(super) fn builtin_multidim_dynamic_adverb(
        &mut self,
        args: &mut [Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 4 {
            return Err(RuntimeError::new(
                "__mutsu_multidim_dynamic_adverb expects var_name, name, value, and indices",
            ));
        }
        let var_name = args[0].to_string_value();
        let adverb_value = args[2].truthy();
        let raw_indices = args[3..].to_vec();

        let target = self.env.get(&var_name).cloned().unwrap_or(Value::NIL);
        let indices = self.resolve_multidim_indices(&target, &raw_indices)?;

        if adverb_value {
            // Multi-result mode for Whatever/list indices
            if has_multi_indices(&indices) {
                let mut leaves = Vec::new();
                multidim_collect_leaves(&target, &indices, &[], &mut leaves);
                if let Some(t) = self.env.get_mut(&var_name) {
                    multidim_delete(t, &indices);
                    self.writeback_multidim_var_to_local(&var_name);
                }
                let values: Vec<Value> =
                    leaves.into_iter().map(|(_, v)| array_to_list(v)).collect();
                return Ok(Value::array(values));
            }
            // A non-existent (out-of-range) element deletes to `Nil`, not the
            // `Any` hole-value that `multidim_delete` returns for a missing slot.
            if multidim_index(&target, &indices).is_nil() {
                return Ok(Value::NIL);
            }
            let Some(target) = self.env.get_mut(&var_name) else {
                return Ok(Value::NIL);
            };
            let result = multidim_delete(target, &indices);
            self.writeback_multidim_var_to_local(&var_name);
            // The deleted element is returned decontainerized — an Array leaf
            // (`[314]`) comes back as a List (`(314,)`), matching raku's
            // multi-dim `:delete` (the test's `$resnona` is `$result.List`).
            Ok(array_to_list(result))
        } else {
            Ok(multidim_index(&target, &indices))
        }
    }

    /// Handle :kv/:k/:v/:p with dynamic :$delete on multidim index.
    /// Args: [var_name_str, adverb_name, delete_flag, dim0, dim1, ...]
    pub(super) fn builtin_multidim_subscript_adverb_dyn(
        &mut self,
        args: &mut [Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 4 {
            return Err(RuntimeError::new(
                "__mutsu_multidim_subscript_adverb_dyn expects var_name, adverb, delete, and indices",
            ));
        }
        let var_name = args[0].to_string_value();
        let adverb = args[1].to_string_value();
        let do_delete = args[2].truthy();
        let raw_indices = args[3..].to_vec();

        let target = self.env.get(&var_name).cloned().unwrap_or(Value::NIL);
        let indices = self.resolve_multidim_indices(&target, &raw_indices)?;

        // Multi-result mode for Whatever/list indices
        if has_multi_indices(&indices) {
            // Collect leaves before potentially deleting
            let mut leaves = Vec::new();
            multidim_collect_leaves(&target, &indices, &[], &mut leaves);
            if do_delete && let Some(t) = self.env.get_mut(&var_name) {
                multidim_delete(t, &indices);
                self.writeback_multidim_var_to_local(&var_name);
            }
            let mut out = Vec::new();
            for (path, value) in leaves {
                let exists = !value.is_nil();
                let key = leaf_key_tuple(path);
                match adverb.as_str() {
                    "k" => {
                        if exists {
                            out.push(key);
                        }
                    }
                    "kv" => {
                        if exists {
                            out.push(key);
                            out.push(array_to_list(value));
                        }
                    }
                    "p" => {
                        if exists {
                            out.push(Value::value_pair(key, array_to_list(value)));
                        }
                    }
                    "v" => {
                        if exists {
                            out.push(array_to_list(value));
                        }
                    }
                    _ => out.push(value),
                }
            }
            return Ok(Value::array(out));
        }

        // Determine existence from a pre-delete read: a non-existent element
        // reads as `Nil`, whereas `multidim_delete` returns the `Any` hole-value
        // for an out-of-range slot, which would wrongly look "present".
        let read_value = multidim_index(&target, &indices);
        let exists = !read_value.is_nil();
        let value = if do_delete {
            if exists && let Some(target) = self.env.get_mut(&var_name) {
                let r = multidim_delete(target, &indices);
                self.writeback_multidim_var_to_local(&var_name);
                r
            } else {
                read_value
            }
        } else {
            read_value
        };

        let key = make_key_tuple(&indices);

        match adverb.as_str() {
            "k" => Ok(if exists { key } else { Value::NIL }),
            "kv" => {
                if exists {
                    let v = array_to_list(value);
                    Ok(Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(vec![key, v])),
                        ArrayKind::List,
                    ))
                } else {
                    Ok(Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(vec![])),
                        ArrayKind::List,
                    ))
                }
            }
            "p" => {
                if exists {
                    let v = array_to_list(value);
                    Ok(Value::value_pair(key, v))
                } else {
                    Ok(Value::NIL)
                }
            }
            "v" => {
                if exists {
                    Ok(array_to_list(value))
                } else {
                    Ok(Value::NIL)
                }
            }
            _ => Ok(value),
        }
    }

    /// Handle :exists:kv/:exists:p with dynamic :$delete on multidim index.
    /// Args: [var_name_str, negated_bool, delete_flag, adverb_name, dim0, dim1, ...]
    pub(super) fn builtin_multidim_exists_adverb_dyn(
        &mut self,
        args: &mut [Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 5 {
            return Err(RuntimeError::new(
                "__mutsu_multidim_exists_adverb_dyn requires 5+ args",
            ));
        }
        let var_name = args[0].to_string_value();
        let negated = args[1].truthy();
        let do_delete = args[2].truthy();
        let adverb = args[3].to_string_value();
        let raw_indices = args[4..].to_vec();

        // First get value (need to read before potentially deleting)
        let target_val = self.env.get(&var_name).cloned().unwrap_or(Value::NIL);
        let indices = self.resolve_multidim_indices(&target_val, &raw_indices)?;

        // Multi-result mode for Whatever/list indices
        if has_multi_indices(&indices) {
            let mut leaves = Vec::new();
            multidim_collect_leaves(&target_val, &indices, &[], &mut leaves);
            if do_delete && let Some(t) = self.env.get_mut(&var_name) {
                multidim_delete(t, &indices);
                self.writeback_multidim_var_to_local(&var_name);
            }
            let mut out = Vec::new();
            for (path, value) in leaves {
                let raw_exists = !value.is_nil();
                let exists = if negated { !raw_exists } else { raw_exists };
                let key = leaf_key_tuple(path);
                match adverb.as_str() {
                    "none" => out.push(Value::truth(exists)),
                    "kv" => {
                        if raw_exists {
                            out.push(key);
                            out.push(Value::truth(exists));
                        }
                    }
                    "p" => {
                        if raw_exists {
                            out.push(Value::value_pair(key, Value::truth(exists)));
                        }
                    }
                    "k" => {
                        if raw_exists {
                            out.push(key);
                        }
                    }
                    _ => out.push(Value::truth(exists)),
                }
            }
            return Ok(Value::array(out));
        }

        let value = multidim_index(&target_val, &indices);
        // Then delete if requested
        if do_delete && let Some(target) = self.env.get_mut(&var_name) {
            multidim_delete(target, &indices);
            self.writeback_multidim_var_to_local(&var_name);
        }

        let raw_exists = !value.is_nil();
        let exists = if negated { !raw_exists } else { raw_exists };
        let key = make_key_tuple(&indices);

        match adverb.as_str() {
            "none" => Ok(Value::truth(exists)),
            "kv" => {
                if raw_exists {
                    Ok(Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(vec![
                            key,
                            Value::truth(exists),
                        ])),
                        ArrayKind::List,
                    ))
                } else {
                    Ok(Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(vec![])),
                        ArrayKind::List,
                    ))
                }
            }
            "p" => {
                if raw_exists {
                    Ok(Value::value_pair(key, Value::truth(exists)))
                } else {
                    Ok(Value::NIL)
                }
            }
            "k" => {
                if raw_exists {
                    Ok(key)
                } else {
                    Ok(Value::NIL)
                }
            }
            _ => Ok(Value::truth(exists)),
        }
    }
}
