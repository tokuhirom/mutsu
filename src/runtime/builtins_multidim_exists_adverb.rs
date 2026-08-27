//! `:exists` (with its secondary `:k`/`:v`/`:kv`/`:p` adverbs, negation, and
//! the dynamic-`:$delete` twin) on a multidim (`;`-separated) subscript.
//!
//! Split out of `builtins_multidim_ops`, which keeps the shared index
//! resolution and local-slot writeback these handlers call.

use super::*;
use crate::value::ArrayKind;

use super::builtins_multidim::{
    has_multi_indices, leaf_key_tuple, make_key_tuple, multidim_collect_leaves, multidim_delete,
    multidim_index_with_hole,
};

impl Interpreter {
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

        let (value, is_hole) = multidim_index_with_hole(target, &indices);
        // Canonical hole predicate (ADR-0049 §1.6/§4 slice 5): an unassigned
        // array cell (per `ArrayData::hole_at`, which recognizes both the
        // untyped `Any` marker and a typed array's own element-type marker,
        // and consults `initialized` so an explicitly-assigned `Any`/type
        // object is NOT a hole) or a missing Hash key (`Value::NIL`) does not
        // exist yet -- for shaped AND non-shaped (autoviv) multidim arrays
        // alike.
        let raw_exists = !value.is_nil() && !is_hole;
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
        for (path, value, is_hole) in leaves {
            // Canonical hole predicate (ADR-0049 §1.6/§4 slice 5): a
            // deleted/uninitialized array slot (per `ArrayData::hole_at`,
            // which also recognizes a typed array's own element-type gap
            // marker and consults `initialized` so an explicitly-assigned
            // `Any`/type-object value is NOT treated as a hole) or a missing
            // Hash key (`Value::NIL`) reports False, not True.
            let raw_exists = !value.is_nil() && !is_hole;
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
            for (path, value, is_hole) in leaves {
                // Canonical hole predicate (ADR-0049 §1.6/§4 slice 5) -- see
                // `multidim_exists_adverb_multi` above.
                let raw_exists = !value.is_nil() && !is_hole;
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

        let (value, is_hole) = multidim_index_with_hole(&target_val, &indices);
        // Then delete if requested
        if do_delete && let Some(target) = self.env.get_mut(&var_name) {
            multidim_delete(target, &indices);
            self.writeback_multidim_var_to_local(&var_name);
        }

        // Canonical hole predicate (ADR-0049 §1.6/§4 slice 5) -- see
        // `multidim_exists_adverb_multi` above.
        let raw_exists = !value.is_nil() && !is_hole;
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
