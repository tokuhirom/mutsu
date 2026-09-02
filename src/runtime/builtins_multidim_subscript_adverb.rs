//! Value adverbs (`:v`/`:k`/`:p`/`:kv`, their negated forms, and the
//! dynamic-`:$delete` twins) on a multidim (`;`-separated) subscript.
//!
//! Split out of `builtins_multidim_ops`, which keeps the shared index
//! resolution, miss-shape rules and local-slot writeback these handlers call.

use super::*;
use crate::value::ArrayKind;

use super::builtins_multidim::{
    array_to_list, has_multi_indices, leaf_key_tuple, make_key_tuple, multidim_collect_leaves,
    multidim_delete, multidim_index, multidim_index_with_hole,
};
use super::builtins_multidim_ops::{multidim_empty_list, multidim_missing_result};

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

        let (value, is_hole) = multidim_index_with_hole(target, &indices);
        let key = make_key_tuple(&indices);
        // Canonical hole predicate (ADR-0049 §1.6/§4 slice 5): a leaf
        // "exists" only if it is neither a missing Hash key (`Nil`) nor an
        // Array hole per `ArrayData::hole_at`.
        let exists = !value.is_nil() && !is_hole;

        // A missing leaf (`!exists`) reports differently depending on WHICH
        // kind of "missing" it is, and on the adverb -- see
        // `multidim_missing_result`'s doc comment for the full rule and its
        // `raku`/roast evidence.
        //
        // The negated forms (`:!v`/`:!k`/`:!p`/`:!kv`) are a genuine Rakudo
        // multidim quirk, also verified directly: unlike the single-dimension
        // form (where each negated adverb keeps its own key/pair/kv shape and
        // only suppresses the *suppression*, e.g. `:!k` on a hole still
        // reports the key), real Rakudo's multidim `[;]` postcircumfix
        // collapses ALL FOUR negated adverbs to plain value access --
        // `@a[i;j]:!k` and `@a[i;j]:!p` both answer the same raw value
        // `:!v` would, never the key or a pair, for both a filled slot and a
        // hole.
        match adverb.as_str() {
            "k" => Ok(if exists {
                key
            } else {
                multidim_missing_result()
            }),
            "kv" => {
                if exists {
                    let v = array_to_list(value);
                    Ok(Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(vec![key, v])),
                        ArrayKind::List,
                    ))
                } else {
                    Ok(multidim_empty_list())
                }
            }
            "p" => {
                if exists {
                    let v = array_to_list(value);
                    Ok(Value::value_pair(key, v))
                } else {
                    Ok(multidim_missing_result())
                }
            }
            "v" => {
                if exists {
                    Ok(array_to_list(value))
                } else {
                    Ok(multidim_missing_result())
                }
            }
            "not-k" | "not-kv" | "not-p" | "not-v" => Ok(array_to_list(value)),
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
        for (path, value, is_hole) in leaves {
            // Canonical hole predicate (ADR-0049 §1.6/§4 slice 5): a leaf
            // "exists" only if it is neither a missing Hash key (`Nil`) nor
            // an Array hole per `ArrayData::hole_at`.
            let exists = !value.is_nil() && !is_hole;
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
                // Self-consistency with `builtin_multidim_subscript_adverb`'s
                // single-coordinate form above (raku itself has no oracle for
                // this Whatever/list-index combination -- it throws X::NYI --
                // so the two mutsu code paths must at least agree with each
                // other): a negated adverb keeps every leaf, filled or hole,
                // reported as the raw value, never the key or a pair.
                "not-k" | "not-kv" | "not-p" | "not-v" => {
                    out.push(array_to_list(value));
                }
                _ => out.push(value),
            }
        }
        Ok(Value::array(out))
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
                let values: Vec<Value> = leaves
                    .into_iter()
                    .map(|(_, v, _)| array_to_list(v))
                    .collect();
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
            for (path, value, is_hole) in leaves {
                // Canonical hole predicate (ADR-0049 §1.6/§4 slice 5) -- see
                // `multidim_exists_adverb_multi` in the sibling exists module.
                let exists = !value.is_nil() && !is_hole;
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
                    // Self-consistency with the plain (non-dyn) handlers above:
                    // a negated adverb keeps every leaf, filled or hole,
                    // reported as the raw value.
                    "not-k" | "not-kv" | "not-p" | "not-v" => {
                        out.push(array_to_list(value));
                    }
                    _ => out.push(value),
                }
            }
            return Ok(Value::array(out));
        }

        // Determine existence from a pre-delete read: a non-existent element
        // reads as `Nil` or an `ArrayData::hole_at` hole (ADR-0049 §1.6/§4
        // slice 5), whereas `multidim_delete` returns the `Any` hole-value
        // for an out-of-range slot, which would wrongly look "present".
        let (read_value, read_is_hole) = multidim_index_with_hole(&target, &indices);
        let exists = !read_value.is_nil() && !read_is_hole;
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

        // Same shape/hole rules as `builtin_multidim_subscript_adverb` above
        // (see `multidim_missing_result`'s doc comment): a missing leaf
        // reports `()` for an in-bounds Array hole, `Nil` for everything
        // else that fails to resolve, `:kv` is always `()`, and every
        // negated adverb collapses to plain (raw) value access.
        match adverb.as_str() {
            "k" => Ok(if exists {
                key
            } else {
                multidim_missing_result()
            }),
            "kv" => {
                if exists {
                    let v = array_to_list(value);
                    Ok(Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(vec![key, v])),
                        ArrayKind::List,
                    ))
                } else {
                    Ok(multidim_empty_list())
                }
            }
            "p" => {
                if exists {
                    let v = array_to_list(value);
                    Ok(Value::value_pair(key, v))
                } else {
                    Ok(multidim_missing_result())
                }
            }
            "v" => {
                if exists {
                    Ok(array_to_list(value))
                } else {
                    Ok(multidim_missing_result())
                }
            }
            "not-k" | "not-kv" | "not-p" | "not-v" => Ok(array_to_list(value)),
            _ => Ok(value),
        }
    }
}
