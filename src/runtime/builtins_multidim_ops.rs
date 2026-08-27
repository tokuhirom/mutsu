//! Shared support for the multidim (`;`-separated) subscript builtins, plus
//! the `:delete` handler.
//!
//! The adverb handlers themselves live in the two sibling modules
//! `builtins_multidim_subscript_adverb` (`:v`/`:k`/`:p`/`:kv` and their
//! dynamic-`:$delete` twins) and `builtins_multidim_exists_adverb`
//! (`:exists` and its secondary adverbs). Everything three of them share --
//! index resolution, the miss-shape rules, the local-slot writeback -- is
//! here.

use super::*;
use crate::value::ArrayKind;

use super::builtins_multidim::{
    array_to_list, has_multi_indices, multidim_collect_leaves, multidim_delete, multidim_index,
};

/// The empty-list `()` a missing multidim leaf reports for a plain
/// (non-negated) value adverb (`:v`/`:k`/`:p`/`:kv`) -- matching raku, which
/// answers `()` rather than `Nil` for a hole under these adverbs.
pub(super) fn multidim_empty_list() -> Value {
    Value::array_with_kind(
        crate::gc::Gc::new(crate::value::ArrayData::new(vec![])),
        ArrayKind::List,
    )
}

/// What a plain (non-negated) `:v`/`:k`/`:p` adverb reports for a missing
/// leaf, which differs by WHICH kind of "missing" it is:
///
/// - An in-bounds Array hole (`ArrayData::hole_at`) reports `raw_value`
///   itself as the non-`Nil` hole marker (e.g. `Package("Any")`), never
///   `Value::NIL` -- this is the ticket's own repro (`my @a[2;2]; @a[0;1]`)
///   and reports the empty list `()`, matching plain `raku`.
/// - Everything else that fails to resolve -- a missing Hash key, OR an
///   out-of-range/non-numeric Array coordinate -- reports the literal
///   `Value::NIL` (no hole marker of its own to carry), and answers `Nil`,
///   not `()`. This is NOT what plain (non-PREVIEW) `raku` does for an
///   out-of-range Array coordinate, but it IS what the vendored roast tests
///   pin for both cases under `v6.e.PREVIEW`
///   (`roast/S32-hash/multislice-6e.t`'s "gives Nil" assertions on a missing
///   key, and `roast/S32-array/multislice-6e.t`'s identical assertions on an
///   out-of-range index into a plain nested/autoviv array) -- roast is the
///   authoritative spec (see CLAUDE.md), so mutsu (which does not currently
///   branch multidim-adverb behavior on the language-version pragma) matches
///   the roast-pinned answer.
///
/// `:kv` does not use this -- it is always `()` for every kind of miss.
pub(super) fn multidim_missing_result(raw_value: &Value) -> Value {
    if raw_value.is_nil() {
        Value::NIL
    } else {
        multidim_empty_list()
    }
}

impl Interpreter {
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
            let values: Vec<Value> = leaves
                .into_iter()
                .map(|(_, v, _)| array_to_list(v))
                .collect();
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
    pub(super) fn resolve_multidim_indices(
        &mut self,
        target: &Value,
        indices: &[Value],
    ) -> Result<Vec<Value>, RuntimeError> {
        // A Range (or Seq) in a subscript dimension is a multi-key slice
        // (`%h{1;1..3}`), so expand it to its element list up front — then the
        // existing multi-index path (`has_multi_indices` / collect-leaves) walks
        // each key, exactly as it already does for a comma list (`%h{1;2,3}`).
        // An unbounded-end range (`1^..*`) is deferred to the per-level walk
        // below, which knows the axis length to clamp against.
        let indices: Vec<Value> = indices
            .iter()
            .map(|idx| {
                if (idx.is_range() || matches!(idx.view(), ValueView::Seq(_)))
                    && !crate::runtime::utils::subscript_range_end_unbounded(idx)
                {
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
                _ if crate::runtime::utils::subscript_range_end_unbounded(idx) => {
                    // Unbounded-end range dimension (`1^..*`): clamp to this
                    // level's length now that it is known.
                    let len = match current.view() {
                        ValueView::Array(items, ..) => items.len(),
                        _ => 0,
                    };
                    let expanded = Value::array(
                        crate::runtime::utils::expand_unbounded_range_dim(idx, len)
                            .unwrap_or_default(),
                    );
                    current = multidim_index(&current, std::slice::from_ref(&expanded));
                    resolved.push(expanded);
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
}
