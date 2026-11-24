//! A chained positional store that reaches a defined scalar part way down
//! (`@c[0][0][0] = 9` with `@c[0]` holding `1`), #18960.
//!
//! rakudo stores through `AT-POS`, and a defined scalar's (or Pair's)
//! `AT-POS(0)` is its own container. So every `[0]` level applied to such a
//! value addresses the same container again, and the whole tail collapses
//! onto the level that reached it: `@c[0][0][0] = 9` is `@c[0][0] = 9`, which
//! assigns `@c[0]`. Any other integer index on that value is out of range
//! `0..0`. The deep chained store collapses the chain first and hands the
//! shorter one to the store that already implements the two-level rule.

use super::*;

/// How a chained positional store over a defined scalar collapses.
pub(super) enum IndexZeroChain {
    /// The chain reaches no defined scalar before its last level, or a level
    /// after it is not positional: store it as written.
    AsWritten,
    /// The first `n` levels reach the scalar; every later level is a `[0]`,
    /// so the store is the one of the first `n + 1` levels (`n = 0`: the
    /// variable itself holds the scalar).
    Keep(usize),
}

/// How many levels of a `depth`-level chain the store keeps once its first
/// `kept` levels reach a defined scalar: one `[0]` over the scalar (the
/// two-level and deep stores implement that rule) unless that is the whole
/// chain again, then just the scalar's own level; none when the variable
/// itself holds the scalar.
// Cost: O(1).
pub(super) fn collapsed_chain_len(kept: usize, depth: usize) -> usize {
    match kept {
        0 => 0,
        _ if kept + 1 < depth => kept + 1,
        _ => kept,
    }
}

impl Interpreter {
    /// Classify the chain `indices` (root level first) over `root`, read-only.
    // Cost: O(d), d = subscript depth.
    pub(super) fn index_zero_chain(
        root: Option<&Value>,
        indices: &[String],
        positional: &[bool],
    ) -> Result<IndexZeroChain, RuntimeError> {
        let depth = indices.len();
        let mut current = root.cloned();
        for level in 0..depth {
            let Some(value) = current.as_ref() else {
                break;
            };
            let value = value.deref_container();
            if Self::scalar_slot_position(&value, positional[level], &indices[level]).is_some() {
                // Every level from here indexes the same scalar.
                for rest in level..depth {
                    match Self::scalar_slot_position(&value, positional[rest], &indices[rest]) {
                        Some(Ok(())) => {}
                        Some(Err(err)) => return Err(err),
                        None => return Ok(IndexZeroChain::AsWritten),
                    }
                }
                return Ok(IndexZeroChain::Keep(level));
            }
            current = Self::subscript_peek_step(&value, &indices[level], positional[level]);
        }
        Ok(IndexZeroChain::AsWritten)
    }

    /// Store `val` through `kept_indices` (root level first), the levels a
    /// collapsed chain keeps (see [`collapsed_chain_len`]); none assigns the
    /// variable itself, which holds the scalar.
    // Cost: O(1) besides the shorter store.
    pub(super) fn store_collapsed_index_zero_chain(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        target_slot: Option<u32>,
        positional_flags_idx: u32,
        kept_indices: &[Value],
        val: Value,
    ) -> Result<(), RuntimeError> {
        let depth = kept_indices.len();
        if depth == 0 {
            return self.store_index_zero_into_var(code, name_idx, target_slot, 0, val);
        }
        // Stack (bottom→top): value, the deepest kept index, ..., the root's.
        self.stack.push(val);
        for index in kept_indices.iter().rev() {
            self.stack.push(index.clone());
        }
        if depth == 2 {
            let flag = |level: usize| match code.constants[positional_flags_idx as usize].view() {
                ValueView::Array(flags, _) => flags.items().get(level).is_none_or(Value::truthy),
                _ => true,
            };
            self.exec_index_assign_expr_nested_op(code, name_idx, flag(1), flag(0), target_slot)
        } else {
            self.exec_index_assign_deep_nested_op(
                code,
                name_idx,
                depth as u32,
                positional_flags_idx,
                target_slot,
            )
        }
    }
}
