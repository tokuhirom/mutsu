//! The deferred vivification token's write path.
//!
//! A `:=` bind (or a `return-rw` operand) that reaches a not-yet-existent hash
//! key hands out a deferred `HashEntryRef` token instead of creating anything.
//! This module holds the two halves that turn a subscript into a step on that
//! token's path: [`Interpreter::deferred_path_step`], which classifies one
//! subscript, and [`Interpreter::try_deferred_token_index_assign`], which
//! materializes a whole element assignment made through a variable that still
//! holds a token.
//!
//! See [`crate::value::EntryStep`] for why the steps are typed rather than
//! stringified.

use super::*;

impl Interpreter {
    /// Element assignment through a variable that still holds a DEFERRED
    /// vivification token: `my %h; my $x := %h<g>; $x[0] = 'x'`.
    ///
    /// Neither the element nor the container that would hold it exists yet, so
    /// the ordinary handlers resolved the token to `Any` and assigned into
    /// nothing — the write was silently lost (`%h` stayed `{}`). Materialize
    /// both here: extend the token's path by the subscript chain, write through
    /// it (so each level is created as the container the next step asks for),
    /// and promote the bound variable to the shared `ContainerRef` cell
    /// installed at the token's own slot — the same materialization
    /// [`Interpreter::materialize_bound_slot_to_cell`] performs for a scalar
    /// write through a bound token. Afterwards the variable and the hash entry
    /// alias, so later writes through either side are mutually visible.
    ///
    /// `positional_flags` runs from the subscript closest to the variable
    /// outward, matching the stack layout every index-assign op shares:
    /// `[value, idx_outermost, ..., idx_innermost]` with the innermost on top.
    ///
    /// Returns `None` (leaving the stack untouched) for every shape it does not
    /// own: a non-token target, an already-materialized token, an eager token
    /// (its entry exists, so the ordinary resolve-and-assign path is correct),
    /// a `:=` bind RHS, or a slice/junction index.
    pub(super) fn try_deferred_token_index_assign(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        positional_flags: &[bool],
    ) -> Option<Result<(), RuntimeError>> {
        let depth = positional_flags.len();
        if depth == 0 || self.stack.len() < depth + 1 {
            return None;
        }
        let var_name = Self::const_str(code, name_idx).to_string();
        let slot = self.find_local_slot(code, &var_name)?;
        let token = self.locals.get(slot)?.clone();
        let ValueView::HashEntryRef { eager, .. } = token.view() else {
            return None;
        };
        // An eager token's entry already exists; resolving it is correct.
        if eager || !token.hash_entry_read().is_any_type_object() {
            return None;
        }
        let base = self.stack.len() - depth - 1;
        let raw_val = self.stack[base].clone();
        // A `:=` bind RHS arrives wrapped in a marker pair and installs a shared
        // cell at the element — a different mechanism, left to its own handler.
        if matches!(raw_val.view(), ValueView::Pair(name, _) if name == "__mutsu_bind_index_value")
        {
            return None;
        }
        // Indices, innermost (top of stack) first — the path order.
        let mut steps = Vec::with_capacity(depth);
        for (i, positional) in positional_flags.iter().enumerate() {
            let idx = &self.stack[self.stack.len() - 1 - i];
            // A slice / junction subscript names several elements at once; the
            // deferred path addresses exactly one.
            if matches!(
                idx.view(),
                ValueView::Array(..)
                    | ValueView::Junction { .. }
                    | ValueView::Seq(..)
                    | ValueView::Slip(..)
                    | ValueView::Range(..)
                    | ValueView::RangeExcl(..)
                    | ValueView::RangeExclStart(..)
                    | ValueView::RangeExclBoth(..)
                    | ValueView::GenericRange { .. }
                    | ValueView::Whatever
            ) {
                return None;
            }
            steps.push(Self::deferred_path_step(idx, *positional));
        }

        // Extend the token's path by the whole chain and write through it: the
        // walk-create builds each level as the container its step asks for and
        // inserts the element, exactly as a deeper deferred bind would have.
        let ValueView::HashEntryRef { root, path, .. } = token.view() else {
            unreachable!("probed a HashEntryRef above")
        };
        let mut extended_path = path.clone();
        extended_path.extend(steps);
        let extended = Value::hash_entry_ref(root.clone(), extended_path);
        extended.hash_entry_write(raw_val.clone());

        // The token's own slot now holds the outermost created container:
        // promote the bound variable to a shared cell over it so the two alias.
        let created = token.hash_entry_locate().and_then(|t| t.peek())?;
        match self.materialize_bound_slot_to_cell(code, slot, created) {
            Ok(true) => {}
            Ok(false) => return None,
            Err(e) => return Some(Err(e)),
        }
        for _ in 0..depth + 1 {
            self.stack.pop();
        }
        self.stack.push(Self::itemize_value(raw_val));
        Some(Ok(()))
    }

    /// The array index a subscript addresses, when it is positional (`[...]`)
    /// *and* the index converts to one. A positional subscript whose index is
    /// not a usable position (a negative, a `Whatever`, a slice, a
    /// `WhateverCode`) has no array slot to name, so it falls back to the
    /// associative treatment the path had before steps were typed rather than
    /// inventing an index.
    pub(super) fn positional_step(index: &Value, is_positional: bool) -> Option<usize> {
        is_positional.then(|| Self::index_to_usize(index)).flatten()
    }

    /// The deferred-path step a subscript contributes.
    pub(super) fn deferred_path_step(
        index: &Value,
        is_positional: bool,
    ) -> crate::value::EntryStep {
        match Self::positional_step(index, is_positional) {
            Some(i) => crate::value::EntryStep::Index(i),
            None => crate::value::EntryStep::Key(Value::hash_key_encode(index)),
        }
    }
}
