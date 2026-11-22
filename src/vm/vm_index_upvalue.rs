//! `OpCode::IndexUpvalue`: a captured array's element read, `@a[$i]` in a
//! closure, without cloning the array or taking a lock when another thread
//! shares it (ADR-18637).
//!
//! The unfused pair -- `GetUpvalue` then `Index` -- clones the captured
//! array's node out of its cell (a refcount increment and decrement on the
//! node's header) and reads it under ADR-0068's stripe. With several threads
//! reading one shared array, every one of those writes bounces the same cache
//! lines between cores, so reads got slower the more threads read
//! (ADR-8069 §1). The fused op first asks the cell for its published snapshot
//! (`ContainerCell::snapshot_element`), which touches no shared line; when
//! there is none, or the node has changed since, it runs exactly the unfused
//! pair and counts the slow read, so a read-mostly array earns a snapshot.

use super::*;

impl Interpreter {
    /// Execute `IndexUpvalue { index, name_idx }` with the subscript on top of
    /// the stack.
    // Cost: O(1) for a snapshot hit; otherwise as `GetUpvalue` then `Index`.
    pub(super) fn exec_index_upvalue_op(
        &mut self,
        code: &CompiledCode,
        index: u32,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        if let Some(elem) = self.index_upvalue_snapshot_element(index) {
            *self
                .stack
                .last_mut()
                .expect("IndexUpvalue: subscript on the stack") = elem;
            return Ok(());
        }
        // The unfused pair: push the upvalue's value under the subscript, then
        // index it exactly as `Index { is_positional: true }` does.
        let subscript = self
            .stack
            .pop()
            .expect("IndexUpvalue: subscript on the stack");
        let mut get_ip = 0usize;
        self.exec_get_upvalue_op(code, index, name_idx, &mut get_ip)?;
        self.stack.push(subscript);
        if self.rw_return_context {
            self.exec_index_autovivify_lazy_op(true, true)?;
        } else {
            self.exec_index_op_with_positional(true)?;
            self.decont_native_pos_ref_on_top()?;
        }
        self.with_shared_upvalue_cell(index, |cell| {
            cell.note_slow_indexed_read();
            Some(())
        });
        Ok(())
    }

    /// The captured cell behind upvalue `index`, when it is one a snapshot
    /// read can serve: a plain (not itemized) `ContainerRef`, read while a
    /// second mutator thread exists, outside the escaping-`our` capture shape
    /// whose upvalue slot `exec_get_upvalue_op` ignores.
    // Cost: O(1).
    #[inline]
    fn with_shared_upvalue_cell<R>(
        &self,
        index: u32,
        f: impl FnOnce(&crate::value::ContainerCell) -> Option<R>,
    ) -> Option<R> {
        if !crate::value::container_lock::multi_mutator_threads_live()
            || !self.lexicals.escaping_our_lexical_names.is_empty()
        {
            return None;
        }
        let value = self.upvalues.get(index as usize)?.as_ref()?;
        if value.container_ref_is_itemized() {
            return None;
        }
        let ValueView::ContainerRef(cell) = value.view() else {
            return None;
        };
        f(&cell)
    }

    /// The element the subscript on top of the stack selects, read from the
    /// captured cell's snapshot, or `None` for the unfused path.
    // Cost: O(1).
    #[inline]
    fn index_upvalue_snapshot_element(&self, index: u32) -> Option<Value> {
        if self.rw_return_context || self.dispatch.skip_postcircumfix_overload {
            return None;
        }
        let ValueView::Int(i) = self.stack.last()?.view() else {
            return None;
        };
        let elem = self.with_shared_upvalue_cell(index, |cell| cell.snapshot_element(i))?;
        // A user `postcircumfix:<[ ]>` candidate outranks the core subscript
        // for the arrays it accepts: leave the read to the general path.
        if self.user_subscript_routine_may_answer(true) {
            return None;
        }
        Some(elem)
    }
}
