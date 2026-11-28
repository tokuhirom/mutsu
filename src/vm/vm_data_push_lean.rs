//! The lean `@a.push($v)` for a declared `@` local (campaign B, #19294).
//!
//! `ArrayPush` reaches `exec_array_push_op` through the attribute prologue,
//! and that function asks, for every push, the questions the general push
//! needs: whether the argument aliases the target, the shared-thread lane, a
//! lazy or immutable or shaped or jagged target, a `Nil` decaying to the
//! element default, the declared element type and its native integer width,
//! a reference push, a cell-held target. Most of them go through by-name env
//! probes (`push_target`, `element_constraint_for_decl`), ~3,500 instructions
//! per push.
//!
//! For the plain case none of them can answer, and each is settled somewhere
//! cheaper. This is the push twin of the lean element store
//! (`vm_var_assign_element_lean`, #19306) and shares its checks:
//!
//! - the chunk: a plain user `@` lexical this chunk declares, not `our` or
//!   `state`, no attribute key (so the attribute prologue has nothing to do);
//! - the process latches: no thread clone, atomic lane, second mutator thread
//!   or shared-vars lane;
//! - the variable: no `:=` pair, not readonly, no `is default`, no
//!   constraint, no sigilless alias that moves the store;
//! - the container: an ordinary `Array` node (not a `List`, lazy, shaped or
//!   jagged), no type meta, native backing or default;
//! - the value: a plain immutable scalar (not `Nil`, a container, a `Proxy`,
//!   a `Slip` or a boot box), so neither the `Nil` default nor a reference
//!   push nor the alias check applies.
//!
//! Anything else returns `false` with the stack untouched, and
//! `exec_array_push_op` runs unchanged as the authority on semantics.

use super::vm_var_assign_element_lean::LeanStoreNode;
use super::*;

impl Interpreter {
    /// Append the value on top of the stack to the `@` local in `slot` when
    /// every condition in the module docs holds, leaving the array on the
    /// stack as `exec_array_push_op` does. `true` = pushed; `false` =
    /// declined, stack untouched.
    // Cost: O(1) amortized (an in-place append), plus O(1) expected latch and
    // symbol-keyed probes.
    pub(super) fn try_lean_local_array_push(
        &mut self,
        code: &CompiledCode,
        slot: u32,
        name_idx: u32,
    ) -> bool {
        let slot = slot as usize;
        if slot >= self.locals.len() || !code.lean_element_store_slot(slot) {
            return false;
        }
        if self.is_thread_clone()
            || crate::runtime::shared_store::atomic_lane_entries_exist()
            || crate::value::container_lock::multi_mutator_threads_live()
            || self.threads.shared_vars_active
        {
            return false;
        }
        match self.stack.last() {
            Some(top) if Self::lean_store_value_is_plain(top) => {}
            _ => return false,
        }
        if !self.lean_store_variable_ok(code, slot, name_idx) {
            return false;
        }
        let Some((LeanStoreNode::Array(items), cell)) = self.lean_store_slot_node(slot) else {
            return false;
        };
        let array = match cell.as_ref() {
            Some(cell) => cell.load(),
            None => self.locals[slot].clone(),
        };
        if !matches!(array.view(), ValueView::Array(_, crate::value::ArrayKind::Array)) {
            return false;
        }
        if items.has_type_meta()
            || items.has_native_backing()
            || items.default.is_some()
            || items.shape.is_some()
            || items.row_view
            || items.bound_slice
        {
            return false;
        }
        let node = crate::gc::Gc::as_ptr(&items) as usize;
        if !self.lean_store_alias_agrees(code.const_sym(name_idx), node, cell.as_ref()) {
            return false;
        }
        // ---- commit ----
        let Some(val) = self.stack.pop() else {
            return false;
        };
        // SAFETY: audited aliased in-place container write (see
        // `value::aliased_mut`), the append `exec_array_push_op` makes through
        // `with_array_inplace`. No borrow into this node is live across the
        // write: `items` is a `Gc` handle, the views and the cell guard it came
        // from are dropped above, and no second mutator thread exists
        // (`multi_mutator_threads_live` was false), so this thread is the only
        // writer of the node.
        let data: &mut crate::value::ArrayData = unsafe { crate::value::gc_contents_mut(&items) };
        data.push(Self::itemize_value(val));
        self.stack.push(array);
        true
    }
}
