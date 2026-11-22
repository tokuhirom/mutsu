//! The `@a[$i] = $v` lane ([`Interpreter::try_fast_array_element_assign`]) for
//! a container shared between threads (#8069, ADR-0068, ADR-17216).
//!
//! Once a second VM mutator thread exists, a plain `@` lexical a worker
//! captured is routed by the name-keyed cross-thread lanes
//! (`runtime_shared_vars.rs`), whose semantics the lane does not reproduce, so
//! it stands down for one. A `:=`-bound container is different: every alias
//! reaches it through one shared `ContainerCell`, the name-keyed lanes decline
//! for it (`assign_array_elem_to_shared_var`), and the full path's store is an
//! in-place write of the cell's node under ADR-0068's container lock. The lane
//! does exactly that write, under exactly that lock, so serving it does not
//! change what any thread can observe; it only skips the full path's
//! name-keyed preamble (~30,000 instructions per store measured on the
//! `bench-threads` write section, against ~1,100 for the lane).

use super::*;
use crate::value::ArrayData;
use crate::value::container_lock::ContainerStructGuard;

impl Interpreter {
    /// Whether the env binding the store names holds a `ContainerCell`
    /// (a `:=`-bound container), so the name-keyed cross-thread lanes are
    /// not its owner.
    // Cost: O(1), one env probe.
    pub(super) fn env_holds_container_cell(&self, code: &CompiledCode, name_idx: u32) -> bool {
        matches!(
            self.env().get_sym(code.const_sym(name_idx)).map(Value::view),
            Some(ValueView::ContainerRef(_))
        )
    }

    /// Whether a store of a plain value at index `i` of `items` is a plain
    /// slot write: in range or exactly one past the end (the append idiom),
    /// and the destination is not something the store writes THROUGH (a
    /// `:=`-bound cell, a `Proxy`, an itemized `Scalar` element, a varref
    /// back-reference, a bound-element marker), whose rules the full path owns.
    // Cost: O(1).
    pub(super) fn element_slot_takes_plain_store(items: &crate::gc::Gc<ArrayData>, i: usize) -> bool {
        if i > items.len() {
            return false;
        }
        if i == items.len() {
            return true;
        }
        match items.items().get(i).map(Value::view) {
            Some(
                ValueView::ContainerRef(_)
                | ValueView::Scalar(_)
                | ValueView::Proxy { .. }
                | ValueView::VarRef { .. },
            )
            | None => false,
            Some(ValueView::Pair(name, _)) => !name.as_str().starts_with("__mutsu_bound"),
            Some(_) => true,
        }
    }

    /// The lane's commit for a celled container while other threads may touch
    /// it: take the cell's container lock (cell and node stripes, as every
    /// writer and reader of the cell does), re-check under it that the cell
    /// still holds the node the lane's checks saw and that the slot still
    /// takes a plain store (another thread may have re-seated the cell or
    /// shrunk the array in between), then write the slot in place. Declines,
    /// with the stack untouched, when this thread already holds a guard that
    /// does not cover the cell and node, or a re-check fails.
    // Cost: O(1) amortized (an in-place slot write, or an append of one), plus
    // one or two uncontended stripe locks.
    pub(super) fn commit_celled_element_store_guarded(
        &mut self,
        cell: &crate::gc::Gc<crate::value::ContainerCell>,
        items: &crate::gc::Gc<ArrayData>,
        i: usize,
    ) -> Option<Result<(), RuntimeError>> {
        // The store opcode usually holds this exact guard already
        // (`named_root_struct_guard`, keyed on the variable's cell and node);
        // a nested acquisition is then `None`, and the held one is checked to
        // cover both keys instead.
        let guard = ContainerStructGuard::acquire_for_celled(cell);
        if guard.is_none()
            && !ContainerStructGuard::held_covers(&[
                crate::gc::Gc::as_ptr(cell) as usize,
                crate::gc::Gc::as_ptr(items) as usize,
            ])
        {
            return None;
        }
        let same_node = matches!(
            cell.lock().ok()?.view(),
            ValueView::Array(now, _) if crate::gc::Gc::ptr_eq(&now, items)
        );
        if !same_node || !Self::element_slot_takes_plain_store(items, i) {
            return None;
        }
        self.stack.pop();
        let val = self.stack.pop()?;
        let stored = Self::itemize_value_for_element_store(val.clone());
        // SAFETY: audited aliased in-place container write (see
        // `value::aliased_mut`). The container lock taken above excludes every
        // other writer and reader of this cell and node (ADR-0068), so no other
        // thread holds a borrow into the node; on this thread, `items` is a `Gc`
        // handle and no `ValueView` into the node is read across the write.
        let data: &mut ArrayData = unsafe { crate::value::gc_contents_mut(items) };
        data.store_element(i, stored);
        self.stack.push(Self::itemize_value(val));
        Some(Ok(()))
    }
}
