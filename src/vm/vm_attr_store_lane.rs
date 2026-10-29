//! The `$!x = <scalar>` store lane: an inert scalar assigned to a private
//! scalar attribute of the running method's `self`.
//!
//! The general store (`exec_set_local_op_inner`) treats the attribute slot like
//! any by-name lexical: it consults the registry for the declared type, writes
//! the value to the frame's env under both twigil spellings and then mirrors the
//! slot into `self`'s attribute cell. For an inert payload (`Int`, `Num`, `Str`,
//! `Bool`, ...) in a store with no bind, declaration or marker pending, all of
//! that reduces to one thing: the attribute cell gets the value (and so does the
//! frame's local slot). The cell is the store of record -- reads of an attribute
//! slot go to it ([`Interpreter::read_self_attr_cell_slot`]).
//!
//! The lane runs only when every one of those checks is settled without asking
//! the registry: the site's `(layout, slot)` cache ([`super::vm_attr_site`])
//! says where the attribute lives, and the layout's [`AttrStoreKind`] for that
//! slot says the declaration is untyped or a native/core scalar type the value
//! satisfies as is. Everything else (an `@`/`%` attribute, a class constraint, a
//! `where` clause, a bound or promoted cell, a role method, a cold site) takes
//! the general path, which also fills the site cache for the next store.
//!
//! [`AttrStoreKind`]: crate::value::AttrStoreKind

use super::*;

impl Interpreter {
    /// Try the lane for the `SetLocal` of slot `idx`; `true` when the store was
    /// done (the value is popped), `false` with the stack untouched.
    // Cost: O(1): a handful of flag and tag probes, one site-cache load and one
    // attribute-cell write; independent of frame size, env size and class depth.
    pub(super) fn exec_set_local_attr_fast(&mut self, code: &CompiledCode, idx: u32) -> bool {
        let idx = idx as usize;
        // A private scalar attribute slot (`$!x`). `$.x = v` inside a method is a
        // different store (it is checked against the accessor's `rw`ness).
        let Some((_, true, '$')) = code.local_attr_key(idx) else {
            return false;
        };
        if !self
            .stack
            .last()
            .is_some_and(Value::is_inert_scalar_store_payload)
            || !self.mark_ctx.store_flags_clear()
            || !self.locals[idx].is_plain_scalar_store_slot()
        {
            return false;
        }
        let Some(desc) = code.binding_descs.get(idx) else {
            return false;
        };
        if !self.set_local_scalar_metadata_lanes_clear(code, idx, desc) {
            return false;
        }
        let Some((layout_id, slot)) = code.attr_sites.cached(code.locals.len(), idx) else {
            return false;
        };
        if self.method_class_top_is_role() {
            return false;
        }
        let Some(self_val) = self.attr_access_self(code) else {
            return false;
        };
        let Some(attributes) = Self::self_instance_attrs(&self_val) else {
            return false;
        };
        // A user `subset` can redirect a constraint's name (`subset Int of ...`),
        // which the layout's kind cannot see.
        let allow_typed = self.registry().subsets.is_empty();
        let Some(v) = self.stack.pop() else {
            return false;
        };
        let local = v.clone();
        match attributes.store_slot_checked(layout_id, slot, v, allow_typed) {
            Ok(key) => {
                self.record_build_attr_write(&attributes, key);
                self.locals[idx] = local;
                true
            }
            Err(v) => {
                self.stack.push(v);
                false
            }
        }
    }
}
