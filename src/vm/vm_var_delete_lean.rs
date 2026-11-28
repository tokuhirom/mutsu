//! The lean `%h{$k}:delete` for a declared `%` local (campaign B, #19294).
//!
//! `DeleteIndexNamed` carries the base container's compile-time slot. Its
//! dispatch takes an attribute-element snapshot, wraps the delete in the
//! package-body lexical seed, publishes a `%*ENV` edit, checks for a user
//! `postcircumfix`, takes the ADR-0068 structure guard, normalizes a key
//! sequence and enters the delete's home (a `:=`-bound variable's cell)
//! before `try_fast_hash_delete` resolves the variable by name in env:
//! ~7,800 instructions per delete.
//!
//! When the running chunk declares the slot as a plain user `%` lexical, none
//! of those steps can do anything, and this lane takes the lean element
//! store's checks (`vm_var_assign_element_lean`) instead:
//!
//! - the chunk: the slot is a plain user `%` lexical this chunk declares, not
//!   `our` or `state`, with no attribute key (so neither the attribute mirror
//!   nor the `%*ENV` publish applies);
//! - the process latches: no thread clone, atomic lane, second mutator thread
//!   (so the structure guard has nothing to exclude) or shared-vars lane;
//! - the variable: no `:=` pair, not readonly, no `is default`, no
//!   constraint, no sigilless alias that moves the delete;
//! - the container: an ordinary `Hash` node (a user `postcircumfix` only
//!   answers for an instance or a mixin) with no type meta, typed keys,
//!   default, bare values, storage cells or pseudo-stash flag;
//! - the key: a `Str` or `Int` (not a list, a `Whatever`, a range, a type
//!   object or an instance), written with `{ }`;
//! - the entry: absent, a plain value or an element container (not a bind
//!   token or a Proxy).
//!
//! It answers what `try_fast_hash_delete` answers for that shape: the removed
//! value, or the `Any` type object for an absent key. Anything else returns
//! `false` with the stack untouched, and the dispatch runs exactly as before.

use super::vm_var_assign_element_lean::LeanStoreNode;
use super::*;

impl Interpreter {
    /// Delete the entry of the `%` local in `slot` that the key on top of the
    /// stack names, leaving the removed value on the stack in place of the
    /// key. `true` = deleted; `false` = declined, stack untouched.
    // Cost: O(1) expected (one hash probe and one removal).
    pub(super) fn try_lean_local_hash_delete(
        &mut self,
        code: &CompiledCode,
        slot: u32,
        name_idx: u32,
        kind: SubscriptKind,
    ) -> bool {
        if kind != SubscriptKind::Associative {
            return false;
        }
        let slot = slot as usize;
        if slot >= self.locals.len() || !code.lean_hash_store_slot(slot) {
            return false;
        }
        if self.is_thread_clone()
            || crate::runtime::shared_store::atomic_lane_entries_exist()
            || crate::value::container_lock::multi_mutator_threads_live()
            || self.threads.shared_vars_active
        {
            return false;
        }
        // The key is read where it sits: every probe below takes `&self`, and
        // the hash node is reached through its own `Gc` handle, so the stack
        // is only touched once the delete has committed.
        let Some(top) = self.stack.last() else {
            return false;
        };
        let top_view = top.view();
        let int_key;
        let key: &str = match &top_view {
            ValueView::Str(key) => key.as_str(),
            ValueView::Int(n) => {
                int_key = super::vm_decimal_key::DecimalKey::of(*n);
                int_key.as_str()
            }
            _ => return false,
        };
        if !self.lean_store_variable_ok(code, slot, name_idx) {
            return false;
        }
        let Some((LeanStoreNode::Hash(hash), slot_cell)) = self.lean_store_slot_node(slot) else {
            return false;
        };
        if hash.has_type_meta()
            || hash.has_typed_keys()
            || hash.default.is_some()
            || hash.bare_values
            || hash.storage_cells
            || hash.pseudo_stash_6e
        {
            return false;
        }
        // A deferred bind token or a Proxy entry is the general delete's. An
        // element container (a `:=` binding, or one `.values` promoted) is
        // removed and answered as it is, as `try_fast_hash_delete` does.
        if let Some(entry) = hash.map.get(key)
            && (entry.is_hash_entry_ref_value() || entry.is_proxy_value())
        {
            return false;
        }
        let node = crate::gc::Gc::as_ptr(&hash) as usize;
        if !self.lean_store_alias_agrees(code.const_sym(name_idx), node, slot_cell.as_ref()) {
            return false;
        }
        // ---- commit ----
        // SAFETY: audited aliased in-place container write (see
        // `value::aliased_mut`), the removal `try_fast_hash_delete` makes
        // through the shared node. No borrow into this node is live across
        // the write: `hash` is a `Gc` handle, the `ValueView` and any cell
        // guard it came from are dropped above, and no second mutator thread
        // exists (`multi_mutator_threads_live` was false), so this thread is
        // the only writer of the node. The key borrows the VM stack, not the
        // node.
        let data: &mut crate::value::HashData = unsafe { crate::value::gc_contents_mut(&hash) };
        // An absent key deletes to `Any`: no `is default` and no element type
        // (both checked above).
        let removed = data
            .map
            .remove(key)
            .unwrap_or_else(|| Value::package(crate::symbol::wk::any()));
        self.stack.pop();
        self.stack.push(removed);
        true
    }
}
