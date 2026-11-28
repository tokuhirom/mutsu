//! The lean `%h{$k}++` / `%h{$k}--` / `++%h{$k}` / `--%h{$k}` for a declared
//! `%` local (campaign B, #19294): the word-frequency idiom.
//!
//! The four `*IncrementIndex` / `*DecrementIndex` opcodes carry the base
//! container's compile-time slot. Their dispatch wraps the step in the
//! package-body lexical seed and then tries `try_fast_hash_element_incdec`,
//! whose guards (`fast_hash_lane_root_is_env`, `plain_hash_lane_target`)
//! resolve the variable by name in env: a unit-lexical cell probe, a
//! lexsub-alias probe, an `our` mirror probe, three env reads, the readonly
//! flag and the declared constraint, ~3,000 instructions per step.
//!
//! When the running chunk declares the slot as a plain user `%` lexical, the
//! slot is the authoritative place of the hash (ADR-14632 §2.1, §2.4), and
//! this lane takes the lean element store's checks
//! (`vm_var_assign_element_lean`) instead:
//!
//! - the chunk: the slot is a plain user `%` lexical this chunk declares, not
//!   `our` or `state`, with no attribute key;
//! - the process latches: no thread clone, atomic lane, second mutator thread,
//!   shared-vars lane or user `postcircumfix` that could answer;
//! - the variable: no `:=` pair, not readonly, no `is default`, no
//!   constraint, no sigilless alias that moves the store;
//! - the container: an ordinary `Hash` node with no type meta, typed keys,
//!   default, bare values, storage cells or pseudo-stash flag;
//! - the key and the element: a `Str` or `Int` key, and an element that is
//!   absent (it counts from `Int` 0, as the generic op does: no default and
//!   no element type, both checked above) or a plain small `Int` whose step
//!   does not overflow.
//!
//! Anything else returns `false` with the stack untouched, and the dispatch
//! runs exactly as before.

use super::vm_var_assign_element_lean::LeanStoreNode;
use super::*;

impl Interpreter {
    /// Step the element of the `%` local in `slot` that the key on top of the
    /// stack names, leaving the old (`return_new == false`) or new value on
    /// the stack in place of the key. `true` = stepped; `false` = declined,
    /// stack untouched.
    // Cost: O(1) expected (one hash probe and one in-place write or insert),
    // plus O(k) to copy a new key, k = key bytes.
    pub(super) fn try_lean_local_hash_incdec(
        &mut self,
        code: &CompiledCode,
        slot: u32,
        name_idx: u32,
        increment: bool,
        return_new: bool,
    ) -> bool {
        let slot = slot as usize;
        if slot >= self.locals.len() || !code.lean_hash_store_slot(slot) {
            return false;
        }
        if self.is_thread_clone()
            || crate::runtime::shared_store::atomic_lane_entries_exist()
            || crate::value::container_lock::multi_mutator_threads_live()
            || self.threads.shared_vars_active
            || self.user_subscript_routine_may_answer(false)
        {
            return false;
        }
        // The key's holder is a clone of the stack word (a refcount bump), so
        // the borrowed key outlives the pop of the commit.
        let Some(key_holder) = self.stack.last().cloned() else {
            return false;
        };
        let int_key;
        let str_key;
        let key: &str = match key_holder.view() {
            ValueView::Str(key) => {
                str_key = key;
                str_key.as_str()
            }
            ValueView::Int(n) => {
                int_key = super::vm_decimal_key::DecimalKey::of(n);
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
        let old = match hash.map.get(key) {
            None => 0,
            Some(entry) => match entry.as_int() {
                Some(n) => n,
                // A container, a bind token, a Proxy, a big Int, a Num, a Str
                // that steps by `.succ`: the generic op's.
                None => return false,
            },
        };
        let new = if increment {
            old.checked_add(1)
        } else {
            old.checked_sub(1)
        };
        let Some(new) = new else {
            return false;
        };
        let node = crate::gc::Gc::as_ptr(&hash) as usize;
        if !self.lean_store_alias_agrees(code.const_sym(name_idx), node, slot_cell.as_ref()) {
            return false;
        }
        // ---- commit ----
        // SAFETY: audited aliased in-place container write (see
        // `value::aliased_mut`), the one the lean element store makes. No
        // borrow into this node is live across the write: `hash` is a `Gc`
        // handle, the `ValueView` and any cell guard it came from are dropped
        // above, and no second mutator thread exists
        // (`multi_mutator_threads_live` was false), so this thread is the only
        // writer of the node.
        let data: &mut crate::value::HashData = unsafe { crate::value::gc_contents_mut(&hash) };
        Value::hash_insert_through_str(&mut data.map, key, Value::int(new));
        self.stack.pop();
        self.stack
            .push(Value::int(if return_new { new } else { old }));
        true
    }
}
