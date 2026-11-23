//! ADR-18752: `@a[$i] = $v` into a celled array that several threads share,
//! as a shared-side store (D1 + D2) instead of an exclusive one.
//!
//! Every store used to hold the cell's and the node's ADR-0068 stripes
//! exclusively for the whole store, so writers of disjoint slots of one shared
//! array handed the locks to one another on every store (`bench-threads`'
//! write section, #8069). An in-range store of a refcount-free word into a
//! plain `Array` excludes nothing but structural writes and live-node readers.
//! It takes the same two stripes on the shared side and swaps the slot word
//! (`value::array_shared_store`). Any other shape declines, and the caller
//! takes the ordinary path, which keeps its exclusive locking.

use super::*;
use crate::value::container_lock::{ContainerStructGuard, multi_mutator_threads_live};

impl Interpreter {
    /// Try the shared-side store for `IndexAssignExprNamed` on a positional
    /// `@name` target. On success the value and index are popped and the
    /// expression's result is pushed, exactly as the ordinary store does;
    /// `false` leaves the stack untouched.
    ///
    /// Taken only once a second mutator thread exists (before that no stripe
    /// is taken at all, ADR-18752 §3's pre-latch row), and only when the
    /// variable is a celled `Array` (a `:=`-bound or captured `@a` that
    /// threads share), the index is a non-negative `Int` and the value is a
    /// refcount-free word other than `Nil`.
    // Cost: O(1).
    pub(super) fn try_shared_in_range_store(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        target_slot: Option<u32>,
    ) -> bool {
        if !multi_mutator_threads_live() {
            return false;
        }
        let name = Self::const_str(code, name_idx);
        // A plain `@name` only: an attribute or dynamic twigil resolves
        // elsewhere, and a `$`-sigil target itemizes differently.
        if !name.starts_with('@')
            || name
                .as_bytes()
                .get(1)
                .is_none_or(|c| matches!(c, b'!' | b'.' | b'*' | b'?' | b'^'))
        {
            return false;
        }
        let n = self.stack.len();
        if n < 2 {
            return false;
        }
        let ValueView::Int(index) = self.stack[n - 1].view() else {
            return false;
        };
        let Ok(index) = usize::try_from(index) else {
            return false;
        };
        // The variable's root as `named_root_struct_guard` finds it: this
        // frame's slot, else the env (a captured `@a` a `start` block closes
        // over).
        let root = self
            .gate_local_slot_at(code, target_slot, name)
            .and_then(|slot| self.locals.get(slot))
            .filter(|v| !v.is_nil())
            .or_else(|| self.env().get(name));
        let Some(ValueView::ContainerRef(cell)) = root.map(Value::view) else {
            return false;
        };
        // The node the cell holds now. A reference is kept for the store, so
        // the node outlives it even if another path re-seats the cell
        // meanwhile; such a store lands in the detached node, which is one
        // ordering of that unsynchronized race (ADR-12814).
        let (node, kind) = match cell
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .view()
        {
            ValueView::Array(node, kind) => (node.clone(), kind),
            _ => return false,
        };
        let cell_addr = crate::gc::Gc::as_ptr(&cell) as usize;
        let node_addr = crate::gc::Gc::as_ptr(&node) as usize;
        let Some(_shared) = ContainerStructGuard::acquire_shared_pair(cell_addr, node_addr) else {
            return false;
        };
        let value = &self.stack[n - 2];
        if !crate::value::array_shared_store::store_word_in_range(&node, kind, index, value) {
            return false;
        }
        drop(_shared);
        self.stack.pop();
        let value = self.stack.pop().unwrap_or(Value::NIL);
        self.stack.push(Self::itemize_value(value));
        true
    }
}
