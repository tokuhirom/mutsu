//! The lean element store for a declared `@` local (#19285).
//!
//! `@a[$i] = $v` compiles to `IndexAssignLocal { slot, .. }` when the running
//! code declares `@a` (ADR-14632 §2.5), and the store body already treats that
//! slot as the authoritative place of the container (§2.1, §2.4: a writer that
//! replaces the container by name and not in the slot is a bug). But the body
//! is reached through a preamble that answers the plain case only after asking,
//! by name, every question the general store needs: a bind expression, an
//! attribute mirror, a package-body seed, a user `postcircumfix` remap, and the
//! name-keyed probes of the older fast lane (`unit_lexical_container_cell`,
//! `unit_lexical_slot`, `our_package_container_key`, an env read of the
//! binding, the slot's constraint record). ~2,500 instructions per store.
//!
//! None of them is a property of the store. This lane answers them from where
//! they are settled:
//!
//! - the chunk (the slot is declared here, a plain user `@` lexical with no
//!   attribute key, not `our` or `state`, and declared untyped -- or left
//!   unrecorded, as a `:=` binding is, with no by-name constraint record);
//! - the process latches the place rule already uses (no cross-thread lane, not a thread clone, no second mutator thread, no
//!   user `postcircumfix:<[ ]>` that could answer) and no `:=` pair recorded
//!   on the variable; a sigilless alias record on the name only when it names
//!   a variable bound to the same node;
//! - the container's embedded metadata (an ordinary `Array`, no type meta,
//!   default or native backing, and an element slot that takes a plain store);
//! - the value and index on the stack (a non-negative `Int` index, a plain
//!   scalar value).
//!
//! Anything else returns `false` with the stack untouched, and the caller runs
//! the unchanged general store, which stays the authority on semantics.

use super::*;

impl Interpreter {
    /// Store the value on the stack into element `i` of the `@` local in
    /// `slot`, when every condition in the module docs holds. `true` = stored
    /// (the stack holds the store's result, as the general store leaves it);
    /// `false` = declined, stack untouched.
    // Cost: O(1) amortized (an in-place slot write, or an append of one).
    pub(super) fn try_lean_local_array_store(
        &mut self,
        code: &CompiledCode,
        slot: u32,
        name_idx: u32,
        index_first: bool,
    ) -> bool {
        let slot = slot as usize;
        if slot >= self.locals.len() || !code.lean_element_store_slot(slot) {
            return false;
        }
        if self.is_thread_clone()
            || crate::runtime::shared_store::atomic_lane_entries_exist()
            || crate::value::container_lock::multi_mutator_threads_live()
            || self.threads.shared_vars_active
            || self.user_subscript_routine_may_answer(true)
        {
            return false;
        }
        let n = self.stack.len();
        if n < 2 {
            return false;
        }
        // `index_first` emits the index below the value; the general store
        // swaps them before it starts.
        let (value_at, index_at) = if index_first {
            (n - 1, n - 2)
        } else {
            (n - 2, n - 1)
        };
        let ValueView::Int(i) = self.stack[index_at].view() else {
            return false;
        };
        let Ok(i) = usize::try_from(i) else {
            return false;
        };
        if !Self::lean_store_value_is_plain(&self.stack[value_at]) {
            return false;
        }
        let var_sym = code.const_sym(name_idx);
        // A `:=` pair recorded on this variable's name: the frame may hold a
        // second slot for it the lane does not check (see
        // `bind_pair_names_container`).
        if self.bind_pair_names_container(code, var_sym.as_str()) {
            return false;
        }
        if self.is_readonly_sym(var_sym)
            || (crate::runtime::runtime_var_default::var_default_possible()
                && self.var_default(var_sym.as_str()).is_some())
        {
            return false;
        }
        // The slot's constraint, as `decl_type_constraint` reads it for this
        // store: none for a slot the chunk declares untyped; the by-name
        // record for one it leaves unrecorded (a `my @b := @a` binding).
        let unconstrained = match code.declared_constraint(slot) {
            crate::binding_desc::DeclaredConstraint::Untyped => true,
            crate::binding_desc::DeclaredConstraint::Unrecorded => {
                self.own_var_type_constraint_value_sym(var_sym).is_none()
            }
            _ => false,
        };
        if !unconstrained {
            return false;
        }
        let (items, slot_cell) = match self.locals[slot].view() {
            ValueView::Array(
                items,
                crate::value::ArrayKind::Array | crate::value::ArrayKind::ItemArray,
            ) => (items.clone(), None),
            ValueView::ContainerRef(cell) if !self.locals[slot].container_ref_is_itemized() => {
                let items = {
                    let Ok(inner) = cell.lock() else {
                        return false;
                    };
                    match inner.view() {
                        ValueView::Array(
                            items,
                            crate::value::ArrayKind::Array | crate::value::ArrayKind::ItemArray,
                        ) => items.clone(),
                        _ => return false,
                    }
                };
                (items, Some(crate::gc::Gc::clone(&cell)))
            }
            _ => return false,
        };
        if items.has_type_meta() || items.has_native_backing() || items.default.is_some() {
            return false;
        }
        if !Self::element_slot_takes_plain_store(&items, i) {
            return false;
        }
        if !self.lean_store_alias_agrees(var_sym, &items, slot_cell.as_ref()) {
            return false;
        }
        // ---- commit ----
        let val = self.stack.swap_remove(value_at);
        self.stack.pop();
        let stored = Self::itemize_value_for_element_store(val.clone());
        // SAFETY: audited aliased in-place container write (see
        // `value::aliased_mut`), the same one `try_fast_array_element_assign`
        // makes. No borrow into this node is live across the write: `items` is
        // a `Gc` handle, the `ValueView` and any cell guard it came from are
        // dropped above, and no second mutator thread exists
        // (`multi_mutator_threads_live` was false), so this thread is the only
        // writer of the node.
        let data: &mut crate::value::ArrayData = unsafe { crate::value::gc_contents_mut(&items) };
        data.store_element(i, stored);
        self.stack.push(Self::itemize_value(val));
        true
    }

    /// Whether a sigilless alias record on the name (ADR-14632 §2.1,
    /// condition 1) leaves the store where the lane would put it. The general
    /// store redirects an aliased name to the variable the record names and
    /// checks that variable's `is default`, readonly flag and constraint by
    /// name; `my @b := @a` records such an alias. When the named variable's
    /// container is the very node the slot holds and it carries none of those,
    /// both routes write the same element of the same node.
    // Cost: O(1) when the latch is clear; else up to three by-name probes,
    // O(d), d = env tiers walked.
    fn lean_store_alias_agrees(
        &self,
        var_sym: Symbol,
        items: &crate::gc::Gc<crate::value::ArrayData>,
        slot_cell: Option<&crate::gc::Gc<crate::value::ContainerCell>>,
    ) -> bool {
        if !self.sigilless_alias_seen() {
            return true;
        }
        let Some(record) = self
            .env()
            .get_sym(crate::meta_ns::MetaNs::SigillessAlias.key(var_sym))
        else {
            return true;
        };
        let ValueView::Str(target) = record.view() else {
            return false;
        };
        // One intern for the target; every probe below takes the symbol.
        let target_sym = Symbol::intern(target.as_str());
        let same_node = match self.env().get_sym(target_sym).map(Value::view) {
            Some(ValueView::Array(node, _)) => crate::gc::Gc::ptr_eq(&node, items),
            // The target's binding cell: the same cell the slot holds is the
            // same node by construction; any other cell is read.
            Some(ValueView::ContainerRef(cell)) => {
                slot_cell.is_some_and(|own| crate::gc::Gc::ptr_eq(own, &cell))
                    || matches!(
                        cell.load().view(),
                        ValueView::Array(node, _) if crate::gc::Gc::ptr_eq(&node, items)
                    )
            }
            _ => false,
        };
        same_node
            && !self.is_readonly_sym(target_sym)
            && !(crate::runtime::runtime_var_default::var_default_possible()
                && self.var_default(target_sym.as_str()).is_some())
            && self.var_type_constraint_value_sym(target_sym).is_none()
    }

    /// A value the lane stores as is: an immutable scalar (not a container, a
    /// list, a lazy value, a bind token or a boot box).
    // Cost: O(1).
    fn lean_store_value_is_plain(val: &Value) -> bool {
        matches!(
            val.view(),
            ValueView::Int(_)
                | ValueView::BigInt(_)
                | ValueView::Num(_)
                | ValueView::Str(_)
                | ValueView::Bool(_)
                | ValueView::Rat(..)
                | ValueView::FatRat(..)
                | ValueView::BigRat(..)
                | ValueView::Complex(..)
                | ValueView::Enum { .. }
                | ValueView::Version { .. }
        )
    }
}
