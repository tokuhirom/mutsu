//! The lean element store for a declared `@` or `%` local (#19285).
//!
//! `@a[$i] = $v` / `%h{$k} = $v` compile to `IndexAssignLocal { slot, .. }`
//! when the running code declares the variable (ADR-14632 §2.5), and the store
//! body already treats that slot as the authoritative place of the container
//! (§2.1, §2.4: a writer that replaces the container by name and not in the
//! slot is a bug). But the body is reached through a preamble that answers the
//! plain case only after asking, by name, every question the general store
//! needs: a bind expression, an attribute mirror, a package-body seed, a user
//! `postcircumfix` remap, and the name-keyed probes of the older fast lanes
//! (`unit_lexical_container_cell`, `unit_lexical_slot`,
//! `our_package_container_key`, an env read of the binding, the slot's
//! constraint record). ~2,500 instructions per array store, ~4,000 per hash
//! store.
//!
//! None of them is a property of the store. This lane answers them from where
//! they are settled:
//!
//! - the chunk (the slot is declared here, a plain user `@`/`%` lexical with
//!   no attribute key, not `our` or `state`, and declared untyped -- or left
//!   unrecorded, as a `:=` binding is, with no by-name constraint record);
//! - the process latches the place rule already uses (no cross-thread lane,
//!   not a thread clone, no second mutator thread, no user `postcircumfix`
//!   that could answer) and no `:=` pair recorded on the variable; a sigilless
//!   alias record on the name only when it names a variable bound to the same
//!   node;
//! - the container's embedded metadata (an ordinary `Array` / `Hash`, no type
//!   meta, default or native backing; an array slot that takes a plain store,
//!   a hash entry that is absent or a plain value);
//! - the value and index on the stack (a non-negative `Int` index, or a `Str`
//!   or `Int` key; a plain scalar value).
//!
//! Anything else returns `false` with the stack untouched, and the caller runs
//! the unchanged general store, which stays the authority on semantics.

use super::*;

/// The container node a store addresses: an ordinary `Array` (or itemized
/// `Array`) node, or a `Hash` node.
pub(super) enum LeanStoreNode {
    Array(crate::gc::Gc<crate::value::ArrayData>),
    Hash(crate::gc::Gc<crate::value::HashData>),
}

impl LeanStoreNode {
    // Cost: O(1).
    #[inline]
    fn of(value: &Value) -> Option<Self> {
        match value.view() {
            ValueView::Array(
                items,
                crate::value::ArrayKind::Array | crate::value::ArrayKind::ItemArray,
            ) => Some(Self::Array(items.clone())),
            ValueView::Hash(hash) => Some(Self::Hash(hash.clone())),
            _ => None,
        }
    }
}

/// Where the value and index sit on the stack for a store the lane accepted
/// up to the container checks.
struct LeanStoreOperands {
    value_at: usize,
    index_at: usize,
}

impl Interpreter {
    /// Store the value on the stack into the element of the `@` or `%` local
    /// in `slot` that the index on the stack names, when every condition in the
    /// module docs holds. `true` = stored (the stack holds the store's result,
    /// as the general store leaves it); `false` = declined, stack untouched.
    // Cost: O(1) amortized (an in-place slot write or append, or one hash
    // insert); plus O(k) to copy a new hash key, k = key bytes.
    pub(super) fn try_lean_local_element_store(
        &mut self,
        code: &CompiledCode,
        slot: u32,
        name_idx: u32,
        is_positional: bool,
        index_first: bool,
    ) -> bool {
        let slot = slot as usize;
        if is_positional {
            self.try_lean_local_array_store(code, slot, name_idx, index_first)
        } else {
            self.try_lean_local_hash_store(code, slot, name_idx, index_first)
        }
    }

    // Cost: O(1) amortized.
    #[inline(always)]
    fn try_lean_local_array_store(
        &mut self,
        code: &CompiledCode,
        slot: usize,
        name_idx: u32,
        index_first: bool,
    ) -> bool {
        if slot >= self.locals.len() || !code.lean_element_store_slot(slot) {
            return false;
        }
        let Some(ops) = self.lean_store_preamble(code, slot, name_idx, true, index_first) else {
            return false;
        };
        let ValueView::Int(i) = self.stack[ops.index_at].view() else {
            return false;
        };
        let Ok(i) = usize::try_from(i) else {
            return false;
        };
        let Some((LeanStoreNode::Array(items), slot_cell)) = self.lean_store_slot_node(slot) else {
            return false;
        };
        // A jagged array's element store makes a row (#19282).
        if items.has_type_meta()
            || items.has_native_backing()
            || items.default.is_some()
            || items.shape.is_some()
        {
            return false;
        }
        if !Self::element_slot_takes_plain_store(&items, i) {
            return false;
        }
        let node = crate::gc::Gc::as_ptr(&items) as usize;
        if !self.lean_store_alias_agrees(code.const_sym(name_idx), node, slot_cell.as_ref()) {
            return false;
        }
        // ---- commit ----
        let val = self.lean_store_take_value(&ops);
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

    // Cost: O(1) expected, plus O(k) to copy a new key, k = key bytes.
    fn try_lean_local_hash_store(
        &mut self,
        code: &CompiledCode,
        slot: usize,
        name_idx: u32,
        index_first: bool,
    ) -> bool {
        if slot >= self.locals.len() || !code.lean_hash_store_slot(slot) {
            return false;
        }
        let Some(ops) = self.lean_store_preamble(code, slot, name_idx, false, index_first) else {
            return false;
        };
        // The key's holder is a clone of the stack word (a refcount bump), so
        // the borrowed key outlives the pops of the commit.
        let key_holder = self.stack[ops.index_at].clone();
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
        // An entry the store writes THROUGH or replaces by a rule of its own
        // (a `:=`-bound cell, an `=`-shared cell, a deferred bind token) is the
        // general store's.
        if let Some(entry) = hash.map.get(key)
            && (entry.is_container_ref()
                || entry.is_hash_entry_ref_value()
                || entry.is_proxy_value())
        {
            return false;
        }
        let node = crate::gc::Gc::as_ptr(&hash) as usize;
        if !self.lean_store_alias_agrees(code.const_sym(name_idx), node, slot_cell.as_ref()) {
            return false;
        }
        // ---- commit ----
        let val = self.lean_store_take_value(&ops);
        // SAFETY: audited aliased in-place container write (see
        // `value::aliased_mut`), the one the general hash store makes for a
        // shared node (`use_inplace`). No borrow into this node is live across
        // the write: `hash` is a `Gc` handle, the `ValueView` and any cell
        // guard it came from are dropped above, and no second mutator thread
        // exists (`multi_mutator_threads_live` was false), so this thread is
        // the only writer of the node.
        let data: &mut crate::value::HashData = unsafe { crate::value::gc_contents_mut(&hash) };
        Value::hash_insert_through_str(
            &mut data.map,
            key,
            Self::itemize_value_for_element_store(val.clone()),
        );
        self.stack.push(Self::itemize_value(val));
        true
    }

    /// The conditions both lanes share, settled before the container is
    /// looked at: the process latches, the stack shape and value, and the
    /// variable's `:=` pair, readonly flag, `is default` and constraint.
    /// `None` = decline.
    // Cost: O(1) expected (a few latch loads and symbol-keyed probes).
    #[inline(always)]
    fn lean_store_preamble(
        &self,
        code: &CompiledCode,
        slot: usize,
        name_idx: u32,
        is_positional: bool,
        index_first: bool,
    ) -> Option<LeanStoreOperands> {
        if self.is_thread_clone()
            || crate::runtime::shared_store::atomic_lane_entries_exist()
            || crate::value::container_lock::multi_mutator_threads_live()
            || self.threads.shared_vars_active
            || self.user_subscript_routine_may_answer(is_positional)
        {
            return None;
        }
        let n = self.stack.len();
        if n < 2 {
            return None;
        }
        // `index_first` emits the index below the value; the general store
        // swaps them before it starts.
        let ops = if index_first {
            LeanStoreOperands {
                value_at: n - 1,
                index_at: n - 2,
            }
        } else {
            LeanStoreOperands {
                value_at: n - 2,
                index_at: n - 1,
            }
        };
        if !Self::lean_store_value_is_plain(&self.stack[ops.value_at]) {
            return None;
        }
        self.lean_store_variable_ok(code, slot, name_idx)
            .then_some(ops)
    }

    /// The variable half of [`Self::lean_store_preamble`]: the conditions a
    /// store into the variable named by `name_idx` (in `slot`) needs that are
    /// decided by the variable, not by the stack or the container -- no `:=`
    /// pair recorded on it, not readonly, no `is default`, no constraint. None
    /// of them can change without running code: the loop kernels' element
    /// stores check them once per kernel entry (`vm_kernel_elem`).
    // Cost: O(1) expected (a few symbol-keyed probes).
    #[inline(always)]
    pub(super) fn lean_store_variable_ok(
        &self,
        code: &CompiledCode,
        slot: usize,
        name_idx: u32,
    ) -> bool {
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
        unconstrained
    }

    /// The container node the slot holds -- in its own word, or inside its
    /// binding cell (a non-itemized `ContainerRef`) -- with that cell. Only the
    /// node's `Gc` handle is cloned, never the slot's word.
    // Cost: O(1) (one refcount bump; one uncontended cell lock).
    #[inline(always)]
    pub(super) fn lean_store_slot_node(
        &self,
        slot: usize,
    ) -> Option<(
        LeanStoreNode,
        Option<crate::gc::Gc<crate::value::ContainerCell>>,
    )> {
        let word = &self.locals[slot];
        match word.view() {
            ValueView::ContainerRef(cell) => {
                if word.container_ref_is_itemized() {
                    return None;
                }
                let node = LeanStoreNode::of(&*cell.lock().ok()?)?;
                Some((node, Some(crate::gc::Gc::clone(&cell))))
            }
            _ => Some((LeanStoreNode::of(word)?, None)),
        }
    }

    /// Pop the index and the value of an accepted store; the value is
    /// returned.
    // Cost: O(1).
    #[inline(always)]
    fn lean_store_take_value(&mut self, ops: &LeanStoreOperands) -> Value {
        let val = self.stack.swap_remove(ops.value_at);
        self.stack.pop();
        val
    }

    /// Whether a sigilless alias record on the name (ADR-14632 §2.1,
    /// condition 1) leaves the store where the lane would put it. The general
    /// store redirects an aliased name to the variable the record names and
    /// checks that variable's `is default`, readonly flag and constraint by
    /// name; `my @b := @a` records such an alias. When the named variable's
    /// container is the very node (`node`, its `Gc` address) the slot holds
    /// and it carries none of those, both routes write the same element of the
    /// same node.
    // Cost: O(1) when the latch is clear; else one intern and up to four
    // symbol-keyed probes, O(d), d = env tiers walked.
    pub(super) fn lean_store_alias_agrees(
        &self,
        var_sym: Symbol,
        node: usize,
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
        let holds_node = |value: &Value| match value.view() {
            ValueView::Array(items, _) => crate::gc::Gc::as_ptr(&items) as usize == node,
            ValueView::Hash(hash) => crate::gc::Gc::as_ptr(&hash) as usize == node,
            _ => false,
        };
        let same_node = match self.env().get_sym(target_sym) {
            // The target's binding cell: the same cell the slot holds is the
            // same node by construction; any other cell is read.
            Some(bound) => match bound.view() {
                ValueView::ContainerRef(cell) => {
                    slot_cell.is_some_and(|own| crate::gc::Gc::ptr_eq(own, &cell))
                        || holds_node(&cell.load())
                }
                _ => holds_node(bound),
            },
            None => false,
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
