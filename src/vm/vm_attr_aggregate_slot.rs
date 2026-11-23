//! The VM half of ADR-18356 D2: a container op on a private aggregate
//! attribute (`@!x` / `%!x`) addressed through its local slot.
//!
//! The compiler gives such an attribute a slot (`compiler/attr_aggregate_slot`),
//! so `@!x.push(v)`, `%!x<k> = v` and `@!x.pop` compile to the slot forms of
//! `ArrayPush`, `IndexAssignLocal` and `CallMethodMut`. Those forms mutate the
//! container the slot holds in place. The slot is only a reference to the
//! container the instance holds, so around each op:
//!
//! - **before**, the slot is refreshed from `self`'s attribute cell (through
//!   the ADR-0121 D3 site cache). A nested call, a closure or another thread
//!   may have stored a new container into the attribute since the slot was
//!   last read;
//! - **after**, the cell is written only when the op *rebound* the slot (a
//!   lazy list reified into a new array, a container replaced instead of
//!   mutated). An in-place mutation leaves the slot holding the very
//!   container the cell holds, and nothing is written.
//!
//! The by-name alternative this replaces (`attr_env_snapshot` /
//! `mirror_attr_env_to_cell`) probed the env by name twice and resolved the
//! attribute again on every op. When the attribute cannot be resolved through
//! the slot (no `self` instance, or no such attribute), the op falls back to
//! that by-name pair, so the result is the general path's.

use super::*;

/// What the prologue of a container op on a possibly-attribute target
/// recorded for its epilogue.
pub(super) enum AttrOpPre {
    /// The by-name path's snapshot (also `None` for a non-attribute target).
    Named(Option<Value>),
    /// The aggregate attribute slot `slot`, refreshed to `container`, and
    /// what the frame's env bound the attribute's name to before the op.
    Slot {
        slot: usize,
        container: Value,
        env_before: Option<Value>,
    },
}

impl Interpreter {
    /// The private aggregate attribute slot of `target_slot`, if it is one.
    // Cost: O(1) (a memoized per-chunk table read).
    fn aggregate_attr_slot(code: &CompiledCode, target_slot: Option<u32>) -> Option<usize> {
        let slot = target_slot? as usize;
        code.is_private_aggregate_attr_slot(slot).then_some(slot)
    }

    /// Prologue of a whole-container op (`ArrayPush`, `CallMethodMut`) on
    /// `name_idx` held in `target_slot`.
    // Cost: O(d) for an aggregate attribute slot on a site-cache hit, d = the
    // frame's env tiers (one in-frame probe); the by-name snapshot's cost
    // otherwise.
    pub(super) fn attr_container_op_prologue(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        target_slot: Option<u32>,
    ) -> AttrOpPre {
        if let Some(pre) = self.aggregate_attr_slot_refresh(code, target_slot) {
            return pre;
        }
        AttrOpPre::Named(self.attr_env_snapshot(code, name_idx))
    }

    /// Prologue of an element op (`IndexAssignLocal`, `IndexElemAutoviv`) on
    /// `name_idx` held in `target_slot`.
    // Cost: as `attr_container_op_prologue`.
    pub(super) fn attr_elem_op_prologue(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        target_slot: Option<u32>,
    ) -> AttrOpPre {
        if let Some(pre) = self.aggregate_attr_slot_refresh(code, target_slot) {
            return pre;
        }
        AttrOpPre::Named(self.attr_elem_env_snapshot(code, name_idx))
    }

    /// Refresh an aggregate attribute slot from `self`'s cell; `None` when
    /// `target_slot` is not one. An attribute the cell does not hold yet (a
    /// fresh instance's unvivified slot) leaves the slot as it is: the op
    /// autovivifies a container into it, and the epilogue stores that.
    // Cost: O(d) on a site-cache hit, d = the frame's env tiers;
    // `read_attr_cell_by_key` otherwise.
    fn aggregate_attr_slot_refresh(
        &mut self,
        code: &CompiledCode,
        target_slot: Option<u32>,
    ) -> Option<AttrOpPre> {
        let slot = Self::aggregate_attr_slot(code, target_slot)?;
        let sym = code.locals_sym[slot];
        let container = match self.read_self_attr_cell_slot(code, slot) {
            Some(container) => {
                self.locals[slot] = container.clone();
                // The op's receiver place writes through the env binding of
                // the attribute's name (`ReceiverPlace::slot`). A visible
                // binding holding another container is a stale copy -- a
                // thread clone's spawn-time snapshot, or a `BUILD`
                // parameter's binding from before a nested call replaced the
                // attribute -- so the op would mutate (or find empty) that
                // copy instead of the instance's container (#18438). Rebind
                // it to the live container in this frame. A `:=`-bound cell
                // keeps its own handling.
                if self.env().get_sym(sym).is_some_and(|bound| {
                    !bound.is_container_ref() && !bound.same_binding(&container)
                }) {
                    self.env_mut().insert_sym(sym, container.clone());
                }
                container
            }
            None => self.locals[slot].clone(),
        };
        let env_before = self.env().get_sym_in_frame(sym).cloned();
        Some(AttrOpPre::Slot {
            slot,
            container,
            env_before,
        })
    }

    /// Epilogue of a whole-container op: see the module docs.
    // Cost: O(d) for a slot the op did not rebind, d = the frame's env tiers;
    // plus one attribute store when it did; the by-name mirror's cost for
    // `AttrOpPre::Named`.
    pub(super) fn attr_container_op_epilogue(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        pre: AttrOpPre,
    ) {
        match pre {
            AttrOpPre::Named(pre) => self.mirror_attr_env_to_cell(code, name_idx, pre),
            AttrOpPre::Slot {
                slot,
                container,
                env_before,
            } => self.aggregate_attr_slot_store_back(code, slot, &container, env_before),
        }
    }

    /// Epilogue of an element op: see the module docs.
    // Cost: as `attr_container_op_epilogue`.
    pub(super) fn attr_elem_op_epilogue(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        pre: AttrOpPre,
    ) {
        match pre {
            AttrOpPre::Named(pre) => self.mirror_attr_elem_env_to_cell(code, name_idx, pre),
            AttrOpPre::Slot {
                slot,
                container,
                env_before,
            } => self.aggregate_attr_slot_store_back(code, slot, &container, env_before),
        }
    }

    /// Store the slot into the attribute when the op rebound it. A mutating
    /// method that answers a new container rather than mutating in place
    /// (`@!a.push(|@v)` through the general dispatch) leaves it in the frame's
    /// env under the attribute's name, the way it rebinds any named receiver.
    /// The slot adopts that binding when the op changed it (`env_before`): an
    /// entry the op did not write is an older by-name store's copy, not the
    /// attribute.
    // Cost: O(d), d = the frame's env tiers (one in-frame probe), plus one
    // attribute store when the slot was rebound.
    fn aggregate_attr_slot_store_back(
        &mut self,
        code: &CompiledCode,
        slot: usize,
        container: &Value,
        env_before: Option<Value>,
    ) {
        let sym = code.locals_sym[slot];
        if let Some(rebound) = self.env().get_sym_in_frame(sym)
            && env_before
                .as_ref()
                .is_none_or(|before| !before.same_binding(rebound))
        {
            self.locals[slot] = rebound.clone();
        }
        if self.locals[slot].same_binding(container) {
            return;
        }
        self.mirror_attr_local_to_cell(code, slot);
    }

    /// The container a push into `target_name` appends to: the held private
    /// aggregate attribute slot (refreshed from the cell by the op's
    /// prologue), else the env binding of the name.
    // Cost: O(1) for an attribute slot; an env probe otherwise.
    pub(super) fn push_target(
        &self,
        decl: crate::vm::vm_slot_type_constraint::StoreDecl<'_>,
        target_sym: Symbol,
    ) -> Option<&Value> {
        match decl {
            Some((code, idx)) if code.is_private_aggregate_attr_slot(idx) => self.locals.get(idx),
            // The op's constant-pool name, memoized per chunk: a push asks
            // for its target several times and used to re-intern the name
            // on each (#18274).
            _ => self.env().get_sym(target_sym),
        }
    }

    /// Store an autovivified push target: into the held attribute slot (the
    /// op's epilogue then stores it into the cell), else under the name.
    // Cost: O(1) for an attribute slot; an env insert otherwise.
    pub(super) fn store_push_target(
        &mut self,
        decl: crate::vm::vm_slot_type_constraint::StoreDecl<'_>,
        target_name: &str,
        value: Value,
    ) {
        match decl {
            Some((code, idx)) if code.is_private_aggregate_attr_slot(idx) => {
                self.locals[idx] = value;
            }
            _ => {
                self.env_mut().insert(target_name.to_string(), value);
            }
        }
    }
}
