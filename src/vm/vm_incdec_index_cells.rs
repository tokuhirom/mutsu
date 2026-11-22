//! The cells an indexed `++`/`--` mutates through and keys its ADR-0068
//! stripe on (split out of `vm_var_assign_post_incdec.rs`).

use super::*;

impl Interpreter {
    /// The capture cell a `%h{$k}++` / `@a[$i]++` on `name` mutates through,
    /// and the cell its ADR-0068 stripe is keyed on: the gate slot (when the
    /// per-store gate resolved one), the variable's own cell, and that cell or
    /// -- for a thread whose env holds the deref'd file-scope container -- the
    /// unit-lexical cell. The read and the write of one step must key on the
    /// same cell, so both take it from here.
    // Cost: O(d), d = the env tiers walked.
    pub(super) fn incdec_index_cells(
        &self,
        code: &CompiledCode,
        slot: Option<u32>,
        name: &str,
        name_sym: crate::symbol::Symbol,
    ) -> (
        Option<usize>,
        Option<crate::gc::Gc<crate::value::ContainerCell>>,
        Option<crate::gc::Gc<crate::value::ContainerCell>>,
    ) {
        let gate_slot = self
            .gate_local_slot_at(code, slot, name)
            .filter(|&s| !self.locals[s].is_nil());
        // When the variable is held in a capture cell, the mutation has to land
        // INSIDE the cell: the slot itself holds only the `ContainerRef`, and a
        // deref'd clone would COW-detach on `gc_data_mut` and silently drop the
        // write. Locking the cell hands out the same `&mut Value` the slot arm
        // does, so every arm below is unchanged.
        //
        // The cell is found the way the read above found the container: a
        // compunit's own file-scope lexical or a routine-nested sub's free
        // variable (`unit_lexical_slot`) before the env key, which belongs to
        // whatever scope called in (mutsu#10114).
        let cell = match gate_slot {
            Some(s) => self.locals.get(s).cloned(),
            None => self
                .unit_lexical_slot(name, None)
                .filter(|v| v.is_container_ref())
                .or_else(|| self.env().get_sym(name_sym).cloned()),
        }
        .and_then(|v| match v.descalarize().view() {
            ValueView::ContainerRef(cell) => Some(cell.clone()),
            _ => None,
        });
        // ADR-0068: the structural write below mutates an aliased container
        // through `gc_contents_mut`. An attribute hash (`%!h{$k}++` in a method)
        // is aliased by the instance's attribute store and by this frame's env,
        // so nothing else excludes a second thread's write (or its resize) -- a
        // double free / use after free in `HashData`'s drop (#11701). Keyed on
        // the cell when there is one, else on the container node (taken below,
        // once the container is in hand). The stripe is taken BEFORE the cell's
        // own `Mutex<Value>`, the order `ContainerStructGuard::acquire_for_cell`
        // readers use, so the two can never be held in opposite orders. A no-op
        // until a VM mutator thread has been spawned; the guarded region calls
        // no user Raku code.
        // A file-scope `my %h` is the same shared hash in every thread, but a
        // thread whose env was seeded from the unit-lexical cell holds the
        // deref'd container, not the cell: key on the unit-lexical cell too, or
        // two threads would lock two different stripes for one hash.
        let key_cell = cell.clone().or_else(|| {
            crate::value::container_lock::multi_mutator_threads_live()
                .then(|| self.unit_lexical_container_cell(name))
                .flatten()
        });
        (gate_slot, cell, key_cell)
    }
}
