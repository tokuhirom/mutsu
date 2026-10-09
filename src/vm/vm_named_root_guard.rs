//! The ADR-0068 container-structure guard for an op that mutates a NAMED
//! `@`/`%` variable's backing container in place (`%!h{$k}:delete`), keyed the
//! way every other store route keys it: on the shared `ContainerRef` cell when
//! the variable is held in one, else on the container node.

use super::*;
use crate::value::container_lock::ContainerStructGuard;

impl Interpreter {
    /// The structure guard for a structural mutation of the container the
    /// variable `name` holds, taken BEFORE any lock on that container's cell
    /// (the order `ContainerStructGuard::acquire_for_cell` readers use).
    /// `None` -- and nothing locked -- until a second VM mutator thread exists,
    /// when this thread already holds one, or when the variable holds no
    /// container.
    // Cost: O(1) once a mutator thread exists, one relaxed load before that.
    pub(crate) fn named_root_struct_guard(
        &self,
        code: &CompiledCode,
        slot: Option<u32>,
        name: &str,
    ) -> Option<ContainerStructGuard> {
        if !crate::value::container_lock::multi_mutator_threads_live() {
            return None;
        }
        let root = self
            .gate_local_slot_value_at(code, slot, name)
            .or_else(|| self.env().get(name).cloned())?;
        match root.descalarize().view() {
            ValueView::ContainerRef(cell) => {
                ContainerStructGuard::acquire(crate::gc::Gc::as_ptr(&cell) as usize)
            }
            _ => ContainerStructGuard::acquire_for(None, &root),
        }
    }
}
