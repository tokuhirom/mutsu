//! A block's own `@`/`%` that shadows a captured file-scope one.
//!
//! A mainline `my %h` that a named sub captures lives in a shared cell
//! (`unit_lexicals`, ADR-0024), found by name. A block of the mainline that
//! declares its own `my %h` names a different variable, in a slot of its
//! own, so a store through that slot must not be redirected into the
//! file-scope cell.

use crate::runtime::Interpreter;
use crate::value::{ContainerCell, ValueView};

impl Interpreter {
    /// [`Self::unit_lexical_container_cell`] for a store into `name` through
    /// the running code's local `target_slot`: `None` when that slot holds a
    /// variable other than the cell (a block's own same-named lexical).
    // Cost: O(1) plus `unit_lexical_container_cell`.
    pub(crate) fn unit_lexical_container_cell_at(
        &self,
        name: &str,
        target_slot: Option<u32>,
    ) -> Option<crate::gc::Gc<ContainerCell>> {
        let cell = self.unit_lexical_container_cell(name)?;
        if let Some(local) = target_slot.and_then(|slot| self.locals.get(slot as usize))
            && !matches!(local.view(), ValueView::ContainerRef(c) if crate::gc::Gc::ptr_eq(&c, &cell))
        {
            return None;
        }
        Some(cell)
    }
}
