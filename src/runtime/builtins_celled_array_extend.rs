//! Appending to a `@` lexical whose binding is a shared `ContainerRef` cell,
//! once threads exist (#11343).
//!
//! A decl-site cell (ADR-0039 / ADR-0055) is the container's one home: the
//! declaring frame's slot, its env mirror and every closure that captured the
//! array hold the same cell. A worker thread running such a closure must append
//! to that cell, not to the name-keyed `__mutsu_atomic_arr::` lane, which only
//! knows the names seeded when the worker was spawned. An `@got` declared after
//! the `start` that later runs the closure is not among them, so the lane gave
//! the worker a private entry and the declaring frame never saw the push.

use super::*;
use crate::value::container_lock::ContainerStructGuard;

impl Interpreter {
    /// Append (or, with `prepend`, insert at the front) `items` to the array
    /// held by `name`'s shared cell, under that cell's structure lock (ADR-0068),
    /// so concurrent appends from several threads serialize and none is lost.
    /// Returns the updated array, or `None` when `name` is not bound to a cell
    /// that holds an array (the caller then takes its usual path).
    // Cost: O(k) to append k items, O(n + k) to prepend them; n = array length.
    pub(crate) fn celled_array_extend(
        &mut self,
        name: &str,
        items: Vec<Value>,
        prepend: bool,
    ) -> Option<Value> {
        let cell = match self.env.get(name)?.view() {
            ValueView::ContainerRef(cell) => Self::value_cell_of(&cell),
            _ => return None,
        };
        // The stripe lock excludes the other celled-container funnels, which
        // mutate through a pointer derived under the cell's mutex but no longer
        // hold it; the cell's own mutex excludes a reader cloning the array out.
        let _structure = ContainerStructGuard::acquire_for_cell(&cell);
        let mut inner = cell
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        inner.with_array_mut(|arc, kind| {
            let data = crate::gc::Gc::make_mut(arc);
            if prepend {
                for (i, item) in items.into_iter().enumerate() {
                    data.insert(i, item);
                }
            } else {
                data.extend(items);
            }
            if *kind == crate::value::ArrayKind::List {
                *kind = crate::value::ArrayKind::Array;
            }
        })?;
        Some(inner.clone())
    }
}
