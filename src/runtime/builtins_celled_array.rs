//! Mutating a `@` lexical whose binding is a shared `ContainerRef` cell, once
//! threads exist (#11343).
//!
//! A decl-site cell (ADR-0039 / ADR-0055) is the container's one home: the
//! declaring frame's slot, its env mirror and every closure that captured the
//! array hold the same cell. A worker thread running such a closure must mutate
//! that cell, not the name-keyed `__mutsu_atomic_arr::` lane, which only knows
//! the names seeded when the worker was spawned. An `@got` declared after the
//! `start` that later runs the closure is not among them, so the lane gave the
//! worker a private entry and the declaring frame never saw the push. Every
//! method that the lane serves (`push`/`unshift`/`append`/`prepend`/`pop`/
//! `shift`/`splice`) goes through the cell instead, so a binding never has
//! its elements split between two homes.

use super::*;
use crate::value::container_lock::ContainerStructGuard;

impl Interpreter {
    /// Apply `f` to the array held by `name`'s shared cell, under that cell's
    /// structure lock (ADR-0068), so concurrent mutations from several threads
    /// serialize and none is lost. Returns `f`'s result and the updated array,
    /// or `None` (nothing touched) when `name` is not bound to a cell holding an
    /// array, and the caller then takes its usual path.
    // Cost: O(1) plus the cost of `f`.
    pub(crate) fn celled_array_mutate<R>(
        &mut self,
        name: &str,
        f: impl FnOnce(&mut crate::value::ArrayData) -> R,
    ) -> Option<(R, Value)> {
        let cell = match self.env.get(name)?.view() {
            ValueView::ContainerRef(cell) => Self::value_cell_of(&cell),
            _ => return None,
        };
        // The stripe lock excludes the other celled-container funnels, which
        // mutate through a pointer derived under the cell's mutex but no longer
        // hold it; the cell's own mutex excludes a reader cloning the array out.
        // Keyed on the held node too: a reader handed the array itself (an
        // argument) never saw the cell and keys on the node (#13016).
        let _structure = ContainerStructGuard::acquire_for_celled(&cell);
        let mut inner = cell
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let result = inner.with_array_mut(|arc, kind| {
            if *kind == crate::value::ArrayKind::List {
                *kind = crate::value::ArrayKind::Array;
            }
            // In place, not `Gc::make_mut`: the node is this variable's own
            // container, and every holder of it (a worker's slot, a parameter
            // bound to it, a `for` source) must see the write. `make_mut`
            // copied a node another holder still referenced, so the write
            // landed on the copy and was lost (#13078). Rakudo's VMArray also
            // grows in place. Approved by the maintainer as a memory-safety
            // trade-off on 2026-10-13: a reader of a shared node holds its
            // stripe or reads a snapshot (ADR-0068 §20).
            // SAFETY: the cell's and the node's stripes are held
            // (`acquire_for_celled`) and so is the cell's mutex, so no other
            // thread mutates the node or walks it through a guarded reader,
            // and no `&`/`&mut` into it is live on this thread while `f` runs.
            f(unsafe { crate::value::gc_contents_mut(arc) })
        })?;
        Some((result, inner.clone()))
    }

    /// Append `items` to (or, with `prepend`, insert them at the front of) the
    /// array held by `name`'s shared cell. See [`Self::celled_array_mutate`].
    // Cost: O(k) to append k items, O(n + k) to prepend them; n = array length.
    pub(crate) fn celled_array_extend(
        &mut self,
        name: &str,
        items: Vec<Value>,
        prepend: bool,
    ) -> Option<Value> {
        self.celled_array_mutate(name, |data| {
            if prepend {
                for (i, item) in items.into_iter().enumerate() {
                    data.insert(i, item);
                }
            } else {
                data.extend(items);
            }
        })
        .map(|(_, updated)| updated)
    }
}
