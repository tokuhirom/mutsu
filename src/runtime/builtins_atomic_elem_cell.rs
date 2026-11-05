//! Which element cell an atomic op on `@a[i]` / `%h{k}` works on (#11833).
//!
//! Every element atomic (`cas(@a[0], ...)`, `atomic-fetch-inc(%h<k>)`,
//! `@a[0]⚛++`, the multi-dimensional `cas` forms) resolves its element through
//! [`Interpreter::atomic_elem_cell`], so the write side makes the same choice
//! of store that the `GetLocal` read side makes.

use super::*;
use crate::runtime::shared_store::atomic_lane_str_key;

/// The element cell an element atomic works on, and whether it lives in the
/// name-keyed `__mutsu_atomic_*::` lane (only a lane write is published to the
/// other threads through `shared_vars_dirty`).
pub(super) struct AtomicElemCell {
    pub(super) cell: crate::gc::Gc<crate::value::ContainerCell>,
    pub(super) on_lane: bool,
}

impl Interpreter {
    /// The element cell of `name[key]` — `container` is the compiler's plain
    /// read of the container variable `name` at the call site.
    ///
    /// Two stores can own the element:
    ///
    /// - **The name-keyed lane** (`init_celled_atomic_store` /
    ///   `celled_array_elem` / `celled_hash_elem`): a celled copy of the
    ///   container under `__mutsu_atomic_arr::@a` / `__mutsu_atomic_hash::%h`,
    ///   which `GetLocal` prefers over the frame's slot. This is what makes an
    ///   element atomic on a container that several threads share coherent:
    ///   every thread resolves the same cells under the lane's write lock.
    /// - **The binding the frame holds** (`container`): the element is promoted
    ///   in place to its `ContainerRef` cell, the way `my $r := @a[1]` does.
    ///
    /// A plain user lexical (`container_lane_applies` is false for it) always
    /// takes the binding: its place is the frame's slot or the declaration-site
    /// cell every capture shares (ADR-16595 §2 rule 3), so an element atomic
    /// on an inner `my @d` can never swap an outer `@d`'s element, and
    /// `await start { 1 }; my @d = 0, 0, 0; cas(@d[1], 0, 5); say @d` prints
    /// the swapped array. The lane keeps only the names that are not such a
    /// lexical (attributes, dynamics, opaque names).
    // Cost: O(1) amortized for a single index/key (one container lock and
    // slot probe, or a lane store probe). The lane's first touch of a container boxes its elements once,
    // O(e), e = its elements; growing an array to a far index is O(i - n).
    pub(super) fn atomic_elem_cell(
        &mut self,
        name: &str,
        container: &Value,
        key: &Value,
    ) -> Result<AtomicElemCell, RuntimeError> {
        let is_hash = name.starts_with('%');
        if !self.container_lane_applies(name) {
            let key = if is_hash {
                key.clone()
            } else {
                Value::int(self.binding_elem_index(container, key)?)
            };
            if let Some(cell) = self.binding_elem_cell(name, container, &key, is_hash) {
                return Ok(AtomicElemCell {
                    cell,
                    on_lane: false,
                });
            }
        }
        let atomic_key = atomic_lane_str_key(name, is_hash);
        let cell = if is_hash {
            self.init_celled_atomic_store(atomic_key, name);
            self.celled_hash_elem(atomic_key, name, &key.to_string_value())
        } else {
            // Resolves a `WhateverCode` against the lane's length (and seeds
            // the lane, #12695).
            let index = self.atomic_elem_index(name, key)?;
            self.init_celled_atomic_store(atomic_key, name);
            self.celled_array_elem(atomic_key, name, index)
        };
        Ok(AtomicElemCell {
            cell,
            on_lane: true,
        })
    }

    /// [`Self::atomic_elem_index`] for the binding path: a `WhateverCode`
    /// (`*-1`) is resolved against the length of `container` itself, not of the
    /// lane.
    // Cost: O(1), plus one call of a WhateverCode index.
    fn binding_elem_index(&mut self, container: &Value, key: &Value) -> Result<i64, RuntimeError> {
        let resolved = if matches!(key.view(), ValueView::Sub(_)) {
            let len = match container.deref_container().view() {
                ValueView::Array(items, _) => items.len() as i64,
                _ => 0,
            };
            self.call_sub_value(key.clone(), vec![Value::int(len)])?
        } else {
            key.clone()
        };
        Ok(match resolved.view() {
            ValueView::Int(i) => i,
            _ => resolved.to_string_value().parse::<i64>().unwrap_or(0),
        })
    }

    /// The element's cell inside `container` itself, promoting the element in
    /// place (and creating it when it is missing: an atomic op needs a cell
    /// before the first write). `None` when `container` is not the array or
    /// hash `name` says it is.
    // Cost: O(1) amortized for an element in range or an existing key;
    // O(i - n) to grow an array of n elements to index i.
    fn binding_elem_cell(
        &self,
        name: &str,
        container: &Value,
        key: &Value,
        is_hash: bool,
    ) -> Option<crate::gc::Gc<crate::value::ContainerCell>> {
        let target = container.deref_container();
        // The container's own ADR-0068 lock serializes the promotion against
        // another element atomic on the same container: two first touches must
        // not each mint a cell for one element. Keyed on the cell the binding
        // is held in when there is one (two threads writing a celled container
        // share the cell, not the node), else on the node.
        let cell_addr = match container.view() {
            ValueView::ContainerRef(cell) => Some(crate::gc::Gc::as_ptr(&cell) as usize),
            _ => None,
        };
        let _structure =
            crate::value::container_lock::ContainerStructGuard::acquire_for(cell_addr, &target);
        let slot = match target.view() {
            ValueView::Hash(h) if is_hash => {
                let key = key.to_string_value();
                let mut slot = target.hash_slot_ref(&key, true)?;
                if matches!(slot.view(), ValueView::HashEntryRef { .. }) {
                    // A missing key: store the hash's hole value (its
                    // `is default`, else its value type's zero or type object)
                    // so there is an element to promote.
                    let hole = match h.default.as_deref() {
                        Some(d) => d.clone(),
                        None => {
                            let ty = h.value_type.as_deref().unwrap_or("Any");
                            crate::native_types::native_zero_value(ty).unwrap_or_else(|| {
                                Value::package(crate::symbol::Symbol::intern(ty))
                            })
                        }
                    };
                    slot.hash_entry_write(hole);
                    slot = target.hash_slot_ref(&key, true)?;
                }
                slot
            }
            ValueView::Array(items, _) if !is_hash => {
                let index = match key.view() {
                    ValueView::Int(i) => i,
                    _ => 0,
                };
                let idx = if index < 0 {
                    (items.len() as i64 + index).max(0) as usize
                } else {
                    index as usize
                };
                target.array_grow_to(idx);
                target.array_slot_ref(idx, true)?
            }
            _ => return None,
        };
        match slot.view() {
            ValueView::ContainerRef(cell) => Some(cell.clone()),
            _ => None,
        }
    }
}
