use super::*;

/// Itemization is local to the holder; assignment sharing and binding differ.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ContainerRefFlavor {
    Plain,
    SharedItemized,
    BoundItemized,
}

impl Value {
    /// An itemized element alias whose assignment writes through its binding.
    // Cost: O(1).
    pub(crate) fn container_ref_bound_itemized(cell: Gc<ContainerCell>) -> Self {
        Self::from_repr(ValueRepr::ContainerRef(
            cell,
            ContainerRefFlavor::BoundItemized,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn itemized_binding_preserves_identity_and_write_through() {
        let cell = Gc::new(ContainerCell::new(Value::int(1)));
        let shared = Value::container_ref_itemized(cell.clone());
        let mut bound = Value::container_ref_bound_itemized(cell.clone());
        let cloned_bound = bound.clone();
        assert!(bound.container_ref_is_itemized());
        assert!(!bound.is_element_share_word());
        assert!(shared.is_element_share_word());
        assert!(bound.same_variant(&shared));
        Value::assign_element_slot(&mut bound, Value::int(3));
        assert_eq!(cell.load().as_int(), Some(3));
        assert_eq!(cloned_bound.deref_container().as_int(), Some(3));
        drop(bound);
        drop(cloned_bound);
        drop(shared);
        assert_eq!(Gc::strong_count(&cell), 1);
    }

    #[test]
    fn deref_walks_a_long_cell_chain_without_recursing() {
        // A chain of 3000 cells (a writer that keeps wrapping a binding cell)
        // must read through to the value without a stack frame per cell.
        let mut cur = Value::int(42);
        for _ in 0..3000 {
            cur = Value::container_ref(Gc::new(ContainerCell::new(cur)));
        }
        assert_eq!(cur.deref_container().as_int(), Some(42));
    }

    #[test]
    fn deref_of_a_cyclic_cell_chain_returns_instead_of_overflowing() {
        // A cell holding itself is a runtime bug elsewhere, but reading it must
        // not take the process down (#15414): the walk stops at the chain bound.
        let cell = Gc::new(ContainerCell::new(Value::int(1)));
        let as_value = Value::container_ref(cell.clone());
        *cell.lock().unwrap() = as_value.clone();
        let read = as_value.deref_container();
        assert!(read.is_container_ref());
        // Break the cycle so the cell can be freed.
        *cell.lock().unwrap() = Value::int(0);
    }
}
