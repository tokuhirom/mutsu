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
    fn itemized_binding_writes_through_while_value_sharing_detaches() {
        let cell = Gc::new(ContainerCell::new(Value::int(1)));
        let mut shared = Value::container_ref_itemized(cell.clone());
        let mut bound = Value::container_ref_bound_itemized(cell.clone());
        let cloned_bound = bound.clone();
        assert!(bound.container_ref_is_itemized());
        assert!(!bound.is_element_share_word());
        assert!(shared.is_element_share_word());
        assert!(bound.same_variant(&shared));
        Value::assign_element_slot(&mut shared, Value::int(2));
        assert_eq!(cell.load().as_int(), Some(1));
        Value::assign_element_slot(&mut bound, Value::int(3));
        assert_eq!(cell.load().as_int(), Some(3));
        assert_eq!(cloned_bound.deref_container().as_int(), Some(3));
        drop(bound);
        drop(cloned_bound);
        assert_eq!(Gc::strong_count(&cell), 1);
    }
}
