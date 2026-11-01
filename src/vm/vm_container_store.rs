//! Checked writes through a shared scalar or element container.

use super::*;

impl Interpreter {
    /// Prepare `val` for a write-through into a `ContainerRef` cell (`$ref = v`
    /// on a `:=`-bound typed slot — a typed rw attribute accessor bind, a
    /// `my T $` anonymous typed scalar, or a promoted array/hash element),
    /// returning the value to store. Mirrors the Pair.value enforcement in
    /// `methods_mut_method_lvalue.rs`.
    ///
    /// A native constraint coerces the value first, exactly as a direct store
    /// does (`wrap_native_int_by_constraint`): `my uint8 @u; my $q := @u[0];
    /// $q = 257` stores 1, like `@u[0] = 257` (#11233). Then the registered
    /// `of`-type is enforced.
    // Cost: O(1) plus the type check of `val` against the cell's constraint.
    pub(crate) fn coerce_container_cell_store(
        &mut self,
        cell: &crate::gc::Gc<crate::value::ContainerCell>,
        val: Value,
    ) -> Result<Value, RuntimeError> {
        // An element bound to a bare value (`%h.BIND-KEY($k, 42)`) has no
        // container to assign into.
        if let Some(kind) = cell.readonly_kind() {
            let bound = cell
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .clone();
            return Err(Self::readonly_binding_error(kind, &bound));
        }
        let Some(c) = crate::value::lookup_cell_constraint(cell) else {
            return Ok(val);
        };
        // Type-check before narrowing: a user `subset int8 of Int where ...`
        // shares its name with a native width, and wrapping first would turn
        // an out-of-range value into an in-range one that then passes (the
        // native check accepts any `Int`, so the order is free for natives).
        if !matches!(c.ty.as_str(), "Any" | "Mu")
            && !val.is_nil()
            && !self.type_matches_value(&c.ty, &val)
        {
            // An ELEMENT's cell blames the container, exactly as a direct
            // `@a[0] = v` store does ("Type check failed for an element of
            // @a"); a plain typed scalar's cell keeps the assignment wording.
            return Err(match c.element_of {
                Some(owner) => self.type_check_element_failure(&owner, &c.ty, &val),
                // `assign_to` is the name the cell was promoted from, so a write
                // arriving through an alias or from another frame still reads
                // "in assignment to $a" like rakudo's descriptor-carried wording.
                None => self.typecheck_assignment_failure(&c.ty, &val, c.assign_to.as_deref()),
            });
        }
        if let Some(check) = &c.where_check
            && !val.is_nil()
        {
            let scope = crate::runtime::AttrWhereScope {
                unit: check.unit,
                package: check.package,
            };
            if !self.check_attribute_where_constraint(&check.pred, &val, scope) {
                return Err(self.typecheck_assignment_failure(&c.ty, &val, c.assign_to.as_deref()));
            }
        }
        Self::wrap_native_int_by_constraint(&c.ty, val)
    }
}
