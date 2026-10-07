//! A routine's identity for `does` (ADR-11827).
//!
//! `does` on a routine stores the resulting composition in the routine's
//! shared cell ([`crate::value::RoutineCell`]); a routine value used as a
//! method receiver or role-checked is viewed through that cell, so every
//! alias of the routine -- an earlier `my $g = &f`, a role argument that
//! captured it, a closure's `self`, a registry rebuild -- answers with the
//! same roles, as rakudo's in-place rebless does.

use super::*;
use crate::value::RoutineCell;

impl Interpreter {
    /// The composition cell of a routine value: a Sub, or a Mixin over one.
    // Cost: O(1).
    pub(crate) fn routine_cell_of(value: &Value) -> Option<RoutineCell> {
        match value.view() {
            ValueView::Sub(data) => Some(data.routine_cell.clone()),
            ValueView::Mixin(inner, _) => match inner.view() {
                ValueView::Sub(data) => Some(data.routine_cell.clone()),
                _ => None,
            },
            _ => None,
        }
    }

    /// The composition cell of the declaration a `Method`/`Submethod` object
    /// stands for (#12287). A lookup (`.^find_method`, `.^lookup`, `.^methods`)
    /// builds a fresh object per call, so the object only carries the identity
    /// (owner, name, candidate index) of its `MethodDef`; the def's
    /// `routine_cell` is where a `does` has to land for later lookups to see it.
    // Cost: O(c), c = candidates of the method family (clones the family).
    pub(crate) fn method_object_cell(&self, value: &Value) -> Option<RoutineCell> {
        let inner = match value.view() {
            ValueView::Mixin(inner, _) => inner.clone(),
            _ => Arc::new(value.clone()),
        };
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = inner.view()
        else {
            return None;
        };
        let class_name = class_name.resolve();
        if !matches!(class_name.as_str(), "Method" | "Submethod") {
            return None;
        }
        let map = attributes.as_map();
        let owner = map.get("__mutsu_lookup_class")?.to_string_value();
        let name = map.get("__mutsu_lookup_method")?.to_string_value();
        let idx = match map.get("__mutsu_lookup_candidate_idx").map(|v| v.view()) {
            Some(ValueView::Int(i)) => i as usize,
            _ => 0,
        };
        self.registry()
            .user_method_overloads(&owner, &name)?
            .get(idx)
            .map(|def| def.routine_cell.clone())
    }

    /// `$method does R` on a method object: compose onto the declaration's
    /// current composition and record the result in its cell, so every later
    /// lookup of the method carries it (#12287).
    // Cost: O(c) for the lookup of the cell, plus the composition itself.
    pub(crate) fn does_on_method_object(
        &mut self,
        cell: &RoutineCell,
        left: Value,
        compose: impl FnOnce(&mut Self, Value) -> Result<Value, RuntimeError>,
    ) -> Result<Value, RuntimeError> {
        let inner = match left.view() {
            ValueView::Mixin(inner, _) => inner.clone(),
            _ => Arc::new(left.clone()),
        };
        let earlier = cell.get();
        let view = match &earlier {
            Some(current) => Value::mixin_parts(inner, current.clone()),
            None => (*inner).clone(),
        };
        let composed = compose(self, view)?;
        Ok(Self::store_composition(cell, composed, earlier.as_ref()))
    }

    /// `value` as its routine currently is: `Mixin(inner Sub, composition)`
    /// when roles were composed into the routine and `value` does not already
    /// carry exactly that composition. `None` for a non-routine, a routine
    /// never mixed into, and a value that is already current.
    // Cost: O(1); one atomic load for a routine never mixed into.
    pub(crate) fn routine_current_view(value: &Value) -> Option<Value> {
        let (inner, current) = match value.view() {
            ValueView::Sub(data) => (Arc::new(value.clone()), data.routine_cell.get()?),
            ValueView::Mixin(inner, overrides) => {
                let ValueView::Sub(data) = inner.view() else {
                    return None;
                };
                let current = data.routine_cell.get()?;
                if crate::gc::Gc::ptr_eq(&current, overrides) {
                    return None;
                }
                (inner.clone(), current)
            }
            _ => return None,
        };
        Some(Value::mixin_parts(inner, current))
    }

    /// Record `composed` -- the result of a `does` on a routine -- as that
    /// routine's composition, and return the value to hand back. When the
    /// routine already had a composition (`earlier`), the new one keeps its
    /// live role cell ([`crate::value::MixinOverrides::rebased_on`]): the
    /// object is the same, so its role attributes are too. A non-routine
    /// result is returned unchanged.
    // Cost: O(1), plus O(m + a) to rebase (m = markers, a = role attributes).
    pub(crate) fn note_routine_composition(
        composed: Value,
        earlier: Option<&crate::gc::Gc<crate::value::MixinOverrides>>,
    ) -> Value {
        let ValueView::Mixin(inner, _) = composed.view() else {
            return composed;
        };
        let ValueView::Sub(data) = inner.view() else {
            return composed;
        };
        Self::store_composition(&data.routine_cell, composed.clone(), earlier)
    }

    /// [`Self::note_routine_composition`] for a `cell` that is not the inner
    /// `Sub`'s own: a method object's, held by its `MethodDef` (#12287).
    // Cost: as `note_routine_composition`.
    pub(crate) fn store_composition(
        cell: &RoutineCell,
        composed: Value,
        earlier: Option<&crate::gc::Gc<crate::value::MixinOverrides>>,
    ) -> Value {
        let ValueView::Mixin(inner, overrides) = composed.view() else {
            return composed;
        };
        let overrides = match earlier {
            Some(earlier) if !crate::gc::Gc::ptr_eq(earlier, overrides) => {
                crate::gc::Gc::new(overrides.rebased_on(earlier))
            }
            _ => overrides.clone(),
        };
        cell.set(overrides.clone());
        Value::mixin_parts(inner.clone(), overrides)
    }
}
