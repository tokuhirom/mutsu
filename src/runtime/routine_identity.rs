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
        let ValueView::Mixin(inner, overrides) = composed.view() else {
            return composed;
        };
        let ValueView::Sub(data) = inner.view() else {
            return composed;
        };
        let overrides = match earlier {
            Some(earlier) if !crate::gc::Gc::ptr_eq(earlier, overrides) => {
                crate::gc::Gc::new(overrides.rebased_on(earlier))
            }
            _ => overrides.clone(),
        };
        data.routine_cell.set(overrides.clone());
        Value::mixin_parts(inner.clone(), overrides)
    }
}
