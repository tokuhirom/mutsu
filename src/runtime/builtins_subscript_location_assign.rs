//! `X<k> = v` / `X[i] = v` through an object that serves the subscript
//! protocol itself, for a receiver reached through an expression or a chain.
use super::*;
use crate::value::ValueView;

impl Interpreter {
    /// Store `val` at `target{idx}` / `target[idx]` when `target` is an object
    /// (a class instance, or one with a role mixed in) that implements the
    /// subscript protocol: its `ASSIGN-KEY`/`ASSIGN-POS` if it has one, else
    /// the location its rw `AT-KEY`/`AT-POS` hands back (an `is rw` accessor
    /// returning a `Proxy`, Red's JSON items). `None` leaves every other
    /// target, and an object whose accessors are not rw, to the container paths.
    // Cost: O(1) to decline; otherwise one method resolution plus the call(s).
    pub(crate) fn assign_through_object_location(
        &mut self,
        target: &Value,
        idx: &Value,
        val: &Value,
        is_positional: bool,
    ) -> Option<Result<(), RuntimeError>> {
        let object = target.deref_container();
        let object = object.descalarize();
        if !matches!(
            object.view(),
            ValueView::Instance { .. } | ValueView::Mixin(..)
        ) {
            return None;
        }
        if let Some(result) = self.assign_through_subscript_protocol(object, idx, val, is_positional)
        {
            return Some(result.map(|_| ()));
        }
        let accessor = self.object_subscript_accessor(object, is_positional)?;
        let location =
            match self.call_method_with_values(object.clone(), accessor, vec![idx.clone()]) {
                Ok(location) => location,
                Err(e) => return Some(Err(e)),
            };
        self.assign_lvalue_container(&location, val.clone())
            .map(|assigned| assigned.map(|_| ()))
    }
}
