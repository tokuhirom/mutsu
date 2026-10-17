//! What a `WrapVarRef` site settled about its variable's declared type
//! (ADR-13757).
//!
//! A `VarRef` argument names a variable of the frame that built it, but the
//! binder and the multi dispatcher read it after the callee's frame exists,
//! so a by-name probe for the variable's type record starts in the wrong
//! frame and can walk into a caller's same-named variable. The `WrapVarRef`
//! site runs in the variable's own frame and knows its slot, so it resolves
//! the declared type there and the `VarRef` carries the answer.

use super::{Value, ValueRepr};
use crate::symbol::Symbol;

/// A `VarRef`'s declared type as its `WrapVarRef` site resolved it.
#[derive(Debug, Clone, Default)]
pub(crate) enum VarRefDeclared {
    /// Built without a resolution (the legacy constructors): consumers ask
    /// for the variable's type record by name.
    #[default]
    Unresolved,
    /// The type constraint (`Str`) in effect at the site, or `None` for a
    /// variable declared without one.
    Resolved(Option<Value>),
}

impl Value {
    /// [`Value::varref_slotted`] with the declared type the `WrapVarRef`
    /// site resolved in the variable's own frame.
    pub(crate) fn varref_resolved(
        name: Symbol,
        value: Value,
        slot: Option<u32>,
        declared: VarRefDeclared,
    ) -> Self {
        Value::from_repr(ValueRepr::VarRef {
            name,
            value: Box::new(value),
            index: None,
            slot,
            declared,
        })
    }

    /// The declared type recorded on a `VarRef`, or `None` when this is not
    /// one.
    // Cost: O(1).
    pub(crate) fn varref_declared(&self) -> Option<VarRefDeclared> {
        self.0.varref_declared()
    }
}
