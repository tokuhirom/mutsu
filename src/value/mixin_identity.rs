//! Which coercion methods are the identity on a value of a core type.
//!
//! Rakudo's core types answer a coercion to a type they already are with
//! `self` (`method Int(Int:D:) { self }`, `Str.Str`, `Num.Numeric`, ...), so a
//! value with a role mixed in (`5 but R`) keeps the role through it. Other
//! coercions build a new value and drop the mixin.

use super::{Value, ValueView};

/// Whether `.<method>()` on `value` returns `value` itself in rakudo.
// Cost: O(1).
pub(crate) fn is_identity_coercion(value: &Value, method: &str) -> bool {
    match value.view() {
        ValueView::Bool(_) => method == "Bool",
        ValueView::Int(_) | ValueView::BigInt(_) => {
            matches!(method, "Int" | "Numeric" | "Real")
        }
        ValueView::Num(_) => matches!(method, "Num" | "Numeric" | "Real"),
        ValueView::Rat(..) | ValueView::BigRat(..) => {
            matches!(method, "Rat" | "Numeric" | "Real")
        }
        ValueView::Complex(..) => matches!(method, "Complex" | "Numeric"),
        ValueView::Str(_) => method == "Str",
        _ => false,
    }
}

/// Whether the prefix coercion that calls `method` (`+` → `Numeric`, `~` →
/// `Str`, `?` → `Bool`) returns the `but`-mixed `value` itself: the wrapped
/// value already is that type, the mixin overrides nothing for it, and the
/// value is not an allomorph (`+<42>` is the plain `Int`).
// Cost: O(1).
pub(crate) fn prefix_coercion_keeps_mixin(value: &Value, method: &str) -> bool {
    let ValueView::Mixin(inner, mixins) = value.view() else {
        return false;
    };
    !mixins.contains_key(method)
        && super::types::allomorph_type_name(inner, mixins).is_none()
        && is_identity_coercion(inner, method)
}
