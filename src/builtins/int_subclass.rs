//! Native `Int` methods on an instance of a user subclass of `Int`.
//!
//! `class MyInt is Int {}; MyInt.new(5)` is an ordinary instance whose integer
//! lives in the reserved `__mutsu_int_value` attribute
//! (`runtime::seed_native_subclass_payloads`). Value-level coercion already
//! reads it, but the native method layer did not, so `.succ` / `.pred` / `.abs`
//! died with "No such method", `.is-prime` answered `False`, and `$x++` fell
//! back to its "not a number" seed of 1. Rakudo's subclass inherits every
//! `Int` method, so the native layer answers them on the payload.
//!
//! User methods (including attribute accessors) never reach here: the native
//! fast path is bypassed when the class or its MRO declares the method.

use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

/// The `Int` payload of an instance of a user subclass of `Int`.
// Cost: O(1) (one attribute-map lookup).
pub(crate) fn int_subclass_payload(target: &Value) -> Option<Value> {
    match target.view() {
        ValueView::Instance { attributes, .. } => {
            attributes.as_map().get("__mutsu_int_value").cloned()
        }
        _ => None,
    }
}

/// Methods whose answer is about the subclass instance itself (its type,
/// identity or representation), which the payload must not answer for it.
fn answered_by_the_instance(method: &str) -> bool {
    matches!(
        method,
        "WHAT"
            | "WHO"
            | "HOW"
            | "WHY"
            | "WHICH"
            | "WHERE"
            | "VAR"
            | "DEFINITE"
            | "raku"
            | "perl"
            | "gist"
            | "Str"
            | "clone"
            | "item"
            | "self"
            | "Capture"
            | "new"
            | "bless"
    )
}

/// Answer a native method called on an `Int`-subclass instance.
///
/// `.Int` / `.Numeric` / `.Real` return the invocant, as Rakudo's do for an
/// `Int` (an `Int` subclass already is one); every other method the payload
/// answers natively is answered on the payload. `None` leaves the call to the
/// ordinary instance dispatch.
// Cost: O(1) plus the delegated native method's own cost.
pub(crate) fn dispatch(
    target: &Value,
    method_sym: Symbol,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    let method = method_sym.as_str();
    if answered_by_the_instance(method) {
        return None;
    }
    let payload = int_subclass_payload(target)?;
    match args {
        [] => match method {
            "Int" | "Numeric" | "Real" => Some(Ok(target.clone())),
            _ => super::methods_0arg::native_method_0arg(&payload, method_sym),
        },
        [arg] => super::native_method_1arg(&payload, method_sym, arg),
        _ => None,
    }
}
