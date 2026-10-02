//! Native numeric methods on an instance of a user subclass of `Int` or `Num`.
//!
//! `class MyInt is Int {}; MyInt.new(5)` is an ordinary instance whose integer
//! lives in the reserved `__mutsu_int_value` attribute, and
//! `class MyNum is Num {}; MyNum.new(2.5)` one whose float lives in
//! `__mutsu_num_value` (`runtime::seed_native_subclass_payloads`). Value-level
//! coercion already reads them, but the native method layer did not, so
//! `.succ` / `.pred` / `.abs` died with "No such method", `.is-prime` answered
//! `False`, and `$x++` fell back to its "not a number" seed of 1. Rakudo's
//! subclass inherits every `Int` / `Num` method, so the native layer answers
//! them on the payload.
//!
//! User methods (including attribute accessors) never reach here: the native
//! fast path is bypassed when the class or its MRO declares the method.

use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

pub(crate) use crate::value::numeric_payload::{
    INT_PAYLOAD, NUM_PAYLOAD, RAT_PAYLOAD, numeric_payload_of, numeric_subclass_payload,
};

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
            | "Stringy"
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
/// `.Numeric` / `.Real`, and `.Int` on an `Int` subclass or `.Num` on a `Num`
/// one, return the invocant, as Rakudo's do (the subclass instance already is
/// one); every other method the payload answers natively is answered on the
/// payload. `None` leaves the call to the
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
    let payload = numeric_subclass_payload(target)?;
    let own_coercer = match payload.view() {
        ValueView::Num(_) => "Num",
        ValueView::Rat(..) | ValueView::BigRat(..) => "Rat",
        _ => "Int",
    };
    match args {
        [] => match method {
            "Numeric" | "Real" => Some(Ok(target.clone())),
            _ if method == own_coercer => Some(Ok(target.clone())),
            _ => super::methods_0arg::native_method_0arg(&payload, method_sym),
        },
        [arg] => super::native_method_1arg(&payload, method_sym, arg),
        _ => None,
    }
}
