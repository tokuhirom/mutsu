//! The native-base resolution (ADR-11276 §9.79, §9.111): built-in methods on an
//! instance of a user class deriving from a built-in scalar type, `Pair` or
//! quant hash.
//!
//! `class MyInt is Int {}; MyInt.new(5)` is an ordinary instance whose integer
//! lives in the reserved `__mutsu_int_value` attribute, `class MyNum is Num {}`
//! one whose float lives in `__mutsu_num_value`, and `class MyStr is Str {}`
//! one whose string lives in `__mutsu_str_value`
//! (`runtime::seed_native_subclass_payloads`). Rakudo's subclass inherits every
//! method of its base, so the method table answers such an instance with the
//! rows of its payload's shape, the payload as the invocant
//! ([`redirect`], called by the table's guard step), and the cascades answer
//! it on the payload too ([`dispatch`]) until they are gone. Without this an
//! instance has no shape, and each cascade arm re-implemented "stringify the
//! instance" on its own (ADR-11276 §9.78).
//!
//! User methods (including attribute accessors) never reach here: the native
//! fast path is bypassed when the class or its MRO declares the method.

use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

pub(crate) use crate::value::numeric_payload::{
    INT_PAYLOAD, NUM_PAYLOAD, RAT_PAYLOAD, numeric_payload_of, numeric_subclass_payload,
};

/// Whether the receiver is a built-in class with a shape of its own and a
/// payload (`Format`, a `Str` subclass; ADR-11276 §9.153) whose own row
/// answers `method`: the payload answers only what the class does not declare.
// Cost: O(1), a tag probe and one table lookup.
fn own_row_answers(target: &Value, method: crate::symbol::Symbol, arity: usize) -> bool {
    target.dispatch_shape().is_some_and(|shape| {
        crate::builtins::method_table::resolve(
            crate::builtins::method_table::Receiver::instance(shape),
            method,
            arity,
        )
        .is_some()
    })
}

/// Methods whose answer is about the subclass instance itself (its type,
/// identity or representation), which the payload must not answer for it.
// Cost: O(1), one match on the name.
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
            | "gist"
            | "Str"
            | "Stringy"
            | "clone"
            | "item"
            | "self"
            | "Capture"
            | "new"
            | "bless"
            // Type tests ask about the subclass: `MyStr.new.isa(MyStr)` is
            // True, which the `Str` payload cannot know (#12907).
            | "isa"
            | "does"
            | "can"
    )
}

/// The coercer that answers the instance itself on a subclass of the payload's
/// type (`MyInt.new(5).Int` is the `MyInt`, as Rakudo's `Int.Int` is `self`),
/// next to `Numeric` and `Real` for a numeric payload.
// Cost: O(1).
fn returns_the_instance(payload: &Value, method: &str) -> bool {
    match payload.view() {
        ValueView::Int(_) | ValueView::BigInt(_) => matches!(method, "Int" | "Numeric" | "Real"),
        ValueView::Num(_) => matches!(method, "Num" | "Numeric" | "Real"),
        ValueView::Rat(..) | ValueView::BigRat(..) => {
            matches!(method, "Rat" | "Numeric" | "Real")
        }
        ValueView::Pair(..) | ValueView::ValuePair(..) => method == "Pair",
        _ => false,
    }
}

/// The built-in value an instance of a user class deriving from a built-in
/// type carries: a scalar payload ([`native_base_payload`]), the `Pair` of an
/// `is Pair` subclass (`__mutsu_pair_value`, see `dispatch_new_unallocated`)
/// or the `Bag`/`Set`/`Mix` of a quant-hash subclass (`__baggy_data__`).
///
/// [`native_base_payload`]: crate::value::numeric_payload::native_base_payload
// Cost: O(1) (at most seven attribute-map lookups).
fn subclass_payload(target: &Value) -> Option<Value> {
    if let Some(payload) = crate::value::numeric_payload::native_base_payload(target) {
        return Some(payload);
    }
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    let map = attributes.as_map();
    map.get("__mutsu_pair_value")
        .or_else(|| map.get("__baggy_data__"))
        .cloned()
}

/// The value standing in for `target` as the invocant of the built-in
/// `method`: the payload of an instance of a user class deriving from a
/// built-in scalar type, unless the method answers about the instance itself.
/// `None` for every other receiver, and for a plain user class, which keeps
/// `X::Method::NotFound`.
// Cost: O(1) for a non-instance receiver; for an instance at most five
// attribute-map lookups.
#[inline]
pub(crate) fn redirect(target: &Value, method: &str) -> Option<Value> {
    if !matches!(target.view(), ValueView::Instance { .. }) || answered_by_the_instance(method) {
        return None;
    }
    let payload = subclass_payload(target)?;
    (!returns_the_instance(&payload, method)).then_some(payload)
}

/// Answer a native method called on an instance with a native base, in the
/// cascades: the coercers that return the instance itself, and every other
/// method the payload answers natively, on the payload. `None` leaves the call
/// to the ordinary instance dispatch.
// Cost: O(1) plus the delegated native method's own cost.
pub(crate) fn dispatch(
    target: &Value,
    method_sym: Symbol,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    let method = method_sym.as_str();
    if answered_by_the_instance(method) || own_row_answers(target, method_sym, args.len()) {
        return None;
    }
    let payload = subclass_payload(target)?;
    match args {
        [] if returns_the_instance(&payload, method) => Some(Ok(target.clone())),
        [] => super::methods_0arg::native_method_0arg(&payload, method_sym),
        [arg] => super::native_method_1arg(&payload, method_sym, arg),
        _ => None,
    }
}
