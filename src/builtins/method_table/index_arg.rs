//! The binding of a positional index the guard step declines (ADR-11276
//! §9.105): a row flagged [`RowFlags::INDEX_ARG`] takes only an `Int`, and
//! this module binds every other argument the way Rakudo's candidates do.
//!
//! Rakudo's `Range`, `Uni` and `List` declare `AT-POS` and `EXISTS-POS` for
//! `Int:D` (and native `uint`) only; any other index reaches `Any`'s
//! candidates: `AT-POS(Any:D \pos)` and `AT-POS(Num:D \pos)` re-dispatch on
//! `pos.Int`, `EXISTS-POS(Any:D \pos)` and `EXISTS-POS(Num:D \pos)` answer
//! `pos.Int == 0` (a non-`Int` index addresses the one-element list `Any`
//! is), and a type object raises `Cannot use 'T' as an index`. `Capture`
//! declares its candidates for `Int(Any) $pos` instead, so its rows
//! ([`RowFlags::INDEX_COERCES`]) are handed the coerced `Int` whatever the
//! argument was. The `.Int` of a user object is user code, so this runs only
//! from the interpreter's entry.

use super::{Receiver, RowFlags, invoke_in, resolve, row};
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{DispatchShape, RuntimeError, Value, ValueView};

/// Whether `target`'s shape resolves `method` with one argument to a row that
/// binds its index here: the cascades decline such a call, so its only
/// answer is the row's.
// Cost: O(1), a bit test, a tag probe and one hash lookup.
pub(crate) fn binds(target: &Value, method: Symbol) -> bool {
    Receiver::of_settled(target.descalarize())
        .and_then(|receiver| resolve(receiver, method, 1))
        .is_some_and(|id| row(id).flags.contains(RowFlags::INDEX_ARG))
}

/// Answer a one-argument call of an [`RowFlags::INDEX_ARG`] row whose
/// argument is not an `Int`. `None` when the receiver resolves no such row,
/// or the argument is one the call must not bind here: a `Junction` (which
/// autothreads), a `Failure` (which may explode) or a named argument.
// Cost: O(1) to find the row, plus the argument's `.Int` (user code for a
// user object), plus the handler's own cost.
pub(crate) fn invoke(
    interp: &mut Interpreter,
    target: &Value,
    method: Symbol,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    let [arg] = args else {
        return None;
    };
    let arg = arg.descalarize();
    if arg.is_string_pair_value() || arg.is_junction_value() || is_failure(arg) {
        return None;
    }
    let target = target.descalarize();
    // A receiver with no shape (an instance, a type object without one)
    // reaches `Any`'s rows through its MRO (§9.106).
    let id = match Receiver::of_settled(target) {
        Some(receiver) => resolve(receiver, method, 1)?,
        None => super::dispatch::shapeless_row(Some(&mut *interp), target, method, 1)?,
    };
    let flags = row(id).flags;
    if !flags.contains(RowFlags::INDEX_ARG) {
        return None;
    }
    // `Any:U`'s `EXISTS-POS(|)` binds any index without coercing it: nothing
    // exists in a type object or in `Nil` (§9.154).
    if method == "EXISTS-POS"
        && row(id).owner == "Any"
        && (target.is_nil() || matches!(target.view(), ValueView::Package(_)))
    {
        return Some(Ok(Value::FALSE));
    }
    if arg.dispatch_shape() == Some(DispatchShape::Int) {
        // An `Int` the row's handler declined: nothing else binds it.
        return None;
    }
    // A `Bool`, an `Int`-valued enum or an instance of a user subclass of
    // `Int` is an `Int:D`, which the row's own candidate binds.
    if let Some(index) = int_of(arg) {
        return invoke_in(id, interp, target, &[index]);
    }
    if !flags.contains(RowFlags::INDEX_COERCES)
        && let Some(name) = type_object_name(arg)
    {
        return Some(Err(RuntimeError::new(format!(
            "Cannot use '{name}' as an index"
        ))));
    }
    let index = match interp.call_method_with_values(arg.clone(), "Int", Vec::new()) {
        // An instance of a user subclass of `Int` is its own `.Int`; the
        // index is its payload.
        Ok(index) => crate::value::numeric_payload::native_base_payload(&index).unwrap_or(index),
        Err(err) => return Some(Err(err)),
    };
    if index.dispatch_shape() != Some(DispatchShape::Int) {
        // A `Failure` (`"x".Int`) or an `.Int` that answers no `Int`: the
        // index is unusable, and using it raises its error.
        return Some(Err(interp
            .failure_to_runtime_error_if_unhandled(&index)
            .unwrap_or_else(|| {
                RuntimeError::new(format!(
                    "Cannot use '{}' as an index",
                    crate::runtime::utils::value_type_name(&index)
                ))
            })));
    }
    if flags.contains(RowFlags::INDEX_COERCES) || method.as_str() == "AT-POS" {
        return invoke_in(id, interp, target, &[index]);
    }
    // `Any.EXISTS-POS`: the one-element list `Any` is has only index 0, which
    // a type object does not hold.
    let is_type_object = matches!(target.view(), ValueView::Package(_));
    Some(Ok(Value::truth(
        matches!(index.view(), ValueView::Int(0)) && !is_type_object,
    )))
}

/// Whether `arg` is a `Failure`.
// Cost: O(1), a tag probe.
fn is_failure(arg: &Value) -> bool {
    matches!(arg.view(), ValueView::Instance { class_name, .. } if class_name == "Failure")
}

/// The `Int` an argument that does `Int` stands for: a `Bool`, an `Int`-valued
/// enum or an instance of a user subclass of `Int`.
// Cost: O(1), a tag probe (and an attribute read for an instance).
fn int_of(arg: &Value) -> Option<Value> {
    match arg.view() {
        ValueView::Bool(b) => Some(Value::int(i64::from(b))),
        ValueView::Enum {
            value: crate::value::EnumValue::Int(i),
            ..
        } => Some(Value::int(*i)),
        ValueView::Instance { .. } => crate::value::numeric_payload::native_base_payload(arg)
            .filter(|payload| payload.dispatch_shape() == Some(DispatchShape::Int)),
        _ => None,
    }
}

/// The name of `arg` when it is a type object (or `Nil`), which `Any`'s
/// candidates bind as `Any:U \pos`.
// Cost: O(1), a tag probe.
fn type_object_name(arg: &Value) -> Option<String> {
    match arg.view() {
        ValueView::Package(name) => Some(name.resolve()),
        ValueView::Nil => Some("Nil".to_string()),
        _ => None,
    }
}
