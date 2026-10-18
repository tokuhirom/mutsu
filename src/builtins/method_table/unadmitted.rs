//! The entry for a call the guard step declined over an argument it does not
//! admit (ADR-11276 §9.99).
//!
//! The guard step hands a row only plain arguments, so an allomorph (`<1>`),
//! an instance of a `Real` class (`Duration.new(0.01)`) or a mixin takes the
//! cascades. A row flagged [`RowFlags::ANY_ARGS`] binds its arguments itself
//! (`Num.Rat($epsilon)` reads any `Real` and raises the bind failure for
//! anything else), so once the cascades have declined too, the interpreter
//! asks that row here, with the arguments as they are.

use super::{Receiver, RowFlags, invoke_in, names_a_row, resolve, row};
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{DispatchShape, RuntimeError, Value, ValueView};

/// Whether `arg` is an argument the guard step declines but whose value is
/// built in, so an `ANY_ARGS` row reads it purely: an allomorph (`<1>`, a
/// number mixed with its `Str`), a `Duration`/`Instant`, or an itemized plain
/// value (handed to the row without its `Scalar`). A user object is
/// not (its `.Numeric` or `.Real` is user code, which the slow path calls).
// Cost: O(1), tag probes.
fn is_builtin_value_argument(arg: &Value) -> bool {
    if arg.is_plain_argument() {
        return true;
    }
    match arg.view() {
        // An itemized value read out of a variable (`$k` holding `1..2`): the
        // row is handed the value itself.
        ValueView::Scalar(inner) => is_builtin_value_argument(inner),
        ValueView::Mixin(inner, mixins) => {
            mixins.contains_key("Str")
                && matches!(
                    inner.view(),
                    ValueView::Int(_)
                        | ValueView::BigInt(_)
                        | ValueView::Rat(..)
                        | ValueView::Num(_)
                        | ValueView::Complex(..)
                )
        }
        _ => matches!(
            arg.dispatch_shape(),
            Some(DispatchShape::Duration | DispatchShape::Instant)
        ),
    }
}

/// The arguments of a [`RowFlags::NUMERIC_ARGS`] row: each user object
/// coerced to its number, through its `Bridge` when its class composes `Real`
/// (the operators' rule, `coerce_infix_operand_numeric`) and its `Numeric`
/// otherwise; a built-in value is left as it is. `None` when a coercion fails,
/// and the call takes the cascades.
// Cost: O(a), a = arguments, plus the coercions' user code.
fn numeric_arguments(interp: &mut Interpreter, args: &[Value]) -> Option<Vec<Value>> {
    args.iter()
        .map(|arg| {
            if !matches!(arg.descalarize().view(), ValueView::Instance { .. })
                || is_builtin_value_argument(arg)
            {
                return Some(arg.clone());
            }
            let arg = arg.descalarize().clone();
            let first = if interp.type_matches_value("Real", &arg) {
                "Bridge"
            } else {
                "Numeric"
            };
            interp
                .call_method_with_values(arg.clone(), first, Vec::new())
                .or_else(|_| interp.call_method_with_values(arg, "Bridge", Vec::new()))
                .ok()
        })
        .collect()
}

/// Answer `method` from the [`RowFlags::ANY_ARGS`] row the receiver's shape
/// (or, for an allomorph, the part it reads) resolves, with arguments the
/// guard step did not admit. `None` when the receiver has no shape, no such row exists, or an argument is not a
/// built-in value ([`is_builtin_value_argument`]): a named argument (split by
/// the guard step), a `Junction` (which autothreads), a `Failure` (which may
/// explode) or a user object. A row flagged [`RowFlags::OBJECT_ARGS`] takes a
/// user object too.
// Cost: O(a) to check a arguments, plus O(1) to find the row (a bit test, a
// tag probe and one hash lookup), plus the handler's own cost.
pub(crate) fn invoke_unadmitted(
    interp: &mut Interpreter,
    target: &Value,
    method: &str,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    let method = Symbol::intern(method);
    if args.is_empty() || !names_a_row(method, args.len()) {
        return None;
    }
    // A positional index is bound by its own rules, a user object included.
    if let Some(answer) = super::index_arg::invoke(interp, target, method, args) {
        return Some(answer);
    }
    if args.iter().any(Value::is_string_pair_value) {
        return None;
    }
    // An allomorph receiver is answered on the part the row reads (§9.101).
    let (part, id) = match Receiver::of_settled(target) {
        Some(receiver) => (None, resolve(receiver, method, args.len())?),
        None => {
            let (part, id) = super::allomorph::redirect(target, method, args.len())?;
            (Some(part), id)
        }
    };
    let flags = row(id).flags;
    if !flags.contains(RowFlags::ANY_ARGS) {
        return None;
    }
    // A numeric parameter coerces a user object (`Num.log($real-object)`).
    let coerced;
    let args = if flags.contains(RowFlags::NUMERIC_ARGS) {
        coerced = numeric_arguments(interp, args)?;
        &coerced[..]
    } else {
        args
    };
    // A user object is an argument only a row that binds `Mu` takes; a
    // `Junction` autothreads and a `Failure` may explode before any row.
    let admitted = if flags.contains(RowFlags::OBJECT_ARGS) {
        args.iter().all(|arg| {
            let arg = arg.descalarize();
            !arg.is_junction_value()
                && !matches!(arg.view(), ValueView::Instance { class_name, .. } if class_name == "Failure")
        })
    } else {
        args.iter().all(is_builtin_value_argument)
    };
    if !admitted {
        return None;
    }
    let args: Vec<Value> = args.iter().map(|arg| arg.descalarize().clone()).collect();
    invoke_in(id, interp, part.as_ref().unwrap_or(target), &args)
}
