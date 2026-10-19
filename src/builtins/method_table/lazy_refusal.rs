//! The lazy step's refusals (ADR-13891): what a counting, ordering or
//! sampling method answers on a lazy receiver instead of reifying it.
//!
//! Rakudo checks `is-lazy` before it iterates, so these methods never run
//! the sequence. They answer a `Failure` of `X::Cannot::Lazy` naming the
//! method and the type it would have reified onto (`.sum` onto a `Seq`),
//! and `tail` and a counted `roll` die with it. A receiver that is not
//! lazy (a plain `gather`, a pipe over a finite source) goes on to the
//! reify rule (`lazy_pull::reifies`) and is answered from its elements.

use crate::runtime::Interpreter;
use crate::runtime::utils::{cannot_lazy_exception, cannot_lazy_failure_onto};
use crate::value::{LazyList, RuntimeError, Value};

type Answer = Option<Result<Value, RuntimeError>>;

/// The type a lazy receiver reports (`Seq`, `List` or `Array`), which
/// Rakudo names as the type the refused method would reify onto.
// Cost: O(1).
fn reported_type(target: &Value) -> &'static str {
    match crate::value::type_name::value_type_name(target) {
        "Array" => "Array",
        "List" => "List",
        _ => "Seq",
    }
}

/// `sum` and `reverse`: a `Failure` naming the receiver's own type.
/// `List.reverse` spells its action without the dot, as Rakudo's does.
// Cost: O(1).
pub(super) fn sum(
    _interp: &mut Interpreter,
    target: &Value,
    list: &LazyList,
    args: &[Value],
) -> Answer {
    sum_reverse(target, list, args, "sum")
}

/// See [`sum`].
// Cost: O(1).
pub(super) fn reverse(
    _interp: &mut Interpreter,
    target: &Value,
    list: &LazyList,
    args: &[Value],
) -> Answer {
    sum_reverse(target, list, args, "reverse")
}

// Cost: O(1).
fn sum_reverse(target: &Value, list: &LazyList, args: &[Value], method: &str) -> Answer {
    if !args.is_empty() || !list.is_genuinely_lazy() {
        return None;
    }
    let what = reported_type(target);
    let action = if method == "reverse" && what == "List" {
        "reverse".to_string()
    } else {
        format!(".{method}")
    };
    Some(Ok(cannot_lazy_failure_onto(&action, what)))
}

/// `pick` and `roll`: Rakudo reads a `Seq` through its `.list`, so the type
/// is `List` there. A counted `roll` dies with the exception instead.
// Cost: O(1).
pub(super) fn pick(
    _interp: &mut Interpreter,
    target: &Value,
    list: &LazyList,
    args: &[Value],
) -> Answer {
    sample(target, list, args, "pick")
}

/// See [`pick`].
// Cost: O(1).
pub(super) fn roll(
    _interp: &mut Interpreter,
    target: &Value,
    list: &LazyList,
    args: &[Value],
) -> Answer {
    sample(target, list, args, "roll")
}

// Cost: O(1).
fn sample(target: &Value, list: &LazyList, args: &[Value], method: &str) -> Answer {
    if args.len() > 1 || !list.is_genuinely_lazy() {
        return None;
    }
    let what = match reported_type(target) {
        "Array" => "Array",
        _ => "List",
    };
    let action = format!(".{method} from");
    if method == "roll" && !args.is_empty() {
        return Some(Err(RuntimeError::from_exception_value(
            cannot_lazy_exception(&action, what),
        )));
    }
    Some(Ok(cannot_lazy_failure_onto(&action, what)))
}

/// `elems`, and `Numeric`/`Int`, which count through `elems`.
// Cost: O(1).
pub(super) fn elems(
    _interp: &mut Interpreter,
    _target: &Value,
    list: &LazyList,
    args: &[Value],
) -> Answer {
    count(list, args, ".elems")
}

/// `end`: a `Seq`'s is a `Failure`; `List.end` is `self.elems - 1`, whose
/// arithmetic throws the `.elems` `Failure`, so a `List` or `Array` dies.
// Cost: O(1).
pub(super) fn end(
    _interp: &mut Interpreter,
    target: &Value,
    list: &LazyList,
    args: &[Value],
) -> Answer {
    if reported_type(target) == "Seq" {
        return count(list, args, ".end");
    }
    (args.is_empty() && list.is_genuinely_lazy()).then(|| {
        Err(RuntimeError::from_exception_value(cannot_lazy_exception(
            ".elems", "",
        )))
    })
}

// Cost: O(1).
fn count(list: &LazyList, args: &[Value], action: &str) -> Answer {
    (args.is_empty() && list.is_genuinely_lazy()).then(|| Ok(cannot_lazy_failure_onto(action, "")))
}

/// `tail`, with or without a count, dies: Rakudo's `Cannot tail a lazy list`.
// Cost: O(1).
pub(super) fn tail(
    _interp: &mut Interpreter,
    _target: &Value,
    list: &LazyList,
    args: &[Value],
) -> Answer {
    if args.len() > 1 || !list.is_genuinely_lazy() {
        return None;
    }
    Some(Err(RuntimeError::from_exception_value(
        cannot_lazy_exception("tail", ""),
    )))
}
