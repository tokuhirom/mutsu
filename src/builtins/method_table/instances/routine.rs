//! `Routine`'s `is_dispatcher`, `dispatcher`, `multi` and `package` (ADR-11276
//! §9.121).
//!
//! A routine is a `Sub` or a `&name` handle on a routine the registry holds;
//! a `Method` object `.^methods` builds is an instance of a code class, which
//! answers only `package`. None has a dispatch shape, so the rows are reached
//! through their owner (`RowFlags::OWNER_ONLY`, `invoke_owner`): from the
//! callable dispatch for a `Sub` or a handle, and from the method-call fallback
//! for a code-class instance. The implementations are `Interpreter` methods in
//! `runtime/methods_routine_rows.rs`.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

macro_rules! row {
    ($name:literal, $handler:ident) => {
        MethodRow {
            owner: "Routine",
            name: $name,
            arity: 0,
            handler: Handler::Interp($handler),
            flags: RowFlags::OWNER_ONLY,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("is_dispatcher", is_dispatcher),
    row!("dispatcher", dispatcher),
    row!("multi", multi),
    row!("package", package),
];

/// The owners the rows reachable from `target` are found under: a `Sub`, a
/// routine handle, or an instance of a code class.
// Cost: O(1).
pub(crate) fn owners_of(target: &Value) -> Option<&'static [&'static str]> {
    match target.view() {
        ValueView::Sub(_) | ValueView::Routine { .. } => Some(&["Routine"]),
        ValueView::Instance { class_name, .. }
            if matches!(
                class_name.resolve().as_str(),
                "Method" | "Submethod" | "Sub" | "Routine" | "Code" | "Block"
            ) =>
        {
            Some(&["Routine"])
        }
        _ => None,
    }
}

/// `Routine.is_dispatcher`.
// Cost: O(c), c = multi candidates.
fn is_dispatcher(i: &mut Interpreter, t: &Value, _args: &[Value], _named: Named<'_>) -> Answer {
    i.routine_is_dispatcher(t).map(Ok)
}

/// `Routine.dispatcher`.
// Cost: O(c), c = multi candidates.
fn dispatcher(i: &mut Interpreter, t: &Value, _args: &[Value], _named: Named<'_>) -> Answer {
    i.routine_dispatcher(t).map(Ok)
}

/// `Routine.multi`.
// Cost: O(1).
fn multi(i: &mut Interpreter, t: &Value, _args: &[Value], _named: Named<'_>) -> Answer {
    i.routine_multi(t).map(Ok)
}

/// `Routine.package`.
// Cost: O(n), n = the handle's name length.
fn package(i: &mut Interpreter, t: &Value, _args: &[Value], _named: Named<'_>) -> Answer {
    i.routine_package(t).map(Ok)
}
