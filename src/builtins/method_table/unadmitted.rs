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
use crate::value::{RuntimeError, Value, ValueView};

/// Answer `method` from the [`RowFlags::ANY_ARGS`] row the receiver's shape
/// resolves, with arguments the guard step did not admit. `None` when the
/// receiver has no shape, no such row exists, or an argument still needs a
/// probe no row makes: a named argument (split by the guard step), a
/// `Junction` (which autothreads) or a `Failure` (which may explode).
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
    let refused = |arg: &Value| {
        arg.is_string_pair_value()
            || arg.is_junction_value()
            || matches!(arg.view(), ValueView::Instance { class_name, .. } if class_name == "Failure")
    };
    if args.iter().any(refused) {
        return None;
    }
    let id = resolve(Receiver::of_settled(target)?, method, args.len())?;
    if !row(id).flags.contains(RowFlags::ANY_ARGS) {
        return None;
    }
    invoke_in(id, interp, target, args)
}
