//! `Exception`'s and `Failure`'s control methods (ADR-11276 §9.163): `throw`,
//! `rethrow`, `die`, `fail`, `resume` and `Exception.Failure`.
//!
//! Each raises, re-arms or resumes through the interpreter (a user
//! `method message`, the throw-site backtrace and dynamics, a class doing
//! `X::Control`), so the rows are `Handler::Interp`. The exception classes are
//! open-ended and have no shape: the rows are `OWNER_ONLY`, reached by the
//! receiver's MRO from `call_method_with_values`
//! (`Interpreter::exception_control_by_mro`). Rakudo declares every one
//! `Exception:D:`, so a type object is refused (`DEFINITE`, `X::NYI.throw`).
//!
//! `Failure` answers `rethrow`, `die` and `fail` through its `FALLBACK`,
//! which throws the exception it carries (its own `throw` is a pure row,
//! `method_table::failure`).

use super::{Handler, MethodRow, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

type Answer = Option<Result<Value, RuntimeError>>;

const CONTROL: RowFlags = RowFlags::OWNER_ONLY.or(RowFlags::DEFINITE);

macro_rules! control {
    ($owner:literal, $name:literal, $handler:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Interp($handler),
            flags: CONTROL,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    control!("Exception", "throw", |interp, target, _, _| raise(
        interp, target, "throw"
    )),
    // `throw(Backtrace $bt?)`: the backtrace to throw with.
    // Cost: O(1) plus the throw, as `Interpreter::exception_control`.
    MethodRow {
        owner: "Exception",
        name: "throw",
        arity: 1,
        handler: Handler::Interp(|interp, target, args, _| {
            Some(interp.exception_throw_with_backtrace(target, &args[0]))
        }),
        flags: CONTROL,
        named: &[],
    },
    control!("Exception", "rethrow", |interp, target, _, _| raise(
        interp, target, "rethrow"
    )),
    control!("Exception", "die", |interp, target, _, _| raise(
        interp, target, "die"
    )),
    control!("Exception", "fail", |interp, target, _, _| raise(
        interp, target, "fail"
    )),
    control!("Exception", "resume", |interp, target, _, _| raise(
        interp, target, "resume"
    )),
    control!("Exception", "Failure", |interp, target, _, _| failure(
        interp, target
    )),
    control!("Failure", "rethrow", |interp, target, _, _| interp
        .failure_control(target)),
    control!("Failure", "die", |interp, target, _, _| interp
        .failure_control(target)),
    control!("Failure", "fail", |interp, target, _, _| interp
        .failure_control(target)),
];

// Cost: as `Interpreter::exception_control`.
fn raise(interp: &mut Interpreter, target: &Value, method: &str) -> Answer {
    Some(interp.exception_control(target, method))
}

/// `Exception.Failure` (the idiom `CATCH { return .Failure }`): the same value
/// `Failure.new($exception)` builds.
// Cost: O(1).
fn failure(interp: &mut Interpreter, target: &Value) -> Answer {
    Some(Ok(
        interp.build_native_failure_value(std::slice::from_ref(target))
    ))
}
