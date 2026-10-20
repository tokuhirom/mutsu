//! `Promise`'s class methods (ADR-11276 §9.120): `start`, `in`, `at`, `kept`,
//! `broken`, `allof` and `anyof`, called on the `Promise` type object or a user
//! subclass's.
//!
//! The rows are reached through their owner (`RowFlags::OWNER_ONLY`,
//! `invoke_owner`) from the by-name dispatch that answered them before
//! (`dispatch_method_by_name_1`), for a type-object receiver. A handler declines
//! (`None`) a type object that is no `Promise` class (`Interpreter::
//! promise_class_name`), and the call goes on to the rest of that dispatch, as
//! the arm did. The implementations are `Interpreter` methods in
//! `runtime/methods_promise_class.rs`; they read the `:scheduler` named argument
//! from the argument list, so a handler hands it the positional and named
//! arguments back as one list.

use super::super::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

type Answer = Option<Result<Value, RuntimeError>>;

/// A class-method row of `Promise` taking `$arity` positional arguments (and,
/// with `slurpy`, any number more).
macro_rules! row {
    ($name:literal, $arity:literal, $handler:ident) => {
        row!(
            $name,
            $arity,
            $handler,
            RowFlags::OWNER_ONLY
                .or(RowFlags::TYPE_OBJECT_OK)
                .or(RowFlags::CLASS_METHOD)
        )
    };
    ($name:literal, $arity:literal, $handler:ident, slurpy) => {
        row!(
            $name,
            $arity,
            $handler,
            RowFlags::OWNER_ONLY
                .or(RowFlags::TYPE_OBJECT_OK)
                .or(RowFlags::CLASS_METHOD)
                .or(RowFlags::SLURPY)
        )
    };
    ($name:literal, $arity:literal, $handler:ident, $flags:expr) => {
        row!($name, $arity, $handler, $flags, &[])
    };
    ($name:literal, $arity:literal, $handler:ident, $flags:expr, $named:expr) => {
        MethodRow {
            owner: "Promise",
            name: $name,
            arity: $arity,
            handler: Handler::Interp($handler),
            flags: $flags,
            named: $named,
        }
    };
}

const CLASS: RowFlags = RowFlags::OWNER_ONLY
    .or(RowFlags::TYPE_OBJECT_OK)
    .or(RowFlags::CLASS_METHOD);

pub(super) static ROWS: &[MethodRow] = &[
    // `start(&code, :&catch, :$scheduler, |c)`: `|c` takes every other named
    // argument.
    row!("start", 1, start, CLASS.or(RowFlags::ANY_NAMED)),
    row!("in", 1, in_row, CLASS, &["scheduler"]),
    row!("at", 1, at, CLASS, &["scheduler"]),
    row!("kept", 0, kept),
    row!("kept", 1, kept),
    row!("broken", 0, broken),
    row!("broken", 1, broken),
    row!("allof", 0, allof, slurpy),
    row!("anyof", 0, anyof, slurpy),
];

/// The call's positional arguments followed by its named ones, the one list the
/// implementations read.
// Cost: O(a), a = arguments.
fn all_args(args: &[Value], named: Named<'_>) -> Vec<Value> {
    let mut all = args.to_vec();
    all.extend(named.pairs().iter().cloned());
    all
}

/// `Promise.start(&code, :$scheduler)`.
// Cost: O(1) to schedule the block.
fn start(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    i.promise_class_start(t, &all_args(args, named))
}

/// `Promise.in($seconds, :$scheduler)`: kept once the delay has elapsed.
// Cost: O(log n) to arm the timer, n = pending timers.
fn in_row(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    i.dispatch_promise_in(t, &all_args(args, named))
}

/// `Promise.at($instant, :$scheduler)`: kept at the instant.
// Cost: O(log n) to arm the timer, n = pending timers.
fn at(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    i.dispatch_promise_at(t, &all_args(args, named))
}

/// `Promise.kept($value?)`: a promise already kept.
// Cost: O(1).
fn kept(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    i.dispatch_promise_kept(t, &all_args(args, named))
}

/// `Promise.broken($reason?)`: a promise already broken.
// Cost: O(1).
fn broken(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    i.dispatch_promise_broken(t, &all_args(args, named))
}

/// `Promise.allof(@promises)`: kept once every promise is resolved.
// Cost: O(p), p = promises.
fn allof(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    i.dispatch_promise_allof(t, &all_args(args, named))
}

/// `Promise.anyof(@promises)`: kept once any promise is resolved.
// Cost: O(p), p = promises.
fn anyof(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    i.dispatch_promise_anyof(t, &all_args(args, named))
}
