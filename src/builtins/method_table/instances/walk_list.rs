//! The rows of `WalkList`, the result of `.WALK(...)` (ADR-11276 §9.140):
//! `invoke` batch-calls the matched candidates, `reverse` and `quiet` answer
//! a derived `WalkList` with that flag changed.
//!
//! The rows are `OWNER_ONLY`, found by their owner for a `WalkList` instance
//! from `call_method_with_values`; every other method of a `WalkList` (`gist`,
//! `isa`, ...) is the ordinary dispatch's.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

type Answer = Option<Result<Value, RuntimeError>>;

const OWN: RowFlags = RowFlags::OWNER_ONLY
    .or(RowFlags::ANY_ARGS)
    .or(RowFlags::SLURPY)
    .or(RowFlags::ANY_NAMED);

macro_rules! row {
    ($name:literal, $handler:ident) => {
        MethodRow {
            owner: "WalkList",
            name: $name,
            arity: 0,
            handler: Handler::Interp($handler),
            flags: OWN,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("invoke", invoke),
    row!("reverse", reverse),
    row!("quiet", quiet),
];

// Cost: O(c), c = matched candidates (one call each).
fn invoke(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    let mut all = Vec::with_capacity(args.len() + named.pairs().len());
    all.extend(args.iter().cloned());
    all.extend(named.pairs().iter().cloned());
    Some(i.walk_list_invoke_direct(t, all))
}

// Cost: O(a), a = attributes of the WalkList (copied).
fn reverse(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(Ok(Interpreter::walk_list_with_flag(t, "reversed", true)))
}

// Cost: O(a), a = attributes of the WalkList (copied).
fn quiet(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(Ok(Interpreter::walk_list_with_flag(t, "quiet", true)))
}
