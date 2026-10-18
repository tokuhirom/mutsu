//! `Mu.WALK` and `Mu.return` (ADR-11276 §9.164).
//!
//! Every receiver reaches them (`MU_FOR_CLOSED_SHAPES`). `WALK` walks the
//! receiver's class chain for the own candidates of a method
//! (`Interpreter::try_walk_method`) and answers a callable that runs them; a
//! receiver with no class (a plain value) declines. `.return` returns its
//! invocant from the enclosing routine.

use crate::builtins::method_table::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Mu",
        name: "WALK",
        arity: 0,
        handler: Handler::Interp(walk),
        flags: RowFlags::TYPE_OBJECT_OK
            .or(RowFlags::ANY_ARGS)
            .or(RowFlags::OBJECT_ARGS)
            .or(RowFlags::SLURPY)
            .or(RowFlags::ANY_NAMED),
        named: &[],
    },
    MethodRow {
        owner: "Mu",
        name: "return",
        arity: 0,
        handler: Handler::Interp(return_),
        flags: RowFlags::TYPE_OBJECT_OK,
        named: &[],
    },
];

// Cost: O(c), c = classes and roles walked, plus one call per candidate found.
fn walk(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let mut all = args.to_vec();
    all.extend_from_slice(named.pairs());
    interp.try_walk_method(target, &all).transpose()
}

/// `.return`: a `return` from the enclosing routine carrying the invocant.
// Cost: O(1).
fn return_(
    _interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let mut err = RuntimeError::new("return");
    err.return_value = Some(target.clone());
    err.set_from_method_return();
    Some(Err(err))
}
