//! `Mu.emit` (ADR-11276 §9.163): Rakudo's `method emit() { emit self }`.
//!
//! Every receiver reaches it, a type object included (`MU_FOR_CLOSED_SHAPES`);
//! a `Supplier` answers its own `emit` first. The handler is the `emit`
//! routine's own (`Interpreter::emit_to_supply`), so the method and the sub
//! deliver to the same emitter.

use crate::builtins::method_table::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

pub(super) static ROWS: &[MethodRow] = &[MethodRow {
    owner: "Mu",
    name: "emit",
    arity: 0,
    handler: Handler::Interp(emit),
    flags: RowFlags::TYPE_OBJECT_OK,
    named: &[],
}];

// Cost: as `Interpreter::emit_to_supply`.
fn emit(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    Some(interp.emit_to_supply(target.clone()))
}
