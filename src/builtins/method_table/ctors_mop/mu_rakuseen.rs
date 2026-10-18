//! `Mu.rakuseen($id, &code)` (ADR-11276 §9.164): the cyclic-structure guard a
//! user `.raku` wraps its body in.
//!
//! Every receiver reaches it (`MU_FOR_CLOSED_SHAPES`). The handler is
//! `Interpreter::mu_rakuseen`, which keeps the ids being rendered.

use crate::builtins::method_table::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

pub(super) static ROWS: &[MethodRow] = &[MethodRow {
    owner: "Mu",
    name: "rakuseen",
    arity: 2,
    handler: Handler::Interp(rakuseen),
    flags: RowFlags::TYPE_OBJECT_OK
        .or(RowFlags::ANY_ARGS)
        .or(RowFlags::OBJECT_ARGS),
    named: &[],
}];

// Cost: as `Interpreter::mu_rakuseen`.
fn rakuseen(
    interp: &mut Interpreter,
    _target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    Some(interp.mu_rakuseen(&args[0], &args[1]))
}
