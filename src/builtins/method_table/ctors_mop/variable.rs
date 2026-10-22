//! `Variable`'s rows (ADR-11276 §9.195): `block` and `var` of the reflective
//! `Variable` a variable trait handler (`trait_mod:<is>(Variable $v, ...)`)
//! receives, which Rakudo declares on `Variable`.
//!
//! A `Variable` is an instance carrying the declaration's facts (`name`,
//! `__mutsu_var_target`). The class has no dispatch shape; the rows are
//! reached through their owner (`RowFlags::OWNER_ONLY`, `invoke_owner`) from
//! the instance dispatch, as `Attribute`'s are (§9.121).

use super::super::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Variable",
        name: "block",
        arity: 0,
        handler: Handler::Interp(block),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Variable",
        name: "var",
        arity: 0,
        handler: Handler::Interp(var),
        flags: RowFlags::NONE,
        named: &[],
    },
];

/// `Variable.block`: the block that owns the declaration
/// (`runtime::attach_target`), to which the handler may add phasers.
// Cost: O(1).
fn block(
    interp: &mut Interpreter,
    _target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(interp.variable_block_handle()))
}

/// `Variable.var`: the variable's container (`Interpreter::variable_var`).
// Cost: see `Interpreter::variable_var`.
fn var(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    interp.variable_var(target).map(Ok)
}
