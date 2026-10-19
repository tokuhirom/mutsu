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
use crate::value::{RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Variable",
        name: "block",
        arity: 0,
        handler: Handler::Interp(block),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "Variable",
        name: "var",
        arity: 0,
        handler: Handler::Interp(var),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
];

/// The owners the rows reachable from `target` are found under: a `Variable`.
// Cost: O(1).
pub(crate) fn owners_of(target: &Value) -> Option<&'static [&'static str]> {
    match target.view() {
        ValueView::Instance { class_name, .. } if class_name == "Variable" => Some(&["Variable"]),
        _ => None,
    }
}

/// `Variable.block`: the block that owns the declaration
/// (`runtime::attach_target`), to which the handler may add phasers.
// Cost: O(1).
fn block(
    _interp: &mut Interpreter,
    _target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(Interpreter::variable_block_handle()))
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
