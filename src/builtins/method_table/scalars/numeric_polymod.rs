//! `polymod` of the `Real` types: `Int`, `Num`, `Rat`, `Instant` and `Duration`
//! (ADR-11276 §9.70).
//!
//! An interpreter row: a lazy divisor list (`xx *`, a `gather`) is pulled
//! element by element. The body is [`Interpreter::polymod_answer`], which the
//! `polymod` interceptor ends in too for a receiver with no shape (a mixin, a
//! subclass), so there is one decomposition. `Complex`, `Str` and the rest do not
//! declare `polymod`: they have no row and reach `X::Method::NotFound`.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

const fn row(owner: &'static str) -> MethodRow {
    MethodRow {
        owner,
        name: "polymod",
        arity: 0,
        handler: Handler::Interp(polymod),
        // `*@mods`: any number of divisors, each a number or a list of them.
        flags: RowFlags::SLURPY.or(RowFlags::ANY_ARGS),
        named: &[],
    }
}

pub(super) static ROWS: &[MethodRow] = &[
    row("Int"),
    row("Num"),
    row("Rat"),
    row("Instant"),
    row("Duration"),
];

/// `.polymod(*@divisors)`.
// Cost: O(k * log(n)) for k divisors and a number of n bits; a lazy divisor
// list is pulled for at most one element per bit of the number.
fn polymod(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    Some(interp.polymod_answer(target, args))
}
