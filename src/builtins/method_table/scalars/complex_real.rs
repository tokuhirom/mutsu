//! `Complex`'s conversions to a `Real` type: `Int`, `Num`, `Rat`, `FatRat` and
//! `Real` (ADR-11276 §9.70).
//!
//! Rakudo's `Complex.Real` answers the real part when the imaginary part is
//! `≅ 0` under `$*TOLERANCE`, and every other conversion goes through it.
//! Reading the dynamic variable needs the interpreter, so these are interpreter
//! rows; their body is [`Interpreter::dispatch_complex_to_real`], which the
//! cascade's other entry (a `Str` that numifies to a `Complex`, `"1+2i".Int`)
//! calls too. `UInt` is `Cool`'s: its `Int` plus a range check.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

/// A `Complex` row whose method name is `$name`.
macro_rules! complex_row {
    ($name:literal, $arity:literal, $handler:ident) => {
        MethodRow {
            owner: "Complex",
            name: $name,
            arity: $arity,
            handler: Handler::Interp($handler),
            // The epsilon of `.Rat($epsilon)` is checked by the real part's own method.
            flags: if $arity == 0 {
                RowFlags::NONE
            } else {
                RowFlags::ANY_ARGS
            },
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    complex_row!("Int", 0, int),
    complex_row!("Num", 0, num),
    complex_row!("Real", 0, real),
    complex_row!("Rat", 0, rat),
    complex_row!("FatRat", 0, fat_rat),
    complex_row!("Rat", 1, rat),
    complex_row!("FatRat", 1, fat_rat),
];

/// The coercion `$method` of a `Complex` receiver. `None` when the receiver is
/// not a `Complex` (the guard never lets that through).
// Cost: see `Interpreter::dispatch_complex_to_real`.
fn coerce(
    interp: &mut Interpreter,
    method: &str,
    target: &Value,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Complex(re, im) = target.view() else {
        return None;
    };
    Some(interp.dispatch_complex_to_real(method, re, im, target, args))
}

fn int(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    coerce(interp, "Int", target, args)
}

fn num(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    coerce(interp, "Num", target, args)
}

fn real(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    coerce(interp, "Real", target, args)
}

fn rat(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    coerce(interp, "Rat", target, args)
}

fn fat_rat(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    coerce(interp, "FatRat", target, args)
}
