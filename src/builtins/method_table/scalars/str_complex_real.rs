//! The real conversions of a string that spells a `Complex` (ADR-11276
//! §9.190): `"1+2i".Int`, `"3.7+0i".Num`, `.Rat`, `.FatRat`, `.Real`.
//!
//! Rakudo's `Str.Int`/`Str.Num` and `Cool.Rat`/`FatRat`/`Real` are
//! `self.Numeric.METHOD`, so such a string is coerced as the `Complex` it
//! spells, which is a `Real` only within `$*TOLERANCE`: the interpreter's
//! (`Interpreter::dispatch_complex_to_real`). The pure rows (`str_numeric`,
//! `cool_coerce`) decline the string, and these late interpreter twins of
//! theirs, reached through the late MRO walk of a `Str` receiver, answer it.
//! `Cool.UInt` is an interpreter row already and answers it itself.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::str_numeric::str_numifies_to_complex;
use crate::value::{RuntimeError, Value, ValueView};

/// The names the twins answer, for the late walk's receiver test.
pub(crate) const NAMES: &[&str] = &["Int", "Num", "Rat", "FatRat", "Real"];

macro_rules! twin {
    ($owner:literal, $name:literal) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Interp({
                fn handler(
                    interp: &mut Interpreter,
                    target: &Value,
                    _args: &[Value],
                    _named: Named<'_>,
                ) -> Option<Result<Value, RuntimeError>> {
                    complex_string_as_real(interp, $name, target)
                }
                handler
            }),
            flags: RowFlags::OWNER_ONLY,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    twin!("Str", "Int"),
    twin!("Str", "Num"),
    twin!("Cool", "Rat"),
    twin!("Cool", "FatRat"),
    twin!("Cool", "Real"),
];

/// `METHOD` of a string that spells a `Complex`: that `Complex`'s `METHOD`.
/// `None` for any other receiver, which the pure rows answer.
// Cost: O(n) to parse a string of n chars, plus
// `Interpreter::dispatch_complex_to_real`'s.
pub(crate) fn complex_string_as_real(
    interp: &mut Interpreter,
    method: &str,
    target: &Value,
) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Str(s) = target.view() else {
        return None;
    };
    let (re, im) = str_numifies_to_complex(&s)?;
    // An error names the number, not the string it was spelled in.
    Some(interp.dispatch_complex_to_real(method, re, im, &Value::complex(re, im), &[]))
}
