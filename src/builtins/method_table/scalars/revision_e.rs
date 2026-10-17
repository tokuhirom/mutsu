//! The methods language revision 6.e adds to a type that has a dispatch shape,
//! over a method 6.d answers from an ancestor (ADR-11276 §9.149, #13738):
//! `Complex.sign` (the unit complex number, where 6.d's `Cool.sign` refuses a
//! non-real `Complex`), `Int.pick` and `Int.roll` (a value of `^self`, where
//! 6.d's `Any.pick` picks from the one-item list `(self,)`).
//!
//! Each is [`RowFlags::REVISION_E`]. The shape tables are static, so the rows
//! are `OWNER_ONLY` and [`super::super::resolve`] looks them up by the shape's
//! own type when the program's revision is 6.e, ahead of the shape's entry.

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

const E: RowFlags = RowFlags::OWNER_ONLY.or(RowFlags::REVISION_E);
const E_RANDOM: RowFlags = E.or(RowFlags::RANDOM);

macro_rules! row {
    ($owner:literal, $name:literal, $arity:literal, $flags:expr, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: Handler::Narrow($handler),
            flags: $flags,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("Complex", "sign", 0, E, complex_sign),
    row!("Int", "pick", 0, E_RANDOM, int_pick),
    row!("Int", "pick", 1, E_RANDOM.or(RowFlags::ANY_ARGS), int_pick),
    row!("Int", "roll", 0, E_RANDOM, int_roll),
    row!("Int", "roll", 1, E_RANDOM.or(RowFlags::ANY_ARGS), int_roll),
];

/// `Complex.sign`: `self / self.abs`, `0+0i` for zero.
// Cost: O(1).
fn complex_sign(target: &Value, _: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Complex(re, im) = target.view() else {
        return None;
    };
    let abs = re.hypot(im);
    Some(Ok(if abs == 0.0 {
        Value::complex(0.0, 0.0)
    } else {
        Value::complex(re / abs, im / abs)
    }))
}

/// The range `^self` of an `Int` receiver, which `pick` and `roll` sample.
// Cost: O(1).
fn upto(target: &Value) -> Option<Value> {
    match target.view() {
        ValueView::Int(n) => Some(Value::range_excl(0, n.max(0))),
        _ => None,
    }
}

/// `Int.pick($count?)`: `(^self).pick($count)`.
// Cost: as `Range.pick`.
fn int_pick(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    crate::builtins::sampling::pick(&upto(target)?, args)
}

/// `Int.roll($count?)`: `(^self).roll($count)`.
// Cost: as `Range.roll`.
fn int_roll(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    crate::builtins::sampling::roll(&upto(target)?, args)
}
