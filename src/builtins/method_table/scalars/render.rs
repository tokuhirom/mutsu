//! `gist`, `raku` and `WHICH` (and `Bool.Str`) on the scalar value
//! types (ADR-11276 remainder: the rendering and identity names).
//!
//! Rakudo declares each of these on `Int`, `Num`, `Rat`, `Complex`, `Bool` and
//! `Str` separately. A number's `gist` is its string value, its `raku` is the
//! form that evaluates back to it (`Value::raku`, also behind a collection's
//! `.raku`), a `Str` quotes and escapes itself, and a `Bool` spells its enum
//! constant. `WHICH` is [`which_of`], the one
//! identity routine every layer shares. A rational with a zero denominator
//! has no decimal form: its `gist` throws `X::Numeric::DivideByZero` and its
//! `raku` is the literal (`<1/0>`, `FatRat.new(1, 0)`). `FatRat` shares the
//! rows of `Rat`, which Rakudo composes from the same `Rational` role.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::methods_0arg::which::which_of;
use crate::value::raku_repr::{escape_raku_str, raku_value};
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! rows {
    ($($owner:literal => [$($name:literal => $handler:ident),*]),* $(,)?) => {
        &[$($(MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: RowFlags::NONE,
            named: &[],
        }),*),*]
    };
}

pub(super) static ROWS: &[MethodRow] = rows! {
    "Int" => ["gist" => gist, "raku" => raku, "WHICH" => which],
    "Num" => ["gist" => gist, "raku" => raku, "WHICH" => which],
    "Rat" => ["gist" => gist, "raku" => raku, "WHICH" => which],
    "FatRat" => ["gist" => gist, "raku" => raku],
    "Complex" => ["gist" => gist, "raku" => raku, "WHICH" => which],
    "Bool" => ["gist" => gist, "raku" => raku, "Str" => gist],
    "Str" => ["gist" => gist, "raku" => raku, "WHICH" => which],
};

/// `.gist` (and `Bool.Str`): the receiver's string value; a `Str` is itself.
// Cost: O(1) for a `Str` (shared, not copied) and a `Bool`; O(n) for a
// number, n = chars of the rendering.
pub(crate) fn gist(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Str(_) => Some(Ok(target.clone())),
        ValueView::Bool(b) => Some(Ok(Value::str_from(if b { "True" } else { "False" }))),
        ValueView::Rat(n, 0) | ValueView::FatRat(n, 0) => Some(Err(
            RuntimeError::rational_to_str_divide_by_zero(Value::int(n)),
        )),
        ValueView::BigRat(n, d) if num_traits::Zero::is_zero(d) => Some(Err(
            RuntimeError::rational_to_str_divide_by_zero(Value::from_bigint(n.clone())),
        )),
        ValueView::Int(_)
        | ValueView::BigInt(_)
        | ValueView::Num(_)
        | ValueView::Rat(..)
        | ValueView::FatRat(..)
        | ValueView::BigRat(..)
        | ValueView::Complex(..) => Some(Ok(Value::str(target.to_string_value()))),
        _ => None,
    }
}

/// `.raku`: the receiver as source text.
// Cost: O(n), n = chars of the rendering.
pub(crate) fn raku(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Str(s) => Some(Ok(Value::str(escape_raku_str(&s)))),
        ValueView::Bool(b) => Some(Ok(Value::str_from(if b {
            "Bool::True"
        } else {
            "Bool::False"
        }))),
        ValueView::Rat(n, 0) => Some(Ok(Value::str(format!("<{n}/0>")))),
        ValueView::FatRat(n, d) => Some(Ok(Value::str(format!("FatRat.new({n}, {d})")))),
        ValueView::BigRat(n, d) if target.is_bigfatrat() => {
            Some(Ok(Value::str(format!("FatRat.new({n}, {d})"))))
        }
        ValueView::Int(_)
        | ValueView::BigInt(_)
        | ValueView::Num(_)
        | ValueView::Rat(..)
        | ValueView::BigRat(..)
        | ValueView::Complex(..) => Some(Ok(Value::str(raku_value(target)))),
        _ => None,
    }
}

/// `.WHICH`: the object identity, `which_of`, the one identity routine; an
/// instance of a user subclass of the type reaches this row through
/// `answer_shapeless`'s MRO walk.
// Cost: O(1) for a scalar except a `Str` (O(1), the invocant is shared and
// rendered only when read) and a big number (O(n), n = digits).
pub(crate) fn which(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(which_of(target)))
}
