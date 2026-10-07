//! `gist`, `raku` and `WHICH` (and `Bool.Str`) on the scalar value
//! types (ADR-11276 remainder: the rendering and identity names).
//!
//! Rakudo declares each of these on `Int`, `Num`, `Rat`, `Complex`, `Bool` and
//! `Str` separately. A number's `gist` is its string value, its `raku` is the
//! form that evaluates back to it (`Value::raku`, also behind a collection's
//! `.raku`), a `Str` quotes and escapes itself, and a `Bool` spells its enum
//! constant. `WHICH` is [`which_of`](crate::builtins::methods_0arg::which::which_of), the one
//! identity routine every layer shares. A rational with a zero denominator
//! cannot be rendered by a pure handler (its `gist` throws with the
//! interpreter's context), so those rows decline it.

use super::{Handler, MethodRow, RowFlags};
use crate::value::raku_repr::{escape_raku_str, raku_value};
use crate::builtins::methods_0arg::which::which_of;
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
    "Complex" => ["gist" => gist, "raku" => raku, "WHICH" => which],
    "Bool" => ["gist" => gist, "raku" => raku, "Str" => gist],
    "Str" => ["gist" => gist, "raku" => raku, "WHICH" => which],
};

/// Whether the receiver is a rational whose denominator is zero.
// Cost: O(1).
fn is_zero_denominator(target: &Value) -> bool {
    match target.view() {
        ValueView::Rat(_, 0) | ValueView::FatRat(_, 0) => true,
        ValueView::BigRat(_, d) => num_traits::Zero::is_zero(d),
        _ => false,
    }
}

/// `.gist` (and `Bool.Str`): the receiver's string value; a `Str` is itself.
// Cost: O(1) for a `Str` (shared, not copied) and a `Bool`; O(n) for a
// number, n = chars of the rendering.
pub(crate) fn gist(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Str(_) => Some(Ok(target.clone())),
        ValueView::Bool(b) => Some(Ok(Value::str_from(if b { "True" } else { "False" }))),
        _ if is_zero_denominator(target) => None,
        ValueView::Int(_)
        | ValueView::BigInt(_)
        | ValueView::Num(_)
        | ValueView::Rat(..)
        | ValueView::BigRat(..)
        | ValueView::Complex(..) => Some(Ok(Value::str(target.to_string_value()))),
        _ => None,
    }
}

/// `.raku` (the cascade also answers `.perl` with it): the receiver as source text.
// Cost: O(n), n = chars of the rendering.
pub(crate) fn raku(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Str(s) => Some(Ok(Value::str(escape_raku_str(&s)))),
        ValueView::Bool(b) => Some(Ok(Value::str_from(if b {
            "Bool::True"
        } else {
            "Bool::False"
        }))),
        _ if is_zero_denominator(target) => None,
        ValueView::Int(_)
        | ValueView::BigInt(_)
        | ValueView::Num(_)
        | ValueView::Rat(..)
        | ValueView::BigRat(..)
        | ValueView::Complex(..) => Some(Ok(Value::str(raku_value(target)))),
        _ => None,
    }
}

/// `.WHICH`: the object identity.
// Cost: O(1) for a scalar except a `Str` (O(1), the invocant is shared and
// rendered only when read) and a big number (O(n), n = digits).
pub(crate) fn which(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Str(_)
        | ValueView::Bool(_)
        | ValueView::Int(_)
        | ValueView::BigInt(_)
        | ValueView::Num(_)
        | ValueView::Rat(..)
        | ValueView::BigRat(..)
        | ValueView::Complex(..) => Some(Ok(which_of(target))),
        _ => None,
    }
}
