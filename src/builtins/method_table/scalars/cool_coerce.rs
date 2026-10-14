//! `Cool.Real` (ADR-11276 slice 5, §9.92): the coercion every `Cool` value
//! reaches when its own type declares none.
//!
//! The numeric types answer their own `Real` (`numeric_coerce`); this row
//! answers a `Str` (parsed as `.Numeric` parses it), a `List`, `Array`, `Hash`,
//! `Map`, `Seq` or `Range` (its element count), a `FatRat` and a `Bool`, a
//! lazy list (`X::Cannot::Lazy`), and a type object (a warning, then zero of
//! the type). A `Complex` is `Real` only within `$*TOLERANCE`, which the
//! interpreter reads (`Interpreter::dispatch_complex_to_real`): the row
//! declines it.

use super::{Handler, MethodRow, RowFlags};
use crate::value::str_numeric::str_numifies_to_complex;
use crate::value::{RuntimeError, Value, ValueView};

pub(super) static COOL_ROWS: &[MethodRow] = &[MethodRow {
    owner: "Cool",
    name: "Real",
    arity: 0,
    handler: Handler::Narrow(real),
    flags: RowFlags::TYPE_OBJECT_OK,
    named: &[],
}];

/// The zero a type object's `.Real` warns and answers with, for the types
/// whose zero is not an `Int`; `None` for a type with no numeric zero.
// Cost: O(1).
fn type_object_zero(name: &str) -> Option<Value> {
    Some(match name {
        "Int" | "IntStr" => Value::int(0),
        "Rat" | "FatRat" | "RatStr" => Value::rat_raw(0, 1),
        "Num" | "NumStr" | "Complex" | "ComplexStr" => Value::num(0.0),
        _ => return None,
    })
}

/// `Cool.Real`.
// Cost: O(1) for a number, a type object, an aggregate or an Int range;
// O(d^2) for a d-digit integer string, O(n) for any other string.
fn real(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if let ValueView::Package(name) = target.view() {
        let name = name.resolve();
        let zero = type_object_zero(&name)?;
        let msg = format!("Use of uninitialized value of type {name} in numeric context");
        return Some(Err(RuntimeError::warn_signal_with_resume(msg, zero)));
    }
    if target.is_range() {
        return Some(crate::builtins::method_table::range::numeric_coercion(
            target, "Real",
        ));
    }
    if crate::builtins::methods_0arg::is_lazy_count_source(target) {
        return Some(Ok(crate::runtime::utils::cannot_lazy_failure("elems")));
    }
    // A `Match` coerces its matched text (`Match.Numeric` is `self.Str.Numeric`).
    if target.is_match_instance() {
        return real(&Value::str(target.to_string_value()), &[]);
    }
    if let Some(real) = crate::builtins::method_table::numeric_coerce::real_of(target) {
        return Some(Ok(real));
    }
    Some(Ok(match target.view() {
        ValueView::Str(s) => {
            // As `.Int`: a string that numifies to a `Complex` is coerced as it.
            if str_numifies_to_complex(&s).is_some() {
                return None;
            }
            match crate::runtime::str_numeric::parse_raku_str_to_numeric(s.trim()) {
                Some(v) => v,
                // The typed `X::Str::Numeric` Failure, with the `⏏` marker.
                None => crate::builtins::methods_0arg::str_numeric_failure(&s),
            }
        }
        ValueView::Array(items, ..) => Value::int(items.len() as i64),
        ValueView::Hash(h) => Value::int(h.len() as i64),
        ValueView::Seq(items) => Value::int(items.len() as i64),
        _ => return None,
    }))
}
