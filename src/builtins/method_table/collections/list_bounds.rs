//! `List.from` and `List.to` (ADR-11276 §9.111): a list of matches (`m:g//`,
//! `:ov`, a quantified capture) answers the span it covers.
//!
//! Rakudo declares both on `List` as `self.elems ?? self[0].from !! Nil` and
//! `self.elems ?? self[self.end].to !! Nil`, so an element is asked through
//! ordinary dispatch: a `Match` answers, a value with no `from`/`to` throws
//! `X::Method::NotFound` of its own type.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

type Answer = Option<Result<Value, RuntimeError>>;

macro_rules! row {
    ($name:literal, $handler:ident) => {
        MethodRow {
            owner: "List",
            name: $name,
            arity: 0,
            handler: Handler::Interp($handler),
            flags: RowFlags::NONE,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[row!("from", from), row!("to", to)];

/// `List.from`: the first element's `from`, or `Nil` for an empty list.
// Cost: O(1) plus the element's `from`.
fn from(interp: &mut Interpreter, target: &Value, _args: &[Value], _named: Named<'_>) -> Answer {
    let first = target.as_list_items()?.first().cloned();
    bound(interp, first, "from")
}

/// `List.to`: the last element's `to`, or `Nil` for an empty list.
// Cost: O(1) plus the element's `to`.
fn to(interp: &mut Interpreter, target: &Value, _args: &[Value], _named: Named<'_>) -> Answer {
    let last = target.as_list_items()?.last().cloned();
    bound(interp, last, "to")
}

fn bound(interp: &mut Interpreter, element: Option<Value>, method: &str) -> Answer {
    Some(match element {
        Some(element) => {
            interp.call_method_with_values(element.deref_container(), method, Vec::new())
        }
        None => Ok(Value::NIL),
    })
}
