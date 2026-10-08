//! `Signature.gist` and `Signature.raku` (ADR-11276 §9.39).
//!
//! A `Signature` instance carries its two renderings as the attributes `gist`
//! and `raku` (the compiler renders them once, from the parsed parameters), so
//! both rows read an attribute. `Signature` has no shape (its other methods,
//! `params`, `arity`, `count` and `returns`, need the interpreter), so the rows
//! are reached through their owner: [`answer`] is the entry for the cascade.

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($name:literal, $handler:ident) => {
        MethodRow {
            owner: "Signature",
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: RowFlags::OWNER_ONLY,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[row!("gist", gist), row!("raku", raku)];

/// The answer of the row for `method` on a `Signature` instance, or `None`.
// Cost: O(1).
pub(crate) fn answer(target: &Value, method: &str) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Instance { class_name, .. } if class_name == "Signature" => {}
        _ => return None,
    }
    match method {
        "gist" => gist(target, &[]),
        "raku" | "perl" => raku(target, &[]),
        _ => None,
    }
}

fn rendering(target: &Value, key: &str) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
    else {
        return None;
    };
    Some(Ok(attributes
        .as_map()
        .get(key)
        .cloned()
        .unwrap_or_else(|| Value::str(format!("{class_name}()")))))
}

/// `Signature.gist`: `(Int $x, Str :$y)`.
// Cost: O(1).
fn gist(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    rendering(target, "gist")
}

/// `Signature.raku`: the form that evaluates back to the signature.
// Cost: O(1).
fn raku(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    rendering(target, "raku")
}
