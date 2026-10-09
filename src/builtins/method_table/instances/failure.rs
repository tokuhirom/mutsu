//! `Failure`'s rows (ADR-11276 §9.52).
//!
//! A `Failure` is an instance of the built-in class `Failure` whose state is the
//! `exception` attribute and a *handled* flag shared by every clone of the value
//! (`Value::is_failure_handled`). Rakudo declares `Bool`, `exception`, `gist`,
//! `handled`, `raku` and `Str` on it. They are reached through their owner:
//! [`answer`] is the entry for the cascade, which must see a `Failure` before any
//! shape-based probe, since every method outside this list explodes it.

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($name:literal, $handler:ident) => {
        MethodRow {
            owner: "Failure",
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: RowFlags::OWNER_ONLY,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("exception", exception),
    row!("handled", handled),
    row!("gist", gist),
    row!("raku", raku),
    row!("Str", str_row),
    row!("Bool", bool_row),
];

/// The answer of the row for `method` on a `Failure`, or `None` when `target` is
/// not one or `Failure` declares no such method (`perl` is `raku`'s old name).
// Cost: O(r), r = rows of this group (a scan by name).
pub(crate) fn answer(target: &Value, method: &str) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance { class_name, .. } = target.view() else {
        return None;
    };
    if class_name != "Failure" {
        return None;
    }
    let method = if method == "perl" { "raku" } else { method };
    ROWS.iter()
        .find(|row| row.name == method)
        .and_then(|row| match row.handler {
            Handler::Narrow(f) => f(target, &[]),
            _ => None,
        })
}

/// The wrapped exception's text, `Failed` without one.
fn message(target: &Value) -> String {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return String::new();
    };
    attributes
        .as_map()
        .get("exception")
        .map(|v| v.to_string_value())
        .unwrap_or_else(|| "Failed".to_string())
}

/// `Failure.exception`: the wrapped exception object, `Nil` without one.
// Cost: O(1).
fn exception(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    Some(Ok(attributes
        .as_map()
        .get("exception")
        .cloned()
        .unwrap_or(Value::NIL)))
}

/// `Failure.handled`: whether it was defused (`.Bool`, `.defined`, `.handled = ...`).
// Cost: O(1), one registry lookup.
fn handled(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(Value::truth(target.is_failure_handled())))
}

/// `Failure.gist`: the wrapped exception's text, `(HANDLED) ` first once defused.
// Cost: O(m), m = chars of the message.
fn gist(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let msg = message(target);
    Some(Ok(Value::str(if target.is_failure_handled() {
        format!("(HANDLED) {msg}")
    } else {
        msg
    })))
}

/// `Failure.raku`: an expression that rebuilds it; a handled one is defused again.
// Cost: O(m), m = chars of the message.
fn raku(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let msg = message(target);
    Some(Ok(Value::str(if target.is_failure_handled() {
        format!("do {{ my $f = Failure.new(\"{msg}\"); $f.Bool; $f }}")
    } else {
        format!("Failure.new(\"{msg}\")")
    })))
}

/// `Failure.Str`: using it as a string throws the wrapped exception.
// Cost: O(1).
fn str_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    Some(Err(match attributes.as_map().get("exception") {
        Some(ex) => RuntimeError::from_exception_value(ex.clone()),
        None => RuntimeError::new("Failed"),
    }))
}

/// `Failure.Bool`: always false, and it defuses the failure.
// Cost: O(1).
fn bool_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    target.mark_failure_handled();
    Some(Ok(Value::truth(target.truthy())))
}
