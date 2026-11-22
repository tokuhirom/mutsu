//! `Signature`'s rows: `gist` and `raku` (ADR-11276 §9.39), `arity`, `count`, `params` and
//! `returns` (§9.55), and `Capture`.
//!
//! A `Signature` instance carries its two renderings as the attributes `gist`
//! and `raku` (the compiler renders them once, from the parsed parameters), so
//! both rows read an attribute, as do `params` and `returns`; `arity` and `count`
//! read the structured parameter list the compiler registered. A `Signature` has an
//! opaque shape, so every entry reaches the rows through the table.

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($name:literal, $handler:ident) => {
        MethodRow {
            owner: "Signature",
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: RowFlags::NONE,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("gist", gist),
    row!("raku", raku),
    row!("arity", arity),
    row!("count", count),
    row!("params", params),
    row!("returns", returns),
    // `Signature.Capture` (both the type object and an instance).
    MethodRow {
        owner: "Signature",
        name: "Capture",
        arity: 0,
        handler: Handler::Narrow(capture),
        flags: RowFlags::TYPE_OBJECT_OK,
        named: &[],
    },
];

/// `Signature.Capture`: rakudo's `X::Cannot::Capture`, naming the signature
/// by its `.gist` (`($a)`, `(Signature)`).
// Cost: O(n), n = chars of the gist.
fn capture(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let gist = match rendering(target, "gist") {
        Some(Ok(g)) => g.to_string_value(),
        _ => crate::value::gist::gist_value(target),
    };
    Some(Err(
        crate::builtins::methods_0arg::coercion::cannot_capture_described(target, &gist),
    ))
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

/// `Signature.arity`: the number of required positional parameters.
// Cost: O(p), p = parameters of the signature.
fn arity(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(crate::runtime::Interpreter::signature_arity_or_count(
        target, "arity",
    )))
}

/// `Signature.count`: the number of positional parameters, `Inf` with a slurpy.
// Cost: O(p), p = parameters of the signature.
fn count(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(crate::runtime::Interpreter::signature_arity_or_count(
        target, "count",
    )))
}

/// `Signature.params`: the `Parameter` objects.
// Cost: O(1).
fn params(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    Some(Ok(attributes
        .as_map()
        .get("params")
        .cloned()
        .unwrap_or_else(|| Value::array(Vec::new()))))
}

/// `Signature.returns`: the declared return type, `Mu` without one.
// Cost: O(1).
pub(super) fn returns(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    Some(Ok(attributes
        .as_map()
        .get("returns")
        .cloned()
        .unwrap_or_else(|| {
            Value::package(crate::symbol::Symbol::intern("Mu"))
        })))
}
