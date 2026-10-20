//! `Label`'s rows: `name`, `file`, `line`, `Str`, `gist`, `raku`, `next`, `last`
//! and `redo`.
//!
//! A `Label` is the first-class value a loop label term evaluates to
//! (`builtins/label.rs`); it keeps its `name`, `file` and `line` as attributes,
//! so every handler is a pure read of them. `next`, `last` and `redo` answer the
//! labelled loop-control signal as an error, the way the static `next FOO`
//! does. The shape is closed: a user subclass has another class name and no
//! shape. Rakudo declares no `raku` on `Label`; the rendering is mutsu's own
//! (`Label.new(name => ..., file => ..., line => ...)`).

use super::{Handler, MethodRow, RowFlags};
use crate::value::label::{LABEL_CLASS, attr_str, label_gist, label_str};
use crate::value::{Control, RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

macro_rules! row {
    ($name:literal, $handler:expr) => {
        MethodRow {
            owner: "Label",
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: RowFlags::NONE,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("name", name),
    row!("file", file),
    row!("line", line),
    row!("Str", str_),
    row!("gist", gist),
    row!("raku", raku),
    row!("next", next),
    row!("last", last),
    row!("redo", redo),
];

/// The attribute map of a `Label` instance; `None` for any other receiver.
// Cost: O(1).
fn attrs(target: &Value) -> Option<crate::value::AttrMap> {
    match target.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == LABEL_CLASS => Some(attributes.as_map().clone()),
        _ => None,
    }
}

// Cost: O(1).
fn name(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(Value::str(attr_str(&attrs(target)?, "name"))))
}

// Cost: O(1).
fn file(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(Value::str(attr_str(&attrs(target)?, "file"))))
}

// Cost: O(1).
fn line(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(attrs(target)?
        .get("line")
        .cloned()
        .unwrap_or(Value::int(0))))
}

// Cost: O(n), n = length of the rendering.
fn str_(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(Value::str(label_str(&attrs(target)?))))
}

// Cost: O(n), n = length of the rendering.
fn gist(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(Value::str(label_gist(&attrs(target)?))))
}

// Cost: O(n), n = length of the rendering.
fn raku(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(Value::str(crate::builtins::label::label_raku(&attrs(
        target,
    )?))))
}

/// The labelled loop-control signal for `control`.
// Cost: O(1).
fn signal(target: &Value, control: Control) -> Answer {
    let attributes = attrs(target)?;
    Some(Err(
        crate::runtime::loop_handler_depth::loop_control_signal(
            control,
            Some(attr_str(&attributes, "name")),
        ),
    ))
}

// Cost: O(1).
fn next(target: &Value, _args: &[Value]) -> Answer {
    signal(target, Control::Next)
}

// Cost: O(1).
fn last(target: &Value, _args: &[Value]) -> Answer {
    signal(target, Control::Last)
}

// Cost: O(1).
fn redo(target: &Value, _args: &[Value]) -> Answer {
    signal(target, Control::Redo)
}
