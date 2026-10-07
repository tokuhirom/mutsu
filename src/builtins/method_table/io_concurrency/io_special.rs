//! `IO::Special`'s rows (ADR-11276 §9.22): the object `$*OUT.path` answers, a
//! stand-in for an already-open standard stream. It has no shape, so every row
//! is reached through its owner (`RowFlags::OWNER_ONLY`, `invoke_owner`); the
//! receiver's one attribute, `what` (`<STDIN>`, `<STDOUT>`, `<STDERR>`), says
//! which stream it stands for.

use crate::builtins::method_table::{Handler, MethodRow, RowFlags};
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($owner:literal, $name:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: RowFlags::OWNER_ONLY,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("IO::Special", "IO", io_row),
    row!("IO::Special", "Str", what_row),
    row!("IO::Special", "what", what_row),
    row!("IO::Special", "raku", raku_row),
    row!("IO::Special", "WHICH", which_row),
    row!("IO::Special", "e", true_row),
    row!("IO::Special", "d", false_row),
    row!("IO::Special", "f", false_row),
    row!("IO::Special", "l", false_row),
    row!("IO::Special", "x", false_row),
    row!("IO::Special", "s", size_row),
    row!("IO::Special", "r", readable_row),
    row!("IO::Special", "w", writable_row),
    row!("IO::Special", "modified", instant_row),
    row!("IO::Special", "accessed", instant_row),
    row!("IO::Special", "changed", instant_row),
    row!("IO::Special", "mode", nil_row),
    // `Mu.gist` of an `IO::Special` is its `raku`; only an instance of the class
    // reaches it (`invoke_owner` lists `Mu` after `IO::Special`).
    row!("Mu", "gist", raku_row),
];

/// The stream a receiver stands for: its `what` attribute, empty when it has none.
// Cost: O(1) to find the attribute, O(w) to copy it, w = chars of the name.
fn what_of(target: &Value) -> Option<String> {
    match target.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "IO::Special" => Some(
            attributes
                .as_map()
                .get("what")
                .map(Value::to_string_value)
                .unwrap_or_default(),
        ),
        _ => None,
    }
}

/// `IO::Special.IO`: the object itself.
// Cost: O(1).
fn io_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    what_of(target)?;
    Some(Ok(target.clone()))
}

/// `IO::Special.what` and `.Str`: `<STDOUT>`.
// Cost: O(w), w = chars of the name.
fn what_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(Value::str(what_of(target)?)))
}

/// `IO::Special.raku`: `IO::Special.new("<STDOUT>")`.
// Cost: O(w), w = chars of the name.
fn raku_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let what = what_of(target)?;
    Some(Ok(Value::str(format!("IO::Special.new(\"{what}\")"))))
}

/// `IO::Special.WHICH`: `IO::Special|<STDOUT>`.
// Cost: O(w), w = chars of the name.
fn which_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let what = what_of(target)?;
    Some(Ok(Value::str(format!("IO::Special|{what}"))))
}

/// `e`: a standard stream exists.
// Cost: O(1).
fn true_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    what_of(target)?;
    Some(Ok(Value::TRUE))
}

/// `d`, `f`, `l`, `x`: a standard stream is none of a directory, a file, a
/// link or executable.
// Cost: O(1).
fn false_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    what_of(target)?;
    Some(Ok(Value::FALSE))
}

/// `s`: no size.
// Cost: O(1).
fn size_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    what_of(target)?;
    Some(Ok(Value::int(0)))
}

/// `r`: only standard input is readable.
// Cost: O(w), w = chars of the name.
fn readable_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(Value::truth(what_of(target)?.contains("STDIN"))))
}

/// `w`: standard output and error are writable.
// Cost: O(w), w = chars of the name.
fn writable_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let what = what_of(target)?;
    Some(Ok(Value::truth(
        what.contains("STDOUT") || what.contains("STDERR"),
    )))
}

/// `modified`, `accessed`, `changed`: the `Instant` type object (no time).
// Cost: O(1).
fn instant_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    what_of(target)?;
    Some(Ok(Value::package(Symbol::intern("Instant"))))
}

/// `mode`: no mode.
// Cost: O(1).
fn nil_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    what_of(target)?;
    Some(Ok(Value::NIL))
}
