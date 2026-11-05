//! `Exception`'s, `X::AdHoc`'s, `CX::Warn`'s and `X::TypeCheck::Assignment`'s
//! rows (ADR-11276 §9.39).
//!
//! An exception is an instance of a built-in class named `Exception`, `X::...` or
//! `CX::...` whose state is attributes (`message`, `payload`, `backtrace`, and
//! the typed ones `format_exception_message` renders a message from). Every
//! handler reads those attributes, so they are pure rows. The classes are many
//! and open-ended (every `X::Foo` is one), so none has a shape: the rows are
//! reached through their owner, and [`answer`] is that lookup for the interpreter's entry,
//! which walks the instance's owner chain (`X::AdHoc` and `CX::Warn` answer
//! some names themselves, every other class answers as `Exception`).

use super::{Handler, MethodRow, RowFlags};
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
    row!("Exception", "message", message),
    row!("Exception", "gist", gist),
    row!("Exception", "Str", str_row),
    row!("Exception", "backtrace", backtrace),
    row!("X::AdHoc", "message", message),
    row!("X::AdHoc", "payload", payload),
    row!("CX::Warn", "message", message),
    row!("X::TypeCheck::Assignment", "message", message),
];

/// The answer of the row for `method` on an exception instance, or `None` when
/// `target` is not one or no row of its owner chain declares the method.
// Cost: O(r), r = rows of this group (a scan by owner and name).
pub(crate) fn answer(target: &Value, method: &str) -> Option<Result<Value, RuntimeError>> {
    if !target.instance_is_exception_by_name() {
        return None;
    }
    let ValueView::Instance { class_name, .. } = target.view() else {
        return None;
    };
    let class = class_name.resolve();
    let own = match class.as_str() {
        "X::AdHoc" => "X::AdHoc",
        "CX::Warn" => "CX::Warn",
        "X::TypeCheck::Assignment" => "X::TypeCheck::Assignment",
        _ => "",
    };
    [own, "Exception"]
        .into_iter()
        .filter(|owner| !owner.is_empty())
        .find_map(|owner| {
            ROWS.iter()
                .find(|row| row.owner == owner && row.name == method)
        })
        .and_then(|row| match row.handler {
            Handler::Narrow(f) => f(target, &[]),
            _ => None,
        })
}

fn attr(target: &Value, name: &str) -> Option<Value> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    attributes.as_map().get(name).cloned()
}

fn class_of(target: &Value) -> String {
    match target.view() {
        ValueView::Instance { class_name, .. } => class_name.resolve(),
        _ => String::new(),
    }
}

/// The message attribute when it holds one: a declared-but-undefined
/// `has $.message` is not a message (it would print the literal `(Any)`).
fn message_attr(target: &Value) -> Option<Value> {
    attr(target, "message").filter(|v| !v.is_nil() && !matches!(v.view(), ValueView::Package(_)))
}

/// The message built from the typed attributes of a built-in `X::` class.
pub(crate) fn formatted(target: &Value) -> Option<String> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    let class = class_of(target);
    let attrs = attributes.as_map();
    // A hand-built `X::TypeCheck::Assignment.new(:symbol, :got, :expected)` (Red's
    // `Red::Model.new` rejects a mistyped column that way) has the text rakudo's
    // `message` builds; the interpreter-raised ones store theirs in `message`.
    // It lives here, not in the value-layer table, because naming the value
    // (`Int (42)`) needs the type-check renderers of the runtime.
    if class == "X::TypeCheck::Assignment"
        && let (Some(got), Some(expected)) = (attrs.get("got"), attrs.get("expected"))
    {
        // Rakudo's `message` names `$!symbol`; with none it dies (see
        // [`message`]), so there is no text to build.
        let symbol = attrs
            .get("symbol")
            .filter(|v| !v.is_nil() && !matches!(v.view(), ValueView::Package(_)))?;
        return Some(crate::runtime::utils::type_check_assignment_error(
            &symbol.to_string_value(),
            &crate::runtime::utils::got_type_name(expected),
            got,
        ));
    }
    crate::value::exception_message::format_exception_message(&class, &attrs)
}

/// `Exception.message` (and its `X::AdHoc`, `CX::Warn`, `X::TypeCheck::Assignment`
/// forms): the `message` attribute, an `X::AdHoc`'s `payload`, or the message
/// built from the typed attributes.
// Cost: O(m), m = chars of the message built.
fn message(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if let Some(msg) = attr(target, "message") {
        return Some(Ok(msg));
    }
    if class_of(target) == "X::AdHoc"
        && let Some(payload) = attr(target, "payload")
    {
        return Some(Ok(payload));
    }
    if class_of(target) == "CX::Warn" {
        return Some(Ok(Value::str(String::new())));
    }
    // A hand-built `X::TypeCheck::Assignment` without a `symbol` dies in
    // rakudo's `message` (it calls `.name` on the undefined symbol), #16047.
    if class_of(target) == "X::TypeCheck::Assignment" && formatted(target).is_none() {
        return Some(Err(crate::runtime::did_you_mean::method_not_found(
            "name", "Mu",
        )));
    }
    match formatted(target) {
        Some(text) => Some(Ok(Value::str(text))),
        // A class that is an `Exception` but never implemented `message` inherits
        // rakudo's stubbed `method message() { ... }`: it fails `X::StubCode`.
        // The built-in classes below keep answering the empty text.
        // TODO: rakudo's bare `Exception.new.message` is that Failure too.
        None if matches!(
            class_of(target).as_str(),
            "Exception" | "X::AdHoc" | "X::OS"
        ) =>
        {
            Some(Ok(Value::str(String::new())))
        }
        None => Some(Ok(RuntimeError::stub_code_failure())),
    }
}

/// `X::AdHoc.payload`: what `die` was given (the message when there is no payload).
// Cost: O(1).
fn payload(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(attr(target, "payload")
        .or_else(|| attr(target, "message"))
        .unwrap_or_else(|| Value::str(String::new()))))
}

/// The text `Str` and `gist` share: the message, an `X::AdHoc`'s payload, or the
/// typed message; `unthrown`/`unexplained` are what a bare `Exception` and an
/// `X::AdHoc` without a payload say, and `fallback` what any other class without
/// a message says.
fn text(target: &Value, unthrown: &str, unexplained: &str) -> String {
    let class = class_of(target);
    if let Some(msg) = message_attr(target) {
        let msg = msg.to_string_value();
        if !msg.is_empty() {
            return msg;
        }
    }
    if class == "CX::Warn" {
        return String::new();
    }
    if class == "Exception" {
        return unthrown.to_string();
    }
    if class == "X::AdHoc" {
        if let Some(payload) = attr(target, "payload") {
            let payload = payload.to_string_value();
            if !payload.is_empty() {
                return payload;
            }
        }
        return unexplained.to_string();
    }
    formatted(target).unwrap_or_else(|| format!("{class} with no message"))
}

/// `Exception.gist`: the message, then the backtrace when there is one.
// Cost: O(m + b), m = chars of the message, b = chars of the backtrace.
fn gist(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let msg = text(
        target,
        "Unthrown Exception with no message",
        "Unexplained error",
    );
    // A `CX::Warn` renders as its message alone.
    if class_of(target) == "CX::Warn" {
        return Some(Ok(
            attr(target, "message").unwrap_or(Value::str(String::new()))
        ));
    }
    let backtrace = attr(target, "backtrace")
        .map(|v| v.to_string_value())
        .unwrap_or_default();
    Some(Ok(Value::str(if backtrace.is_empty() {
        msg
    } else {
        format!("{msg}\n{backtrace}")
    })))
}

/// Rakudo's `Exception.Str` placeholder for an exception with no usable
/// message: the class's last name part in parentheses.
// Cost: O(|class|), one interned lookup.
pub(crate) fn no_message_text(class: &str) -> String {
    let short = crate::qualified::unqualified_part(crate::symbol::Symbol::intern(class));
    format!("Something went wrong in ({})", short.as_str())
}

/// `Exception.Str`: the message.
// Cost: O(m), m = chars of the message.
fn str_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if class_of(target) == "CX::Warn" {
        return Some(Ok(
            attr(target, "message").unwrap_or(Value::str(String::new()))
        ));
    }
    let class = class_of(target);
    // Rakudo's `Str` falls back to the class's last name part when `message`
    // has nothing (or dies): `Something went wrong in (Assignment)`.
    let fallback = no_message_text(&class);
    if !matches!(class.as_str(), "Exception" | "X::AdHoc" | "CX::Warn")
        && message_attr(target).is_none_or(|m| m.to_string_value().is_empty())
        && formatted(target).is_none()
    {
        return Some(Ok(Value::str(fallback)));
    }
    Some(Ok(Value::str(text(target, &fallback, "Unexplained error"))))
}

/// `Exception.backtrace`: the backtrace attribute, `Nil` for an unthrown one.
// Cost: O(1).
fn backtrace(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(attr(target, "backtrace").unwrap_or(Value::NIL)))
}
