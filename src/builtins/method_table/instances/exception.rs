//! `Exception`'s, `X::AdHoc`'s, `CX::Warn`'s and `X::TypeCheck::Assignment`'s
//! rows (ADR-11276 §9.39).
//!
//! An exception is an instance of a built-in class named `Exception`, `X::...` or
//! `CX::...` whose state is attributes (`message`, `payload`, `backtrace`, and
//! the typed ones `format_exception_message` renders a message from). Every
//! handler reads those attributes, so they are pure rows. The classes are many
//! and open-ended (every `X::Foo` is one), so none has a shape: the rows are
//! reached through their owner, and [`answer`] is that entry for the cascade,
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
    row!("Exception", "resume", resume),
    row!("Exception", "throw", throw),
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
fn formatted(target: &Value) -> Option<String> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    crate::value::exception_message::format_exception_message(
        &class_of(target),
        &attributes.as_map(),
    )
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
    Some(Ok(Value::str(formatted(target).unwrap_or_default())))
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

/// `Exception.Str`: the message.
// Cost: O(m), m = chars of the message.
fn str_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if class_of(target) == "CX::Warn" {
        return Some(Ok(
            attr(target, "message").unwrap_or(Value::str(String::new()))
        ));
    }
    let class = class_of(target);
    Some(Ok(Value::str(text(
        target,
        &format!("Something went wrong in ({class})"),
        "Unexplained error",
    ))))
}

/// `Exception.backtrace`: the backtrace attribute, `Nil` for an unthrown one.
// Cost: O(1).
fn backtrace(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(attr(target, "backtrace").unwrap_or(Value::NIL)))
}

/// `Exception.resume`: a warning's `CX::Warn` is resumable; the signal unwinds to
/// the handler that resumes it. Any other exception declines.
// Cost: O(1).
fn resume(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    (class_of(target) == "CX::Warn").then(|| Err(RuntimeError::resume_signal()))
}

/// `Exception.throw`: raise the exception itself. Only the core `Exception` and
/// `X::` classes answer; a `CX::` class declines, since the interpreter inspects
/// its composed roles (`X::Control`) to decide whether it is a control exception.
/// The text is the message a `.message` would give, never the type repr.
// Cost: O(m), m = chars of the message built.
fn throw(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let class = class_of(target);
    if class != "Exception" && !class.starts_with("X::") {
        return None;
    }
    let msg = message_attr(target)
        .map(|v| v.to_string_value())
        .filter(|s| !s.is_empty())
        .or_else(|| {
            (class == "X::AdHoc")
                .then(|| attr(target, "payload"))
                .flatten()
                .map(|v| v.to_string_value())
                .filter(|s| !s.is_empty())
        })
        .or_else(|| formatted(target))
        .unwrap_or_else(|| target.to_string_value());
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(target.clone()));
    Some(Err(err))
}
