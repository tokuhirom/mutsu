//! `Failure`'s rows (ADR-11276 §9.52), as Rakudo's `src/core.c/Failure.rakumod`
//! declares them.
//!
//! A `Failure` is an instance of the built-in class `Failure` whose state is the
//! `exception` attribute, the backtrace of the `fail` site and a *handled* flag
//! shared by every clone of the value (`Value::is_failure_handled`). Using it
//! where a value is needed throws its exception through `!throw`, which marks it
//! handled first; asking its truth (`Bool`, `defined`) handles it without a
//! throw. Once handled, the coercions answer a defused value (`Int` the type
//! object, `Num`/`Numeric` `NaN`, `Str`/`gist` its [`mess`], a QuantHash of it).
//!
//! The rows are reached through their owner: the method-call entries ask
//! `Interpreter::failure_own_method`, which runs a row when `Failure` declares
//! the method and otherwise throws an unhandled `Failure` (Rakudo's `FALLBACK`),
//! and [`answer`] is the entry for the value renderers, which must see a
//! `Failure` before any shape-based probe.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

/// A `Failure:D` method with no positional argument.
const OWN: RowFlags = RowFlags::OWNER_ONLY;

/// A `Failure:D: |` (or `*@`) method: any argument list.
const ANY: RowFlags = RowFlags::OWNER_ONLY
    .or(RowFlags::SLURPY)
    .or(RowFlags::ANY_ARGS)
    .or(RowFlags::OBJECT_ARGS);

macro_rules! row {
    ($name:literal, $handler:ident) => {
        row!($name, $handler, OWN)
    };
    ($name:literal, $handler:ident, $flags:expr) => {
        MethodRow {
            owner: "Failure",
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: $flags,
            named: &[],
        }
    };
}

/// `method Set(Failure:D:) { $!handled ?? Set.new(self) !! self!throw }` and kin.
macro_rules! quanthash {
    ($name:literal) => {
        MethodRow {
            owner: "Failure",
            name: $name,
            arity: 0,
            handler: Handler::Interp(|interp, target, _, _| quanthash(interp, target, $name)),
            flags: OWN,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("exception", exception),
    row!("handled", handled),
    row!("backtrace", backtrace),
    row!("mess", mess),
    row!("gist", gist),
    row!("Str", str_row),
    MethodRow {
        owner: "Failure",
        name: "raku",
        arity: 0,
        handler: Handler::InterpPureFirst(raku_pure, raku),
        flags: OWN,
        named: &[],
    },
    row!("Bool", bool_row),
    row!("Int", int_row),
    row!("Num", nan_row),
    row!("Numeric", nan_row),
    row!("Failure", itself),
    row!("self", throw_unless_handled),
    row!("sink", sink),
    row!("throw", throw),
    row!("AT-POS", itself, ANY),
    row!("AT-KEY", itself, ANY),
    row!("CALL-ME", throw, ANY),
    row!("STORE", throw, ANY),
    quanthash!("Set"),
    quanthash!("SetHash"),
    quanthash!("Bag"),
    quanthash!("BagHash"),
    quanthash!("Mix"),
    quanthash!("MixHash"),
];

/// Whether `target` is a `Failure` instance.
// Cost: O(1).
pub(crate) fn is_failure(target: &Value) -> bool {
    // Tag-probed: `view()` of a lazy Match would materialize it (#12917).
    !target.is_lazy_match_value()
        && matches!(target.view(), ValueView::Instance { class_name, .. } if class_name == "Failure")
}

/// The answer of the pure row for `method` on a `Failure`, or `None` when
/// `target` is not one or `Failure` declares no pure row of that name (`perl`
/// is `raku`'s old name). The value renderers' entry: they hold no
/// interpreter.
// Cost: O(r), r = rows of this group (a scan by name).
pub(crate) fn answer(target: &Value, method: &str) -> Option<Result<Value, RuntimeError>> {
    if !is_failure(target) {
        return None;
    }
    ROWS.iter()
        .find(|row| row.name == method)
        .and_then(|row| match row.handler {
            Handler::Narrow(f) => f(target, &[]),
            // A renderer with no interpreter: an exception other than
            // `X::AdHoc` renders by its plain value form.
            Handler::InterpPureFirst(pure, _) => pure(target, &[]).or_else(|| {
                let exception = exception_value(target)?;
                Some(Ok(raku_with(
                    target,
                    &crate::value::raku_repr::raku_value(&exception),
                )))
            }),
            _ => None,
        })
}

/// `self!throw`: mark the Failure handled, then throw its exception with the
/// backtrace of the `fail` site.
// Cost: O(b), b = chars of the origin backtrace.
pub(crate) fn explode(target: &Value) -> RuntimeError {
    let err = match Interpreter::unhandled_failure_error(target) {
        Err(err) => err,
        Ok(()) => match exception_value(target) {
            Some(ex) => RuntimeError::from_exception_value(ex),
            None => RuntimeError::new("Failed"),
        },
    };
    target.mark_failure_handled();
    err
}

// Cost: O(1).
fn exception_value(target: &Value) -> Option<Value> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    attributes.as_map().get("exception").cloned()
}

/// The wrapped exception's message, `<type> with no message` without one, and
/// `Failed` for a Failure that holds no exception.
// Cost: O(m), m = chars of the message.
fn message(target: &Value) -> String {
    match exception_value(target) {
        Some(ex) => {
            let text = ex.to_string_value();
            if text.is_empty() {
                format!(
                    "{} with no message",
                    crate::value::types::what_type_name(&ex)
                )
            } else {
                text
            }
        }
        None => "Failed".to_string(),
    }
}

/// The text of the `fail` site's backtrace: the Failure's own origin (a
/// `Backtrace` or its text), else the one its exception carries.
// Cost: O(b), b = chars of the backtrace.
fn backtrace_text(target: &Value) -> String {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return String::new();
    };
    let map = attributes.as_map();
    match map.get(Interpreter::FAILURE_ORIGIN_BACKTRACE_ATTR) {
        Some(origin) => match origin.view() {
            ValueView::Instance { attributes, .. } => attributes
                .as_map()
                .get("text")
                .map(Value::to_string_value)
                .unwrap_or_default(),
            _ => origin.to_string_value(),
        },
        None => map
            .get("exception")
            .and_then(Interpreter::exception_backtrace_text)
            .unwrap_or_default(),
    }
}

/// `Failure.exception`: the wrapped exception object, `Nil` without one.
// Cost: O(1).
fn exception(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(exception_value(target).unwrap_or(Value::NIL)))
}

/// `Failure.handled`: whether it was defused (`.Bool`, `.defined`, a throw,
/// `.handled = ...`).
// Cost: O(1), one registry lookup.
fn handled(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(Value::truth(target.is_failure_handled())))
}

/// `Failure.backtrace`: the `Backtrace` of the `fail` site.
// Cost: O(b), b = frames of the backtrace.
fn backtrace(target: &Value, _args: &[Value]) -> Answer {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    if let Some(origin) = attributes
        .as_map()
        .get(Interpreter::FAILURE_ORIGIN_BACKTRACE_ATTR)
        && matches!(origin.view(), ValueView::Instance { .. })
    {
        return Some(Ok(origin.clone()));
    }
    Some(Ok(Interpreter::backtrace_value_from_string_with_runtime(
        &backtrace_text(target),
        true,
    )))
}

/// `Failure.mess`: `(HANDLED) ` once defused, the message, a newline and the
/// backtrace of the `fail` site.
// Cost: O(m + b), m = chars of the message, b = chars of the backtrace.
fn mess_text(target: &Value) -> String {
    let handled = if target.is_failure_handled() {
        "(HANDLED) "
    } else {
        ""
    };
    // `Backtrace.Str` ends each frame with a newline.
    let mut backtrace = backtrace_text(target);
    if !backtrace.is_empty() && !backtrace.ends_with('\n') {
        backtrace.push('\n');
    }
    format!("{handled}{}\n{backtrace}", message(target))
}

// Cost: as `mess_text`.
fn mess(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(Value::str(mess_text(target))))
}

/// `Failure.gist` and `Failure.Str`: the [`mess`] of a handled Failure; an
/// unhandled one throws.
// Cost: as `mess_text`.
fn gist(target: &Value, _args: &[Value]) -> Answer {
    Some(if target.is_failure_handled() {
        Ok(Value::str(mess_text(target)))
    } else {
        Err(explode(target))
    })
}

// Cost: as `gist`.
fn str_row(target: &Value, args: &[Value]) -> Answer {
    gist(target, args)
}

/// `Failure.raku`: Rakudo's `self.Mu::raku`, the object with its exception and
/// a `Backtrace.new`; a handled one is wrapped in `orelse ... *.self`, which
/// rebuilds it defused. `exception` is the exception's own `.raku`.
// Cost: O(e), e = chars of `exception`.
fn raku_with(target: &Value, exception: &str) -> Value {
    let object = format!("Failure.new(exception => {exception}, backtrace => Backtrace.new)");
    Value::str(if target.is_failure_handled() {
        format!("&CORE::infix:<orelse>({object}, *.self)")
    } else {
        object
    })
}

/// The pure half of `Failure.raku`: the commonest exception, an `X::AdHoc`
/// (`fail "message"`), renders without an interpreter; any other declines.
// Cost: O(p), p = chars of the payload's rendering.
fn raku_pure(target: &Value, _args: &[Value]) -> Answer {
    let exception = exception_value(target)?;
    let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = exception.view()
    else {
        return None;
    };
    if class_name != "X::AdHoc" {
        return None;
    }
    let payload = attributes.as_map().get("payload").cloned()?;
    let rendered = format!(
        "X::AdHoc.new(payload => {})",
        crate::value::raku_repr::raku_value(&payload)
    );
    Some(Ok(raku_with(target, &rendered)))
}

/// `Failure.raku` for any exception: its rendering is the instance's own.
// Cost: one rendering of the exception.
fn raku(interp: &mut Interpreter, target: &Value, args: &[Value], _: Named<'_>) -> Answer {
    if let Some(answer) = raku_pure(target, args) {
        return Some(answer);
    }
    let exception = exception_value(target)?;
    let rendered = match interp.default_instance_repr(&exception, "raku", &[]) {
        Some(Ok(text)) => text.to_string_value(),
        Some(Err(err)) => return Some(Err(err)),
        None => crate::value::raku_repr::raku_value(&exception),
    };
    Some(Ok(raku_with(target, &rendered)))
}

/// `Failure.Bool`: always false, and it defuses the failure.
// Cost: O(1).
fn bool_row(target: &Value, _args: &[Value]) -> Answer {
    target.mark_failure_handled();
    Some(Ok(Value::FALSE))
}

/// `Failure.Int`: the `Int` type object once handled; an unhandled one throws.
// Cost: O(1).
fn int_row(target: &Value, _args: &[Value]) -> Answer {
    Some(if target.is_failure_handled() {
        Ok(Value::package(Symbol::intern("Int")))
    } else {
        Err(explode(target))
    })
}

/// `Failure.Num` and `Failure.Numeric`: `NaN` once handled; an unhandled one
/// throws.
// Cost: O(1).
fn nan_row(target: &Value, _args: &[Value]) -> Answer {
    Some(if target.is_failure_handled() {
        Ok(Value::num(f64::NAN))
    } else {
        Err(explode(target))
    })
}

/// `Failure.self`: the Failure itself once handled; an unhandled one throws.
// Cost: O(1).
fn throw_unless_handled(target: &Value, _args: &[Value]) -> Answer {
    Some(if target.is_failure_handled() {
        Ok(target.clone())
    } else {
        Err(explode(target))
    })
}

/// `Failure.sink`: sinking an unhandled Failure throws it; a handled one
/// answers `Empty` (`self!throw() unless $!handled`).
// Cost: O(1).
fn sink(target: &Value, _args: &[Value]) -> Answer {
    Some(if target.is_failure_handled() {
        Ok(Value::slip(Vec::new()))
    } else {
        Err(explode(target))
    })
}

/// `Failure.throw`, `CALL-ME` and `STORE`: throw the wrapped exception, handled
/// or not.
// Cost: O(1).
fn throw(target: &Value, _args: &[Value]) -> Answer {
    Some(Err(explode(target)))
}

/// `Failure.AT-POS`, `AT-KEY` and `Failure`: the Failure itself, so a
/// subscript propagates it.
// Cost: O(1).
fn itself(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(target.clone()))
}

/// `$name.new(self)` once handled; an unhandled one throws.
// Cost: O(1), one element.
fn quanthash(interp: &mut Interpreter, target: &Value, name: &str) -> Answer {
    if !target.is_failure_handled() {
        return Some(Err(explode(target)));
    }
    interp.try_native_quanthash_construct_for_package(
        Symbol::intern(name),
        std::slice::from_ref(target),
    )
}

/// The answer of `Failure`'s own method `method` for `target`, run with the
/// call's arguments, or `None` when `Failure` declares no row that binds them.
// Cost: O(k) to find the row, k = rows named `method`, plus the handler's.
pub(crate) fn own_method(
    interp: &mut Interpreter,
    target: &Value,
    method: &str,
    args: &[Value],
) -> Answer {
    if !is_failure(target) {
        return None;
    }
    crate::builtins::method_table::invoke_owner(interp, &["Failure"], method, args, || {
        target.clone()
    })
}
