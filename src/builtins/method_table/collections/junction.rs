//! `Junction`'s own rows (ADR-11276 §9.151): `gist`, `raku`, `Str` and `sink`.
//!
//! A junction has no dispatch shape (§9.66): a method it does not declare is
//! autothreaded over its values by the interpreter
//! (`Interpreter::should_autothread_method`). These are the methods Rakudo
//! declares on `Junction` itself, reached by owner from the by-name dispatch:
//!
//! - `gist` and `raku` render the junction (`any(1, 2)`), each value by its
//!   own `gist` (`Nil` for a `Nil`) or `raku`; `perl` is `raku`;
//! - `Str` threads (`any(1, "a").Str` is `any("1", "a")`, Rakudo's
//!   `self.THREAD(*.Str)`);
//! - `sink` is `Nil`: sinking a junction runs nothing per value.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{JunctionKind, RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

macro_rules! row {
    ($name:literal, $handler:expr) => {
        MethodRow {
            owner: "Junction",
            name: $name,
            arity: 0,
            handler: $handler,
            flags: RowFlags::OWNER_ONLY,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("gist", Handler::Interp(gist)),
    row!("raku", Handler::Interp(raku)),
    row!("Str", Handler::Interp(str_thread)),
    row!("sink", Handler::Narrow(sink)),
];

/// The values of a junction receiver, with its kind's name.
// Cost: O(1).
fn parts(target: &Value) -> Option<(&'static str, Vec<Value>)> {
    let ValueView::Junction { kind, values } = target.view() else {
        return None;
    };
    let name = match kind {
        JunctionKind::Any => "any",
        JunctionKind::All => "all",
        JunctionKind::One => "one",
        JunctionKind::None => "none",
    };
    Some((name, values.to_vec()))
}

/// `kind(v1, v2, ...)`, each value rendered by `render`.
// Cost: O(v) render calls, v = values.
fn render(i: &mut Interpreter, target: &Value, render: &str) -> Answer {
    let (kind, values) = parts(target)?;
    let mut rendered = Vec::with_capacity(values.len());
    for value in values {
        if render == "gist" && value.is_nil() {
            rendered.push("Nil".to_string());
            continue;
        }
        match i.call_method_with_values(value, render, Vec::new()) {
            Ok(text) => rendered.push(text.to_string_value()),
            Err(err) => return Some(Err(err)),
        }
    }
    Some(Ok(Value::str(format!("{kind}({})", rendered.join(", ")))))
}

// Cost: O(v) render calls, v = values.
fn gist(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    render(i, t, "gist")
}

// Cost: O(v) render calls, v = values.
fn raku(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    render(i, t, "raku")
}

/// `Junction.Str`: a junction of each value's `Str`.
// Cost: O(v) `Str` calls, v = values.
fn str_thread(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    let ValueView::Junction { kind, values } = t.view() else {
        return None;
    };
    let mut strs = Vec::with_capacity(values.len());
    for value in values.iter() {
        match i.call_method_with_values(value.clone(), "Str", Vec::new()) {
            Ok(text) => strs.push(text),
            Err(err) => return Some(Err(err)),
        }
    }
    Some(Ok(Value::junction(kind, strs)))
}

// Cost: O(1).
fn sink(t: &Value, _: &[Value]) -> Answer {
    matches!(t.view(), ValueView::Junction { .. }).then_some(Ok(Value::NIL))
}
