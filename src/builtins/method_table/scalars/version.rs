//! `Version`'s rows (ADR-11276 §10, slice 3A proof rows for the `Version`
//! shape; slice 3B moves the rest of its methods).

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::methods_0arg::which::which_of;
use crate::value::raku_repr::raku_value;
use crate::value::{RuntimeError, Value, ValueView, VersionPart};

macro_rules! row {
    ($name:literal, $handler:ident) => {
        MethodRow {
            owner: "Version",
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: RowFlags::NONE,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("parts", parts),
    row!("plus", plus),
    row!("whatever", whatever),
    row!("Str", text),
    row!("gist", raku),
    row!("raku", raku),
    row!("WHICH", which),
    row!("Version", itself),
    MethodRow {
        owner: "Version",
        name: "ACCEPTS",
        arity: 1,
        handler: Handler::Narrow(accepts),
        flags: RowFlags::NONE,
        named: &[],
    },
];

/// `Version.Str`: the version without the `v` prefix (`1.2.3+`).
// Cost: O(n), n = chars of the rendering.
pub(crate) fn text(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Version { .. } => Some(Ok(Value::str(target.to_string_value()))),
        _ => None,
    }
}

/// `Version.gist` and `Version.raku`: the `v` literal form (`v1.2.3+`), or
/// `Version.new('..')` when the text has no literal form.
// Cost: O(n), n = chars of the rendering.
pub(crate) fn raku(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Version { .. } => Some(Ok(Value::str(raku_value(target)))),
        _ => None,
    }
}

/// `Version.WHICH`: `Version|` and the canonical text.
// Cost: O(n), n = chars of the rendering.
pub(crate) fn which(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Version { .. } => Some(Ok(which_of(target))),
        _ => None,
    }
}

/// `Version.Version`: the version itself.
// Cost: O(1).
pub(crate) fn itself(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Version { .. } => Some(Ok(target.clone())),
        _ => None,
    }
}

/// `Version.ACCEPTS($candidate)`: whether the candidate version matches this
/// one (`v1.2.*`, `v1.2+`); a non-version candidate is read as its text.
// Cost: O(p), p = parts of the version matcher.
pub(crate) fn accepts(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Version {
        parts, plus, minus, ..
    } = target.view()
    else {
        return None;
    };
    let candidate = args.first()?;
    // An instance of a `Version` subclass carries its version in an attribute;
    // the cascade reads it.
    if matches!(candidate.view(), ValueView::Instance { .. }) {
        return None;
    }
    Some(Ok(Value::truth(
        crate::runtime::Interpreter::version_smart_match(candidate, parts, plus, minus),
    )))
}

/// `Version.parts`: the version's parts as a `List` of `Int` and `Str`; a `*`
/// part is the `Str` `"*"`, as in Rakudo, not a `Whatever` (zef's
/// `DependencySpecification` matching reads it, and joins the parts back).
// Cost: O(p), p = parts of the version.
pub(crate) fn parts(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Version { parts, .. } = target.view() else {
        return None;
    };
    let items: Vec<Value> = parts
        .iter()
        .map(|part| match part {
            VersionPart::Num(n) => Value::int(*n),
            VersionPart::Str(s) => Value::str_from(s.as_str()),
            VersionPart::Whatever => Value::str_from("*"),
        })
        .collect();
    Some(Ok(Value::array_with_kind(
        crate::gc::Gc::new(crate::value::ArrayData::new(items)),
        crate::value::ArrayKind::List,
    )))
}

/// `Version.plus`: whether the version ends in `+`.
// Cost: O(1).
pub(crate) fn plus(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Version { plus, .. } => Some(Ok(Value::truth(plus))),
        _ => None,
    }
}

/// `Version.whatever`: whether any part is a `*`.
// Cost: O(p), p = parts of the version.
pub(crate) fn whatever(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Version { parts, .. } => Some(Ok(Value::truth(
            parts
                .iter()
                .any(|part| matches!(part, VersionPart::Whatever)),
        ))),
        _ => None,
    }
}
