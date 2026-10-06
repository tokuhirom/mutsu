//! `Version`'s rows (ADR-11276 §10, slice 3A proof rows for the `Version`
//! shape; slice 3B moves the rest of its methods).

use super::{Handler, MethodRow, RowFlags};
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
];

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
