//! `Date`'s and `DateTime`'s component rows (ADR-11276 §10, slice 3A proof
//! rows for the two instance-class shapes; slice 3D moves the rest).
//!
//! The functions take the instance's attributes, which is what the native
//! cascade's `date_method_0arg` / `datetime_method_0arg` hold for a *subclass*
//! of `Date` (no shape, so no row); both call them.

use super::{Handler, MethodRow, RowFlags};
use crate::value::temporal_core::{date_attrs, datetime_attrs};
use crate::value::{AttrMap, RuntimeError, Value, ValueView};

macro_rules! rows {
    ($owner:literal: $($name:literal => $handler:ident),* $(,)?) => {
        &[$(MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: RowFlags::NONE,
            named: &[],
        }),*]
    };
}

pub(super) static DATE_ROWS: &[MethodRow] = rows!["Date":
    "year" => date_year_row,
    "month" => date_month_row,
    "day" => date_day_row,
];

pub(super) static DATETIME_ROWS: &[MethodRow] = rows!["DateTime":
    "year" => datetime_year_row,
    "month" => datetime_month_row,
    "day" => datetime_day_row,
    "hour" => datetime_hour_row,
    "minute" => datetime_minute_row,
];

/// Run `f` on the attributes of an `Instance` receiver.
fn with_attrs(
    target: &Value,
    f: impl FnOnce(&AttrMap) -> Value,
) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Instance { attributes, .. } => Some(Ok(f(&attributes.as_map()))),
        _ => None,
    }
}

/// `Date.year`.
// Cost: O(a), a = attributes of the instance (read through a copy of the map).
pub(crate) fn date_year(attributes: &AttrMap) -> Value {
    Value::int(date_attrs(attributes).0)
}

/// `Date.month`.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn date_month(attributes: &AttrMap) -> Value {
    Value::int(date_attrs(attributes).1)
}

/// `Date.day` (and `day-of-month`).
// Cost: O(a), a = attributes of the instance.
pub(crate) fn date_day(attributes: &AttrMap) -> Value {
    Value::int(date_attrs(attributes).2)
}

/// `DateTime.year`.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn datetime_year(attributes: &AttrMap) -> Value {
    Value::int(datetime_attrs(attributes).0)
}

/// `DateTime.month`.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn datetime_month(attributes: &AttrMap) -> Value {
    Value::int(datetime_attrs(attributes).1)
}

/// `DateTime.day` (and `day-of-month`).
// Cost: O(a), a = attributes of the instance.
pub(crate) fn datetime_day(attributes: &AttrMap) -> Value {
    Value::int(datetime_attrs(attributes).2)
}

/// `DateTime.hour`.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn datetime_hour(attributes: &AttrMap) -> Value {
    Value::int(datetime_attrs(attributes).3)
}

/// `DateTime.minute`.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn datetime_minute(attributes: &AttrMap) -> Value {
    Value::int(datetime_attrs(attributes).4)
}

fn date_year_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    with_attrs(target, date_year)
}

fn date_month_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    with_attrs(target, date_month)
}

fn date_day_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    with_attrs(target, date_day)
}

fn datetime_year_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    with_attrs(target, datetime_year)
}

fn datetime_month_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    with_attrs(target, datetime_month)
}

fn datetime_day_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    with_attrs(target, datetime_day)
}

fn datetime_hour_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    with_attrs(target, datetime_hour)
}

fn datetime_minute_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    with_attrs(target, datetime_minute)
}
