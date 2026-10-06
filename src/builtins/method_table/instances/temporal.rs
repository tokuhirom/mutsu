//! `Date`'s and `DateTime`'s component rows (ADR-11276 §10, slice 3A proof
//! rows for the two instance-class shapes; slice 3D moves the rest: see
//! `dateish.rs`, `date.rs` and `datetime.rs`).
//!
//! The functions take the instance's attributes, which is what the native
//! cascade's `date_method_0arg` / `datetime_method_0arg` hold for a *subclass*
//! of `Date` (no shape, so no row); both call them.

use super::{Handler, MethodRow, RowFlags};
use crate::value::temporal_core::{date_attrs, datetime_attrs};
use crate::value::{AttrMap, RuntimeError, Value, ValueView};

pub(super) static DATE_ROWS: &[MethodRow] = &[
    super::narrow_row!("Date", "year", 0, date_year_row),
    super::narrow_row!("Date", "month", 0, date_month_row),
    super::narrow_row!("Date", "day", 0, date_day_row),
];

pub(super) static DATETIME_ROWS: &[MethodRow] = &[
    super::narrow_row!("DateTime", "year", 0, datetime_year_row),
    super::narrow_row!("DateTime", "month", 0, datetime_month_row),
    super::narrow_row!("DateTime", "day", 0, datetime_day_row),
    super::narrow_row!("DateTime", "hour", 0, datetime_hour_row),
    super::narrow_row!("DateTime", "minute", 0, datetime_minute_row),
];

/// Run `f` on the attributes of an `Instance` receiver.
pub(super) fn with_attrs(
    target: &Value,
    f: impl FnOnce(&AttrMap) -> Value,
) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Instance { attributes, .. } => Some(Ok(f(&attributes.as_map()))),
        _ => None,
    }
}

/// The separator argument of a `Dateish` ordering. The guard hands a row plain
/// scalars only, and the cascade this replaces read any of them as the
/// separator's text, so a row does the same.
// Cost: O(n), n = chars of the argument's string form (one copy).
pub(super) fn sep_arg(arg: &Value) -> String {
    arg.to_string_value()
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
