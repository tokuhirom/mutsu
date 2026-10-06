//! `DateTime`'s own rows (ADR-11276 §9.18): the time of day, the UTC offset,
//! the Julian dates, the coercions and the renderings.
//!
//! The handlers take the instance's attributes. The cascade's
//! `datetime_method_0arg` calls the same functions for the receivers the table
//! has no shape for (an instance of a subclass).

use super::temporal::with_attrs;
use super::{Handler, MethodRow, RowFlags};
use crate::builtins::methods_0arg::temporal;
use crate::builtins::methods_0arg::temporal_dispatch::{f64_to_decimal_rat, gcd_i64};
use crate::symbol::Symbol;
use crate::value::temporal_core::datetime_attrs;
use crate::value::{AttrMap, RuntimeError, Value, ValueView};
use std::collections::HashMap;

pub(super) static ROWS: &[MethodRow] = &[
    super::narrow_row!("DateTime", "second", 0, second_row),
    super::narrow_row!("DateTime", "timezone", 0, timezone_row),
    super::narrow_row!("DateTime", "offset", 0, timezone_row),
    super::narrow_row!("DateTime", "offset-in-hours", 0, offset_in_hours_row),
    super::narrow_row!("DateTime", "offset-in-minutes", 0, offset_in_minutes_row),
    super::narrow_row!("DateTime", "whole-second", 0, whole_second_row),
    super::narrow_row!("DateTime", "hh-mm-ss", 0, hh_mm_ss_row),
    super::narrow_row!("DateTime", "posix", 0, posix_row),
    super::narrow_row!("DateTime", "utc", 0, utc_row),
    super::narrow_row!("DateTime", "julian-date", 0, julian_date_row),
    super::narrow_row!(
        "DateTime",
        "modified-julian-date",
        0,
        modified_julian_date_row
    ),
    super::narrow_row!("DateTime", "day-fraction", 0, day_fraction_row),
    super::narrow_row!("DateTime", "Date", 0, to_date_row),
    super::narrow_row!("DateTime", "DateTime", 0, to_datetime_row),
    super::narrow_row!("DateTime", "Instant", 0, instant_row),
    super::narrow_row!("DateTime", "Numeric", 0, instant_row),
    super::narrow_row!("DateTime", "Real", 0, instant_row),
    super::narrow_row!("DateTime", "WHICH", 0, which_row),
    super::narrow_row!("DateTime", "raku", 0, raku_row),
    super::narrow_row!("DateTime", "Str", 0, str_row),
    super::narrow_row!("DateTime", "gist", 0, str_row),
];

/// `DateTime.second`: an `Int` for a whole second, a `Rat` otherwise.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn second(attributes: &AttrMap) -> Value {
    let second = datetime_attrs(attributes).5;
    if second == second.floor() {
        Value::int(second as i64)
    } else {
        f64_to_decimal_rat(second)
    }
}

/// `DateTime.timezone` (and `offset`): the offset from UTC in seconds.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn timezone(attributes: &AttrMap) -> Value {
    Value::int(datetime_attrs(attributes).6)
}

/// The exact `Rat` `timezone / divisor`.
fn offset_in(attributes: &AttrMap, divisor: i64) -> Value {
    let timezone = datetime_attrs(attributes).6;
    let gcd = gcd_i64(timezone.abs(), divisor).max(1);
    Value::rat_raw(timezone / gcd, divisor / gcd)
}

/// `DateTime.offset-in-hours`.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn offset_in_hours(attributes: &AttrMap) -> Value {
    offset_in(attributes, 3600)
}

/// `DateTime.offset-in-minutes`: a `Rat`, a whole one included.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn offset_in_minutes(attributes: &AttrMap) -> Value {
    offset_in(attributes, 60)
}

/// `DateTime.whole-second`.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn whole_second(attributes: &AttrMap) -> Value {
    Value::int(datetime_attrs(attributes).5.floor() as i64)
}

/// `DateTime.hh-mm-ss`.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn hh_mm_ss(attributes: &AttrMap) -> Value {
    let (_, _, _, hour, minute, second, _) = datetime_attrs(attributes);
    Value::str(format!(
        "{:02}:{:02}:{:02}",
        hour,
        minute,
        second.floor() as i64
    ))
}

/// `DateTime.posix`: the POSIX timestamp, whole seconds.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn posix(attributes: &AttrMap) -> Value {
    let (year, month, day, hour, minute, second, timezone) = datetime_attrs(attributes);
    let posix = temporal::datetime_to_posix(year, month, day, hour, minute, second, timezone);
    Value::int(posix.floor() as i64)
}

/// `DateTime.utc` (`in-timezone(0)`): the same instant at offset zero, keeping
/// the formatter. Leap-second aware: `19:59:60-04:00` is `23:59:60Z`.
// Cost: O(a + l), a = attributes of the instance, l = leap seconds (a table of
// 28).
pub(crate) fn utc(attributes: &AttrMap) -> Value {
    let (year, month, day, hour, minute, second, timezone) = datetime_attrs(attributes);
    let (int_part, frac) =
        temporal::datetime_to_instant_parts(year, month, day, hour, minute, second, timezone);
    let (year, month, day, hour, minute, second) =
        temporal::instant_to_datetime_leap_aware_parts(int_part, frac, 0);
    temporal::with_formatter(
        temporal::make_datetime(year, month, day, hour, minute, second, 0),
        attributes.get("formatter").cloned(),
    )
}

/// The components of the instant in UTC, which is what the Julian dates
/// count from.
fn utc_parts(attributes: &AttrMap) -> (i64, i64, i64, i64, i64, f64) {
    let (year, month, day, hour, minute, second, timezone) = datetime_attrs(attributes);
    let (int_part, frac) =
        temporal::datetime_to_instant_parts(year, month, day, hour, minute, second, timezone);
    temporal::instant_to_datetime_leap_aware_parts(int_part, frac, 0)
}

/// `DateTime.julian-date`: of the instant in UTC, as an exact `Rat`.
// Cost: O(a + l), a = attributes of the instance, l = leap seconds (a table of
// 28).
pub(crate) fn julian_date(attributes: &AttrMap) -> Result<Value, RuntimeError> {
    let (year, month, day, hour, minute, second) = utc_parts(attributes);
    temporal::julian_date(year, month, day, hour, minute, second)
}

/// `DateTime.modified-julian-date`: of the instant in UTC, as an exact `Rat`.
// Cost: O(a + l), a = attributes of the instance, l = leap seconds (a table of
// 28).
pub(crate) fn modified_julian_date(attributes: &AttrMap) -> Result<Value, RuntimeError> {
    let (year, month, day, hour, minute, second) = utc_parts(attributes);
    temporal::modified_julian_date(year, month, day, hour, minute, second)
}

/// `DateTime.day-fraction`: the fraction of the local day, as an exact `Rat`.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn day_fraction(attributes: &AttrMap) -> Value {
    let (year, month, day, hour, minute, second, _) = datetime_attrs(attributes);
    let (numerator, denominator) =
        temporal::day_fraction_rational(year, month, day, hour, minute, second);
    crate::value::make_rat(numerator, denominator)
}

/// `DateTime.Date`: the local date.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn to_date(attributes: &AttrMap) -> Value {
    let (year, month, day, ..) = datetime_attrs(attributes);
    temporal::make_date(year, month, day)
}

/// `DateTime.DateTime`: the value itself, formatter included.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn to_datetime(attributes: &AttrMap) -> Value {
    let (year, month, day, hour, minute, second, timezone) = datetime_attrs(attributes);
    temporal::with_formatter(
        temporal::make_datetime(year, month, day, hour, minute, second, timezone),
        attributes.get("formatter").cloned(),
    )
}

/// `DateTime.Instant` (and `Numeric`, `Real`): the `Instant` the value names.
// Cost: O(a + l), a = attributes of the instance, l = leap seconds (a table of
// 28).
pub(crate) fn instant(attributes: &AttrMap) -> Value {
    let (year, month, day, hour, minute, second, timezone) = datetime_attrs(attributes);
    let (int_part, frac) =
        temporal::datetime_to_instant_parts(year, month, day, hour, minute, second, timezone);
    let mut attrs = HashMap::new();
    if frac == 0.0 {
        attrs.insert("value".to_string(), Value::int(int_part));
    } else {
        let scale = 1_000_000_000i64;
        let numerator = int_part * scale + (frac * scale as f64).round() as i64;
        let gcd = gcd_i64(numerator.abs(), scale);
        attrs.insert(
            "value".to_string(),
            Value::rat_raw(numerator / gcd, scale / gcd),
        );
    }
    Value::make_instance(Symbol::intern("Instant"), attrs)
}

/// `DateTime.WHICH`: a `ValueObjAt` naming the value by its ISO 8601 form.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn which(attributes: &AttrMap) -> Value {
    let (year, month, day, hour, minute, second, timezone) = datetime_attrs(attributes);
    let mut attrs = HashMap::new();
    attrs.insert(
        "WHICH".to_string(),
        Value::str(format!(
            "DateTime|{}",
            temporal::format_datetime(year, month, day, hour, minute, second, timezone)
        )),
    );
    Value::make_instance(Symbol::intern("ValueObjAt"), attrs)
}

/// `DateTime.raku`: the numeric-argument constructor call, a fractional second
/// as a decimal and a non-UTC offset as a trailing `:timezone(N)`.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn raku(attributes: &AttrMap) -> Value {
    let (year, month, day, hour, minute, second, timezone) = datetime_attrs(attributes);
    let second = if second == second.floor() {
        format!("{}", second as i64)
    } else {
        format!("{second}")
    };
    let mut text = format!("DateTime.new({year},{month},{day},{hour},{minute},{second}");
    if timezone != 0 {
        text.push_str(&format!(",:timezone({timezone})"));
    }
    text.push(')');
    Value::str(text)
}

/// `DateTime.Str` (and `gist`): the ISO 8601 form. A value made with a
/// `:formatter` is rendered by running that Callable, which only the
/// interpreter can do, so it answers `None` and takes the interpreter's path.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn str_of(attributes: &AttrMap) -> Option<Value> {
    if attributes.contains_key("formatter") {
        return None;
    }
    let (year, month, day, hour, minute, second, timezone) = datetime_attrs(attributes);
    Some(Value::str(temporal::format_datetime(
        year, month, day, hour, minute, second, timezone,
    )))
}

macro_rules! attr_row_fns {
    ($($row:ident => $handler:ident),* $(,)?) => {
        $(
            fn $row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
                with_attrs(target, $handler)
            }
        )*
    };
}

attr_row_fns! {
    second_row => second,
    timezone_row => timezone,
    offset_in_hours_row => offset_in_hours,
    offset_in_minutes_row => offset_in_minutes,
    whole_second_row => whole_second,
    hh_mm_ss_row => hh_mm_ss,
    posix_row => posix,
    utc_row => utc,
    day_fraction_row => day_fraction,
    to_date_row => to_date,
    to_datetime_row => to_datetime,
    instant_row => instant,
    which_row => which,
    raku_row => raku,
}

/// A row for a handler that can fail.
macro_rules! fallible_row_fns {
    ($($row:ident => $handler:ident),* $(,)?) => {
        $(
            fn $row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
                match target.view() {
                    ValueView::Instance { attributes, .. } => Some($handler(&attributes.as_map())),
                    _ => None,
                }
            }
        )*
    };
}

fallible_row_fns! {
    julian_date_row => julian_date,
    modified_julian_date_row => modified_julian_date,
}

fn str_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Instance { attributes, .. } => str_of(&attributes.as_map()).map(Ok),
        _ => None,
    }
}
