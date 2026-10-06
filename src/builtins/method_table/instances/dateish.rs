//! The calendar rows `Date` and `DateTime` share (ADR-11276 §9.18).
//!
//! Rakudo composes the `Dateish` role into both classes, so each declares every
//! one of these methods itself, and both owners' rows point at one handler
//! here. A handler reads the instance's `year`/`month`/`day` attributes, which
//! a `Date` and a `DateTime` both carry; the cascade's `date_method_0arg` and
//! `datetime_method_0arg` call the same functions for a *subclass* instance (it
//! has no shape, so no row).

use super::temporal::{sep_arg, with_attrs};
use super::{Handler, MethodRow, RowFlags};
use crate::builtins::methods_0arg::temporal;
use crate::value::temporal_core::date_attrs;
use crate::value::{AttrMap, RuntimeError, Value};

macro_rules! calendar_rows {
    ($owner:literal) => {
        &[
            super::narrow_row!($owner, "day-of-month", 0, day_of_month_row),
            super::narrow_row!($owner, "day-of-week", 0, day_of_week_row),
            super::narrow_row!($owner, "day-of-year", 0, day_of_year_row),
            super::narrow_row!($owner, "daycount", 0, daycount_row),
            super::narrow_row!($owner, "days-in-month", 0, days_in_month_row),
            super::narrow_row!($owner, "days-in-year", 0, days_in_year_row),
            super::narrow_row!($owner, "is-leap-year", 0, is_leap_year_row),
            super::narrow_row!($owner, "week", 0, week_row),
            super::narrow_row!($owner, "week-number", 0, week_number_row),
            super::narrow_row!($owner, "week-year", 0, week_year_row),
            super::narrow_row!($owner, "weekday-of-month", 0, weekday_of_month_row),
            super::narrow_row!($owner, "formatter", 0, formatter_row),
            super::narrow_row!($owner, "yyyy-mm-dd", 0, yyyy_mm_dd_row),
            super::narrow_row!($owner, "yyyy-mm-dd", 1, yyyy_mm_dd_sep_row),
            super::narrow_row!($owner, "mm-dd-yyyy", 0, mm_dd_yyyy_row),
            super::narrow_row!($owner, "mm-dd-yyyy", 1, mm_dd_yyyy_sep_row),
            super::narrow_row!($owner, "dd-mm-yyyy", 0, dd_mm_yyyy_row),
            super::narrow_row!($owner, "dd-mm-yyyy", 1, dd_mm_yyyy_sep_row),
            super::narrow_row!($owner, "mm-dd", 0, mm_dd_row),
            super::narrow_row!($owner, "mm-dd", 1, mm_dd_sep_row),
            super::narrow_row!($owner, "yyyy-mm", 0, yyyy_mm_row),
            super::narrow_row!($owner, "yyyy-mm", 1, yyyy_mm_sep_row),
        ]
    };
}

pub(super) static DATE_ROWS: &[MethodRow] = calendar_rows!("Date");
pub(super) static DATETIME_ROWS: &[MethodRow] = calendar_rows!("DateTime");

/// The civil date of the instance, as `(year, month, day, epoch days)`.
fn civil(attributes: &AttrMap) -> (i64, i64, i64, i64) {
    let (year, month, day) = date_attrs(attributes);
    (
        year,
        month,
        day,
        temporal::civil_to_epoch_days(year, month, day),
    )
}

/// `Dateish.day-of-month`.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn day_of_month(attributes: &AttrMap) -> Value {
    Value::int(date_attrs(attributes).2)
}

/// `Dateish.day-of-week`: 1 (Monday) to 7 (Sunday).
// Cost: O(a), a = attributes of the instance.
pub(crate) fn day_of_week(attributes: &AttrMap) -> Value {
    Value::int(temporal::day_of_week(civil(attributes).3))
}

/// `Dateish.day-of-year`.
// Cost: O(a + m), a = attributes of the instance, m = month (at most 12).
pub(crate) fn day_of_year(attributes: &AttrMap) -> Value {
    let (year, month, day, _) = civil(attributes);
    Value::int(temporal::day_of_year(year, month, day))
}

/// `Dateish.daycount`: the Modified Julian Day number of the local date.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn daycount(attributes: &AttrMap) -> Value {
    let (year, month, day, _) = civil(attributes);
    Value::int(temporal::daycount(year, month, day))
}

/// `Dateish.days-in-month`.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn days_in_month(attributes: &AttrMap) -> Value {
    let (year, month, ..) = civil(attributes);
    Value::int(temporal::days_in_month(year, month))
}

/// `Dateish.days-in-year`.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn days_in_year(attributes: &AttrMap) -> Value {
    let (year, ..) = civil(attributes);
    Value::int(if temporal::is_leap_year(year) {
        366
    } else {
        365
    })
}

/// `Dateish.is-leap-year`.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn is_leap_year(attributes: &AttrMap) -> Value {
    let (year, ..) = civil(attributes);
    Value::truth(temporal::is_leap_year(year))
}

/// `Dateish.week`: the ISO week-year and week number, as a two-element list.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn week(attributes: &AttrMap) -> Value {
    let (year, month, day, _) = civil(attributes);
    let (week_year, week_number) = temporal::iso_week(year, month, day);
    Value::array(vec![Value::int(week_year), Value::int(week_number)])
}

/// `Dateish.week-number`.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn week_number(attributes: &AttrMap) -> Value {
    let (year, month, day, _) = civil(attributes);
    Value::int(temporal::iso_week(year, month, day).1)
}

/// `Dateish.week-year`.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn week_year(attributes: &AttrMap) -> Value {
    let (year, month, day, _) = civil(attributes);
    Value::int(temporal::iso_week(year, month, day).0)
}

/// `Dateish.weekday-of-month`: which occurrence of its weekday the day is.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn weekday_of_month(attributes: &AttrMap) -> Value {
    Value::int((date_attrs(attributes).2 - 1) / 7 + 1)
}

/// `Dateish.formatter`: the `:formatter` the value was made with, or the
/// `Callable` type object when it has none.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn formatter(attributes: &AttrMap) -> Value {
    attributes
        .get("formatter")
        .cloned()
        .unwrap_or_else(|| Value::package(crate::symbol::Symbol::intern("Callable")))
}

/// A date in one of `Dateish`'s orderings (`yyyy-mm-dd`, `mm-dd-yyyy`,
/// `dd-mm-yyyy`, `mm-dd`, `yyyy-mm`), its fields joined by `sep`.
// Cost: O(a + s), a = attributes of the instance, s = length of the separator.
pub(crate) fn ordered(order: &str, attributes: &AttrMap, sep: &str) -> Value {
    let (year, month, day, _) = civil(attributes);
    Value::str(temporal::format_date_ordered(order, year, month, day, sep))
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
    day_of_month_row => day_of_month,
    day_of_week_row => day_of_week,
    day_of_year_row => day_of_year,
    daycount_row => daycount,
    days_in_month_row => days_in_month,
    days_in_year_row => days_in_year,
    is_leap_year_row => is_leap_year,
    week_row => week,
    week_number_row => week_number,
    week_year_row => week_year,
    weekday_of_month_row => weekday_of_month,
    formatter_row => formatter,
}

/// The rows of an ordering: with no separator, and with one.
macro_rules! ordered_row_fns {
    ($($zero:ident, $one:ident => $order:literal;)*) => {
        $(
            fn $zero(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
                with_attrs(target, |attributes| ordered($order, attributes, "-"))
            }

            fn $one(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
                let sep = sep_arg(args.first()?);
                with_attrs(target, |attributes| ordered($order, attributes, &sep))
            }
        )*
    };
}

ordered_row_fns! {
    yyyy_mm_dd_row, yyyy_mm_dd_sep_row => "yyyy-mm-dd";
    mm_dd_yyyy_row, mm_dd_yyyy_sep_row => "mm-dd-yyyy";
    dd_mm_yyyy_row, dd_mm_yyyy_sep_row => "dd-mm-yyyy";
    mm_dd_row, mm_dd_sep_row => "mm-dd";
    yyyy_mm_row, yyyy_mm_sep_row => "yyyy-mm";
}
