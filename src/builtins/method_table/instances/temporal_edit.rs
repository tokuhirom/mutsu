//! `Date`'s and `DateTime`'s `truncated-to`, `in-timezone`, `local` and `clone`
//! rows (ADR-11276 §9.34, §9.127).
//!
//! The functions take the receiver and read its attributes, so an instance of a
//! subclass (which has no shape) is answered by the same rows through their
//! owner (`runtime/methods_temporal.rs`), and keeps its class through
//! [`rebless_date_result`] / [`rebless_datetime_result`].

use super::temporal_shift::{
    has_date_attrs, has_datetime_attrs, keep_formatter, rebless_date_result,
    rebless_datetime_result,
};
use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::builtins::methods_0arg::temporal;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[
    super::narrow_row!("Date", "truncated-to", 1, truncated_to_row),
    super::narrow_row!("DateTime", "truncated-to", 1, truncated_to_row),
    super::narrow_row!("DateTime", "in-timezone", 1, in_timezone_row),
    clone_row("Date", date_clone_row),
    clone_row("DateTime", datetime_clone_row),
    MethodRow {
        owner: "DateTime",
        name: "local",
        arity: 0,
        handler: Handler::Interp(local_row),
        flags: RowFlags::NONE,
        named: &[],
    },
];

/// A `clone` row: the overrides are the call's named arguments, every one of
/// them (Rakudo's `clone(*%_)`).
const fn clone_row(
    owner: &'static str,
    handler: crate::builtins::method_table::NamedFn,
) -> MethodRow {
    MethodRow {
        owner,
        name: "clone",
        arity: 0,
        handler: Handler::Named(handler),
        flags: RowFlags::ANY_NAMED,
        named: &[],
    }
}

/// `Date.clone(*%overrides)`; a subclass keeps its class and attributes.
// Cost: O(a + n), a = attributes of the instance, n = named arguments.
fn date_clone_row(
    target: &Value,
    _args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
    else {
        return None;
    };
    if !has_date_attrs(&attributes) || has_datetime_attrs(&attributes) {
        return None;
    }
    let (year, month, day) = temporal::date_attrs(&attributes.as_map());
    let formatter = attributes.as_map().get("formatter").cloned();
    Some(
        date_clone(year, month, day, formatter, named.pairs())
            .map(|v| rebless_date_result(v, class_name, &attributes)),
    )
}

/// `DateTime.clone(*%overrides)`; a subclass keeps its class and attributes.
// Cost: O(a + n), a = attributes of the instance, n = named arguments.
fn datetime_clone_row(
    target: &Value,
    _args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
    else {
        return None;
    };
    if !has_datetime_attrs(&attributes) {
        return None;
    }
    let (year, month, day, hour, minute, second, timezone) =
        temporal::datetime_attrs(&attributes.as_map());
    let formatter = attributes.as_map().get("formatter").cloned();
    Some(
        datetime_clone(
            year,
            month,
            day,
            hour,
            minute,
            second,
            timezone,
            formatter,
            named.pairs(),
        )
        .map(|v| rebless_datetime_result(v, class_name, &attributes)),
    )
}

fn truncated_to_row(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    truncated_to(target, args)
}

fn in_timezone_row(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    in_timezone(target, args.first())
}

/// `DateTime.local`: the same moment in the `$*TZ` offset.
fn local_row(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let tz = interp
        .env()
        .get("*TZ")
        .and_then(|v| v.as_int())
        .unwrap_or(0);
    in_timezone(target, Some(&Value::int(tz)))
}

/// `truncated-to($unit)` of a `Date` or `DateTime` instance (a subclass keeps
/// its class), or `None` for any other receiver.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn truncated_to(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
    else {
        return None;
    };
    if has_datetime_attrs(&attributes) {
        let (year, month, day, hour, minute, second, timezone) =
            temporal::datetime_attrs(&attributes.as_map());
        Some(
            datetime_truncated_to(year, month, day, hour, minute, second, timezone, args).map(
                |v| {
                    rebless_datetime_result(keep_formatter(v, &attributes), class_name, &attributes)
                },
            ),
        )
    } else if has_date_attrs(&attributes) {
        let (year, month, day) = temporal::date_attrs(&attributes.as_map());
        Some(
            date_truncated_to(year, month, day, args).map(|v| {
                rebless_date_result(keep_formatter(v, &attributes), class_name, &attributes)
            }),
        )
    } else {
        None
    }
}

/// `in-timezone($offset)` of a `DateTime` instance (no argument keeps the
/// offset), or `None` for any other receiver.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn in_timezone(
    target: &Value,
    offset: Option<&Value>,
) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
    else {
        return None;
    };
    if !has_datetime_attrs(&attributes) {
        return None;
    }
    let (year, month, day, hour, minute, second, timezone) =
        temporal::datetime_attrs(&attributes.as_map());
    let result = match offset {
        Some(arg) => datetime_in_timezone(
            year,
            month,
            day,
            hour,
            minute,
            second,
            timezone,
            arg.to_f64() as i64,
        ),
        None => Ok(temporal::make_datetime(
            year, month, day, hour, minute, second, timezone,
        )),
    };
    Some(
        result.map(|v| {
            rebless_datetime_result(keep_formatter(v, &attributes), class_name, &attributes)
        }),
    )
}

/// Date.clone with optional overrides.
pub(crate) fn date_clone(
    mut year: i64,
    mut month: i64,
    mut day: i64,
    existing_formatter: Option<Value>,
    args: &[Value],
) -> Result<Value, RuntimeError> {
    let mut formatter = existing_formatter;
    for arg in args {
        if let ValueView::Pair(key, value) = arg.view() {
            match key.as_str() {
                "year" => year = value.to_f64() as i64,
                "month" => month = value.to_f64() as i64,
                "day" => day = value.to_f64() as i64,
                // A type object (`:formatter(Callable)`, what `.now.formatter` returns)
                // resets to the default formatter.
                "formatter" => {
                    formatter =
                        (!matches!(value.view(), ValueView::Package(_))).then(|| value.clone());
                }
                _ => {}
            }
        }
    }
    temporal::validate_date(year, month, day)?;
    Ok(temporal::make_date_with_formatter(
        year, month, day, formatter,
    ))
}

/// DateTime.clone with optional overrides.
#[allow(clippy::too_many_arguments)]
pub(crate) fn datetime_clone(
    mut year: i64,
    mut month: i64,
    mut day: i64,
    mut hour: i64,
    mut minute: i64,
    mut second: f64,
    mut timezone: i64,
    existing_formatter: Option<Value>,
    args: &[Value],
) -> Result<Value, RuntimeError> {
    let mut formatter = existing_formatter;
    for arg in args {
        if let ValueView::Pair(key, value) = arg.view() {
            match key.as_str() {
                "year" => year = value.to_f64() as i64,
                "month" => month = value.to_f64() as i64,
                "day" => day = value.to_f64() as i64,
                "hour" => hour = value.to_f64() as i64,
                "minute" => minute = value.to_f64() as i64,
                "second" => second = value.to_f64(),
                "timezone" => timezone = value.to_f64() as i64,
                // A type object (`:formatter(Callable)`, what `.now.formatter` returns)
                // resets to the default formatter.
                "formatter" => {
                    formatter =
                        (!matches!(value.view(), ValueView::Package(_))).then(|| value.clone());
                }
                _ => {}
            }
        }
    }
    temporal::validate_datetime(year, month, day, hour, minute, second, timezone)?;
    Ok(temporal::with_formatter(
        temporal::make_datetime(year, month, day, hour, minute, second, timezone),
        formatter,
    ))
}

/// Date.truncated-to
fn date_truncated_to(
    year: i64,
    month: i64,
    day: i64,
    args: &[Value],
) -> Result<Value, RuntimeError> {
    let unit = args
        .first()
        .map(|v| v.to_string_value())
        .unwrap_or_default();
    match unit.as_str() {
        "year" => Ok(temporal::make_date(year, 1, 1)),
        "month" => Ok(temporal::make_date(year, month, 1)),
        "week" => {
            let days = temporal::civil_to_epoch_days(year, month, day);
            let dow = temporal::day_of_week(days); // 1=Mon..7=Sun
            let monday = days - (dow - 1);
            let (ny, nm, nd) = temporal::epoch_days_to_civil(monday);
            Ok(temporal::make_date(ny, nm, nd))
        }
        "day" => Ok(temporal::make_date(year, month, day)),
        _ => Err(RuntimeError::new(format!(
            "Unknown truncation unit '{}'",
            unit
        ))),
    }
}

/// DateTime.truncated-to
#[allow(clippy::too_many_arguments)]
fn datetime_truncated_to(
    year: i64,
    month: i64,
    day: i64,
    _hour: i64,
    _minute: i64,
    _second: f64,
    timezone: i64,
    args: &[Value],
) -> Result<Value, RuntimeError> {
    let unit = args
        .first()
        .map(|v| v.to_string_value())
        .unwrap_or_default();
    match unit.as_str() {
        "year" => Ok(temporal::make_datetime(year, 1, 1, 0, 0, 0.0, timezone)),
        "month" => Ok(temporal::make_datetime(year, month, 1, 0, 0, 0.0, timezone)),
        "week" => {
            let days = temporal::civil_to_epoch_days(year, month, day);
            let dow = temporal::day_of_week(days);
            let monday = days - (dow - 1);
            let (ny, nm, nd) = temporal::epoch_days_to_civil(monday);
            Ok(temporal::make_datetime(ny, nm, nd, 0, 0, 0.0, timezone))
        }
        "day" => Ok(temporal::make_datetime(
            year, month, day, 0, 0, 0.0, timezone,
        )),
        "hour" => Ok(temporal::make_datetime(
            year, month, day, _hour, 0, 0.0, timezone,
        )),
        "minute" => Ok(temporal::make_datetime(
            year, month, day, _hour, _minute, 0.0, timezone,
        )),
        "second" => Ok(temporal::make_datetime(
            year,
            month,
            day,
            _hour,
            _minute,
            _second.floor(),
            timezone,
        )),
        _ => Err(RuntimeError::new(format!(
            "Unknown truncation unit '{}'",
            unit
        ))),
    }
}

/// DateTime.in-timezone
#[allow(clippy::too_many_arguments)]
fn datetime_in_timezone(
    year: i64,
    month: i64,
    day: i64,
    hour: i64,
    minute: i64,
    second: f64,
    old_tz: i64,
    new_tz: i64,
) -> Result<Value, RuntimeError> {
    // Use leap-second-aware instant conversion to correctly handle leap seconds
    // (e.g. 23:59:60 must survive a timezone round-trip unchanged).
    let (instant_int, instant_frac) =
        temporal::datetime_to_instant_parts(year, month, day, hour, minute, second, old_tz);
    let (ny, nm, nd, nh, nmi, ns) =
        temporal::instant_to_datetime_leap_aware_parts(instant_int, instant_frac, new_tz);
    Ok(temporal::make_datetime(ny, nm, nd, nh, nmi, ns, new_tz))
}
