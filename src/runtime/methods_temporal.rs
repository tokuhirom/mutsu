#![allow(clippy::result_large_err)]

use crate::builtins::methods_0arg::temporal;
use crate::value::{RuntimeError, Value};
use std::sync::Arc;

fn has_date_attrs(attributes: &Arc<crate::value::InstanceAttrs>) -> bool {
    attributes.contains_key("year")
        && attributes.contains_key("month")
        && attributes.contains_key("day")
}

fn has_datetime_attrs(attributes: &Arc<crate::value::InstanceAttrs>) -> bool {
    has_date_attrs(attributes)
        && attributes.contains_key("hour")
        && attributes.contains_key("minute")
        && attributes.contains_key("second")
        && attributes.contains_key("timezone")
}

fn rebless_datetime_result(
    result: Value,
    target_class_name: crate::symbol::Symbol,
    original_attrs: &Arc<crate::value::InstanceAttrs>,
) -> Value {
    if target_class_name == "DateTime" {
        return result;
    }
    let Value::Instance {
        class_name,
        attributes,
        id,
        ..
    } = &result
    else {
        return result;
    };
    if class_name != "DateTime" {
        return result;
    }
    let mut merged = (**original_attrs).clone();
    for key in [
        "year", "month", "day", "hour", "minute", "second", "timezone",
    ] {
        if let Some(value) = attributes.get(key) {
            merged.insert(key.to_string(), value.clone());
        }
    }
    Value::Instance {
        class_name: target_class_name,
        attributes: Arc::new(crate::value::InstanceAttrs::new(
            target_class_name,
            merged,
            *id,
            true,
        )),
        id: *id,
    }
}

fn rebless_date_result(
    result: Value,
    target_class_name: crate::symbol::Symbol,
    original_attrs: &Arc<crate::value::InstanceAttrs>,
) -> Value {
    if target_class_name == "Date" {
        return result;
    }
    let Value::Instance {
        class_name,
        attributes,
        id,
        ..
    } = &result
    else {
        return result;
    };
    if class_name != "Date" {
        return result;
    }
    let mut merged = (**original_attrs).clone();
    for key in ["year", "month", "day"] {
        if let Some(value) = attributes.get(key) {
            merged.insert(key.to_string(), value.clone());
        }
    }
    Value::Instance {
        class_name: target_class_name,
        attributes: Arc::new(crate::value::InstanceAttrs::new(
            target_class_name,
            merged,
            *id,
            true,
        )),
        id: *id,
    }
}

/// Dispatch temporal n-arg methods for Date/DateTime instances.
/// Returns Some(result) if handled, None if not a temporal method.
pub(super) fn dispatch_temporal_method(
    target: &Value,
    method: &str,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    match target {
        Value::Instance {
            class_name,
            attributes,
            ..
        } if has_date_attrs(attributes) && !has_datetime_attrs(attributes) => {
            let (year, month, day) = temporal::date_attrs(attributes);
            match method {
                "later" | "earlier" => Some(
                    date_later_earlier(year, month, day, args, method)
                        .map(|v| rebless_date_result(v, *class_name, attributes)),
                ),
                "clone" => {
                    let existing_formatter = attributes.get("formatter").cloned();
                    Some(
                        date_clone(year, month, day, existing_formatter, args)
                            .map(|v| rebless_date_result(v, *class_name, attributes)),
                    )
                }
                "truncated-to" => Some(
                    date_truncated_to(year, month, day, args)
                        .map(|v| rebless_date_result(v, *class_name, attributes)),
                ),
                "in-timezone" => {
                    // Date.in-timezone returns a DateTime
                    if let Some(arg) = args.first() {
                        let tz = arg.to_f64() as i64;
                        Some(Ok(temporal::make_datetime(year, month, day, 0, 0, 0.0, tz)))
                    } else {
                        Some(Ok(temporal::make_datetime(year, month, day, 0, 0, 0.0, 0)))
                    }
                }
                "first-date-in-month" if args.is_empty() => {
                    let formatter = attributes.get("formatter").cloned();
                    Some(Ok(temporal::make_date_with_formatter(
                        year, month, 1, formatter,
                    )))
                }
                "last-date-in-month" if args.is_empty() => {
                    let last_day = temporal::days_in_month(year, month);
                    let formatter = attributes.get("formatter").cloned();
                    Some(Ok(temporal::make_date_with_formatter(
                        year, month, last_day, formatter,
                    )))
                }
                _ => None,
            }
        }
        Value::Instance {
            class_name,
            attributes,
            ..
        } if has_datetime_attrs(attributes) => {
            let (year, month, day, hour, minute, second, timezone) =
                temporal::datetime_attrs(attributes);
            match method {
                "later" | "earlier" => Some(
                    datetime_later_earlier(
                        year, month, day, hour, minute, second, timezone, args, method,
                    )
                    .map(|v| rebless_datetime_result(v, *class_name, attributes)),
                ),
                "clone" => Some(
                    datetime_clone(year, month, day, hour, minute, second, timezone, args)
                        .map(|v| rebless_datetime_result(v, *class_name, attributes)),
                ),
                "truncated-to" => Some(
                    datetime_truncated_to(year, month, day, hour, minute, second, timezone, args)
                        .map(|v| rebless_datetime_result(v, *class_name, attributes)),
                ),
                "in-timezone" => {
                    if let Some(arg) = args.first() {
                        let new_tz = arg.to_f64() as i64;
                        Some(
                            datetime_in_timezone(
                                year, month, day, hour, minute, second, timezone, new_tz,
                            )
                            .map(|v| rebless_datetime_result(v, *class_name, attributes)),
                        )
                    } else {
                        Some(
                            Ok(temporal::make_datetime(
                                year, month, day, hour, minute, second, timezone,
                            ))
                            .map(|v| rebless_datetime_result(v, *class_name, attributes)),
                        )
                    }
                }
                "posix" if args.len() == 1 => {
                    // .posix(True) / .posix(:real) keeps fractional seconds.
                    // .posix(False) / .posix(:!real) truncates to whole seconds.
                    let posix = temporal::datetime_to_posix(
                        year, month, day, hour, minute, second, timezone,
                    );
                    let real = match &args[0] {
                        Value::Pair(key, value) if key == "real" => value.truthy(),
                        other => other.truthy(),
                    };
                    if !real {
                        Some(Ok(Value::Int(posix.floor() as i64)))
                    } else if posix == posix.floor() {
                        Some(Ok(Value::Int(posix as i64)))
                    } else {
                        Some(Ok(Value::Num(posix)))
                    }
                }
                _ => None,
            }
        }
        _ => None,
    }
}

/// Date.later / Date.earlier
fn date_later_earlier(
    year: i64,
    month: i64,
    day: i64,
    args: &[Value],
    method: &str,
) -> Result<Value, RuntimeError> {
    let sign: i64 = if method == "later" { 1 } else { -1 };
    let mut y = year;
    let mut m = month;
    let mut d = day;

    let mut apply_pair = |key: &str, value: &Value| -> Result<(), RuntimeError> {
        let amount = value.to_f64() as i64 * sign;
        let key_str = normalize_unit(key);
        match key_str.as_str() {
            "day" | "days" => {
                let days = temporal::civil_to_epoch_days(y, m, d) + amount;
                let (ny, nm, nd) = temporal::epoch_days_to_civil(days);
                y = ny;
                m = nm;
                d = nd;
            }
            "week" | "weeks" => {
                let days = temporal::civil_to_epoch_days(y, m, d) + amount * 7;
                let (ny, nm, nd) = temporal::epoch_days_to_civil(days);
                y = ny;
                m = nm;
                d = nd;
            }
            "month" | "months" => {
                let total_months = (y * 12 + (m - 1)) + amount;
                y = total_months.div_euclid(12);
                m = total_months.rem_euclid(12) + 1;
                let max_d = temporal::days_in_month(y, m);
                if d > max_d {
                    d = max_d;
                }
            }
            "year" | "years" => {
                y += amount;
                let max_d = temporal::days_in_month(y, m);
                if d > max_d {
                    d = max_d;
                }
            }
            _ => {
                return Err(RuntimeError::new(format!(
                    "Unknown unit '{}' for Date.{}",
                    key, method
                )));
            }
        }
        Ok(())
    };

    for arg in args {
        if let Value::Pair(key, value) = arg {
            apply_pair(key, value)?;
            continue;
        }
        if let Some(items) = arg.as_list_items() {
            for item in items.iter() {
                if let Value::Pair(key, value) = item {
                    apply_pair(key, value)?;
                }
            }
        }
    }
    Ok(temporal::make_date(y, m, d))
}

/// DateTime.later / DateTime.earlier
#[allow(clippy::too_many_arguments)]
fn datetime_later_earlier(
    year: i64,
    month: i64,
    day: i64,
    hour: i64,
    minute: i64,
    second: f64,
    timezone: i64,
    args: &[Value],
    method: &str,
) -> Result<Value, RuntimeError> {
    let sign: i64 = if method == "later" { 1 } else { -1 };
    let sign_f: f64 = sign as f64;
    let mut y = year;
    let mut m = month;
    let mut d = day;
    let mut h = hour;
    let mut mi = minute;
    let mut s = second;

    let clip_non_leap_second = |y: i64, m: i64, d: i64, h: i64, mi: i64, s: &mut f64, tz: i64| {
        if *s < 60.0 {
            return;
        }
        if temporal::validate_datetime(y, m, d, h, mi, *s, tz).is_ok() {
            return;
        }
        let frac = (*s - 60.0).clamp(0.0, 0.999_999);
        *s = 59.0 + frac;
    };

    let mut apply_pair = |key: &str, value: &Value| -> Result<(), RuntimeError> {
        let key_str = normalize_unit(key);
        match key_str.as_str() {
            "second" | "seconds" => {
                let amount = value.to_f64() * sign_f;
                let instant = temporal::datetime_to_instant_leap_aware(y, m, d, h, mi, s, timezone);
                let (ny, nm, nd, nh, nmi, ns) =
                    temporal::instant_to_datetime_leap_aware(instant + amount, timezone);
                y = ny;
                m = nm;
                d = nd;
                h = nh;
                mi = nmi;
                s = ns;
            }
            "minute" | "minutes" => {
                let amount = value.to_f64() * 60.0 * sign_f;
                let instant = temporal::datetime_to_instant_leap_aware(y, m, d, h, mi, s, timezone);
                let (ny, nm, nd, nh, nmi, ns) =
                    temporal::instant_to_datetime_leap_aware(instant + amount, timezone);
                y = ny;
                m = nm;
                d = nd;
                h = nh;
                mi = nmi;
                s = ns;
            }
            "hour" | "hours" => {
                let amount = value.to_f64() * 3_600.0 * sign_f;
                let instant = temporal::datetime_to_instant_leap_aware(y, m, d, h, mi, s, timezone);
                let (ny, nm, nd, nh, nmi, ns) =
                    temporal::instant_to_datetime_leap_aware(instant + amount, timezone);
                y = ny;
                m = nm;
                d = nd;
                h = nh;
                mi = nmi;
                s = ns;
            }
            "day" | "days" => {
                let amount = value.to_f64() as i64 * sign;
                let days = temporal::civil_to_epoch_days(y, m, d) + amount;
                let (ny, nm, nd) = temporal::epoch_days_to_civil(days);
                y = ny;
                m = nm;
                d = nd;
                clip_non_leap_second(y, m, d, h, mi, &mut s, timezone);
            }
            "week" | "weeks" => {
                let amount = value.to_f64() as i64 * sign;
                let days = temporal::civil_to_epoch_days(y, m, d) + amount * 7;
                let (ny, nm, nd) = temporal::epoch_days_to_civil(days);
                y = ny;
                m = nm;
                d = nd;
                clip_non_leap_second(y, m, d, h, mi, &mut s, timezone);
            }
            "month" | "months" => {
                let amount = value.to_f64() as i64 * sign;
                let total_months = (y * 12 + (m - 1)) + amount;
                y = total_months.div_euclid(12);
                m = total_months.rem_euclid(12) + 1;
                let max_d = temporal::days_in_month(y, m);
                if d > max_d {
                    d = max_d;
                }
                clip_non_leap_second(y, m, d, h, mi, &mut s, timezone);
            }
            "year" | "years" => {
                let amount = value.to_f64() as i64 * sign;
                y += amount;
                let max_d = temporal::days_in_month(y, m);
                if d > max_d {
                    d = max_d;
                }
                clip_non_leap_second(y, m, d, h, mi, &mut s, timezone);
            }
            _ => {
                return Err(RuntimeError::new(format!(
                    "Unknown unit '{}' for DateTime.{}",
                    key, method
                )));
            }
        }
        Ok(())
    };

    for arg in args {
        if let Value::Pair(key, value) = arg {
            apply_pair(key, value)?;
            continue;
        }
        if let Some(items) = arg.as_list_items() {
            for item in items.iter() {
                if let Value::Pair(key, value) = item {
                    apply_pair(key, value)?;
                }
            }
        }
    }
    s = (s * 1_000_000.0).round() / 1_000_000.0;
    Ok(temporal::make_datetime(y, m, d, h, mi, s, timezone))
}

/// Date.clone with optional overrides.
fn date_clone(
    mut year: i64,
    mut month: i64,
    mut day: i64,
    existing_formatter: Option<Value>,
    args: &[Value],
) -> Result<Value, RuntimeError> {
    let mut formatter = existing_formatter;
    for arg in args {
        if let Value::Pair(key, value) = arg {
            match key.as_str() {
                "year" => year = value.to_f64() as i64,
                "month" => month = value.to_f64() as i64,
                "day" => day = value.to_f64() as i64,
                "formatter" => formatter = Some(*value.clone()),
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
fn datetime_clone(
    mut year: i64,
    mut month: i64,
    mut day: i64,
    mut hour: i64,
    mut minute: i64,
    mut second: f64,
    mut timezone: i64,
    args: &[Value],
) -> Result<Value, RuntimeError> {
    for arg in args {
        if let Value::Pair(key, value) = arg {
            match key.as_str() {
                "year" => year = value.to_f64() as i64,
                "month" => month = value.to_f64() as i64,
                "day" => day = value.to_f64() as i64,
                "hour" => hour = value.to_f64() as i64,
                "minute" => minute = value.to_f64() as i64,
                "second" => second = value.to_f64(),
                "timezone" => timezone = value.to_f64() as i64,
                "formatter" => {} // TODO: support formatter
                _ => {}
            }
        }
    }
    temporal::validate_datetime(year, month, day, hour, minute, second, timezone)?;
    Ok(temporal::make_datetime(
        year, month, day, hour, minute, second, timezone,
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
    // Convert to UTC epoch, then to new timezone
    let posix = temporal::datetime_to_posix(year, month, day, hour, minute, second, old_tz);
    // Now convert from UTC to new timezone
    let local_secs = posix + new_tz as f64;
    let total_i = local_secs.floor() as i64;
    let frac = local_secs - total_i as f64;
    let day_secs = total_i.rem_euclid(86400);
    let epoch_days = (total_i - day_secs) / 86400;
    let (ny, nm, nd) = temporal::epoch_days_to_civil(epoch_days);
    let nh = day_secs / 3600;
    let nmi = (day_secs % 3600) / 60;
    let ns = (day_secs % 60) as f64 + frac;
    Ok(temporal::make_datetime(ny, nm, nd, nh, nmi, ns, new_tz))
}

/// Normalize unit names (strip trailing 's', handle singular/plural).
fn normalize_unit(key: &str) -> String {
    key.to_lowercase()
}
