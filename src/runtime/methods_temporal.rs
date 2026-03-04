#![allow(clippy::result_large_err)]

use crate::builtins::methods_0arg::temporal;
use crate::value::{RuntimeError, Value};

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
        } if class_name == "Date" => {
            let (year, month, day) = temporal::date_attrs(attributes);
            match method {
                "later" | "earlier" => Some(date_later_earlier(year, month, day, args, method)),
                "clone" => Some(date_clone(year, month, day, args)),
                "truncated-to" => Some(date_truncated_to(year, month, day, args)),
                "in-timezone" => {
                    // Date.in-timezone returns a DateTime
                    if let Some(arg) = args.first() {
                        let tz = arg.to_f64() as i64;
                        Some(Ok(temporal::make_datetime(year, month, day, 0, 0, 0.0, tz)))
                    } else {
                        Some(Ok(temporal::make_datetime(year, month, day, 0, 0, 0.0, 0)))
                    }
                }
                _ => None,
            }
        }
        Value::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "DateTime" => {
            let (year, month, day, hour, minute, second, timezone) =
                temporal::datetime_attrs(attributes);
            match method {
                "later" | "earlier" => Some(datetime_later_earlier(
                    year, month, day, hour, minute, second, timezone, args, method,
                )),
                "clone" => Some(datetime_clone(
                    year, month, day, hour, minute, second, timezone, args,
                )),
                "truncated-to" => Some(datetime_truncated_to(
                    year, month, day, hour, minute, second, timezone, args,
                )),
                "in-timezone" => {
                    if let Some(arg) = args.first() {
                        let new_tz = arg.to_f64() as i64;
                        Some(datetime_in_timezone(
                            year, month, day, hour, minute, second, timezone, new_tz,
                        ))
                    } else {
                        Some(Ok(temporal::make_datetime(
                            year, month, day, hour, minute, second, timezone,
                        )))
                    }
                }
                "posix" if args.len() == 1 => {
                    // .posix(True) includes leap seconds
                    let posix = temporal::datetime_to_posix(
                        year, month, day, hour, minute, second, timezone,
                    );
                    if posix == posix.floor() {
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

    for arg in args {
        if let Value::Pair(key, value) = arg {
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

    for arg in args {
        if let Value::Pair(key, value) = arg {
            let key_str = normalize_unit(key);
            match key_str.as_str() {
                "second" | "seconds" => {
                    let amount = value.to_f64() * sign_f;
                    s += amount;
                    normalize_datetime(&mut y, &mut m, &mut d, &mut h, &mut mi, &mut s);
                }
                "minute" | "minutes" => {
                    let amount = value.to_f64() as i64 * sign;
                    mi += amount;
                    normalize_datetime(&mut y, &mut m, &mut d, &mut h, &mut mi, &mut s);
                }
                "hour" | "hours" => {
                    let amount = value.to_f64() as i64 * sign;
                    h += amount;
                    normalize_datetime(&mut y, &mut m, &mut d, &mut h, &mut mi, &mut s);
                }
                "day" | "days" => {
                    let amount = value.to_f64() as i64 * sign;
                    let days = temporal::civil_to_epoch_days(y, m, d) + amount;
                    let (ny, nm, nd) = temporal::epoch_days_to_civil(days);
                    y = ny;
                    m = nm;
                    d = nd;
                }
                "week" | "weeks" => {
                    let amount = value.to_f64() as i64 * sign;
                    let days = temporal::civil_to_epoch_days(y, m, d) + amount * 7;
                    let (ny, nm, nd) = temporal::epoch_days_to_civil(days);
                    y = ny;
                    m = nm;
                    d = nd;
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
                }
                "year" | "years" => {
                    let amount = value.to_f64() as i64 * sign;
                    y += amount;
                    let max_d = temporal::days_in_month(y, m);
                    if d > max_d {
                        d = max_d;
                    }
                }
                _ => {
                    return Err(RuntimeError::new(format!(
                        "Unknown unit '{}' for DateTime.{}",
                        key, method
                    )));
                }
            }
        }
    }
    Ok(temporal::make_datetime(y, m, d, h, mi, s, timezone))
}

/// Normalize datetime components after arithmetic.
fn normalize_datetime(
    year: &mut i64,
    month: &mut i64,
    day: &mut i64,
    hour: &mut i64,
    minute: &mut i64,
    second: &mut f64,
) {
    // Normalize seconds -> minutes
    if *second < 0.0 || *second >= 60.0 {
        let extra_min = (*second / 60.0).floor() as i64;
        *minute += extra_min;
        *second -= extra_min as f64 * 60.0;
        if *second < 0.0 {
            *minute -= 1;
            *second += 60.0;
        }
    }
    // Normalize minutes -> hours
    if *minute < 0 || *minute >= 60 {
        let extra_h = (*minute).div_euclid(60);
        *hour += extra_h;
        *minute = (*minute).rem_euclid(60);
    }
    // Normalize hours -> days
    if *hour < 0 || *hour >= 24 {
        let extra_d = (*hour).div_euclid(24);
        let days = temporal::civil_to_epoch_days(*year, *month, *day) + extra_d;
        let (ny, nm, nd) = temporal::epoch_days_to_civil(days);
        *year = ny;
        *month = nm;
        *day = nd;
        *hour = (*hour).rem_euclid(24);
    }
}

/// Date.clone with optional overrides.
fn date_clone(
    mut year: i64,
    mut month: i64,
    mut day: i64,
    args: &[Value],
) -> Result<Value, RuntimeError> {
    for arg in args {
        if let Value::Pair(key, value) = arg {
            match key.as_str() {
                "year" => year = value.to_f64() as i64,
                "month" => month = value.to_f64() as i64,
                "day" => day = value.to_f64() as i64,
                "formatter" => {} // TODO: support formatter
                _ => {}
            }
        }
    }
    temporal::validate_date(year, month, day)?;
    Ok(temporal::make_date(year, month, day))
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
    temporal::validate_date(year, month, day)?;
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
