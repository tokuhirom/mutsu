#![allow(clippy::result_large_err)]

use super::temporal::*;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};
use std::collections::HashMap;

/// Convert an f64 to a Rat by using its decimal string representation.
/// This preserves the visible decimal digits (e.g. 10.987654321 → 10987654321/1000000000).
fn f64_to_decimal_rat(f: f64) -> Value {
    let s = format!("{}", f);
    if let Some(dot_pos) = s.find('.') {
        let decimals = s.len() - dot_pos - 1;
        let mut den = 1i64;
        for _ in 0..decimals {
            den = den.saturating_mul(10);
        }
        // Remove the dot and parse as integer numerator
        let num_str: String = s.chars().filter(|c| *c != '.').collect();
        if let Ok(num) = num_str.parse::<i64>() {
            // Simplify the fraction
            let g = gcd_i64(num.abs(), den);
            return Value::Rat(num / g, den / g);
        }
    }
    Value::Num(f)
}

fn gcd_i64(mut a: i64, mut b: i64) -> i64 {
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

/// Dispatch 0-arg methods for Date instances.
pub fn date_method_0arg(
    attributes: &HashMap<String, Value>,
    method: &str,
) -> Option<Result<Value, RuntimeError>> {
    let (year, month, day) = date_attrs(attributes);
    let days = civil_to_epoch_days(year, month, day);

    match method {
        "year" => Some(Ok(Value::Int(year))),
        "month" => Some(Ok(Value::Int(month))),
        "day" | "day-of-month" => Some(Ok(Value::Int(day))),
        "day-of-week" | "weekday" => Some(Ok(Value::Int(day_of_week(days)))),
        "day-of-year" => Some(Ok(Value::Int(day_of_year(year, month, day)))),
        "is-leap-year" => Some(Ok(Value::Bool(is_leap_year(year)))),
        "days-in-month" => Some(Ok(Value::Int(days_in_month(year, month)))),
        "daycount" => Some(Ok(Value::Int(daycount(year, month, day)))),
        "Str" | "gist" => {
            // If a formatter was applied and rendered, use that
            if let Some(Value::Str(rendered)) = attributes.get("__formatter_rendered") {
                return Some(Ok(Value::str(rendered.to_string())));
            }
            // If there's a formatter but no rendered output, fall through to runtime
            // so the formatter can be called
            if attributes.contains_key("formatter") {
                return None;
            }
            Some(Ok(Value::str(format_date(year, month, day))))
        }
        "Date" => Some(Ok(make_date(year, month, day))),
        "yyyy-mm-dd" => Some(Ok(Value::str(format_date(year, month, day)))),
        "succ" => {
            let new_days = days + 1;
            let (ny, nm, nd) = epoch_days_to_civil(new_days);
            let formatter = attributes.get("formatter").cloned();
            Some(Ok(make_date_with_formatter(ny, nm, nd, formatter)))
        }
        "pred" => {
            let new_days = days - 1;
            let (ny, nm, nd) = epoch_days_to_civil(new_days);
            let formatter = attributes.get("formatter").cloned();
            Some(Ok(make_date_with_formatter(ny, nm, nd, formatter)))
        }
        "week-year" => {
            let (wy, _) = iso_week(year, month, day);
            Some(Ok(Value::Int(wy)))
        }
        "week-number" => {
            let (_, wn) = iso_week(year, month, day);
            Some(Ok(Value::Int(wn)))
        }
        "week" => {
            let (wy, wn) = iso_week(year, month, day);
            Some(Ok(Value::array(vec![Value::Int(wy), Value::Int(wn)])))
        }
        "weekday-of-month" => Some(Ok(Value::Int((day - 1) / 7 + 1))),
        "posix" | "Numeric" | "Int" => {
            let epoch = civil_to_epoch_days(year, month, day) as f64 * 86400.0;
            Some(Ok(Value::Int(epoch as i64)))
        }
        "DateTime" => Some(Ok(make_datetime(year, month, day, 0, 0, 0.0, 0))),
        "Instant" => {
            let posix = civil_to_epoch_days(year, month, day) as f64 * 86400.0;
            let tai = posix_to_instant(posix);
            let mut attrs = HashMap::new();
            attrs.insert("value".to_string(), Value::Num(tai));
            Some(Ok(Value::make_instance(Symbol::intern("Instant"), attrs)))
        }
        "raku" | "perl" => Some(Ok(Value::str(format!(
            "Date.new(\"{}\")",
            format_date(year, month, day)
        )))),
        "WHICH" => {
            let which = format!("Date|{}", days);
            let mut attrs = HashMap::new();
            attrs.insert("WHICH".to_string(), Value::str(which));
            Some(Ok(Value::make_instance(
                Symbol::intern("ValueObjAt"),
                attrs,
            )))
        }
        _ => None,
    }
}

/// Dispatch 0-arg methods for DateTime instances.
pub fn datetime_method_0arg(
    attributes: &HashMap<String, Value>,
    method: &str,
) -> Option<Result<Value, RuntimeError>> {
    let (year, month, day, hour, minute, second, timezone) = datetime_attrs(attributes);
    let days = civil_to_epoch_days(year, month, day);

    match method {
        "year" => Some(Ok(Value::Int(year))),
        "month" => Some(Ok(Value::Int(month))),
        "day" | "day-of-month" => Some(Ok(Value::Int(day))),
        "hour" => Some(Ok(Value::Int(hour))),
        "minute" => Some(Ok(Value::Int(minute))),
        "second" => {
            if second == second.floor() {
                Some(Ok(Value::Int(second as i64)))
            } else {
                // Raku's .second returns a Rat for fractional seconds.
                // Convert f64 via its decimal representation to preserve precision.
                Some(Ok(f64_to_decimal_rat(second)))
            }
        }
        "timezone" => Some(Ok(Value::Int(timezone))),
        "offset" => Some(Ok(Value::Int(timezone))),
        "offset-in-hours" => {
            let mut num = timezone;
            let mut den = 3600i64;
            let mut a = num.abs();
            let mut b = den;
            while b != 0 {
                let t = b;
                b = a % b;
                a = t;
            }
            let gcd = a.max(1);
            num /= gcd;
            den /= gcd;
            Some(Ok(Value::Rat(num, den)))
        }
        "day-of-week" | "weekday" => Some(Ok(Value::Int(day_of_week(days)))),
        "day-of-year" => Some(Ok(Value::Int(day_of_year(year, month, day)))),
        "is-leap-year" => Some(Ok(Value::Bool(is_leap_year(year)))),
        "days-in-month" => Some(Ok(Value::Int(days_in_month(year, month)))),
        "daycount" => Some(Ok(Value::Int(daycount(year, month, day)))),
        "whole-second" => Some(Ok(Value::Int(second.floor() as i64))),
        "hh-mm-ss" => Some(Ok(Value::str(format!(
            "{:02}:{:02}:{:02}",
            hour,
            minute,
            second.floor() as i64
        )))),
        "Str" | "gist" => Some(Ok(Value::str(format_datetime(
            year, month, day, hour, minute, second, timezone,
        )))),
        "Date" => Some(Ok(make_date(year, month, day))),
        "posix" => {
            let posix = datetime_to_posix(year, month, day, hour, minute, second, timezone);
            // Raku's .posix always returns Int (truncating fractional seconds)
            Some(Ok(Value::Int(posix.floor() as i64)))
        }
        "Numeric" => {
            let posix = datetime_to_posix(year, month, day, hour, minute, second, timezone);
            Some(Ok(Value::Num(posix)))
        }
        "Int" => {
            let posix = datetime_to_posix(year, month, day, hour, minute, second, timezone);
            Some(Ok(Value::Int(posix as i64)))
        }
        "utc" => {
            // Keep the same instant, convert representation to UTC timezone.
            let posix = datetime_to_posix(year, month, day, hour, minute, second, timezone);
            let total_i = posix.floor() as i64;
            let frac = posix - total_i as f64;
            let day_secs = total_i.rem_euclid(86400);
            let epoch_days = (total_i - day_secs) / 86400;
            let (uy, um, ud) = epoch_days_to_civil(epoch_days);
            let uh = day_secs / 3600;
            let umi = (day_secs % 3600) / 60;
            let us = (day_secs % 60) as f64 + frac;
            Some(Ok(make_datetime(uy, um, ud, uh, umi, us, 0)))
        }
        "week-year" => {
            let (wy, _) = iso_week(year, month, day);
            Some(Ok(Value::Int(wy)))
        }
        "week-number" => {
            let (_, wn) = iso_week(year, month, day);
            Some(Ok(Value::Int(wn)))
        }
        "week" => {
            let (wy, wn) = iso_week(year, month, day);
            Some(Ok(Value::array(vec![Value::Int(wy), Value::Int(wn)])))
        }
        "weekday-of-month" => Some(Ok(Value::Int((day - 1) / 7 + 1))),
        "julian-date" => Some(Ok(Value::Num(julian_date(
            year, month, day, hour, minute, second,
        )))),
        "modified-julian-date" => Some(Ok(Value::Num(modified_julian_date(
            year, month, day, hour, minute, second,
        )))),
        "day-fraction" => {
            let (n, d) = day_fraction_rational(year, month, day, hour, minute, second);
            Some(Ok(crate::value::make_rat(n, d)))
        }
        "Instant" => {
            let posix = datetime_to_posix(year, month, day, hour, minute, second, timezone);
            let tai = posix_to_instant(posix);
            let mut attrs = HashMap::new();
            attrs.insert("value".to_string(), Value::Num(tai));
            Some(Ok(Value::make_instance(Symbol::intern("Instant"), attrs)))
        }
        "DateTime" => Some(Ok(make_datetime(
            year, month, day, hour, minute, second, timezone,
        ))),
        "raku" | "perl" => {
            let s = format_datetime(year, month, day, hour, minute, second, timezone);
            Some(Ok(Value::str(format!("DateTime.new(\"{}\")", s))))
        }
        "WHICH" => {
            let posix = datetime_to_posix(year, month, day, hour, minute, second, timezone);
            let which = format!("DateTime|{}", posix);
            let mut attrs = HashMap::new();
            attrs.insert("WHICH".to_string(), Value::str(which));
            Some(Ok(Value::make_instance(Symbol::intern("ObjAt"), attrs)))
        }
        _ => None,
    }
}
