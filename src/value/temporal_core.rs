//! The pure core of `Date`/`DateTime`/`Instant`: civil-date arithmetic,
//! ISO 8601 rendering and the leap-second table. `Value`'s `Str`/`eqv` read
//! these, so they live below the builtins (#10779); the builtins re-export
//! them from `methods_0arg::temporal`.

use crate::value::{AttrMap, Value, ValueView};

/// Convert epoch days back to (year, month, day).
pub(crate) fn epoch_days_to_civil(days: i64) -> (i64, i64, i64) {
    let z = days + 719_468;
    let era = if z >= 0 { z } else { z - 146_096 } / 146_097;
    let doe = z - era * 146_097;
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146_096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = mp + if mp < 10 { 3 } else { -9 };
    let year = y + i64::from(m <= 2);
    (year, m, d)
}

/// Format a Date as YYYY-MM-DD, with ISO 8601 sign prefix for negative/large years.
pub(crate) fn format_date(year: i64, month: i64, day: i64) -> String {
    if year < 0 {
        format!("-{:04}-{:02}-{:02}", -year, month, day)
    } else if year > 9999 {
        format!("+{:04}-{:02}-{:02}", year, month, day)
    } else {
        format!("{:04}-{:02}-{:02}", year, month, day)
    }
}

/// Format the year component the way `format_date` does (signed 4+ digits,
/// `+`-prefixed past 9999).
pub(crate) fn format_year_part(year: i64) -> String {
    if year < 0 {
        format!("-{:04}", -year)
    } else if year > 9999 {
        format!("+{:04}", year)
    } else {
        format!("{:04}", year)
    }
}

/// Format a DateTime as ISO 8601.
pub(crate) fn format_datetime(
    year: i64,
    month: i64,
    day: i64,
    hour: i64,
    minute: i64,
    second: f64,
    timezone: i64,
) -> String {
    let sec_str = format_second(second);
    let tz_str = format_timezone(timezone);
    let year_str = if year >= 10_000 {
        format!("+{}", year)
    } else {
        format!("{:04}", year)
    };
    format!(
        "{}-{:02}-{:02}T{:02}:{:02}:{}{}",
        year_str, month, day, hour, minute, sec_str, tz_str
    )
}

/// Format second value (with optional fractional part).
fn format_second(second: f64) -> String {
    if second == second.floor() {
        format!("{:02}", second as i64)
    } else {
        // DateTime.Str renders subsecond values with fixed microsecond precision.
        let mut int_part = second.floor() as i64;
        let mut micros = ((second - int_part as f64) * 1_000_000.0).round() as i64;
        if micros >= 1_000_000 {
            micros = 0;
            int_part += 1;
        }
        if micros < 0 {
            micros = 0;
        }
        format!("{:02}.{:06}", int_part, micros)
    }
}

/// Format timezone offset.
fn format_timezone(timezone: i64) -> String {
    if timezone == 0 {
        "Z".to_string()
    } else {
        let sign = if timezone >= 0 { '+' } else { '-' };
        let abs_tz = timezone.unsigned_abs();
        let hours = abs_tz / 3600;
        let minutes = (abs_tz % 3600) / 60;
        format!("{}{:02}:{:02}", sign, hours, minutes)
    }
}

/// Extract (year, month, day) from a Date instance's attributes.
pub(crate) fn date_attrs(attributes: &AttrMap) -> (i64, i64, i64) {
    // New format: year/month/day as separate attributes
    if let Some(ValueView::Int(y)) = attributes.get("year").map(Value::view) {
        let m = match attributes.get("month").map(Value::view) {
            Some(ValueView::Int(m)) => m,
            _ => 1,
        };
        let d = match attributes.get("day").map(Value::view) {
            Some(ValueView::Int(d)) => d,
            _ => 1,
        };
        return (y, m, d);
    }
    // Legacy format: days as epoch days
    if let Some(ValueView::Int(days)) = attributes.get("days").map(Value::view) {
        return epoch_days_to_civil(days);
    }
    (1970, 1, 1)
}

/// Extract DateTime components from attributes.
pub(crate) fn datetime_attrs(attributes: &AttrMap) -> (i64, i64, i64, i64, i64, f64, i64) {
    let year = match attributes.get("year").map(Value::view) {
        Some(ValueView::Int(y)) => y,
        _ => 1970,
    };
    let month = match attributes.get("month").map(Value::view) {
        Some(ValueView::Int(m)) => m,
        _ => 1,
    };
    let day = match attributes.get("day").map(Value::view) {
        Some(ValueView::Int(d)) => d,
        _ => 1,
    };
    let hour = match attributes.get("hour").map(Value::view) {
        Some(ValueView::Int(h)) => h,
        _ => 0,
    };
    let minute = match attributes.get("minute").map(Value::view) {
        Some(ValueView::Int(m)) => m,
        _ => 0,
    };
    let second = match attributes.get("second").map(Value::view) {
        Some(ValueView::Num(s)) => s,
        Some(ValueView::Int(s)) => s as f64,
        _ => 0.0,
    };
    let timezone = match attributes.get("timezone").map(Value::view) {
        Some(ValueView::Int(tz)) => tz,
        _ => 0,
    };
    (year, month, day, hour, minute, second, timezone)
}

/// Leap seconds table: (posix_timestamp_of_insertion, cumulative_leap_seconds).
/// Each entry marks a point where a leap second was inserted.
/// Raku's Instant uses TAI-like time = POSIX + offset (including leap seconds).
pub(crate) const LEAP_SECONDS: &[(i64, i64)] = &[
    (63_072_000, 10),    // 1972-01-01
    (78_796_800, 11),    // 1972-07-01
    (94_694_400, 12),    // 1973-01-01
    (126_230_400, 13),   // 1974-01-01
    (157_766_400, 14),   // 1975-01-01
    (189_302_400, 15),   // 1976-01-01
    (220_924_800, 16),   // 1977-01-01
    (252_460_800, 17),   // 1978-01-01
    (283_996_800, 18),   // 1979-01-01
    (315_532_800, 19),   // 1980-01-01
    (362_793_600, 20),   // 1981-07-01
    (394_329_600, 21),   // 1982-07-01
    (425_865_600, 22),   // 1983-07-01
    (489_024_000, 23),   // 1985-07-01
    (567_993_600, 24),   // 1988-01-01
    (631_152_000, 25),   // 1990-01-01
    (662_688_000, 26),   // 1991-01-01
    (709_948_800, 27),   // 1992-07-01
    (741_484_800, 28),   // 1993-07-01
    (773_020_800, 29),   // 1994-07-01
    (820_454_400, 30),   // 1996-01-01
    (867_715_200, 31),   // 1997-07-01
    (915_148_800, 32),   // 1999-01-01
    (1_136_073_600, 33), // 2006-01-01
    (1_230_768_000, 34), // 2009-01-01
    (1_341_100_800, 35), // 2012-07-01
    (1_435_708_800, 36), // 2015-07-01
    (1_483_228_800, 37), // 2017-01-01
];

/// TAI-UTC offset at a given POSIX timestamp.
/// Before 1972, the offset is the initial 10 seconds that TAI was ahead of UTC.
/// Each leap second after 1972-01-01 adds 1 to the cumulative offset.
pub(crate) fn leap_seconds_at(posix: f64) -> i64 {
    let posix_i = posix.floor() as i64;
    // The initial TAI-UTC offset is 10 seconds (set at 1972-01-01).
    // All cumulative values in the table include this base offset.
    let mut result = 10;
    for &(threshold, cumulative) in LEAP_SECONDS {
        if posix_i >= threshold {
            result = cumulative;
        } else {
            break;
        }
    }
    result
}

/// Convert POSIX timestamp to Raku Instant value (TAI-like, includes leap seconds).
pub(crate) fn posix_to_instant(posix: f64) -> f64 {
    posix + leap_seconds_at(posix) as f64
}

/// Convert Raku Instant value back to POSIX timestamp.
pub(crate) fn instant_to_posix(instant: f64) -> f64 {
    // Binary search: find posix such that posix + leap_seconds_at(posix) == instant
    // Simple approach: subtract leap seconds iteratively
    let mut posix = instant;
    for _ in 0..3 {
        let ls = leap_seconds_at(posix) as f64;
        posix = instant - ls;
    }
    posix
}
