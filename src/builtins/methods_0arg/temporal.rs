#![allow(clippy::result_large_err)]

use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};
use std::collections::HashMap;

/// Check if a year is a leap year.
pub fn is_leap_year(year: i64) -> bool {
    (year % 4 == 0 && year % 100 != 0) || year % 400 == 0
}

/// Days in a given month (1-based) for a given year.
pub fn days_in_month(year: i64, month: i64) -> i64 {
    match month {
        1 | 3 | 5 | 7 | 8 | 10 | 12 => 31,
        4 | 6 | 9 | 11 => 30,
        2 => {
            if is_leap_year(year) {
                29
            } else {
                28
            }
        }
        _ => 0,
    }
}

/// Convert civil date to epoch days (days since 1970-01-01).
pub fn civil_to_epoch_days(year: i64, month: i64, day: i64) -> i64 {
    let y = year - i64::from(month <= 2);
    let era = if y >= 0 { y } else { y - 399 } / 400;
    let yoe = y - era * 400;
    let mp = month + if month > 2 { -3 } else { 9 };
    let doy = (153 * mp + 2) / 5 + day - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    era * 146_097 + doe - 719_468
}

/// Convert epoch days back to (year, month, day).
pub fn epoch_days_to_civil(days: i64) -> (i64, i64, i64) {
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

/// Validate a civil date. Returns Ok(()) or an error.
pub fn validate_date(year: i64, month: i64, day: i64) -> Result<(), RuntimeError> {
    if !(1..=12).contains(&month) {
        return Err(RuntimeError::new(format!(
            "X::OutOfRange: Month out of range. Is: {}, should be in 1..12",
            month
        )));
    }
    let max_day = days_in_month(year, month);
    if day < 1 || day > max_day {
        return Err(RuntimeError::new(format!(
            "X::OutOfRange: Day out of range. Is: {}, should be in 1..{}",
            day, max_day
        )));
    }
    Ok(())
}

/// Day of week (1=Monday .. 7=Sunday) from epoch days.
pub fn day_of_week(days: i64) -> i64 {
    (days + 3).rem_euclid(7) + 1
}

/// Day of year (1-based).
pub fn day_of_year(year: i64, month: i64, day: i64) -> i64 {
    let mut doy = 0;
    for m in 1..month {
        doy += days_in_month(year, m);
    }
    doy + day
}

/// ISO week number and week year.
/// Returns (week_year, week_number).
pub fn iso_week(year: i64, month: i64, day: i64) -> (i64, i64) {
    let days = civil_to_epoch_days(year, month, day);
    let dow = day_of_week(days); // 1=Mon..7=Sun
    // Thursday of this week
    let thu = days + (4 - dow);
    let (thu_year, _, _) = epoch_days_to_civil(thu);
    // Jan 4 of that year is always in week 1
    let jan4 = civil_to_epoch_days(thu_year, 1, 4);
    let jan4_dow = day_of_week(jan4);
    let week1_mon = jan4 - (jan4_dow - 1);
    let week_number = (thu - week1_mon) / 7 + 1;
    (thu_year, week_number)
}

/// Create a Date instance with year/month/day attributes.
pub fn make_date(year: i64, month: i64, day: i64) -> Value {
    let mut attrs = HashMap::new();
    attrs.insert("year".to_string(), Value::Int(year));
    attrs.insert("month".to_string(), Value::Int(month));
    attrs.insert("day".to_string(), Value::Int(day));
    // Also store epoch days for backward compat and arithmetic
    attrs.insert(
        "days".to_string(),
        Value::Int(civil_to_epoch_days(year, month, day)),
    );
    Value::make_instance(Symbol::intern("Date"), attrs)
}

/// Create a DateTime instance.
pub fn make_datetime(
    year: i64,
    month: i64,
    day: i64,
    hour: i64,
    minute: i64,
    second: f64,
    timezone: i64,
) -> Value {
    let mut attrs = HashMap::new();
    attrs.insert("year".to_string(), Value::Int(year));
    attrs.insert("month".to_string(), Value::Int(month));
    attrs.insert("day".to_string(), Value::Int(day));
    attrs.insert("hour".to_string(), Value::Int(hour));
    attrs.insert("minute".to_string(), Value::Int(minute));
    attrs.insert("second".to_string(), Value::Num(second));
    attrs.insert("timezone".to_string(), Value::Int(timezone));
    // Store epoch for backward compat
    let epoch_days = civil_to_epoch_days(year, month, day);
    let epoch_secs =
        epoch_days as f64 * 86400.0 + hour as f64 * 3600.0 + minute as f64 * 60.0 + second
            - timezone as f64;
    attrs.insert("epoch".to_string(), Value::Num(epoch_secs));
    Value::make_instance(Symbol::intern("DateTime"), attrs)
}

/// Format a Date as YYYY-MM-DD, with ISO 8601 sign prefix for negative/large years.
pub fn format_date(year: i64, month: i64, day: i64) -> String {
    if year < 0 {
        format!("-{:04}-{:02}-{:02}", -year, month, day)
    } else if year > 9999 {
        format!("+{:04}-{:02}-{:02}", year, month, day)
    } else {
        format!("{:04}-{:02}-{:02}", year, month, day)
    }
}

/// Format a DateTime as ISO 8601.
pub fn format_datetime(
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
    format!(
        "{:04}-{:02}-{:02}T{:02}:{:02}:{}{}",
        year, month, day, hour, minute, sec_str, tz_str
    )
}

/// Format second value (with optional fractional part).
fn format_second(second: f64) -> String {
    if second == second.floor() {
        format!("{:02}", second as i64)
    } else {
        // Format with up to 6 decimal places, removing trailing zeros
        let s = format!("{:09.6}", second);
        let s = s.trim_end_matches('0');
        s.to_string()
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

/// Parse an ISO 8601 date string (YYYY-MM-DD, -YYYY-MM-DD, +YYYY-MM-DD).
pub fn parse_date_string(s: &str) -> Result<(i64, i64, i64), RuntimeError> {
    let make_err = || {
        RuntimeError::new(format!(
            "X::Temporal::InvalidFormat: Invalid Date string '{}'; use yyyy-mm-dd",
            s
        ))
    };

    // Handle leading sign
    let (negative, rest) = if let Some(stripped) = s.strip_prefix('-') {
        // Distinguish between -YYYY-MM-DD and just malformed
        if stripped.contains('-') {
            (true, stripped)
        } else {
            return Err(make_err());
        }
    } else if let Some(stripped) = s.strip_prefix('+') {
        (false, stripped)
    } else {
        (false, s)
    };

    let parts: Vec<&str> = rest.split('-').collect();
    if parts.len() != 3 {
        return Err(make_err());
    }
    let year_abs = parts[0].parse::<i64>().map_err(|_| make_err())?;
    let year = if negative { -year_abs } else { year_abs };
    let month = parts[1].parse::<i64>().map_err(|_| make_err())?;
    let day = parts[2].parse::<i64>().map_err(|_| make_err())?;
    validate_date(year, month, day)?;
    Ok((year, month, day))
}

/// Parse an ISO 8601 DateTime string.
/// Supports: YYYY-MM-DDThh:mm:ssZ, YYYY-MM-DDThh:mm:ss+HH:MM, etc.
/// Also supports leading + sign (e.g., +2000-01-01T12:00:00Z).
type DateTimeParts = (i64, i64, i64, i64, i64, f64, i64);

pub fn parse_datetime_string(s: &str) -> Result<DateTimeParts, RuntimeError> {
    let s = s.strip_prefix('+').unwrap_or(s);

    let make_err = || {
        RuntimeError::new(format!(
            "X::Temporal::InvalidFormat: Invalid DateTime string '{}'; \
             use yyyy-mm-ddThh:mm:ss±hhmm or bg an abbreviation",
            s
        ))
    };

    // Split on T
    let (date_part, time_tz) = s.split_once('T').ok_or_else(make_err)?;

    let (year, month, day) = parse_date_string(date_part)?;

    // Parse time and timezone
    // Time can end with Z, +HH:MM, -HH:MM, +HHMM, -HHMM, or nothing (implies UTC)
    let (time_part, timezone) = if let Some(pos) = time_tz.rfind('Z') {
        (&time_tz[..pos], 0i64)
    } else if let Some(pos) = time_tz.rfind('+') {
        if pos > 0 {
            let tz = parse_tz_offset(&time_tz[pos..])?;
            (&time_tz[..pos], tz)
        } else {
            (time_tz, 0)
        }
    } else if let Some(pos) = time_tz[1..].rfind('-') {
        let pos = pos + 1; // adjust for the skipped first char
        let tz = parse_tz_offset(&time_tz[pos..])?;
        (&time_tz[..pos], tz)
    } else {
        (time_tz, 0)
    };

    let time_parts: Vec<&str> = time_part.split(':').collect();
    if time_parts.len() < 2 {
        return Err(make_err());
    }
    let hour = time_parts[0].parse::<i64>().map_err(|_| make_err())?;
    let minute = time_parts[1].parse::<i64>().map_err(|_| make_err())?;
    let second = if time_parts.len() > 2 {
        time_parts[2].parse::<f64>().map_err(|_| make_err())?
    } else {
        0.0
    };

    Ok((year, month, day, hour, minute, second, timezone))
}

/// Parse timezone offset string like "+05:30", "-05:00", "+0530", "-0500".
fn parse_tz_offset(s: &str) -> Result<i64, RuntimeError> {
    let make_err = || RuntimeError::new(format!("Invalid timezone offset: '{}'", s));
    let sign = match s.chars().next() {
        Some('+') => 1i64,
        Some('-') => -1i64,
        _ => return Err(make_err()),
    };
    let rest = &s[1..];
    let (hours, minutes) = if rest.contains(':') {
        let parts: Vec<&str> = rest.split(':').collect();
        if parts.len() != 2 {
            return Err(make_err());
        }
        (
            parts[0].parse::<i64>().map_err(|_| make_err())?,
            parts[1].parse::<i64>().map_err(|_| make_err())?,
        )
    } else if rest.len() == 4 {
        (
            rest[..2].parse::<i64>().map_err(|_| make_err())?,
            rest[2..].parse::<i64>().map_err(|_| make_err())?,
        )
    } else {
        return Err(make_err());
    };
    Ok(sign * (hours * 3600 + minutes * 60))
}

/// Extract (year, month, day) from a Date instance's attributes.
pub fn date_attrs(attributes: &HashMap<String, Value>) -> (i64, i64, i64) {
    // New format: year/month/day as separate attributes
    if let Some(Value::Int(y)) = attributes.get("year") {
        let m = match attributes.get("month") {
            Some(Value::Int(m)) => *m,
            _ => 1,
        };
        let d = match attributes.get("day") {
            Some(Value::Int(d)) => *d,
            _ => 1,
        };
        return (*y, m, d);
    }
    // Legacy format: days as epoch days
    if let Some(Value::Int(days)) = attributes.get("days") {
        return epoch_days_to_civil(*days);
    }
    (1970, 1, 1)
}

/// Extract DateTime components from attributes.
pub fn datetime_attrs(attributes: &HashMap<String, Value>) -> (i64, i64, i64, i64, i64, f64, i64) {
    let year = match attributes.get("year") {
        Some(Value::Int(y)) => *y,
        _ => 1970,
    };
    let month = match attributes.get("month") {
        Some(Value::Int(m)) => *m,
        _ => 1,
    };
    let day = match attributes.get("day") {
        Some(Value::Int(d)) => *d,
        _ => 1,
    };
    let hour = match attributes.get("hour") {
        Some(Value::Int(h)) => *h,
        _ => 0,
    };
    let minute = match attributes.get("minute") {
        Some(Value::Int(m)) => *m,
        _ => 0,
    };
    let second = match attributes.get("second") {
        Some(Value::Num(s)) => *s,
        Some(Value::Int(s)) => *s as f64,
        _ => 0.0,
    };
    let timezone = match attributes.get("timezone") {
        Some(Value::Int(tz)) => *tz,
        _ => 0,
    };
    (year, month, day, hour, minute, second, timezone)
}

/// Compute POSIX timestamp from DateTime components.
pub fn datetime_to_posix(
    year: i64,
    month: i64,
    day: i64,
    hour: i64,
    minute: i64,
    second: f64,
    timezone: i64,
) -> f64 {
    let epoch_days = civil_to_epoch_days(year, month, day);
    epoch_days as f64 * 86400.0 + hour as f64 * 3600.0 + minute as f64 * 60.0 + second
        - timezone as f64
}

/// Compute daycount from year/month/day.
pub fn daycount(year: i64, month: i64, day: i64) -> i64 {
    // Raku's daycount is the Modified Julian Day Number
    // MJD = JD - 2400000.5
    // For a Date, the JD at noon is what we want
    // Actually, Raku's .daycount returns the number of days since
    // the Modified Julian Day epoch (November 17, 1858)
    // daycount = epoch_days + 40587
    civil_to_epoch_days(year, month, day) + 40587
}

/// Julian Date from DateTime.
pub fn julian_date(year: i64, month: i64, day: i64, hour: i64, minute: i64, second: f64) -> f64 {
    let dc = daycount(year, month, day) as f64;
    let day_fraction = (hour as f64 * 3600.0 + minute as f64 * 60.0 + second) / 86400.0;
    dc + day_fraction + 2400000.5
}

/// Modified Julian Date from DateTime.
pub fn modified_julian_date(
    year: i64,
    month: i64,
    day: i64,
    hour: i64,
    minute: i64,
    second: f64,
) -> f64 {
    let dc = daycount(year, month, day) as f64;
    let day_fraction = (hour as f64 * 3600.0 + minute as f64 * 60.0 + second) / 86400.0;
    dc + day_fraction
}

/// Day fraction for DateTime.
pub fn day_fraction(hour: i64, minute: i64, second: f64) -> f64 {
    (hour as f64 * 3600.0 + minute as f64 * 60.0 + second) / 86400.0
}

/// Leap seconds table: (posix_timestamp_of_insertion, cumulative_leap_seconds).
/// Each entry marks a point where a leap second was inserted.
/// Raku's Instant uses TAI-like time = POSIX + offset (including leap seconds).
const LEAP_SECONDS: &[(i64, i64)] = &[
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
pub fn leap_seconds_at(posix: f64) -> i64 {
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
pub fn posix_to_instant(posix: f64) -> f64 {
    posix + leap_seconds_at(posix) as f64
}

/// Convert Raku Instant value back to POSIX timestamp.
pub fn instant_to_posix(instant: f64) -> f64 {
    // Binary search: find posix such that posix + leap_seconds_at(posix) == instant
    // Simple approach: subtract leap seconds iteratively
    let mut posix = instant;
    for _ in 0..3 {
        let ls = leap_seconds_at(posix) as f64;
        posix = instant - ls;
    }
    posix
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_civil_epoch_roundtrip() {
        let test_cases = vec![
            (2000, 1, 1),
            (1970, 1, 1),
            (2024, 2, 29),
            (1969, 12, 31),
            (1, 1, 1),
        ];
        for (y, m, d) in test_cases {
            let days = civil_to_epoch_days(y, m, d);
            let (y2, m2, d2) = epoch_days_to_civil(days);
            assert_eq!(
                (y, m, d),
                (y2, m2, d2),
                "roundtrip failed for {}-{}-{}",
                y,
                m,
                d
            );
        }
    }

    #[test]
    fn test_day_of_week() {
        // 2000-02-28 is Monday (1)
        let days = civil_to_epoch_days(2000, 2, 28);
        assert_eq!(day_of_week(days), 1);
        // 1970-01-01 (epoch) is Thursday (4)
        assert_eq!(day_of_week(0), 4);
    }

    #[test]
    fn test_parse_date_string() {
        assert!(parse_date_string("2010-01-01").is_ok());
        assert!(parse_date_string("malformed").is_err());
        assert!(parse_date_string("2010-00-23").is_err());
        assert!(parse_date_string("2010-13-23").is_err());
        assert!(parse_date_string("1999-02-29").is_err());
        assert!(parse_date_string("2000-02-29").is_ok());
    }
}
