use crate::symbol::Symbol;
use crate::value::AttrMap;
use crate::value::{RuntimeError, Value};

/// Convert an f64 to a Rat by using its decimal string representation.
/// This preserves the visible decimal digits (e.g. 10.987654321 → 10987654321/1000000000).
pub(crate) fn f64_to_decimal_rat(f: f64) -> Value {
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
            return Value::rat_raw(num / g, den / g);
        }
    }
    Value::num(f)
}

pub(crate) fn gcd_i64(mut a: i64, mut b: i64) -> i64 {
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

/// Dispatch 0-arg methods for Date instances.
///
/// A plain `Date` is answered by the rows of the method table
/// (`method_table::instances`); this is the answer for the receivers that have
/// no shape (an instance of a subclass), and it calls the rows' own handlers,
/// so there is one implementation of each method.
pub fn date_method_0arg(attributes: &AttrMap, method: &str) -> Option<Result<Value, RuntimeError>> {
    use crate::builtins::method_table::{date, dateish, temporal as fields};

    Some(Ok(match method {
        "year" => fields::date_year(attributes),
        "month" => fields::date_month(attributes),
        "day" => fields::date_day(attributes),
        "day-of-month" => dateish::day_of_month(attributes),
        "day-of-week" => dateish::day_of_week(attributes),
        "day-of-year" => dateish::day_of_year(attributes),
        "is-leap-year" => dateish::is_leap_year(attributes),
        "days-in-month" => dateish::days_in_month(attributes),
        "days-in-year" => dateish::days_in_year(attributes),
        "formatter" => dateish::formatter(attributes),
        "daycount" => dateish::daycount(attributes),
        // A formatter is a user Callable: the interpreter-aware dispatch path
        // calls it against this invocant on every stringification.
        "Str" | "gist" => return date::str_of(attributes).map(Ok),
        "Date" => date::to_date(attributes),
        "yyyy-mm-dd" | "mm-dd-yyyy" | "dd-mm-yyyy" | "mm-dd" | "yyyy-mm" => {
            dateish::ordered(method, attributes, "-")
        }
        "succ" => date::succ(attributes),
        "pred" => date::pred(attributes),
        "week-year" => dateish::week_year(attributes),
        "week-number" => dateish::week_number(attributes),
        "week" => dateish::week(attributes),
        "weekday-of-month" => dateish::weekday_of_month(attributes),
        "Int" | "Numeric" | "Real" => date::int(attributes),
        "DateTime" => date::to_datetime(attributes),
        "raku" | "perl" => date::raku(attributes),
        "WHICH" => date::which(attributes),
        _ => return None,
    }))
}

/// Dispatch 0-arg methods for DateTime instances: the rows' handlers, for the
/// receivers that have no shape (see [`date_method_0arg`]).
pub fn datetime_method_0arg(
    attributes: &AttrMap,
    method: &str,
) -> Option<Result<Value, RuntimeError>> {
    use crate::builtins::method_table::{dateish, datetime, temporal as fields};

    Some(Ok(match method {
        "year" => fields::datetime_year(attributes),
        "month" => fields::datetime_month(attributes),
        "day" => fields::datetime_day(attributes),
        "hour" => fields::datetime_hour(attributes),
        "minute" => fields::datetime_minute(attributes),
        "day-of-month" => dateish::day_of_month(attributes),
        "second" => datetime::second(attributes),
        "timezone" | "offset" => datetime::timezone(attributes),
        "offset-in-hours" => datetime::offset_in_hours(attributes),
        "offset-in-minutes" => datetime::offset_in_minutes(attributes),
        "day-of-week" => dateish::day_of_week(attributes),
        "day-of-year" => dateish::day_of_year(attributes),
        "is-leap-year" => dateish::is_leap_year(attributes),
        "days-in-month" => dateish::days_in_month(attributes),
        "days-in-year" => dateish::days_in_year(attributes),
        "formatter" => dateish::formatter(attributes),
        "yyyy-mm-dd" | "mm-dd-yyyy" | "dd-mm-yyyy" | "mm-dd" | "yyyy-mm" => {
            dateish::ordered(method, attributes, "-")
        }
        "daycount" => dateish::daycount(attributes),
        "whole-second" => datetime::whole_second(attributes),
        "hh-mm-ss" => datetime::hh_mm_ss(attributes),
        // A formatter is a user Callable: the interpreter-aware dispatch path
        // calls it against this invocant on every stringification.
        "Str" | "gist" => return datetime::str_of(attributes).map(Ok),
        "Date" => datetime::to_date(attributes),
        "posix" => datetime::posix(attributes),
        "utc" => datetime::utc(attributes),
        "week-year" => dateish::week_year(attributes),
        "week-number" => dateish::week_number(attributes),
        "week" => dateish::week(attributes),
        "weekday-of-month" => dateish::weekday_of_month(attributes),
        "julian-date" => return Some(datetime::julian_date(attributes)),
        "modified-julian-date" => return Some(datetime::modified_julian_date(attributes)),
        "day-fraction" => datetime::day_fraction(attributes),
        "Instant" | "Numeric" | "Real" => datetime::instant(attributes),
        "DateTime" => datetime::to_datetime(attributes),
        "raku" | "perl" => datetime::raku(attributes),
        "WHICH" => datetime::which(attributes),
        _ => return None,
    }))
}

/// `succ`/`pred` for `Instant`/`Duration`: both do `Real`, whose `succ`/`pred`
/// step the value by one second and keep the receiver's type (raku:
/// `Instant.from-posix(1).succ` is `Instant:12`, `Duration.new(3).succ` is `4`).
// Cost: O(1).
pub(crate) fn real_role_step(
    class_name: Symbol,
    attributes: AttrMap,
    forward: bool,
) -> Result<Value, RuntimeError> {
    let mut attributes = attributes;
    let value = attributes.get("value").cloned().unwrap_or(Value::int(0));
    let stepped = if forward {
        crate::builtins::arith::arith_add(value, Value::int(1))?
    } else {
        crate::builtins::arith::arith_sub(value, Value::int(1))
    };
    attributes.insert("value", stepped);
    Ok(Value::make_instance(class_name, attributes))
}
