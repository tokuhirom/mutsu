//! `Date`'s and `DateTime`'s `later` and `earlier` rows (ADR-11276 §9.34), and
//! the helpers the derived-value rows share (`keep_formatter`, the subclass
//! re-bless).
//!
//! The rows are `Handler::Named`: the units are the call's named arguments
//! (`.later(:2days, :1hour)`), so a call that spells them as a positional
//! list of pairs (`.later((:2hours, :30minutes))`) is outside the row's
//! signature and takes the cascade, which reads both spellings. The cascade
//! (`runtime/methods_temporal.rs`) calls [`later_earlier`] too, for the
//! receivers the table has no shape for (an instance of a subclass).

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::builtins::methods_0arg::temporal;
use crate::value::{RuntimeError, Value, ValueView};

/// The units `later` and `earlier` bind, as the named arguments Rakudo's
/// `*%unit` takes.
const UNITS: &[&str] = &[
    "second", "seconds", "minute", "minutes", "hour", "hours", "day", "days", "week", "weeks",
    "month", "months", "year", "years",
];

const fn row(owner: &'static str, name: &'static str, handler: Handler) -> MethodRow {
    MethodRow {
        owner,
        name,
        arity: 0,
        handler,
        flags: RowFlags::NONE,
        named: UNITS,
    }
}

pub(super) static ROWS: &[MethodRow] = &[
    row("Date", "later", Handler::Named(later_row)),
    row("Date", "earlier", Handler::Named(earlier_row)),
    row("DateTime", "later", Handler::Named(later_row)),
    row("DateTime", "earlier", Handler::Named(earlier_row)),
];

fn later_row(
    target: &Value,
    _args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    later_earlier(target, "later", named.pairs())
}

fn earlier_row(
    target: &Value,
    _args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    later_earlier(target, "earlier", named.pairs())
}

/// `later` or `earlier` of a `Date` or `DateTime` instance (a subclass keeps
/// its class), or `None` for any other receiver.
// Cost: O(a + u), a = attributes of the instance, u = unit arguments.
pub(crate) fn later_earlier(
    target: &Value,
    method: &str,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
    else {
        return None;
    };
    // Several named units have no defined order of application; Rakudo
    // refuses them and asks for a positional list of pairs instead.
    let named_units = args
        .iter()
        .filter(|a| matches!(a.view(), ValueView::Pair(..)))
        .count();
    if named_units > 1 && (has_date_attrs(&attributes) || has_datetime_attrs(&attributes)) {
        return Some(Err(RuntimeError::new(
            "More than one time unit supplied. Please provide these as a List of\nPairs to indicate order of application if this is intended.",
        )));
    }
    if has_datetime_attrs(&attributes) {
        let (year, month, day, hour, minute, second, timezone) =
            temporal::datetime_attrs(&attributes.as_map());
        Some(
            datetime_later_earlier(
                year, month, day, hour, minute, second, timezone, args, method,
            )
            .map(|v| {
                rebless_datetime_result(keep_formatter(v, &attributes), class_name, &attributes)
            }),
        )
    } else if has_date_attrs(&attributes) {
        let (year, month, day) = temporal::date_attrs(&attributes.as_map());
        Some(
            date_later_earlier(year, month, day, args, method).map(|v| {
                rebless_date_result(keep_formatter(v, &attributes), class_name, &attributes)
            }),
        )
    } else {
        None
    }
}

/// Read a `:2hours`/`:30minutes`-style adverb argument regardless of Pair
/// flavour (ADR-0021): these adverbs are commonly collected into a list
/// literal (`.later((:2hours, :30minutes))`), which is a positional
/// (`ValuePair`) context, not a call site — the named flavour is not
/// guaranteed. A `Str`-keyed positional Pair is treated identically.
fn temporal_adverb_pair(v: &Value) -> Option<(String, Value)> {
    match v.view() {
        ValueView::Pair(key, value) => Some((key.clone(), value.clone())),
        ValueView::ValuePair(key, value) => match key.view() {
            ValueView::Str(s) => Some((s.to_string(), value.clone())),
            _ => None,
        },
        _ => None,
    }
}

pub(crate) fn has_date_attrs(attributes: &crate::gc::Gc<crate::value::InstanceAttrs>) -> bool {
    attributes.contains_key("year")
        && attributes.contains_key("month")
        && attributes.contains_key("day")
}

pub(crate) fn has_datetime_attrs(attributes: &crate::gc::Gc<crate::value::InstanceAttrs>) -> bool {
    has_date_attrs(attributes)
        && attributes.contains_key("hour")
        && attributes.contains_key("minute")
        && attributes.contains_key("second")
        && attributes.contains_key("timezone")
}

/// Carry the invocant's `:formatter` over to a value derived from it. Rakudo
/// passes `:&!formatter` along in `later`/`earlier`/`truncated-to`/
/// `in-timezone` (and so `utc`/`local`), so the derived value renders through
/// the same formatter -- against its own fields, since nothing is cached.
// Cost: O(a), a = attributes of the result (one copy when a formatter exists).
pub(crate) fn keep_formatter(
    result: Value,
    original_attrs: &crate::gc::Gc<crate::value::InstanceAttrs>,
) -> Value {
    temporal::with_formatter(result, original_attrs.as_map().get("formatter").cloned())
}

pub(crate) fn rebless_datetime_result(
    result: Value,
    target_class_name: crate::symbol::Symbol,
    original_attrs: &crate::gc::Gc<crate::value::InstanceAttrs>,
) -> Value {
    if target_class_name == "DateTime" {
        return result;
    }
    let ValueView::Instance {
        class_name,
        attributes,
        id,
    } = result.view()
    else {
        return result;
    };
    if class_name != "DateTime" {
        return result;
    }
    let merged = (**original_attrs).clone();
    for key in [
        "year", "month", "day", "hour", "minute", "second", "timezone", "epoch",
    ] {
        if let Some(value) = attributes.as_map().get(key) {
            merged.insert(key.to_string(), value.clone());
        }
    }
    let mut merged_map = (merged).to_map();
    // The result decides the formatter: `.clone(:formatter(Callable))` resets it.
    if !attributes.as_map().contains_key("formatter") {
        merged_map.remove("formatter");
    }
    Value::instance_parts(
        target_class_name,
        crate::gc::Gc::new(crate::value::InstanceAttrs::new(
            target_class_name,
            merged_map,
            id,
            true,
        )),
        id,
    )
}

pub(crate) fn rebless_date_result(
    result: Value,
    target_class_name: crate::symbol::Symbol,
    original_attrs: &crate::gc::Gc<crate::value::InstanceAttrs>,
) -> Value {
    if target_class_name == "Date" {
        return result;
    }
    let ValueView::Instance {
        class_name,
        attributes,
        id,
    } = result.view()
    else {
        return result;
    };
    if class_name != "Date" {
        return result;
    }
    let merged = (**original_attrs).clone();
    for key in ["year", "month", "day", "days"] {
        if let Some(value) = attributes.as_map().get(key) {
            merged.insert(key.to_string(), value.clone());
        }
    }
    Value::instance_parts(
        target_class_name,
        crate::gc::Gc::new(crate::value::InstanceAttrs::new(
            target_class_name,
            (merged).to_map(),
            id,
            true,
        )),
        id,
    )
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
        if let Some((key, value)) = temporal_adverb_pair(arg) {
            apply_pair(&key, &value)?;
            continue;
        }
        if let Some(items) = arg.as_list_items() {
            for item in items.iter() {
                if let Some((key, value)) = temporal_adverb_pair(item) {
                    apply_pair(&key, &value)?;
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
            // Minutes and hours move the wall clock (a leap second in between
            // is not counted, as in Rakudo); only `seconds` is Instant-based.
            "minute" | "minutes" | "hour" | "hours" => {
                let unit_secs = if key_str.starts_with("hour") {
                    3_600
                } else {
                    60
                };
                let amount = value.to_f64() as i64 * unit_secs * sign;
                let total = h * 3_600 + mi * 60 + amount;
                let day_shift = total.div_euclid(86_400);
                let in_day = total.rem_euclid(86_400);
                let (ny, nm, nd) = temporal::epoch_days_to_civil(
                    temporal::civil_to_epoch_days(y, m, d) + day_shift,
                );
                y = ny;
                m = nm;
                d = nd;
                h = in_day / 3_600;
                mi = (in_day % 3_600) / 60;
                clip_non_leap_second(y, m, d, h, mi, &mut s, timezone);
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
        if let Some((key, value)) = temporal_adverb_pair(arg) {
            apply_pair(&key, &value)?;
            continue;
        }
        if let Some(items) = arg.as_list_items() {
            for item in items.iter() {
                if let Some((key, value)) = temporal_adverb_pair(item) {
                    apply_pair(&key, &value)?;
                }
            }
        }
    }
    s = (s * 1_000_000.0).round() / 1_000_000.0;
    Ok(temporal::make_datetime(y, m, d, h, mi, s, timezone))
}

/// Normalize unit names (strip trailing 's', handle singular/plural).
fn normalize_unit(key: &str) -> String {
    key.to_lowercase()
}
