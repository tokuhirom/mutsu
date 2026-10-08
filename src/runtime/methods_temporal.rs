use crate::builtins::method_table::temporal_edit::{
    date_clone, datetime_clone, in_timezone, truncated_to,
};
use crate::builtins::method_table::temporal_shift::{
    has_date_attrs, has_datetime_attrs, later_earlier, rebless_date_result,
    rebless_datetime_result,
};
use crate::builtins::methods_0arg::temporal;
use crate::value::{RuntimeError, Value, ValueView};

/// Dispatch temporal n-arg methods for Date/DateTime instances.
/// Returns Some(result) if handled, None if not a temporal method.
///
/// `later`, `earlier`, `truncated-to` and `in-timezone` are rows of the method
/// table (ADR-11276 §9.34); this is their path for the receivers the table has
/// no shape for (an instance of a subclass) and for the spellings a row does not
/// bind (the units as a positional list of pairs).
pub(super) fn dispatch_temporal_method(
    target: &Value,
    method: &str,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    match method {
        "later" | "earlier" => return later_earlier(target, method, args),
        "truncated-to" => return truncated_to(target, args),
        "in-timezone" if let Some(result) = in_timezone(target, args.first()) => {
            return Some(result);
        }
        _ => {}
    }
    match target.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if has_date_attrs(&attributes) && !has_datetime_attrs(&attributes) => {
            let (year, month, day) = temporal::date_attrs(&(attributes).as_map());
            match method {
                // `.yyyy-mm-dd($sep)` / `.mm-dd-yyyy($sep)` / `.dd-mm-yyyy($sep)`
                // with an optional separator string (default `-`).
                "yyyy-mm-dd" | "mm-dd-yyyy" | "dd-mm-yyyy" => {
                    let sep = args
                        .first()
                        .map(|v| v.to_string_value())
                        .unwrap_or_else(|| "-".to_string());
                    Some(Ok(Value::str(temporal::format_date_ordered(
                        method, year, month, day, &sep,
                    ))))
                }
                "clone" => {
                    let existing_formatter = attributes.as_map().get("formatter").cloned();
                    Some(
                        date_clone(year, month, day, existing_formatter, args)
                            .map(|v| rebless_date_result(v, class_name, &attributes)),
                    )
                }
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
                    let formatter = attributes.as_map().get("formatter").cloned();
                    Some(Ok(temporal::make_date_with_formatter(
                        year, month, 1, formatter,
                    )))
                }
                "last-date-in-month" if args.is_empty() => {
                    let last_day = temporal::days_in_month(year, month);
                    let formatter = attributes.as_map().get("formatter").cloned();
                    Some(Ok(temporal::make_date_with_formatter(
                        year, month, last_day, formatter,
                    )))
                }
                _ => None,
            }
        }
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if has_datetime_attrs(&attributes) => {
            let (year, month, day, hour, minute, second, timezone) =
                temporal::datetime_attrs(&(attributes).as_map());
            match method {
                // `.yyyy-mm-dd($sep)` etc. with an optional separator (default `-`).
                "yyyy-mm-dd" | "mm-dd-yyyy" | "dd-mm-yyyy" => {
                    let sep = args
                        .first()
                        .map(|v| v.to_string_value())
                        .unwrap_or_else(|| "-".to_string());
                    Some(Ok(Value::str(temporal::format_date_ordered(
                        method, year, month, day, &sep,
                    ))))
                }
                "clone" => {
                    let existing_formatter = attributes.as_map().get("formatter").cloned();
                    Some(
                        datetime_clone(
                            year,
                            month,
                            day,
                            hour,
                            minute,
                            second,
                            timezone,
                            existing_formatter,
                            args,
                        )
                        .map(|v| rebless_datetime_result(v, class_name, &attributes)),
                    )
                }
                // `utc` is `in-timezone(0)` (Rakudo), reached here for a
                // DateTime subclass so the result keeps the subclass.
                "utc" if args.is_empty() => in_timezone(target, Some(&Value::int(0))),
                "posix" if args.len() == 1 => {
                    // .posix(True) / .posix(:real) keeps fractional seconds.
                    // .posix(False) / .posix(:!real) truncates to whole seconds.
                    let posix = temporal::datetime_to_posix(
                        year, month, day, hour, minute, second, timezone,
                    );
                    let real = match args[0].view() {
                        ValueView::Pair(key, value) if key == "real" => value.truthy(),
                        _ => args[0].truthy(),
                    };
                    if !real {
                        Some(Ok(Value::int(posix.floor() as i64)))
                    } else if posix == posix.floor() {
                        Some(Ok(Value::int(posix as i64)))
                    } else {
                        Some(Ok(Value::num(posix)))
                    }
                }
                _ => None,
            }
        }
        _ => None,
    }
}
