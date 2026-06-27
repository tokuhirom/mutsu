use super::*;
use crate::symbol::Symbol;
use num_traits::ToPrimitive;

fn string_has_numeric_tz_offset(s: &str) -> bool {
    match s.find(['T', 't']) {
        Some(pos) => {
            let time_part = &s[pos + 1..];
            time_part.contains('+') || time_part.contains('-')
        }
        None => false,
    }
}

impl Interpreter {
    pub(crate) fn build_native_date(
        args: &[Value],
    ) -> Result<(Value, Option<Value>), RuntimeError> {
        use crate::builtins::methods_0arg::temporal;
        let mut year: i64 = 1970;
        let mut month: i64 = 1;
        let mut day: i64 = 1;
        let mut positional = Vec::new();
        let mut has_named = false;
        let mut formatter: Option<Value> = None;
        for arg in args {
            match arg {
                Value::Pair(key, value) => match key.as_str() {
                    "year" => {
                        year = to_int(value);
                        has_named = true;
                    }
                    "month" => {
                        month = to_int(value);
                        has_named = true;
                    }
                    "day" => {
                        day = to_int(value);
                        has_named = true;
                    }
                    "formatter" => {
                        formatter = Some(*value.clone());
                    }
                    _ => {}
                },
                other => positional.push(other),
            }
        }
        // Positional args: a date string, a DateTime/Instant, or y/m/d.
        if let Some(v) = positional.first() {
            match v {
                Value::Str(s) if positional.len() == 1 => {
                    let (y, m, d) = temporal::parse_date_string(s)?;
                    year = y;
                    month = m;
                    day = d;
                }
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "DateTime" => {
                    let (y, m, d, _, _, _, _) = temporal::datetime_attrs(&attributes.as_map());
                    year = y;
                    month = m;
                    day = d;
                }
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Instant" => {
                    let tai = attributes
                        .as_map()
                        .get("value")
                        .and_then(crate::runtime::to_float_value)
                        .unwrap_or(0.0);
                    let posix = temporal::instant_to_posix(tai);
                    let epoch_days = (posix / 86400.0).floor() as i64;
                    let (y, m, d) = temporal::epoch_days_to_civil(epoch_days);
                    year = y;
                    month = m;
                    day = d;
                }
                _ => {
                    year = to_int(v);
                    if let Some(v2) = positional.get(1) {
                        month = to_int(v2);
                    }
                    if let Some(v3) = positional.get(2) {
                        day = to_int(v3);
                    }
                }
            }
        } else if !has_named {
            return Err(RuntimeError::new("Date.new requires arguments"));
        }
        temporal::validate_date(year, month, day)?;
        let date = temporal::make_date_with_formatter(year, month, day, formatter.clone());
        Ok((date, formatter))
    }

    /// Build a `DateTime` from `.new` arguments as pure data: parse named
    /// (`year`..`second`/`timezone`/`date`/`formatter`) and positional args (a
    /// datetime string, a posix epoch as Int/BigInt/Num/Rat, a `Date`/`Instant`,
    /// or `y, mo, d, h, mi, s`), validate, and construct the `DateTime`. Returns
    /// the datetime plus the formatter that still needs *rendering* (a user
    /// `Callable` — `eval_call_on_value`, the only `self`-dependent step), so a
    /// `Some` formatter makes the VM fall through while the common no-formatter
    /// case stays native. All parsing/validation (`temporal::*`) is self-free.
    #[allow(clippy::type_complexity)]
    pub(crate) fn build_native_datetime(
        args: &[Value],
    ) -> Result<(Value, Option<Value>), RuntimeError> {
        use crate::builtins::methods_0arg::temporal;
        let mut year: i64 = 1970;
        let mut month: i64 = 1;
        let mut day: i64 = 1;
        let mut hour: i64 = 0;
        let mut minute: i64 = 0;
        let mut second: f64 = 0.0;
        let mut timezone: i64 = 0;
        let mut timezone_set = false;
        let mut formatter: Option<Value> = None;
        let mut has_component_named = false;
        let mut positional = Vec::new();
        let mut has_named = false;
        for arg in args {
            match arg {
                Value::Pair(key, value) => match key.as_str() {
                    "year" => {
                        year = to_int(value);
                        has_named = true;
                        has_component_named = true;
                    }
                    "month" => {
                        month = to_int(value);
                        has_named = true;
                        has_component_named = true;
                    }
                    "day" => {
                        day = to_int(value);
                        has_named = true;
                        has_component_named = true;
                    }
                    "hour" => {
                        hour = to_int(value);
                        has_named = true;
                        has_component_named = true;
                    }
                    "minute" => {
                        minute = to_int(value);
                        has_named = true;
                        has_component_named = true;
                    }
                    "second" => {
                        second = to_float_value(value).unwrap_or(0.0);
                        has_named = true;
                        has_component_named = true;
                    }
                    "timezone" => {
                        timezone = to_int(value);
                        timezone_set = true;
                        has_named = true;
                    }
                    "date" => {
                        if let Value::Instance {
                            class_name,
                            attributes,
                            ..
                        } = value.as_ref()
                            && class_name == "Date"
                        {
                            let (y, m, d) = temporal::date_attrs(&attributes.as_map());
                            year = y;
                            month = m;
                            day = d;
                            has_named = true;
                            has_component_named = true;
                        }
                    }
                    "formatter" => {
                        formatter = Some(*value.clone());
                        has_named = true;
                    }
                    _ => {}
                },
                other => positional.push(other),
            }
        }
        if has_component_named && !positional.is_empty() {
            return Err(RuntimeError::new(
                "DateTime.new cannot mix positional and component named arguments",
            ));
        }
        if positional.len() >= 6 {
            year = to_int(positional[0]);
            month = to_int(positional[1]);
            day = to_int(positional[2]);
            hour = to_int(positional[3]);
            minute = to_int(positional[4]);
            second = to_float_value(positional[5]).unwrap_or(0.0);
            has_named = true;
        } else if let Some(v) = positional.first() {
            match v {
                Value::Str(s) => {
                    if timezone_set && string_has_numeric_tz_offset(s) {
                        let message =
                            "DateTime.new(Str): :timezone argument not allowed with a timestamp offset"
                                .to_string();
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("message".to_string(), Value::str(message.clone()));
                        let ex = Value::make_instance(
                            crate::symbol::Symbol::intern("X::DateTime::TimezoneClash"),
                            attrs,
                        );
                        let mut err = RuntimeError::new(message);
                        err.exception = Some(Box::new(ex));
                        return Err(err);
                    }
                    let (y, mo, d, h, mi, sec, tz) = temporal::parse_datetime_string(s)?;
                    year = y;
                    month = mo;
                    day = d;
                    hour = h;
                    minute = mi;
                    second = sec;
                    if !timezone_set {
                        timezone = tz;
                    }
                    has_named = true;
                }
                Value::Int(epoch) => {
                    let total = *epoch as f64 + timezone as f64;
                    let total_i = total.floor() as i64;
                    let frac = total - total_i as f64;
                    let day_secs = total_i.rem_euclid(86400);
                    let epoch_days = (total_i - day_secs) / 86400;
                    let (y, m, d) = temporal::epoch_days_to_civil(epoch_days);
                    year = y;
                    month = m;
                    day = d;
                    hour = day_secs / 3600;
                    minute = (day_secs % 3600) / 60;
                    second = (day_secs % 60) as f64 + frac;
                    has_named = true;
                }
                Value::BigInt(epoch) => {
                    let total = epoch.as_ref().clone() + num_bigint::BigInt::from(timezone);
                    let secs_per_day = num_bigint::BigInt::from(86_400i64);
                    let day_secs_big = ((&total % &secs_per_day) + &secs_per_day) % &secs_per_day;
                    let epoch_days_big = (&total - &day_secs_big) / &secs_per_day;
                    let epoch_days = epoch_days_big.to_i64().ok_or_else(|| {
                        RuntimeError::new("X::DateTime::Range: epoch day out of range".to_string())
                    })?;
                    let day_secs = day_secs_big.to_i64().ok_or_else(|| {
                        RuntimeError::new("X::DateTime::Range: day second out of range".to_string())
                    })?;
                    let (y, m, d) = temporal::epoch_days_to_civil(epoch_days);
                    year = y;
                    month = m;
                    day = d;
                    hour = day_secs / 3600;
                    minute = (day_secs % 3600) / 60;
                    second = (day_secs % 60) as f64;
                    has_named = true;
                }
                Value::Num(epoch) => {
                    let total = *epoch + timezone as f64;
                    let total_i = total.floor() as i64;
                    let frac = total - total_i as f64;
                    let day_secs = total_i.rem_euclid(86400);
                    let epoch_days = (total_i - day_secs) / 86400;
                    let (y, m, d) = temporal::epoch_days_to_civil(epoch_days);
                    year = y;
                    month = m;
                    day = d;
                    hour = day_secs / 3600;
                    minute = (day_secs % 3600) / 60;
                    second = (day_secs % 60) as f64 + frac;
                    has_named = true;
                }
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Date" => {
                    let (y, m, d) = temporal::date_attrs(&attributes.as_map());
                    year = y;
                    month = m;
                    day = d;
                    hour = 0;
                    minute = 0;
                    second = 0.0;
                    has_named = true;
                }
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Instant" => {
                    let val = attributes
                        .as_map()
                        .get("value")
                        .cloned()
                        .unwrap_or(Value::Int(0));
                    let (tai_int, tai_frac) = match &val {
                        Value::Rat(n, d) if *d != 0 => (*n / *d, (*n % *d) as f64 / *d as f64),
                        _ => {
                            let f = crate::runtime::to_float_value(&val).unwrap_or(0.0);
                            (f.floor() as i64, f - f.floor())
                        }
                    };
                    let (y, m, d, h, mi, s) =
                        temporal::instant_to_datetime_leap_aware_parts(tai_int, tai_frac, timezone);
                    year = y;
                    month = m;
                    day = d;
                    hour = h;
                    minute = mi;
                    second = s;
                    has_named = true;
                }
                other if other.is_numeric() => {
                    let epoch = other.to_f64() + timezone as f64;
                    let total_i = epoch.floor() as i64;
                    let frac = epoch - total_i as f64;
                    let day_secs = total_i.rem_euclid(86400);
                    let epoch_days = (total_i - day_secs) / 86400;
                    let (y, m, d) = temporal::epoch_days_to_civil(epoch_days);
                    year = y;
                    month = m;
                    day = d;
                    hour = day_secs / 3600;
                    minute = (day_secs % 3600) / 60;
                    second = (day_secs % 60) as f64 + frac;
                    has_named = true;
                }
                _ => {}
            }
        }
        if !has_named {
            return Err(RuntimeError::new("DateTime.new requires arguments"));
        }
        temporal::validate_datetime(year, month, day, hour, minute, second, timezone)?;
        let dt = temporal::make_datetime(year, month, day, hour, minute, second, timezone);
        Ok((dt, formatter))
    }

    /// Build a `Duration` instance from `.new` arguments as pure data: the
    /// seconds argument is stored as a `Rational` (matching Rakudo, where
    /// `Duration.new(...).tai` is always a `Rat`); `Inf`/`-Inf`/`NaN` map to the
    /// degenerate Rats `1/0`/`-1/0`/`0/0`. A non-numeric string argument is an
    /// `X::Str::Numeric` error (the one fallible built-in builder).
    pub(crate) fn build_native_duration_value(args: &[Value]) -> Result<Value, RuntimeError> {
        let secs = if let Some(arg) = args.first() {
            if let Value::Str(s) = arg {
                match s.parse::<f64>() {
                    Ok(f) => f,
                    Err(_) => {
                        let mut err = RuntimeError::new(format!(
                            "Cannot convert string to number: base-10 number must begin with valid digits or '.' in '{}'",
                            s
                        ));
                        let mut eattrs = HashMap::new();
                        eattrs.insert("source".to_string(), Value::str(s.to_string()));
                        eattrs.insert("pos".to_string(), Value::Int(0));
                        eattrs.insert(
                            "reason".to_string(),
                            Value::str(
                                "base-10 number must begin with valid digits or '.'".to_string(),
                            ),
                        );
                        err.exception = Some(Box::new(Value::make_instance(
                            Symbol::intern("X::Str::Numeric"),
                            eattrs,
                        )));
                        return Err(err);
                    }
                }
            } else {
                to_float_value(arg).unwrap_or(0.0)
            }
        } else {
            0.0
        };
        let val = if secs.is_infinite() {
            if secs > 0.0 {
                Value::Rat(1, 0)
            } else {
                Value::Rat(-1, 0)
            }
        } else if secs.is_nan() {
            Value::Rat(0, 0)
        } else {
            match args.first() {
                Some(v) => crate::builtins::arith::real_to_rat(v),
                None => crate::value::make_rat(0, 1),
            }
        };
        let mut attrs = HashMap::new();
        attrs.insert("value".to_string(), val);
        Ok(Value::make_instance(Symbol::intern("Duration"), attrs))
    }
}
