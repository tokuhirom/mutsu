use super::*;

fn parse_unicode_decimal_digits(input: &str) -> Option<(&str, String)> {
    let mut end = 0;
    let mut clean = String::new();
    let mut saw_digit = false;
    for c in input.chars() {
        if c == '_' {
            end += c.len_utf8();
            continue;
        }
        let Some(dv) = crate::builtins::unicode::unicode_decimal_digit_value(c) else {
            break;
        };
        saw_digit = true;
        clean.push(char::from_digit(dv, 10).unwrap());
        end += c.len_utf8();
    }
    if saw_digit {
        Some((&input[end..], clean))
    } else {
        None
    }
}

pub(crate) fn parse_radix_number_body(body: &str, base: u32) -> Option<Value> {
    if !(2..=36).contains(&base) {
        return None;
    }
    let compact: String = body.chars().filter(|c| !c.is_whitespace()).collect();
    if compact.is_empty() {
        return None;
    }

    let (digits_body, exponent_scale) =
        if let Some((digits, exp_part)) = compact.split_once("*10**") {
            let exp_part = exp_part.trim();
            if exp_part.is_empty() {
                return None;
            }
            let (sign, after_sign) = if let Some(rest) = exp_part.strip_prefix('+') {
                (1_i64, rest)
            } else if let Some(rest) = exp_part.strip_prefix('-') {
                (-1_i64, rest)
            } else {
                (1_i64, exp_part)
            };
            let (exp_rest, exp_clean) = parse_unicode_decimal_digits(after_sign)?;
            if !exp_rest.is_empty() {
                return None;
            }
            let exp_abs: i64 = exp_clean.parse().ok()?;
            (digits.trim(), sign * exp_abs)
        } else {
            (compact.trim(), 0_i64)
        };
    if digits_body.is_empty() {
        return None;
    }

    let mut int_clean = String::new();
    let mut frac_clean = String::new();
    let mut saw_dot = false;
    let mut saw_digit = false;
    for c in digits_body.chars() {
        if c == '_' {
            continue;
        }
        if c == '.' {
            if saw_dot {
                return None;
            }
            saw_dot = true;
            continue;
        }
        let value =
            crate::builtins::unicode::unicode_decimal_digit_value(c).or_else(|| match c {
                'a'..='z' => Some(10 + (c as u32 - 'a' as u32)),
                'A'..='Z' => Some(10 + (c as u32 - 'A' as u32)),
                'ａ'..='ｚ' => Some(10 + (c as u32 - 'ａ' as u32)),
                'Ａ'..='Ｚ' => Some(10 + (c as u32 - 'Ａ' as u32)),
                _ => None,
            })?;
        if value >= base {
            return None;
        }
        let digit = char::from_digit(value, 36)?;
        saw_digit = true;
        if saw_dot {
            frac_clean.push(digit);
        } else {
            int_clean.push(digit);
        }
    }
    if !saw_digit {
        return None;
    }

    if !saw_dot && exponent_scale == 0 {
        if let Ok(n) = i64::from_str_radix(&int_clean, base) {
            return Some(Value::Int(n));
        }
        if let Some(n) = num_bigint::BigInt::parse_bytes(int_clean.as_bytes(), base) {
            return Some(Value::from_bigint(n));
        }
        return None;
    }

    let base_big = num_bigint::BigInt::from(base);
    let int_value = if int_clean.is_empty() {
        num_bigint::BigInt::from(0_i64)
    } else {
        num_bigint::BigInt::parse_bytes(int_clean.as_bytes(), base)?
    };
    let frac_value = if frac_clean.is_empty() {
        num_bigint::BigInt::from(0_i64)
    } else {
        num_bigint::BigInt::parse_bytes(frac_clean.as_bytes(), base)?
    };
    let frac_scale = base_big.pow(frac_clean.len() as u32);
    let mut numerator = int_value * &frac_scale + frac_value;
    let mut denominator = frac_scale;
    if exponent_scale != 0 {
        let exp_abs = exponent_scale.unsigned_abs() as u32;
        let scale10 = num_bigint::BigInt::from(10_u32).pow(exp_abs);
        if exponent_scale > 0 {
            numerator *= scale10;
        } else {
            denominator *= scale10;
        }
    }
    Some(crate::value::make_big_rat(numerator, denominator))
}

pub(crate) fn coerce_to_numeric(val: Value) -> Value {
    let val = val.into_descalarized();
    match val {
        Value::Mixin(inner, _) => coerce_to_numeric(inner.as_ref().clone()),
        Value::Int(_)
        | Value::BigInt(_)
        | Value::Num(_)
        | Value::Rat(_, _)
        | Value::FatRat(_, _)
        | Value::BigRat(_, _)
        | Value::Complex(_, _) => val,
        Value::Bool(b) => Value::Int(if b { 1 } else { 0 }),
        Value::Enum { value, .. } => Value::Int(value.as_i64()),
        Value::Str(ref s) => {
            if let Some(v) = crate::runtime::str_numeric::parse_raku_str_to_numeric(s) {
                v
            } else {
                Value::Int(0)
            }
        }
        _ if val.as_list_items().is_some() => Value::Int(val.as_list_items().unwrap().len() as i64),
        Value::Hash(items) => Value::Int(items.len() as i64),
        Value::Set(items, _) => Value::Int(items.len() as i64),
        Value::Bag(items, _) => Value::from_bigint(items.values().sum::<BigInt>()),
        Value::Mix(items, _) => {
            // Sort values before summing for deterministic results
            // regardless of HashMap iteration order.
            let mut vals: Vec<f64> = items.values().copied().collect();
            vals.sort_by(|a, b| a.total_cmp(b));
            let total: f64 = vals.iter().copied().fold(0.0, std::ops::Add::add);
            if total == 0.0 && items.is_empty() {
                Value::Int(0)
            } else if (total - (total as i64 as f64)).abs() < f64::EPSILON {
                Value::Int(total as i64)
            } else {
                Value::Num(total)
            }
        }
        Value::LazyList(ll) => {
            if let Some(count) = &ll.elems_count {
                count.clone()
            } else if let Some(cached) = ll.cache.lock().unwrap().as_ref() {
                Value::Int(cached.len() as i64)
            } else {
                Value::Int(0)
            }
        }
        Value::Range(..)
        | Value::RangeExcl(..)
        | Value::RangeExclStart(..)
        | Value::RangeExclBoth(..)
        | Value::GenericRange { .. } => Value::Int(value_to_list(&val).len() as i64),
        Value::Nil => Value::Int(0),
        Value::Instance {
            ref class_name,
            ref attributes,
            ..
        } if class_name == "Instant" => attributes
            .as_map()
            .get("value")
            .cloned()
            .unwrap_or(Value::Num(0.0)),
        Value::Instance {
            ref class_name,
            ref attributes,
            ..
        } if class_name == "Duration" => attributes
            .as_map()
            .get("value")
            .cloned()
            .unwrap_or(Value::Num(0.0)),
        Value::Instance {
            ref class_name,
            ref attributes,
            ..
        } if class_name == "Date" => {
            let (y, m, d) =
                crate::builtins::methods_0arg::temporal::date_attrs(&(attributes).as_map());
            let epoch = crate::builtins::methods_0arg::temporal::civil_to_epoch_days(y, m, d);
            Value::Int(epoch * 86400)
        }
        Value::Instance {
            ref class_name,
            ref attributes,
            ..
        } if class_name == "DateTime" => {
            let (y, mo, d, h, mi, s, tz) =
                crate::builtins::methods_0arg::temporal::datetime_attrs(&(attributes).as_map());
            Value::Num(crate::builtins::methods_0arg::temporal::datetime_to_posix(
                y, mo, d, h, mi, s, tz,
            ))
        }
        Value::Uni(u) => Value::Int(u.text.chars().count() as i64),
        Value::Capture { ref positional, .. } => Value::Int(positional.len() as i64),
        _ => Value::Int(0),
    }
}
