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
            return Some(Value::int(n));
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
    match val.view() {
        ValueView::Mixin(inner, _) => coerce_to_numeric(inner.as_ref().clone()),
        ValueView::Int(_)
        | ValueView::BigInt(_)
        | ValueView::Num(_)
        | ValueView::Rat(_, _)
        | ValueView::FatRat(_, _)
        | ValueView::BigRat(_, _)
        | ValueView::Complex(_, _) => val.clone(),
        ValueView::Bool(b) => Value::int(if b { 1 } else { 0 }),
        ValueView::Enum { value, .. } => Value::int(value.as_i64()),
        ValueView::Str(s) => {
            if let Some(v) = crate::runtime::str_numeric::parse_raku_str_to_numeric(s) {
                v
            } else {
                Value::int(0)
            }
        }
        _ if val.as_list_items().is_some() => Value::int(val.as_list_items().unwrap().len() as i64),
        ValueView::Hash(items) => Value::int(items.len() as i64),
        ValueView::Set(items, _) => Value::int(items.len() as i64),
        ValueView::Bag(items, _) => Value::from_bigint(items.values().sum::<BigInt>()),
        ValueView::Mix(items, _) => {
            // Sort values before summing for deterministic results
            // regardless of HashMap iteration order.
            let mut vals: Vec<f64> = items.values().copied().collect();
            vals.sort_by(|a, b| a.total_cmp(b));
            let total: f64 = vals.iter().copied().fold(0.0, std::ops::Add::add);
            if total == 0.0 && items.is_empty() {
                Value::int(0)
            } else if (total - (total as i64 as f64)).abs() < f64::EPSILON {
                Value::int(total as i64)
            } else {
                Value::num(total)
            }
        }
        ValueView::LazyList(ll) => {
            if let Some(count) = &ll.elems_count {
                count.clone()
            } else if let Some(cached) = ll.cache.lock().unwrap().as_ref() {
                Value::int(cached.len() as i64)
            } else {
                Value::int(0)
            }
        }
        ValueView::Range(..)
        | ValueView::RangeExcl(..)
        | ValueView::RangeExclStart(..)
        | ValueView::RangeExclBoth(..)
        | ValueView::GenericRange { .. } => Value::int(value_to_list(&val).len() as i64),
        ValueView::Nil => Value::int(0),
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Instant" => attributes
            .as_map()
            .get("value")
            .cloned()
            .unwrap_or(Value::num(0.0)),
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Duration" => attributes
            .as_map()
            .get("value")
            .cloned()
            .unwrap_or(Value::num(0.0)),
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Date" => {
            let (y, m, d) =
                crate::builtins::methods_0arg::temporal::date_attrs(&(attributes).as_map());
            let epoch = crate::builtins::methods_0arg::temporal::civil_to_epoch_days(y, m, d);
            Value::int(epoch * 86400)
        }
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "DateTime" => {
            let (y, mo, d, h, mi, s, tz) =
                crate::builtins::methods_0arg::temporal::datetime_attrs(&(attributes).as_map());
            Value::num(crate::builtins::methods_0arg::temporal::datetime_to_posix(
                y, mo, d, h, mi, s, tz,
            ))
        }
        ValueView::Uni(u) => Value::int(u.text.chars().count() as i64),
        ValueView::Capture { positional, .. } => Value::int(positional.len() as i64),
        _ => Value::int(0),
    }
}
