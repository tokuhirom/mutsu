use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(super) fn builtin_unbase(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            return Err(RuntimeError::new(
                "UNBASE expects a radix and at least one argument",
            ));
        }
        if args.len() < 2 {
            // :N() with no value -> X::Numeric::Confused
            let radix = match args[0] {
                Value::Int(n) => n,
                _ => 0,
            };
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("base".to_string(), Value::Int(radix));
            attrs.insert(
                "message".to_string(),
                Value::str(format!("No value supplied for base-{} conversion", radix)),
            );
            let exception = Value::make_instance(Symbol::intern("X::Numeric::Confused"), attrs);
            let mut err =
                RuntimeError::new(format!("No value supplied for base-{} conversion", radix));
            err.exception = Some(Box::new(exception));
            return Err(err);
        }
        let radix = match args[0] {
            Value::Int(n) if (2..=36).contains(&n) => n as u32,
            _ => {
                return Err(RuntimeError::new(
                    "UNBASE radix must be an integer in 2..36",
                ));
            }
        };

        let mut out = Vec::with_capacity(args.len() - 1);
        for arg in args.iter().skip(1) {
            // :N() requires string arguments; non-string throws X::Numeric::Confused
            if !matches!(arg, Value::Str(_)) {
                let type_name = match arg {
                    Value::Int(_) => "Int",
                    Value::Num(_) => "Num",
                    Value::Rat(_, _) | Value::BigRat(..) => "Rat",
                    Value::Array(..) => "Array",
                    Value::Hash(..) => "Hash",
                    Value::Bool(_) => "Bool",
                    _ => "value",
                };
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("num".to_string(), arg.clone());
                attrs.insert("base".to_string(), Value::Int(radix as i64));
                attrs.insert(
                    "message".to_string(),
                    Value::str(format!(
                        "Cannot convert {} to a radix-{} number: \
                         please supply a Str value, not a {}",
                        arg.to_string_value(),
                        radix,
                        type_name
                    )),
                );
                let exception = Value::make_instance(Symbol::intern("X::Numeric::Confused"), attrs);
                let mut err = RuntimeError::new(format!(
                    "Cannot convert {} to a radix-{} number: \
                     please supply a Str value, not a {}",
                    arg.to_string_value(),
                    radix,
                    type_name
                ));
                err.exception = Some(Box::new(exception));
                return Err(err);
            }
            let text = arg.to_string_value();
            let parsed = unbase_parse_with_overrides(&text, radix)?;
            out.push(parsed);
        }

        if out.len() == 1 {
            Ok(out.remove(0))
        } else {
            Ok(Value::array(out))
        }
    }

    /// Implement :N[list] radix list notation.
    /// First arg is the base (Int), remaining args are digit values or "." for fractional separator.
    pub(super) fn builtin_radix_list(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new(
                "RADIX_LIST expects a base and at least one element",
            ));
        }
        let base = args[0].to_bigint();
        if base < num_bigint::BigInt::from(2_i64) {
            return Err(RuntimeError::new("RADIX_LIST base must be an integer >= 2"));
        }

        // Split items into integer part and fractional part at the "." separator
        let items = &args[1..];
        let mut int_digits: Vec<i64> = Vec::new();
        let mut frac_digits: Vec<i64> = Vec::new();
        let mut saw_dot = false;

        for item in items {
            let s = item.to_string_value();
            if s == "." {
                saw_dot = true;
                continue;
            }
            let digit = item.to_f64() as i64;
            if saw_dot {
                frac_digits.push(digit);
            } else {
                int_digits.push(digit);
            }
        }

        if !saw_dot {
            // Pure integer: compute sum of digits[i] * base^(n-1-i)
            let mut result = num_bigint::BigInt::from(0_i64);
            for d in &int_digits {
                result = result * &base + num_bigint::BigInt::from(*d);
            }
            // Try to fit in i64
            if let Ok(n) = i64::try_from(&result) {
                Ok(Value::Int(n))
            } else {
                Ok(Value::bigint(result))
            }
        } else {
            // Has fractional part: compute as Rat
            let mut int_value = num_bigint::BigInt::from(0_i64);
            for d in &int_digits {
                int_value = int_value * &base + num_bigint::BigInt::from(*d);
            }
            let mut frac_value = num_bigint::BigInt::from(0_i64);
            for d in &frac_digits {
                frac_value = frac_value * &base + num_bigint::BigInt::from(*d);
            }
            let frac_scale = base.pow(frac_digits.len() as u32);
            let numerator = int_value * &frac_scale + frac_value;
            let denominator = frac_scale;
            Ok(crate::value::make_big_rat(numerator, denominator))
        }
    }
}

/// Check if a character (as a letter) represents a digit valid for the given base.
fn is_valid_digit_for_base(c: char, base: u32) -> bool {
    if let Some(v) = c.to_digit(36) {
        v < base
    } else {
        false
    }
}

/// Parse a string with a default radix, respecting prefix overrides (0b, 0o, 0x, 0d, :N<...>).
/// Used by the UNBASE builtin (:N('string') form).
/// A prefix like `0b` is only treated as a base override if the letter is NOT a valid digit
/// in the default base (e.g., `:16('0b1110')` treats b as hex digit, but `:10('0b1110')` uses binary).
fn unbase_parse_with_overrides(text: &str, default_radix: u32) -> Result<Value, RuntimeError> {
    let trimmed = text.trim();

    // Check for prefix overrides (only if the prefix letter is not a valid digit in default base)
    if let Some(after_zero) = trimmed.strip_prefix('0')
        && !after_zero.is_empty()
    {
        let prefix_char = after_zero.as_bytes()[0] as char;
        let lower = prefix_char.to_ascii_lowercase();
        if !is_valid_digit_for_base(lower, default_radix) {
            let rest = &after_zero[1..];
            match lower {
                'b' => {
                    if let Some(v) = crate::runtime::parse_radix_number_body(rest, 2) {
                        return Ok(v);
                    }
                }
                'o' => {
                    if let Some(v) = crate::runtime::parse_radix_number_body(rest, 8) {
                        return Ok(v);
                    }
                }
                'x' => {
                    if let Some(v) = crate::runtime::parse_radix_number_body(rest, 16) {
                        return Ok(v);
                    }
                }
                'd' => {
                    if let Some(v) = crate::runtime::parse_radix_number_body(rest, 10) {
                        return Ok(v);
                    }
                }
                _ => {}
            }
        }
    }

    // Check for :N<...> override (always takes priority regardless of default base)
    if let Some(after_colon) = trimmed.strip_prefix(':') {
        let digit_end = after_colon
            .chars()
            .take_while(|c| c.is_ascii_digit())
            .count();
        if digit_end > 0 {
            let base_str = &after_colon[..digit_end];
            let rest = &after_colon[digit_end..];
            if let Ok(base) = base_str.parse::<u32>()
                && (2..=36).contains(&base)
                && let Some(rest) = rest.strip_prefix('<')
                && let Some(close) = rest.find('>')
            {
                let body = &rest[..close];
                if let Some(v) = crate::runtime::parse_radix_number_body(body, base) {
                    return Ok(v);
                }
            }
        }
    }

    // Default: parse using the given radix
    if let Some(v) = crate::runtime::parse_radix_number_body(trimmed, default_radix) {
        return Ok(v);
    }

    Err(RuntimeError::new(format!(
        "Cannot parse '{}' as base {}",
        text, default_radix
    )))
}
