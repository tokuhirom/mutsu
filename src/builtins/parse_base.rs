//! Implementation of the `parse-base` builtin (sub and method form).
//!
//! Parses a string in the given numeric base (2..36), supporting:
//! - Optional leading sign: `+`, `-`, or U+2212 (MINUS SIGN)
//! - Integer or fractional values (with `.` separator)
//! - ASCII digits/letters and Unicode decimal digits (Nd category)
//!
//! Errors thrown:
//! - `X::Syntax::Number::RadixOutOfRange` when radix is not in 2..36
//! - `X::Str::Numeric` for malformed input (with `source`, `pos`, `reason`)

#![allow(clippy::result_large_err)]

use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, make_big_rat};
use num_bigint::BigInt;
use num_traits::{One, Zero};
use std::collections::HashMap;

fn radix_out_of_range(radix: i64) -> RuntimeError {
    let msg = format!(
        "Radix must be in range 2..36, not {} (use :{}[...] notation for radices outside this range)",
        radix, radix
    );
    let mut attrs = HashMap::new();
    attrs.insert("radix".to_string(), Value::Int(radix));
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Syntax::Number::RadixOutOfRange"), attrs);
    let mut err = RuntimeError::new(&msg);
    err.exception = Some(Box::new(ex));
    err
}

fn str_numeric_error(source: &str, pos: usize, radix: i64) -> RuntimeError {
    let reason = format!("malformed base-{} number", radix);
    let msg = format!("Cannot convert string to number: {}", reason);
    let mut attrs = HashMap::new();
    attrs.insert("source".to_string(), Value::str(source.to_string()));
    attrs.insert("pos".to_string(), Value::Int(pos as i64));
    attrs.insert("reason".to_string(), Value::str(reason.clone()));
    attrs.insert("target-name".to_string(), Value::str("Numeric".to_string()));
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Str::Numeric"), attrs);
    let mut err = RuntimeError::new(&msg);
    err.exception = Some(Box::new(ex));
    err
}

/// Convert a single character to its digit value in the given radix.
/// Returns Some(value) if the character is a valid digit, None otherwise.
fn char_digit_value(ch: char, radix: u32) -> Option<u32> {
    // ASCII fast path: 0-9, A-Z, a-z
    let v = if ch.is_ascii_digit() {
        (ch as u32) - ('0' as u32)
    } else if ch.is_ascii_uppercase() {
        (ch as u32) - ('A' as u32) + 10
    } else if ch.is_ascii_lowercase() {
        (ch as u32) - ('a' as u32) + 10
    } else if let Some(d) = crate::builtins::unicode::unicode_decimal_digit_value(ch) {
        // Unicode Nd character
        d
    } else {
        return None;
    };
    if v < radix { Some(v) } else { None }
}

/// Parse a string in the given base. The `source` parameter is used for error
/// reporting and should be the original string before any sign stripping.
pub(crate) fn parse_base(s: &str, radix: i64) -> Result<Value, RuntimeError> {
    if !(2..=36).contains(&radix) {
        return Err(radix_out_of_range(radix));
    }
    let radix_u = radix as u32;
    let source = s;

    // Iterate by characters, tracking position in CHAR units (not bytes).
    let chars: Vec<char> = s.chars().collect();
    let mut idx = 0usize;
    let mut negative = false;

    // Optional sign
    if idx < chars.len() {
        match chars[idx] {
            '+' => idx += 1,
            '-' | '\u{2212}' => {
                negative = true;
                idx += 1;
            }
            _ => {}
        }
    }

    if idx >= chars.len() {
        return Err(str_numeric_error(source, idx, radix));
    }

    let radix_big = BigInt::from(radix_u);

    // Integer part
    let int_start = idx;
    let mut int_val: BigInt = BigInt::zero();
    let mut int_digits = 0usize;
    while idx < chars.len() && chars[idx] != '.' {
        match char_digit_value(chars[idx], radix_u) {
            Some(d) => {
                int_val = &int_val * &radix_big + BigInt::from(d);
                int_digits += 1;
                idx += 1;
            }
            None => {
                return Err(str_numeric_error(source, idx, radix));
            }
        }
    }

    let mut has_frac = false;
    let mut frac_num: BigInt = BigInt::zero();
    let mut frac_den: BigInt = BigInt::one();

    if idx < chars.len() && chars[idx] == '.' {
        has_frac = true;
        idx += 1;
        let frac_start = idx;
        while idx < chars.len() {
            match char_digit_value(chars[idx], radix_u) {
                Some(d) => {
                    frac_num = &frac_num * &radix_big + BigInt::from(d);
                    frac_den *= &radix_big;
                    idx += 1;
                }
                None => {
                    return Err(str_numeric_error(source, idx, radix));
                }
            }
        }
        if idx == frac_start {
            // Trailing dot with no fractional digits — treat as malformed
            return Err(str_numeric_error(source, idx, radix));
        }
    }

    if int_digits == 0 && !has_frac {
        return Err(str_numeric_error(source, int_start, radix));
    }

    if has_frac {
        // Combine: int_val + frac_num/frac_den
        let combined_num = &int_val * &frac_den + &frac_num;
        let combined_num = if negative {
            -combined_num
        } else {
            combined_num
        };
        Ok(make_big_rat(combined_num, frac_den))
    } else {
        let val = if negative { -int_val } else { int_val };
        // Try to represent as Int (i64) if it fits, otherwise BigInt
        use num_traits::ToPrimitive;
        if let Some(n) = val.to_i64() {
            Ok(Value::Int(n))
        } else {
            Ok(Value::BigInt(std::sync::Arc::new(val)))
        }
    }
}
