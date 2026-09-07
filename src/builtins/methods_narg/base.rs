use crate::value::{RuntimeError, Value, ValueView};

pub(crate) fn parse_radix_checked(arg: &Value) -> Option<Result<u32, RuntimeError>> {
    match arg.view() {
        ValueView::Int(r) => {
            if (2..=36).contains(&r) {
                Some(Ok(r as u32))
            } else {
                Some(Err(RuntimeError::new(
                    "X::OutOfRange: base requires radix 2..36",
                )))
            }
        }
        ValueView::Str(s) => match s.parse::<u32>() {
            Ok(r) if (2..=36).contains(&r) => Some(Ok(r)),
            Ok(_) => Some(Err(RuntimeError::new(
                "X::OutOfRange: base requires radix 2..36",
            ))),
            _ => None,
        },
        _ => None,
    }
}

/// Specifies how many fractional digits to produce in base conversion.
pub(crate) enum BaseDigits {
    /// Auto-detect: scale to denominator size, minimum 6 (default for 1-arg .base)
    Auto,
    /// Exact number of digits (from explicit integer argument)
    Fixed(u32),
    /// Whatever: produce digits until remainder is 0 (no rounding)
    Whatever,
}

/// Convert a rational number n/d to a string in the given base.
pub(crate) fn rat_to_base(n: i64, d: i64, radix: u32, digits_mode: BaseDigits) -> String {
    if d == 0 {
        return if n > 0 {
            "Inf".to_string()
        } else if n < 0 {
            "-Inf".to_string()
        } else {
            "NaN".to_string()
        };
    }
    let negative = (n < 0) != (d < 0);
    let mut num = (n as i128).unsigned_abs();
    let den = (d as i128).unsigned_abs();
    let radix = radix as u128;

    let int_part = num / den;
    num %= den;

    let mut result = if negative {
        "-".to_string()
    } else {
        String::new()
    };
    result.push_str(&int_to_base(int_part as u64, radix as u32));

    if num == 0 {
        if let BaseDigits::Fixed(digits) = digits_mode
            && digits > 0
        {
            result.push('.');
            for _ in 0..digits {
                result.push('0');
            }
        }
        return result;
    }

    // Whatever mode: produce exact digits until remainder is 0
    if matches!(digits_mode, BaseDigits::Whatever) {
        result.push('.');
        let digit_chars = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        // Safety limit to prevent infinite loops for non-terminating fractions
        for _ in 0..100000 {
            num *= radix;
            let digit = num / den;
            result.push(digit_chars[digit as usize] as char);
            num %= den;
            if num == 0 {
                break;
            }
        }
        return result;
    }

    let limit = match digits_mode {
        BaseDigits::Fixed(d) => d,
        BaseDigits::Auto => {
            // For Rat without explicit digits, scale to denominator size with minimum of 6
            let log_den = (den as f64).log(radix as f64).ceil() as u32;
            std::cmp::max(6, log_den)
        }
        BaseDigits::Whatever => unreachable!(),
    };
    if limit == 0 {
        // Round integer part: if fractional part >= 0.5, round up
        if num * 2 >= den {
            // Increment the integer part and rebuild
            let rounded_int = int_part + 1;
            let mut rounded_result = if negative {
                "-".to_string()
            } else {
                String::new()
            };
            rounded_result.push_str(&int_to_base(rounded_int as u64, radix as u32));
            return rounded_result;
        }
        return result;
    }

    result.push('.');
    let digit_chars = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    let mut frac_digits = Vec::new();
    let is_auto = matches!(digits_mode, BaseDigits::Auto);
    for _ in 0..limit {
        num *= radix;
        let digit = num / den;
        frac_digits.push(digit as u8);
        num %= den;
        if num == 0 && is_auto {
            break;
        }
    }
    // Round last digit if we have a remainder
    let mut int_carry = false;
    if num > 0 && !frac_digits.is_empty() && num * 2 >= den {
        let mut carry = true;
        for d in frac_digits.iter_mut().rev() {
            if carry {
                *d += 1;
                if *d >= radix as u8 {
                    *d = 0;
                } else {
                    carry = false;
                }
            }
        }
        if carry {
            // Carry propagated past all fractional digits (e.g., 0.9 -> 1.0)
            int_carry = true;
        }
    }
    if int_carry {
        // Rebuild result with incremented integer part
        let rounded_int = int_part + 1;
        result.clear();
        if negative {
            result.push('-');
        }
        result.push_str(&int_to_base(rounded_int as u64, radix as u32));
        result.push('.');
        for _ in 0..frac_digits.len() {
            result.push('0');
        }
    } else {
        for d in &frac_digits {
            result.push(digit_chars[*d as usize] as char);
        }
    }
    result
}

/// `.base($radix, $digits?, :no-trailing-zeroes)`.
///
/// The adverb is Rakudo's, and it is declared on **`Rational.base` only**:
/// `255.base(16, 4, :no-trailing-zeroes)` is still `"FF.0000"` and
/// `2.5e0.base(10, 5, :no-trailing-zeroes)` still `"2.50000"`, while
/// `0.5.base(10, 5, :no-trailing-zeroes)` is `"0.5"` and
/// `1.0.base(10, 5, :no-trailing-zeroes)` is `"1"` -- when every fractional
/// digit goes, so does the radix point (all measured against `raku`).
///
/// This is an interceptor, in front of the arity cascade, for the reason the
/// other `native_*_with_options` helpers are: the cascade picks its arm by
/// argument count, so the three-argument call never reaches a 2-ary `base`.
/// Returns `None` when the adverb is absent, so the ordinary forms keep their
/// existing arms.
pub(crate) fn native_base_with_options(
    target: &Value,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    let mut no_trailing_zeroes = None;
    let mut positional: Vec<&Value> = Vec::with_capacity(args.len());
    for arg in args {
        if arg.is_string_pair_value()
            && let ValueView::Pair(key, value) = arg.view()
            && key.as_str() == "no-trailing-zeroes"
        {
            no_trailing_zeroes = Some(value.truthy());
            continue;
        }
        positional.push(arg);
    }
    let strip = no_trailing_zeroes?;
    let radix = parse_radix_checked(positional.first().copied()?)?;
    let radix = match radix {
        Ok(r) => r,
        Err(e) => return Some(Err(e)),
    };
    let digits_mode = match positional.get(1).copied().map(Value::view) {
        None => BaseDigits::Auto,
        Some(ValueView::Int(d)) if d >= 0 => BaseDigits::Fixed(d as u32),
        Some(ValueView::Whatever) => BaseDigits::Whatever,
        Some(_) => return None,
    };
    if positional.len() > 2 {
        return None;
    }
    // Only a Rational honours the adverb; everything else keeps the plain
    // answer, which is what the ordinary arms already produce.
    let (n, d, rational) = match target.view() {
        ValueView::Rat(n, d) | ValueView::FatRat(n, d) => (n, d, true),
        ValueView::Int(i) => (i, 1, false),
        ValueView::Num(f) => {
            let (n, d) = f64_to_rat(f);
            (n, d, false)
        }
        _ => return None,
    };
    let rendered = rat_to_base(n, d, radix, digits_mode);
    if !(strip && rational) {
        return Some(Ok(Value::str(rendered)));
    }
    Some(Ok(Value::str(strip_trailing_base_zeroes(&rendered))))
}

/// Drop the trailing zero digits of a base-rendered fraction, and the radix
/// point with them when nothing is left after it.
fn strip_trailing_base_zeroes(rendered: &str) -> String {
    let Some(point) = rendered.find('.') else {
        return rendered.to_string();
    };
    let trimmed = rendered[point + 1..].trim_end_matches('0');
    if trimmed.is_empty() {
        rendered[..point].to_string()
    } else {
        format!("{}.{}", &rendered[..point], trimmed)
    }
}

fn int_to_base(mut n: u64, radix: u32) -> String {
    if n == 0 {
        return "0".to_string();
    }
    let digits = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    let mut buf = Vec::new();
    while n > 0 {
        buf.push(digits[(n % radix as u64) as usize]);
        n /= radix as u64;
    }
    buf.reverse();
    String::from_utf8(buf).unwrap()
}

/// Convert f64 to a rational approximation (numerator, denominator).
pub(crate) fn f64_to_rat(f: f64) -> (i64, i64) {
    if f.is_nan() {
        return (0, 0);
    }
    if f.is_infinite() {
        return if f > 0.0 { (1, 0) } else { (-1, 0) };
    }
    // Multiply by increasing powers of 10 to find exact fraction
    let negative = f < 0.0;
    let f = f.abs();
    let mut den: i64 = 1;
    let mut num = f;
    for _ in 0..18 {
        if (num - num.round()).abs() < 1e-10 {
            break;
        }
        num *= 10.0;
        den *= 10;
    }
    let n = num.round() as i64;
    // Simplify
    let g = gcd_u64(n.unsigned_abs(), den.unsigned_abs());
    let n = n / g as i64;
    let d = den / g as i64;
    if negative { (-n, d) } else { (n, d) }
}

fn gcd_u64(mut a: u64, mut b: u64) -> u64 {
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

/// Compute base-repeating for a rational number.
/// Returns (non_repeating_part, repeating_part) as strings.
pub(crate) fn rat_base_repeating(n: i64, d: i64, radix: u32) -> (String, String) {
    use std::collections::HashMap;

    if d == 0 {
        return (
            if n > 0 {
                "Inf".to_string()
            } else if n < 0 {
                "-Inf".to_string()
            } else {
                "NaN".to_string()
            },
            String::new(),
        );
    }
    let negative = (n < 0) != (d < 0);
    let mut num = (n as i128).unsigned_abs();
    let den = (d as i128).unsigned_abs();
    let radix128 = radix as u128;

    let int_part = num / den;
    num %= den;

    let mut prefix = if negative {
        "-".to_string()
    } else {
        String::new()
    };
    prefix.push_str(&int_to_base(int_part as u64, radix));

    if num == 0 {
        return (prefix, String::new());
    }

    prefix.push('.');
    let digit_chars = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

    // Track remainders to find the cycle
    let mut remainder_positions: HashMap<u128, usize> = HashMap::new();
    let mut frac_digits = Vec::new();

    loop {
        if num == 0 {
            // Exact representation, no repeating part
            let non_rep: String = frac_digits
                .iter()
                .map(|&d| digit_chars[d] as char)
                .collect();
            return (format!("{}{}", prefix, non_rep), String::new());
        }
        if let Some(&pos) = remainder_positions.get(&num) {
            // Found a cycle
            let non_rep: String = frac_digits[..pos]
                .iter()
                .map(|&d| digit_chars[d] as char)
                .collect();
            let rep: String = frac_digits[pos..]
                .iter()
                .map(|&d| digit_chars[d] as char)
                .collect();
            return (format!("{}{}", prefix, non_rep), rep);
        }
        remainder_positions.insert(num, frac_digits.len());
        num *= radix128;
        let digit = (num / den) as usize;
        frac_digits.push(digit);
        num %= den;
    }
}

/// Fast path for `.pick(n)` on integer Range types.
/// Returns None if target is not an integer range (caller should fall back).
pub(crate) fn range_pick_n_fast(target: &Value, arg: &Value) -> Option<Value> {
    use crate::builtins::methods_0arg::dispatch_core_range::{
        generic_range_pick_n, range_pick_n_i64,
    };

    // Determine effective inclusive bounds
    let (start, end, is_generic, generic_start, generic_end, excl_start, excl_end) =
        match target.view() {
            ValueView::Range(a, b) => (a, b, false, None, None, false, false),
            ValueView::RangeExcl(a, b) => (a, b - 1, false, None, None, false, false),
            ValueView::RangeExclStart(a, b) => (a + 1, b, false, None, None, false, false),
            ValueView::RangeExclBoth(a, b) => (a + 1, b - 1, false, None, None, false, false),
            ValueView::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => (
                0,
                0,
                true,
                Some(start.clone()),
                Some(end.clone()),
                excl_start,
                excl_end,
            ),
            _ => return None,
        };

    // Determine count from arg
    let is_whatever = matches!(arg.view(), ValueView::Whatever)
        || matches!(arg.view(), ValueView::Num(f) if f.is_infinite() && f.is_sign_positive());

    if is_generic {
        let gs = generic_start.as_ref().unwrap();
        let ge = generic_end.as_ref().unwrap();
        // Check if endpoints are integer-like
        if !matches!(
            gs.as_ref().view(),
            ValueView::Int(_) | ValueView::BigInt(_) | ValueView::Bool(_)
        ) || !matches!(
            ge.as_ref().view(),
            ValueView::Int(_) | ValueView::BigInt(_) | ValueView::Bool(_)
        ) {
            return None;
        }

        if is_whatever {
            // pick(*) on a huge range — cannot materialize, return what we can
            // Actually for GenericRange with BigInt endpoints, pick(*) means return all
            // elements shuffled. For very large ranges this is impossible, so fall back.
            return None;
        }

        let count = match arg.view() {
            ValueView::Int(n) => n.max(0) as usize,
            ValueView::Num(f) => (f as i64).max(0) as usize,
            ValueView::Rat(n, d) if d != 0 => (n / d).max(0) as usize,
            ValueView::Str(s) => s.trim().parse::<i64>().unwrap_or(0).max(0) as usize,
            _ => return None,
        };

        if count == 0 {
            return Some(Value::seq(Vec::new()));
        }

        let items = generic_range_pick_n(gs, ge, excl_start, excl_end, count)?;
        Some(Value::seq(items))
    } else {
        if end < start {
            return Some(Value::seq(Vec::new()));
        }

        if is_whatever {
            // pick(*) — return all elements shuffled
            // Only feasible for ranges that fit in memory
            let range_size = (end as i128) - (start as i128) + 1;
            if range_size > 100_000_000 {
                // Too large to materialize, fall back
                return None;
            }
            let items = range_pick_n_i64(start, end, range_size as usize);
            return Some(Value::seq(items));
        }

        let count = match arg.view() {
            ValueView::Int(n) => n.max(0) as usize,
            ValueView::Num(f) => (f as i64).max(0) as usize,
            ValueView::Rat(n, d) if d != 0 => (n / d).max(0) as usize,
            ValueView::Str(s) => s.trim().parse::<i64>().unwrap_or(0).max(0) as usize,
            _ => return None,
        };

        if count == 0 {
            return Some(Value::seq(Vec::new()));
        }

        let items = range_pick_n_i64(start, end, count);
        Some(Value::seq(items))
    }
}
