use crate::value::Value;
use num_bigint::BigInt;

pub(crate) fn format_sprintf(fmt: &str, arg: Option<&Value>) -> String {
    match arg {
        Some(value) => format_sprintf_args(fmt, std::slice::from_ref(value)),
        None => format_sprintf_args(fmt, &[]),
    }
}

pub(crate) fn format_sprintf_args(fmt: &str, args: &[Value]) -> String {
    let mut chars = fmt.chars().peekable();
    let mut out = String::new();
    let mut arg_index = 0usize;
    while let Some(c) = chars.next() {
        if c != '%' {
            out.push(c);
            continue;
        }
        if chars.peek() == Some(&'%') {
            chars.next();
            out.push('%');
            continue;
        }
        let mut flags = String::new();
        while let Some(f) = chars.peek().copied() {
            if f == '-' || f == '+' || f == ' ' || f == '#' || f == '0' {
                flags.push(f);
                chars.next();
            } else {
                break;
            }
        }
        let mut width = String::new();
        if chars.peek() == Some(&'*') {
            // Width from argument
            chars.next();
            let w = match args.get(arg_index) {
                Some(Value::Int(i)) => *i as usize,
                Some(Value::Num(f)) => *f as usize,
                _ => 0,
            };
            arg_index += 1;
            width = w.to_string();
        } else {
            while let Some(d) = chars.peek().copied() {
                if d.is_ascii_digit() {
                    width.push(d);
                    chars.next();
                } else {
                    break;
                }
            }
        }
        let mut precision = String::new();
        let mut has_precision = false;
        if chars.peek() == Some(&'.') {
            chars.next();
            has_precision = true;
            if chars.peek() == Some(&'*') {
                // Variable precision from argument
                chars.next();
                let p = match args.get(arg_index) {
                    Some(Value::Int(i)) => *i as usize,
                    Some(Value::Num(f)) => *f as usize,
                    _ => 0,
                };
                arg_index += 1;
                precision = p.to_string();
            } else {
                while let Some(d) = chars.peek().copied() {
                    if d.is_ascii_digit() {
                        precision.push(d);
                        chars.next();
                    } else {
                        break;
                    }
                }
            }
        }
        let spec = chars.next().unwrap_or('s');
        let width_num = width.parse::<usize>().unwrap_or(0);
        let prec_num = if has_precision {
            Some(precision.parse::<usize>().unwrap_or(0))
        } else {
            None
        };
        // The 0 flag is ignored when:
        // - the '-' flag is also present, or
        // - precision is specified for %s or integer specifiers (b/B/d/i/o/x/X/u)
        let prec_cancels_zero = prec_num.is_some()
            && matches!(spec, 's' | 'b' | 'B' | 'd' | 'i' | 'o' | 'x' | 'X' | 'u');
        let zero_pad = flags.contains('0') && !flags.contains('-') && !prec_cancels_zero;
        let left_align = flags.contains('-');
        // In Raku, for float specifiers (f/F/e/E), when both '-' and '0' flags are
        // present, zero-padding takes priority over left-align. Additionally, if the
        // formatted number has no sign prefix, one leading zero is shifted from the
        // integer part to the fractional part (precision is effectively incremented).
        let float_minus_zero =
            matches!(spec, 'f' | 'F' | 'e' | 'E') && flags.contains('-') && flags.contains('0');
        let plus_sign = flags.contains('+');
        let space_flag = flags.contains(' ');
        let hash_flag = flags.contains('#');
        let arg = args.get(arg_index);
        arg_index += 1;
        let int_val = || match arg {
            Some(Value::Int(i)) => *i,
            Some(Value::BigInt(bi)) => {
                num_traits::ToPrimitive::to_i64(bi.as_ref()).unwrap_or_else(|| {
                    if bi.sign() == num_bigint::Sign::Minus {
                        i64::MIN
                    } else {
                        i64::MAX
                    }
                })
            }
            Some(Value::Num(f)) => *f as i64,
            Some(Value::Rat(n, d)) if *d != 0 => *n / *d,
            Some(Value::FatRat(n, d)) if *d != 0 => *n / *d,
            Some(Value::BigRat(n, d)) if *d != num_bigint::BigInt::from(0) => {
                use num_traits::ToPrimitive;
                (n / d).to_i64().unwrap_or(0)
            }
            Some(Value::Str(s)) => s.trim().parse::<i64>().unwrap_or(0),
            Some(Value::Bool(b)) => {
                if *b {
                    1
                } else {
                    0
                }
            }
            _ => 0,
        };
        let bigint_val = || match arg {
            Some(Value::BigInt(bi)) => (**bi).clone(),
            Some(Value::Int(i)) => BigInt::from(*i),
            Some(Value::Num(f)) => BigInt::from(*f as i64),
            Some(Value::Rat(n, d)) if *d != 0 => BigInt::from(*n / *d),
            Some(Value::FatRat(n, d)) if *d != 0 => BigInt::from(*n / *d),
            Some(Value::BigRat(n, d)) if *d != num_bigint::BigInt::from(0) => n / d,
            Some(Value::Str(s)) => s
                .trim()
                .parse::<BigInt>()
                .unwrap_or_else(|_| BigInt::from(0)),
            Some(Value::Bool(true)) => BigInt::from(1),
            Some(Value::Bool(false)) => BigInt::from(0),
            _ => BigInt::from(0),
        };
        let float_val = || match arg {
            Some(Value::Int(i)) => *i as f64,
            Some(Value::Num(f)) => *f,
            Some(Value::Rat(n, d)) if *d != 0 => *n as f64 / *d as f64,
            Some(Value::FatRat(n, d)) if *d != 0 => *n as f64 / *d as f64,
            Some(Value::BigRat(n, d)) if *d != num_bigint::BigInt::from(0) => {
                use num_traits::ToPrimitive;
                let result = n * BigInt::from(1_000_000_000i64) / d;
                result.to_f64().unwrap_or(0.0) / 1_000_000_000.0
            }
            Some(Value::Str(s)) => s.trim().parse::<f64>().unwrap_or(0.0),
            _ => 0.0,
        };
        let rendered = match spec {
            's' => match arg {
                Some(v) => {
                    let s = v.to_string_value();
                    if let Some(p) = prec_num {
                        s.chars().take(p).collect()
                    } else {
                        s
                    }
                }
                _ => String::new(),
            },
            'd' | 'i' => {
                let i = bigint_val();
                let is_neg = i < BigInt::from(0);
                let prefix = sign_prefix(is_neg, plus_sign, space_flag);
                let mut abs = if is_neg {
                    format!("{}", -&i)
                } else {
                    format!("{}", i)
                };
                // Apply precision: minimum number of digits
                if let Some(p) = prec_num {
                    if p == 0 && abs == "0" {
                        abs.clear();
                    } else if abs.len() < p {
                        abs = format!("{:0>width$}", abs, width = p);
                    }
                }
                if abs.is_empty() && prefix.is_empty() {
                    String::new()
                } else {
                    format!("{}{}", prefix, abs)
                }
            }
            'u' => {
                let i = bigint_val();
                if i < BigInt::from(0) {
                    "0".to_string()
                } else {
                    i.to_str_radix(10)
                }
            }
            'x' | 'X' => {
                let i = bigint_val();
                let is_neg = i < BigInt::from(0);
                let is_zero = i == BigInt::from(0);
                let abs_val = if is_neg { -&i } else { i };
                let mut digits = abs_val.to_str_radix(16);
                if spec == 'X' {
                    digits = digits.to_uppercase();
                }
                if let Some(p) = prec_num {
                    if p == 0 && digits == "0" {
                        digits.clear();
                    } else if digits.len() < p {
                        digits = format!("{:0>width$}", digits, width = p);
                    }
                }
                if digits.is_empty() {
                    String::new()
                } else {
                    // Raku: hash prefix goes before the sign for %x/%X
                    let neg_sign = if is_neg { "-" } else { "" };
                    let hash_prefix = if hash_flag && !is_zero {
                        if spec == 'X' { "0X" } else { "0x" }
                    } else {
                        ""
                    };
                    format!("{}{}{}", hash_prefix, neg_sign, digits)
                }
            }
            'o' => {
                let i = bigint_val();
                let is_neg = i < BigInt::from(0);
                let is_zero = i == BigInt::from(0);
                let abs_val = if is_neg { -&i } else { i };
                let mut digits = abs_val.to_str_radix(8);
                if let Some(p) = prec_num {
                    if p == 0 && digits == "0" {
                        digits.clear();
                    } else if digits.len() < p {
                        digits = format!("{:0>width$}", digits, width = p);
                    }
                }
                if digits.is_empty() {
                    String::new()
                } else {
                    let neg_sign = if is_neg { "-" } else { "" };
                    // %#o: for non-negative, ensure leading "0" (C-style).
                    // For negative, prefix "0" goes before the minus sign.
                    if hash_flag && !is_zero {
                        if is_neg {
                            format!("0{}{}", neg_sign, digits)
                        } else if !digits.starts_with('0') {
                            format!("0{}", digits)
                        } else {
                            digits
                        }
                    } else {
                        format!("{}{}", neg_sign, digits)
                    }
                }
            }
            'b' | 'B' => {
                let i = bigint_val();
                let is_neg = i < BigInt::from(0);
                let is_zero = i == BigInt::from(0);
                let sign = sign_prefix(is_neg, plus_sign, space_flag);
                let abs_val = if is_neg { -&i } else { i };
                let mut digits = abs_val.to_str_radix(2);
                // Apply precision: minimum number of binary digits
                if let Some(p) = prec_num {
                    if p == 0 && digits == "0" {
                        digits.clear();
                    } else if digits.len() < p {
                        digits = format!("{:0>width$}", digits, width = p);
                    }
                }
                // When precision makes digits empty, suppress all prefixes
                if digits.is_empty() {
                    String::new()
                } else {
                    // Hash prefix only for non-zero values
                    let hash_prefix = if hash_flag && !is_zero {
                        if spec == 'B' { "0B" } else { "0b" }
                    } else {
                        ""
                    };
                    format!("{}{}{}", sign, hash_prefix, digits)
                }
            }
            'f' | 'F' => {
                let f = float_val();
                if f.is_infinite() || f.is_nan() {
                    format_inf_nan(f, plus_sign, space_flag)
                } else {
                    let p = prec_num.unwrap_or(6);
                    let is_neg = f.is_sign_negative() && (f != 0.0 || f.is_sign_negative());
                    let prefix = sign_prefix(is_neg, plus_sign, space_flag);
                    let abs = f.abs();
                    format!("{}{:.*}", prefix, p, abs)
                }
            }
            'e' | 'E' => {
                let f = float_val();
                if f.is_infinite() || f.is_nan() {
                    format_inf_nan(f, plus_sign, space_flag)
                } else {
                    let p = prec_num.unwrap_or(6);
                    let is_neg = f.is_sign_negative() && (f != 0.0 || f.is_sign_negative());
                    let prefix = sign_prefix(is_neg, plus_sign, space_flag);
                    let abs = f.abs();
                    let formatted = if spec == 'e' {
                        normalize_sci_exponent(&format!("{:.*e}", p, abs))
                    } else {
                        normalize_sci_exponent(&format!("{:.*E}", p, abs))
                    };
                    format!("{}{}", prefix, formatted)
                }
            }
            'g' | 'G' => {
                let f = float_val();
                if f.is_infinite() || f.is_nan() {
                    format_inf_nan(f, plus_sign, space_flag)
                } else {
                    let is_neg = f.is_sign_negative() && (f != 0.0 || f.is_sign_negative());
                    let prefix = sign_prefix(is_neg, plus_sign, space_flag);
                    let abs = f.abs();
                    let p = prec_num.unwrap_or(6).max(1);
                    let formatted = format_g(abs, p, spec == 'G', hash_flag);
                    format!("{}{}", prefix, formatted)
                }
            }
            'c' => {
                let i = int_val();
                char::from_u32(i as u32).unwrap_or('\0').to_string()
            }
            _ => match arg {
                Some(v) => v.to_string_value(),
                None => String::new(),
            },
        };
        // For %o/%x/%X, zero-padding is plain left-fill (no prefix splitting)
        let plain_zero = matches!(spec, 'o' | 'x' | 'X');
        if float_minus_zero && width_num > 0 {
            // Raku quirk: -0 combo on floats uses zero-padding (0 wins over -)
            // then shifts one leading zero into the fractional part when no sign prefix
            apply_float_minus_zero(&mut out, &rendered, width_num);
        } else {
            apply_width(
                &mut out, &rendered, width_num, left_align, zero_pad, plain_zero,
            );
        }
    }
    out
}

/// Format a non-negative float using %g/%G rules:
/// - Use %e/%E if exponent < -4 or >= precision
/// - Otherwise use %f
/// - Precision specifies total significant digits
/// - Trailing zeros are removed (unless # flag is set)
fn format_g(abs: f64, prec: usize, upper: bool, hash_flag: bool) -> String {
    // Determine exponent
    let exp = if abs == 0.0 {
        0i32
    } else {
        abs.log10().floor() as i32
    };
    let use_sci = exp < -4 || exp >= prec as i32;
    if use_sci {
        // Scientific notation with (prec-1) decimal digits
        let decimal_digits = prec.saturating_sub(1);
        let raw = format!("{:.*e}", decimal_digits, abs);
        let normalized = normalize_sci_exponent(&raw);
        // Replace 'e' with correct case
        let normalized = if upper {
            normalized.replacen('e', "E", 1)
        } else {
            normalized
        };
        if hash_flag {
            normalized
        } else {
            strip_trailing_zeros_sci(&normalized)
        }
    } else {
        // Fixed notation with (prec - 1 - exp) decimal places
        let decimal_digits = if prec as i32 > exp + 1 {
            (prec as i32 - exp - 1) as usize
        } else {
            0
        };
        let raw = format!("{:.*}", decimal_digits, abs);
        if hash_flag {
            raw
        } else {
            strip_trailing_zeros_fixed(&raw)
        }
    }
}

/// Strip trailing zeros after decimal point in fixed notation.
/// "3.1400" -> "3.14", "3.0" -> "3", "100" -> "100"
fn strip_trailing_zeros_fixed(s: &str) -> String {
    if s.contains('.') {
        let trimmed = s.trim_end_matches('0');
        if let Some(without_dot) = trimmed.strip_suffix('.') {
            without_dot.to_string()
        } else {
            trimmed.to_string()
        }
    } else {
        s.to_string()
    }
}

/// Strip trailing zeros in scientific notation mantissa.
/// "3.1400e+02" -> "3.14e+02", "3.0000e+02" -> "3e+02"
fn strip_trailing_zeros_sci(s: &str) -> String {
    let e_pos = s.rfind('e').or_else(|| s.rfind('E'));
    if let Some(pos) = e_pos {
        let mantissa = &s[..pos];
        let exp_part = &s[pos..];
        if mantissa.contains('.') {
            let trimmed = mantissa.trim_end_matches('0');
            if let Some(without_dot) = trimmed.strip_suffix('.') {
                format!("{}{}", without_dot, exp_part)
            } else {
                format!("{}{}", trimmed, exp_part)
            }
        } else {
            s.to_string()
        }
    } else {
        s.to_string()
    }
}

/// Format Inf/NaN values consistently across all format specifiers.
/// Raku always uses "Inf", "-Inf", "NaN" regardless of %e/%E/%f/%g/%G.
fn format_inf_nan(f: f64, plus_sign: bool, space_flag: bool) -> String {
    if f.is_nan() {
        "NaN".to_string()
    } else {
        let is_neg = f.is_sign_negative();
        let prefix = if is_neg {
            "-"
        } else if plus_sign {
            "+"
        } else if space_flag {
            " "
        } else {
            ""
        };
        format!("{}Inf", prefix)
    }
}

/// Returns the sign prefix for a numeric value.
/// - Negative: "-"
/// - Positive with plus_sign flag: "+"
/// - Positive with space_flag (and no plus_sign): " "
/// - Otherwise: ""
fn sign_prefix(is_neg: bool, plus_sign: bool, space_flag: bool) -> &'static str {
    if is_neg {
        "-"
    } else if plus_sign {
        "+"
    } else if space_flag {
        " "
    } else {
        ""
    }
}

/// Apply width formatting with proper zero-padding.
/// When `plain_zero` is true, zero-padding is simple left-fill (for %o/%x/%X).
/// When false, sign and base prefix are placed before the zeros (for %b/%B/%d/etc).
fn apply_width(
    out: &mut String,
    rendered: &str,
    width_num: usize,
    left_align: bool,
    zero_pad: bool,
    plain_zero: bool,
) {
    let rendered_width = rendered.chars().count();
    if width_num > rendered_width {
        let pad_len = width_num - rendered_width;
        if left_align {
            out.push_str(rendered);
            for _ in 0..pad_len {
                out.push(' ');
            }
        } else if zero_pad {
            if plain_zero {
                // Simple left zero-fill, no prefix splitting
                for _ in 0..pad_len {
                    out.push('0');
                }
                out.push_str(rendered);
            } else {
                // Sign and base prefix go before zeros.
                let prefix_len = zero_pad_prefix_len(rendered);
                if prefix_len > 0 {
                    out.push_str(&rendered[..prefix_len]);
                    for _ in 0..pad_len {
                        out.push('0');
                    }
                    out.push_str(&rendered[prefix_len..]);
                } else {
                    for _ in 0..pad_len {
                        out.push('0');
                    }
                    out.push_str(rendered);
                }
            }
        } else {
            for _ in 0..pad_len {
                out.push(' ');
            }
            out.push_str(rendered);
        }
    } else {
        out.push_str(rendered);
    }
}

/// Determine the length of the prefix (sign + base indicator) that should
/// appear before zero-padding.  E.g. "+0b" → 3, "-" → 1, "0x" → 2, "" → 0.
fn zero_pad_prefix_len(s: &str) -> usize {
    let bytes = s.as_bytes();
    let mut pos = 0;
    // Optional sign character
    if matches!(bytes.first(), Some(b'+' | b'-' | b' ')) {
        pos = 1;
    }
    // Optional base prefix: 0b, 0B, 0x, 0X, 0o, 0O
    if pos + 1 < bytes.len() && bytes[pos] == b'0' {
        let next = bytes[pos + 1];
        if next == b'b'
            || next == b'B'
            || next == b'x'
            || next == b'X'
            || next == b'o'
            || next == b'O'
        {
            pos += 2;
        }
    }
    pos
}

/// Handle Raku's quirky behavior when both '-' and '0' flags are present on float specifiers.
/// Zero-padding takes priority over left-align, and if the number has no sign prefix,
/// one leading zero is shifted from the integer part to the fractional part.
fn apply_float_minus_zero(out: &mut String, rendered: &str, width: usize) {
    let len = rendered.chars().count();
    if width <= len {
        out.push_str(rendered);
        return;
    }
    let pad_len = width - len;
    let has_sign = matches!(
        rendered.as_bytes().first(),
        Some(b'+') | Some(b'-') | Some(b' ')
    );

    // First, apply zero-padding (same as apply_width with zero_pad=true)
    let mut padded = String::with_capacity(width);
    if has_sign {
        padded.push(rendered.chars().next().unwrap());
        for _ in 0..pad_len {
            padded.push('0');
        }
        padded.push_str(&rendered[1..]);
    } else {
        for _ in 0..pad_len {
            padded.push('0');
        }
        padded.push_str(rendered);
    }

    // Then, if no sign prefix, shift one leading zero to the fractional part.
    // This effectively increments precision by 1 while keeping total width the same.
    if !has_sign && should_shift_zero(&padded) {
        // Remove leading zero, append '0' to fractional part
        let mut shifted = String::with_capacity(width);
        shifted.push_str(&padded[1..]);
        shifted.push('0');
        out.push_str(&shifted);
        return;
    }

    out.push_str(&padded);
}

/// Check if a zero-padded float string has a leading zero that can be shifted
/// to the fractional part (e.g., "0001.00" -> true, "1.00" -> false).
fn should_shift_zero(padded: &str) -> bool {
    if let Some(dot_pos) = padded.find('.') {
        let int_part = &padded[..dot_pos];
        int_part.len() >= 2 && int_part.starts_with('0')
    } else {
        false
    }
}

/// Normalize Rust scientific notation (e.g. `1.5e1`) to C-style (`1.5e+01`).
/// Ensures the exponent has an explicit sign and at least two digits.
fn normalize_sci_exponent(s: &str) -> String {
    let e_marker = if s.contains('E') { 'E' } else { 'e' };
    if let Some(pos) = s.rfind(e_marker) {
        let (mantissa, exp_part) = s.split_at(pos);
        let exp_str = &exp_part[1..]; // skip 'e'/'E'
        let (sign, digits) = if let Some(d) = exp_str.strip_prefix('-') {
            ("-", d)
        } else if let Some(d) = exp_str.strip_prefix('+') {
            ("+", d)
        } else {
            ("+", exp_str)
        };
        let exp_num: i32 = digits.parse().unwrap_or(0);
        format!(
            "{}{}{}{:02}",
            mantissa,
            e_marker,
            sign,
            exp_num.unsigned_abs()
        )
    } else {
        s.to_string()
    }
}
