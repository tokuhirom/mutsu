use crate::value::{Value, ValueView};
use num_bigint::BigInt;

use super::sprintf_helpers::{
    apply_float_minus_zero, apply_width, format_float_fixed, format_g, format_inf_nan,
    format_rat_fixed, format_rat_sci, normalize_sci_exponent, sign_prefix,
};
pub(crate) use super::sprintf_validate::{
    directives_count_message, sprintf_arg_specs, sprintf_directive_count,
    validate_sprintf_arg_types, validate_sprintf_directives,
};

/// Numify a string argument the way Raku does when a numeric directive (%d,
/// %x, %f, ...) is given a string: this honours radix prefixes (0x/0o/0b),
/// underscores and rationals, so sprintf("%d", "0x1F") is 31, not 0.
fn sprintf_numify_str(s: &str) -> Option<Value> {
    super::str_numeric::parse_raku_str_to_numeric(s.trim())
}

/// Whether the lexically-active language version is 6.e or later. Several
/// sprintf flag semantics changed in 6.e: the sign is emitted before the radix
/// prefix (`%#x` of -256 is `-0x100`, not `0x-100`), the `+`/space flags no
/// longer apply to radix conversions (`%b`/`%o`/`%x`), `#` forces a trailing
/// decimal point on `%e`/`%f` even at precision 0, and the `0` flag zero-pads
/// strings. The version is read the same way `$*RAKU.version` is (the parser
/// global reflects the active `use vX` pragma).
fn v6e_active() -> bool {
    let version = crate::parser::current_language_version();
    match version.strip_prefix("6.").and_then(|s| s.chars().next()) {
        Some(letter) => letter >= 'e',
        None => false,
    }
}

pub(crate) fn format_sprintf(fmt: &str, arg: Option<&Value>) -> String {
    match arg {
        Some(value) => format_sprintf_impl(fmt, std::slice::from_ref(value), false),
        None => format_sprintf_impl(fmt, &[], false),
    }
}

pub(crate) fn format_zprintf(fmt: &str, arg: Option<&Value>) -> String {
    match arg {
        Some(value) => format_sprintf_impl(fmt, std::slice::from_ref(value), true),
        None => format_sprintf_impl(fmt, &[], true),
    }
}

pub(crate) fn format_sprintf_args(fmt: &str, args: &[Value]) -> String {
    format_sprintf_impl(fmt, args, false)
}

pub(crate) fn format_zprintf_args(fmt: &str, args: &[Value]) -> String {
    format_sprintf_impl(fmt, args, true)
}

fn format_sprintf_impl(fmt: &str, args: &[Value], z_mode: bool) -> String {
    let bytes = fmt.as_bytes();
    let len = bytes.len();
    let mut out = String::new();
    let mut pos = 0usize;
    let mut arg_index = 0usize;
    let v6e = v6e_active();
    while pos < len {
        if bytes[pos] != b'%' {
            out.push(bytes[pos] as char);
            pos += 1;
            continue;
        }
        pos += 1; // skip '%'
        if pos < len && bytes[pos] == b'%' {
            out.push('%');
            pos += 1;
            continue;
        }
        // Check for positional argument specifier: N$ (digits followed by '$')
        let mut positional_arg: Option<usize> = None;
        {
            let start = pos;
            while pos < len && bytes[pos].is_ascii_digit() {
                pos += 1;
            }
            if pos > start && pos < len && bytes[pos] == b'$' {
                let n: usize = fmt[start..pos].parse().unwrap_or(1);
                positional_arg = Some(n - 1); // convert 1-based to 0-based
                pos += 1; // skip '$'
            } else {
                pos = start; // reset, not a positional specifier
            }
        }
        // Parse flags
        let mut flags = String::new();
        while pos < len {
            let b = bytes[pos];
            if b == b'-' || b == b'+' || b == b' ' || b == b'#' || b == b'0' {
                flags.push(b as char);
                pos += 1;
            } else {
                break;
            }
        }
        // Parse width
        let mut width = String::new();
        if pos < len && bytes[pos] == b'*' {
            pos += 1;
            let w = match args.get(arg_index).map(Value::view) {
                Some(ValueView::Int(i)) => i as usize,
                Some(ValueView::Num(f)) => f as usize,
                _ => 0,
            };
            arg_index += 1;
            width = w.to_string();
        } else {
            while pos < len && bytes[pos].is_ascii_digit() {
                width.push(bytes[pos] as char);
                pos += 1;
            }
        }
        // Parse precision
        let mut precision = String::new();
        let mut has_precision = false;
        if pos < len && bytes[pos] == b'.' {
            pos += 1;
            has_precision = true;
            if pos < len && bytes[pos] == b'*' {
                pos += 1;
                let p = match args.get(arg_index).map(Value::view) {
                    Some(ValueView::Int(i)) => i as usize,
                    Some(ValueView::Num(f)) => f as usize,
                    _ => 0,
                };
                arg_index += 1;
                precision = p.to_string();
            } else {
                while pos < len && bytes[pos].is_ascii_digit() {
                    precision.push(bytes[pos] as char);
                    pos += 1;
                }
            }
        }
        let spec = if pos < len {
            let s = bytes[pos] as char;
            pos += 1;
            s
        } else {
            's'
        };
        let width_num = width.parse::<usize>().unwrap_or(0);
        let prec_num = if has_precision {
            Some(precision.parse::<usize>().unwrap_or(0))
        } else {
            None
        };
        // In 6.e the `0` flag zero-pads strings even when a precision is given,
        // so precision no longer cancels the `0` flag for `%s`.
        let prec_cancels_zero = prec_num.is_some()
            && (matches!(spec, 'b' | 'B' | 'd' | 'i' | 'o' | 'x' | 'X' | 'u')
                || (spec == 's' && !v6e));
        // 6.e: for float specs the `0` flag beats `-` (zero-pads instead of
        // left-aligning), dropping the 6.d `apply_float_minus_zero` quirk.
        let zero_beats_minus = v6e
            && matches!(spec, 'f' | 'F' | 'e' | 'E')
            && flags.contains('0')
            && flags.contains('-');
        let zero_pad =
            flags.contains('0') && (!flags.contains('-') || zero_beats_minus) && !prec_cancels_zero;
        let left_align = flags.contains('-') && !zero_beats_minus;
        let float_minus_zero = !v6e
            && matches!(spec, 'f' | 'F' | 'e' | 'E')
            && flags.contains('-')
            && flags.contains('0');
        let plus_sign = flags.contains('+');
        let space_flag = flags.contains(' ');
        let hash_flag = flags.contains('#');
        // Determine which argument to use
        let effective_arg_index = if let Some(pa) = positional_arg {
            pa
        } else {
            let idx = arg_index;
            arg_index += 1;
            idx
        };
        let raw_arg = args.get(effective_arg_index);
        let _mixin_storage: Value;
        let arg = match raw_arg.map(Value::view) {
            Some(ValueView::Mixin(inner, _)) => {
                _mixin_storage = (**inner).clone();
                Some(&_mixin_storage)
            }
            _ => raw_arg,
        };
        let int_val = || match arg.map(Value::view) {
            Some(ValueView::Int(i)) => i,
            Some(ValueView::BigInt(bi)) => num_traits::ToPrimitive::to_i64(bi.as_ref())
                .unwrap_or_else(|| {
                    if bi.sign() == num_bigint::Sign::Minus {
                        i64::MIN
                    } else {
                        i64::MAX
                    }
                }),
            Some(ValueView::Num(f)) => f as i64,
            Some(ValueView::Rat(n, d)) if d != 0 => n / d,
            Some(ValueView::FatRat(n, d)) if d != 0 => n / d,
            Some(ValueView::BigRat(n, d)) if d != &num_bigint::BigInt::from(0) => {
                use num_traits::ToPrimitive;
                (n / d).to_i64().unwrap_or(0)
            }
            Some(ValueView::Str(s)) => match sprintf_numify_str(&s).as_ref().map(Value::view) {
                Some(ValueView::Int(i)) => i,
                Some(ValueView::BigInt(bi)) => {
                    num_traits::ToPrimitive::to_i64(bi.as_ref()).unwrap_or(0)
                }
                Some(ValueView::Num(f)) => f as i64,
                Some(ValueView::Rat(n, d)) if d != 0 => n / d,
                _ => s
                    .trim()
                    .parse::<i64>()
                    .unwrap_or_else(|_| s.trim().parse::<f64>().unwrap_or(0.0) as i64),
            },
            Some(ValueView::Bool(true)) => 1,
            Some(ValueView::Bool(false)) => 0,
            _ => 0,
        };
        let bigint_val = || match arg.map(Value::view) {
            Some(ValueView::BigInt(bi)) => (**bi).clone(),
            Some(ValueView::Int(i)) => BigInt::from(i),
            Some(ValueView::Num(f)) => BigInt::from(f as i64),
            Some(ValueView::Rat(n, d)) if d != 0 => BigInt::from(n / d),
            Some(ValueView::FatRat(n, d)) if d != 0 => BigInt::from(n / d),
            Some(ValueView::BigRat(n, d)) if d != &num_bigint::BigInt::from(0) => n / d,
            Some(ValueView::Str(s)) => match sprintf_numify_str(&s).as_ref().map(Value::view) {
                Some(ValueView::BigInt(bi)) => (**bi).clone(),
                Some(ValueView::Int(i)) => BigInt::from(i),
                Some(ValueView::Num(f)) => BigInt::from(f as i64),
                Some(ValueView::Rat(n, d)) if d != 0 => BigInt::from(n / d),
                _ => s.trim().parse::<BigInt>().unwrap_or_else(|_| {
                    BigInt::from(s.trim().parse::<f64>().unwrap_or(0.0) as i64)
                }),
            },
            Some(ValueView::Bool(true)) => BigInt::from(1),
            Some(ValueView::Bool(false)) => BigInt::from(0),
            _ => BigInt::from(0),
        };
        let float_val = || match arg.map(Value::view) {
            Some(ValueView::Int(i)) => i as f64,
            Some(ValueView::Num(f)) => f,
            Some(ValueView::Rat(n, d)) if d != 0 => n as f64 / d as f64,
            Some(ValueView::FatRat(n, d)) if d != 0 => n as f64 / d as f64,
            Some(ValueView::BigRat(n, d)) if d != &num_bigint::BigInt::from(0) => {
                use num_traits::ToPrimitive;
                let result = n * BigInt::from(1_000_000_000i64) / d;
                result.to_f64().unwrap_or(0.0) / 1_000_000_000.0
            }
            Some(ValueView::Str(s)) => match sprintf_numify_str(&s).as_ref().map(Value::view) {
                Some(ValueView::Int(i)) => i as f64,
                Some(ValueView::Num(f)) => f,
                Some(ValueView::Rat(n, d)) if d != 0 => n as f64 / d as f64,
                Some(ValueView::BigInt(bi)) => {
                    num_traits::ToPrimitive::to_f64(bi.as_ref()).unwrap_or(0.0)
                }
                _ => s.trim().parse::<f64>().unwrap_or(0.0),
            },
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
                    let neg_sign = if is_neg { "-" } else { "" };
                    let hash_prefix = if hash_flag && !is_zero {
                        if spec == 'X' { "0X" } else { "0x" }
                    } else {
                        ""
                    };
                    if v6e {
                        // 6.e: sign precedes the radix prefix (`-0x100`).
                        format!("{}{}{}", neg_sign, hash_prefix, digits)
                    } else {
                        format!("{}{}{}", hash_prefix, neg_sign, digits)
                    }
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
                    if v6e {
                        // 6.e: sign precedes the `0` prefix (`-0400`).
                        let prefix = if hash_flag && !is_zero && !digits.starts_with('0') {
                            "0"
                        } else {
                            ""
                        };
                        format!("{}{}{}", neg_sign, prefix, digits)
                    } else if hash_flag && !is_zero {
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
                // 6.e: the `+`/space flags do not apply to radix conversions.
                let sign = if v6e {
                    if is_neg { "-" } else { "" }
                } else {
                    sign_prefix(is_neg, plus_sign, space_flag)
                };
                let abs_val = if is_neg { -&i } else { i };
                let mut digits = abs_val.to_str_radix(2);
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
                    let hash_prefix = if hash_flag && !is_zero {
                        if spec == 'B' { "0B" } else { "0b" }
                    } else {
                        ""
                    };
                    format!("{}{}{}", sign, hash_prefix, digits)
                }
            }
            'f' | 'F' => {
                let p = prec_num.unwrap_or(6);
                // Use exact rational formatting (round-half-away-from-zero, like
                // Rakudo) for all numeric args; fall back to float only for
                // non-numeric arguments.
                let mut rendered =
                    if let Some(rendered) = format_rat_fixed(arg, p, plus_sign, space_flag) {
                        rendered
                    } else {
                        let f = float_val();
                        format_float_fixed(f, p, plus_sign, space_flag)
                    };
                // 6.e: `#` forces a trailing decimal point even at precision 0.
                if v6e
                    && hash_flag
                    && prec_num == Some(0)
                    && rendered.as_bytes().last().is_some_and(u8::is_ascii_digit)
                {
                    rendered.push('.');
                }
                rendered
            }
            'e' | 'E' => {
                let p = prec_num.unwrap_or(6);
                // Use exact rational formatting (exponent from the original
                // value, mantissa rounded half-away-from-zero and NOT re-
                // normalized on carry) for numeric args, matching Rakudo; fall
                // back to float only for non-numeric arguments and inf/nan.
                let mut rendered = if let Some(rendered) =
                    format_rat_sci(arg, p, plus_sign, space_flag, spec == 'E')
                {
                    rendered
                } else {
                    let f = float_val();
                    if f.is_infinite() || f.is_nan() {
                        format_inf_nan(f, plus_sign, space_flag)
                    } else {
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
                };
                // 6.e: `#` forces a decimal point before the exponent even at
                // precision 0 (`%#.0e` of 1 is `1.e+00`).
                if v6e && hash_flag && prec_num == Some(0) {
                    let marker = if spec == 'E' { 'E' } else { 'e' };
                    if let Some(m) = rendered.find(marker)
                        && !rendered[..m].contains('.')
                    {
                        rendered.insert(m, '.');
                    }
                }
                rendered
            }
            'g' | 'G' => {
                let f = float_val();
                if f.is_infinite() || f.is_nan() {
                    format_inf_nan(f, plus_sign, space_flag)
                } else if z_mode {
                    // zprintf %g/%G: precision means decimal places (like %f),
                    // uses e/E notation for large/small numbers.
                    // Switching threshold matches standard %g: exp < -4 or exp >= prec.
                    let p = prec_num.unwrap_or(6);
                    let is_neg = f.is_sign_negative() && (f != 0.0 || f.is_sign_negative());
                    let prefix = sign_prefix(is_neg, plus_sign, space_flag);
                    let abs = f.abs();
                    let exp = if abs == 0.0 {
                        0i32
                    } else {
                        abs.log10().floor() as i32
                    };
                    let use_sci = exp < -4 || exp >= p.max(1) as i32;
                    if use_sci {
                        // Scientific notation: format mantissa with p decimal places
                        let mantissa = abs / 10f64.powi(exp);
                        let formatted_mantissa = format!("{:.*}", p, mantissa);
                        let exp_sign = if exp >= 0 { '+' } else { '-' };
                        let exp_digits = format!("{:02}", exp.unsigned_abs());
                        // zprintf's round-trippable form uses the format letter
                        // (`g`/`G`) as the exponent separator. But once the field
                        // is wider than the natural output (so width padding
                        // applies), raku falls back to the standard `e`/`E`
                        // separator. Decide by comparing the natural length.
                        let natural_len = prefix.len()
                            + formatted_mantissa.len()
                            + 1 // exponent char
                            + 1 // exponent sign
                            + exp_digits.len();
                        let e_char = if width_num > natural_len {
                            if spec == 'G' { 'E' } else { 'e' }
                        } else if spec == 'G' {
                            'G'
                        } else {
                            'g'
                        };
                        format!(
                            "{}{}{}{}{}",
                            prefix, formatted_mantissa, e_char, exp_sign, exp_digits
                        )
                    } else {
                        // Fixed notation: p decimal places, then strip trailing
                        // zeros (and a bare trailing decimal point) like standard
                        // `%g` — so 0 renders as "0", not "0.000000". The `#`
                        // (alternate) flag keeps the trailing zeros.
                        let mut body = format!("{:.*}", p, abs);
                        if !hash_flag && body.contains('.') {
                            body.truncate(body.trim_end_matches('0').trim_end_matches('.').len());
                        }
                        format!("{prefix}{body}")
                    }
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
        // 6.d zero-pads o/x/X by prepending zeros before the whole (sign-first)
        // body. 6.e instead threads the sign/prefix out (handled by
        // `zero_pad_prefix_len`), so o/x/X drop out of `plain_zero`; strings,
        // which have no sign to thread, join it (zeros always prepend).
        let plain_zero = (matches!(spec, 'o' | 'x' | 'X') && !v6e) || (spec == 's' && v6e);
        if float_minus_zero && width_num > 0 {
            apply_float_minus_zero(&mut out, &rendered, width_num);
        } else {
            apply_width(
                &mut out, &rendered, width_num, left_align, zero_pad, plain_zero,
            );
        }
    }
    out
}
