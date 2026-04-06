use crate::value::Value;
use num_bigint::BigInt;

use super::sprintf_helpers::{
    apply_float_minus_zero, apply_width, format_float_fixed, format_g, format_inf_nan,
    format_rat_fixed, normalize_sci_exponent, sign_prefix,
};
pub(crate) use super::sprintf_validate::{validate_sprintf_arg_types, validate_sprintf_directives};

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
            let w = match args.get(arg_index) {
                Some(Value::Int(i)) => *i as usize,
                Some(Value::Num(f)) => *f as usize,
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
                let p = match args.get(arg_index) {
                    Some(Value::Int(i)) => *i as usize,
                    Some(Value::Num(f)) => *f as usize,
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
        let prec_cancels_zero = prec_num.is_some()
            && matches!(spec, 's' | 'b' | 'B' | 'd' | 'i' | 'o' | 'x' | 'X' | 'u');
        let zero_pad = flags.contains('0') && !flags.contains('-') && !prec_cancels_zero;
        let left_align = flags.contains('-');
        let float_minus_zero =
            matches!(spec, 'f' | 'F' | 'e' | 'E') && flags.contains('-') && flags.contains('0');
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
        let arg = match raw_arg {
            Some(Value::Mixin(inner, _)) => {
                _mixin_storage = (**inner).clone();
                Some(&_mixin_storage)
            }
            other => other,
        };
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
            Some(Value::Str(s)) => s
                .trim()
                .parse::<i64>()
                .unwrap_or_else(|_| s.trim().parse::<f64>().unwrap_or(0.0) as i64),
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
                .unwrap_or_else(|_| BigInt::from(s.trim().parse::<f64>().unwrap_or(0.0) as i64)),
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
                // Use exact rational formatting for Rat/FatRat with high precision
                if p > 17 {
                    if let Some(rendered) = format_rat_fixed(arg, p, plus_sign, space_flag) {
                        rendered
                    } else {
                        let f = float_val();
                        format_float_fixed(f, p, plus_sign, space_flag)
                    }
                } else {
                    let f = float_val();
                    format_float_fixed(f, p, plus_sign, space_flag)
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
                        let e_char = if spec == 'G' { 'E' } else { 'e' };
                        let exp_sign = if exp >= 0 { '+' } else { '-' };
                        format!(
                            "{}{}{}{}{:02}",
                            prefix,
                            formatted_mantissa,
                            e_char,
                            exp_sign,
                            exp.unsigned_abs()
                        )
                    } else {
                        // Fixed notation: same as %f with p decimal places
                        format!("{}{:.*}", prefix, p, abs)
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
        let plain_zero = matches!(spec, 'o' | 'x' | 'X');
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
