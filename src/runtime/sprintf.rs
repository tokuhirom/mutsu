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
        while let Some(d) = chars.peek().copied() {
            if d.is_ascii_digit() {
                width.push(d);
                chars.next();
            } else {
                break;
            }
        }
        let mut precision = String::new();
        if chars.peek() == Some(&'.') {
            chars.next();
            while let Some(d) = chars.peek().copied() {
                if d.is_ascii_digit() {
                    precision.push(d);
                    chars.next();
                } else {
                    break;
                }
            }
        }
        let spec = chars.next().unwrap_or('s');
        let width_num = width.parse::<usize>().unwrap_or(0);
        let prec_num = precision.parse::<usize>().ok();
        let zero_pad = flags.contains('0') && !flags.contains('-');
        let left_align = flags.contains('-');
        let plus_sign = flags.contains('+');
        let hash_flag = flags.contains('#');
        let arg = args.get(arg_index);
        arg_index += 1;
        let int_val = || match arg {
            Some(Value::Int(i)) => *i,
            Some(Value::BigInt(bi)) => num_traits::ToPrimitive::to_i64(bi).unwrap_or_else(|| {
                if bi.sign() == num_bigint::Sign::Minus {
                    i64::MIN
                } else {
                    i64::MAX
                }
            }),
            Some(Value::Num(f)) => *f as i64,
            Some(Value::Rat(n, d)) if *d != 0 => *n / *d,
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
            Some(Value::BigInt(bi)) => bi.clone(),
            Some(Value::Int(i)) => BigInt::from(*i),
            Some(Value::Num(f)) => BigInt::from(*f as i64),
            Some(Value::Rat(n, d)) if *d != 0 => BigInt::from(*n / *d),
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
                if plus_sign && i >= BigInt::from(0) {
                    format!("+{}", i)
                } else {
                    format!("{}", i)
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
            'x' => {
                let i = bigint_val();
                if i < BigInt::from(0) {
                    format!("{}", i)
                } else {
                    let body = i.to_str_radix(16);
                    if hash_flag {
                        format!("0x{}", body)
                    } else {
                        body
                    }
                }
            }
            'X' => {
                let i = bigint_val();
                if i < BigInt::from(0) {
                    format!("{}", i)
                } else {
                    let body = i.to_str_radix(16).to_uppercase();
                    if hash_flag {
                        format!("0X{}", body)
                    } else {
                        body
                    }
                }
            }
            'o' => {
                let i = bigint_val();
                if i < BigInt::from(0) {
                    format!("{}", i)
                } else {
                    let body = i.to_str_radix(8);
                    if hash_flag {
                        format!("0o{}", body)
                    } else {
                        body
                    }
                }
            }
            'b' => {
                let i = bigint_val();
                if i < BigInt::from(0) {
                    format!("{}", i)
                } else if hash_flag {
                    format!("0b{}", i.to_str_radix(2))
                } else {
                    i.to_str_radix(2)
                }
            }
            'f' | 'F' => {
                let f = float_val();
                let p = prec_num.unwrap_or(6);
                if plus_sign && f >= 0.0 {
                    format!("+{:.*}", p, f)
                } else {
                    format!("{:.*}", p, f)
                }
            }
            'e' => {
                let f = float_val();
                let p = prec_num.unwrap_or(6);
                normalize_sci_exponent(&format!("{:.*e}", p, f))
            }
            'E' => {
                let f = float_val();
                let p = prec_num.unwrap_or(6);
                normalize_sci_exponent(&format!("{:.*E}", p, f))
            }
            'g' | 'G' => {
                let f = float_val();
                if let Some(p) = prec_num {
                    format!("{:.*}", p, f)
                } else {
                    format!("{}", f)
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
        let rendered_width = rendered.chars().count();
        if width_num > rendered_width {
            let pad_len = width_num - rendered_width;
            let pad_char = if zero_pad { '0' } else { ' ' };
            let pad: String = std::iter::repeat_n(pad_char, pad_len).collect();
            if left_align {
                out.push_str(&rendered);
                out.push_str(&pad);
            } else {
                out.push_str(&pad);
                out.push_str(&rendered);
            }
        } else {
            out.push_str(&rendered);
        }
    }
    out
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
