use crate::value::Value;
use num_bigint::BigInt;
use num_traits::Signed;

pub(crate) fn format_sprintf(fmt: &str, arg: Option<&Value>) -> String {
    let mut chars = fmt.chars().peekable();
    let mut out = String::new();
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
        let int_val = || match arg {
            Some(Value::Int(i)) => *i,
            Some(Value::BigInt(n)) => n.to_string().parse::<i64>().unwrap_or(0),
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
        let big_int_val = || match arg {
            Some(Value::Int(i)) => BigInt::from(*i),
            Some(Value::BigInt(n)) => n.clone(),
            Some(Value::Num(f)) => BigInt::from(*f as i64),
            Some(Value::Rat(n, d)) if *d != 0 => BigInt::from(*n / *d),
            Some(Value::Str(s)) => s
                .trim()
                .parse::<i64>()
                .map(BigInt::from)
                .unwrap_or_else(|_| BigInt::from(0)),
            Some(Value::Bool(b)) => BigInt::from(i64::from(*b)),
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
                let i = big_int_val();
                if plus_sign && !i.is_negative() {
                    format!("+{}", i)
                } else {
                    format!("{}", i)
                }
            }
            'u' => {
                let i = big_int_val();
                if i.is_negative() {
                    "0".to_string()
                } else {
                    i.to_str_radix(10)
                }
            }
            'x' => {
                let hex = big_int_val().to_str_radix(16);
                if hash_flag { format!("0x{}", hex) } else { hex }
            }
            'X' => {
                let hex = big_int_val().to_str_radix(16).to_uppercase();
                if hash_flag { format!("0X{}", hex) } else { hex }
            }
            'o' => {
                let oct = big_int_val().to_str_radix(8);
                if hash_flag { format!("0o{}", oct) } else { oct }
            }
            'b' | 'B' => {
                let mut bin = big_int_val().to_str_radix(2);
                if spec == 'B' {
                    bin = bin.to_uppercase();
                }
                if hash_flag { format!("0b{}", bin) } else { bin }
            }
            'f' | 'F' => {
                let f = float_val();
                if let Some(p) = prec_num {
                    if plus_sign && f >= 0.0 {
                        format!("+{:.*}", p, f)
                    } else {
                        format!("{:.*}", p, f)
                    }
                } else if plus_sign && f >= 0.0 {
                    format!("+{}", f)
                } else {
                    format!("{}", f)
                }
            }
            'e' => {
                let f = float_val();
                if let Some(p) = prec_num {
                    format!("{:.*e}", p, f)
                } else {
                    format!("{:e}", f)
                }
            }
            'E' => {
                let f = float_val();
                if let Some(p) = prec_num {
                    format!("{:.*E}", p, f)
                } else {
                    format!("{:E}", f)
                }
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
