use crate::value::Value;

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
                let i = int_val();
                if plus_sign && i >= 0 {
                    format!("+{}", i)
                } else {
                    format!("{}", i)
                }
            }
            'u' => format!("{}", int_val().max(0) as u64),
            'x' => {
                if hash_flag {
                    format!("0x{:x}", int_val())
                } else {
                    format!("{:x}", int_val())
                }
            }
            'X' => {
                if hash_flag {
                    format!("0X{:X}", int_val())
                } else {
                    format!("{:X}", int_val())
                }
            }
            'o' => {
                if hash_flag {
                    format!("0o{:o}", int_val())
                } else {
                    format!("{:o}", int_val())
                }
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
