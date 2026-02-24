use super::*;

impl Interpreter {
    pub(super) fn builtin_coerce(&self, name: &str, args: &[Value]) -> Result<Value, RuntimeError> {
        let Some(value) = args.first().cloned() else {
            return Ok(Value::Nil);
        };
        let coerced = match name {
            "Int" => match value {
                Value::Int(i) => Value::Int(i),
                Value::Num(f) => Value::Int(f as i64),
                Value::Rat(n, d) => {
                    if d == 0 {
                        Value::Int(0)
                    } else {
                        Value::Int(n / d)
                    }
                }
                Value::Complex(r, _) => Value::Int(r as i64),
                Value::Str(s) => Value::Int(s.trim().parse::<i64>().unwrap_or(0)),
                Value::Bool(b) => Value::Int(if b { 1 } else { 0 }),
                _ => Value::Int(0),
            },
            "Num" => match value {
                Value::Int(i) => Value::Num(i as f64),
                Value::Num(f) => Value::Num(f),
                Value::Rat(n, d) => {
                    if d == 0 {
                        Value::Num(if n == 0 {
                            f64::NAN
                        } else if n > 0 {
                            f64::INFINITY
                        } else {
                            f64::NEG_INFINITY
                        })
                    } else {
                        Value::Num(n as f64 / d as f64)
                    }
                }
                Value::Complex(r, _) => Value::Num(r),
                Value::Str(s) => {
                    if let Ok(i) = s.trim().parse::<i64>() {
                        Value::Num(i as f64)
                    } else if let Ok(f) = s.trim().parse::<f64>() {
                        Value::Num(f)
                    } else {
                        Value::Num(0.0)
                    }
                }
                Value::Bool(b) => Value::Num(if b { 1.0 } else { 0.0 }),
                _ => Value::Num(0.0),
            },
            "Str" => Value::Str(crate::runtime::utils::coerce_to_str(&value)),
            "Bool" => Value::Bool(value.truthy()),
            _ => Value::Nil,
        };
        Ok(coerced)
    }
}
