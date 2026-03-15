use super::*;

impl Interpreter {
    pub(super) fn builtin_coerce(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let Some(value) = args.first().cloned() else {
            return Ok(Value::Nil);
        };
        if let Some(source) = match &value {
            Value::Package(sym) => Some(sym.resolve()),
            Value::Nil => Some("Any".to_string()),
            _ => None,
        } {
            return Ok(Value::Package(Symbol::intern(&format!("{name}({source})"))));
        }
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
                Value::Complex(r, im) => {
                    let tolerance = self
                        .get_dynamic_var("*TOLERANCE")
                        .ok()
                        .and_then(|v| match v {
                            Value::Num(n) => Some(n),
                            Value::Rat(n, d) if d != 0 => Some(n as f64 / d as f64),
                            Value::Int(n) => Some(n as f64),
                            _ => None,
                        })
                        .unwrap_or(1e-15);
                    if im.abs() > tolerance {
                        let msg = format!(
                            "Cannot convert {}{}{}i to Num: imaginary part not zero",
                            r,
                            if im >= 0.0 { "+" } else { "" },
                            im
                        );
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("message".to_string(), Value::str(msg.clone()));
                        attrs.insert("target".to_string(), Value::str_from("Num"));
                        attrs.insert("source".to_string(), value.clone());
                        let ex = Value::make_instance(
                            crate::symbol::Symbol::intern("X::Numeric::Real"),
                            attrs,
                        );
                        let mut err = RuntimeError::new(msg);
                        err.exception = Some(Box::new(ex));
                        return Err(err);
                    }
                    Value::Num(r)
                }
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
            "Str" => self.call_method_with_values(value, "Str", vec![])?,
            "Bool" => Value::Bool(value.truthy()),
            _ => Value::Nil,
        };
        Ok(coerced)
    }
}
