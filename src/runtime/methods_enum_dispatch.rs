use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Dispatch methods on Enum values.
    /// Returns Some(result) if handled, None if not an enum method.
    pub(super) fn dispatch_enum_method(
        &self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let Value::Enum {
            enum_type,
            key,
            value,
            index,
        } = target
        else {
            return None;
        };

        match method {
            "key" => Some(Ok(Value::str(key.resolve()))),
            "value" | "Int" | "Numeric" => match value {
                EnumValue::Int(i) => Some(Ok(Value::Int(*i))),
                EnumValue::Str(s) => {
                    if method == "value" {
                        Some(Ok(Value::str(s.clone())))
                    } else {
                        // .Int / .Numeric on a string enum should try to coerce
                        None // fall through to runtime for proper error
                    }
                }
            },
            "WHAT" => Some(Ok(Value::Package(*enum_type))),
            "raku" | "perl" => Some(Ok(Value::str(format!("{}::{}", enum_type, key)))),
            "gist" => Some(Ok(Value::str(key.resolve()))),
            "Str" => match value {
                EnumValue::Int(_) => Some(Ok(Value::str(key.resolve()))),
                EnumValue::Str(s) => Some(Ok(Value::str(s.clone()))),
            },
            "kv" => {
                let val = match value {
                    EnumValue::Int(i) => Value::Int(*i),
                    EnumValue::Str(s) => Value::str(s.clone()),
                };
                Some(Ok(Value::array(vec![Value::str(key.resolve()), val])))
            }
            "pair" => {
                let val = match value {
                    EnumValue::Int(i) => Value::Int(*i),
                    EnumValue::Str(s) => Value::str(s.clone()),
                };
                Some(Ok(Value::Pair(key.resolve(), Box::new(val))))
            }
            "ACCEPTS" => {
                if args.is_empty() {
                    return Some(Err(RuntimeError::new("ACCEPTS requires an argument")));
                }
                let other = &args[0];
                Some(Ok(Value::Bool(match other {
                    Value::Enum {
                        enum_type: other_type,
                        key: other_key,
                        ..
                    } => enum_type == other_type && key == other_key,
                    _ => false,
                })))
            }
            "pred" => {
                if *index == 0 {
                    return Some(Ok(Value::Nil));
                }
                if let Some(variants) = self.enum_types.get(&enum_type.resolve())
                    && let Some((prev_key, prev_val)) = variants.get(index - 1)
                {
                    return Some(Ok(Value::Enum {
                        enum_type: *enum_type,
                        key: Symbol::intern(prev_key),
                        value: prev_val.clone(),
                        index: index - 1,
                    }));
                }
                Some(Ok(Value::Nil))
            }
            "succ" => {
                if let Some(variants) = self.enum_types.get(&enum_type.resolve())
                    && let Some((next_key, next_val)) = variants.get(index + 1)
                {
                    return Some(Ok(Value::Enum {
                        enum_type: *enum_type,
                        key: Symbol::intern(next_key),
                        value: next_val.clone(),
                        index: index + 1,
                    }));
                }
                Some(Ok(Value::Nil))
            }
            _ => None,
        }
    }
}
