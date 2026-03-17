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
            str_value,
        } = target
        else {
            return None;
        };

        match method {
            "key" => Some(Ok(Value::str(key.resolve()))),
            "value" => {
                if let Some(sv) = str_value {
                    Some(Ok(Value::str(sv.resolve())))
                } else {
                    Some(Ok(Value::Int(*value)))
                }
            }
            "Int" | "Numeric" => Some(Ok(Value::Int(*value))),
            "WHAT" => Some(Ok(Value::Package(*enum_type))),
            "raku" | "perl" => Some(Ok(Value::str(format!("{}::{}", enum_type, key)))),
            "gist" => Some(Ok(Value::str(key.resolve()))),
            "Str" => {
                if let Some(sv) = str_value {
                    Some(Ok(Value::str(sv.resolve())))
                } else {
                    Some(Ok(Value::str(key.resolve())))
                }
            }
            "kv" => {
                let val = if let Some(sv) = str_value {
                    Value::str(sv.resolve())
                } else {
                    Value::Int(*value)
                };
                Some(Ok(Value::array(vec![Value::str(key.resolve()), val])))
            }
            "pair" => {
                let val = if let Some(sv) = str_value {
                    Value::str(sv.resolve())
                } else {
                    Value::Int(*value)
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
                    && let Some((prev_key, prev_val, prev_sv)) = variants.get(index - 1)
                {
                    return Some(Ok(Value::Enum {
                        enum_type: *enum_type,
                        key: Symbol::intern(prev_key),
                        value: *prev_val,
                        index: index - 1,
                        str_value: *prev_sv,
                    }));
                }
                Some(Ok(Value::Nil))
            }
            "succ" => {
                if let Some(variants) = self.enum_types.get(&enum_type.resolve())
                    && let Some((next_key, next_val, next_sv)) = variants.get(index + 1)
                {
                    return Some(Ok(Value::Enum {
                        enum_type: *enum_type,
                        key: Symbol::intern(next_key),
                        value: *next_val,
                        index: index + 1,
                        str_value: *next_sv,
                    }));
                }
                Some(Ok(Value::Nil))
            }
            _ => None,
        }
    }
}
