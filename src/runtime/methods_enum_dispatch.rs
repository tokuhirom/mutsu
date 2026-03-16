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
            "value" | "Int" | "Numeric" => Some(Ok(Value::Int(*value))),
            "WHAT" => Some(Ok(Value::Package(*enum_type))),
            "raku" | "perl" => Some(Ok(Value::str(format!("{}::{}", enum_type, key)))),
            "gist" | "Str" => Some(Ok(Value::str(key.resolve()))),
            "kv" => Some(Ok(Value::array(vec![
                Value::str(key.resolve()),
                Value::Int(*value),
            ]))),
            "pair" => Some(Ok(Value::Pair(key.resolve(), Box::new(Value::Int(*value))))),
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
                        value: *prev_val,
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
                        value: *next_val,
                        index: index + 1,
                    }));
                }
                Some(Ok(Value::Nil))
            }
            _ => None,
        }
    }
}
