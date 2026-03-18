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
                EnumValue::Generic(v) => {
                    if method == "value" {
                        Some(Ok(v.as_ref().clone()))
                    } else {
                        None
                    }
                }
            },
            "WHAT" => Some(Ok(Value::Package(*enum_type))),
            "raku" | "perl" => Some(Ok(Value::str(format!("{}::{}", enum_type, key)))),
            "gist" => Some(Ok(Value::str(key.resolve()))),
            "Str" => match value {
                EnumValue::Int(_) => Some(Ok(Value::str(key.resolve()))),
                EnumValue::Str(s) => Some(Ok(Value::str(s.clone()))),
                EnumValue::Generic(_) => Some(Ok(Value::str(key.resolve()))),
            },
            "kv" => {
                let val = value.to_value();
                Some(Ok(Value::array(vec![Value::str(key.resolve()), val])))
            }
            "pair" => {
                let val = value.to_value();
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

    /// Dispatch collection methods (.pairs, .keys, .values, .kv, .antipairs, .invert)
    /// on an enum type object (Package value).
    pub(super) fn dispatch_enum_type_collection(
        &self,
        method: &str,
        variants: &[(String, EnumValue)],
    ) -> Result<Value, RuntimeError> {
        match method {
            "pairs" => {
                let pairs: Vec<Value> = variants
                    .iter()
                    .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.to_value())))
                    .collect();
                Ok(Value::array(pairs))
            }
            "keys" => {
                let keys: Vec<Value> = variants
                    .iter()
                    .map(|(k, _)| Value::str(k.clone()))
                    .collect();
                Ok(Value::array(keys))
            }
            "values" => {
                let values: Vec<Value> = variants.iter().map(|(_, v)| v.to_value()).collect();
                Ok(Value::array(values))
            }
            "kv" => {
                let mut kv = Vec::with_capacity(variants.len() * 2);
                for (k, v) in variants {
                    kv.push(Value::str(k.clone()));
                    kv.push(v.to_value());
                }
                Ok(Value::array(kv))
            }
            "antipairs" => {
                let pairs: Vec<Value> = variants
                    .iter()
                    .map(|(k, v)| {
                        Value::ValuePair(Box::new(v.to_value()), Box::new(Value::str(k.clone())))
                    })
                    .collect();
                Ok(Value::array(pairs))
            }
            "invert" => {
                let mut pairs = Vec::new();
                for (k, v) in variants {
                    let val = v.to_value();
                    match &val {
                        Value::Array(items, _) => {
                            for item in items.as_ref() {
                                pairs.push(Value::ValuePair(
                                    Box::new(item.clone()),
                                    Box::new(Value::str(k.clone())),
                                ));
                            }
                        }
                        _ => {
                            pairs.push(Value::ValuePair(
                                Box::new(val),
                                Box::new(Value::str(k.clone())),
                            ));
                        }
                    }
                }
                Ok(Value::array(pairs))
            }
            _ => Ok(Value::array(Vec::new())),
        }
    }
}
