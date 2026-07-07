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
        let ValueView::Enum {
            enum_type,
            key,
            value,
            index,
        } = target.view()
        else {
            return None;
        };

        match method {
            "key" => Some(Ok(Value::str(key.resolve()))),
            "value" | "Int" | "Numeric" => match value {
                EnumValue::Int(i) => Some(Ok(Value::int(*i))),
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
            "WHAT" => Some(Ok(Value::package(enum_type))),
            "raku" | "perl" => {
                let k = key.resolve();
                let is_ident = k
                    .chars()
                    .next()
                    .is_some_and(|c| c == '_' || (c.is_alphabetic() && !c.is_numeric()))
                    && k.chars()
                        .all(|c| c.is_alphanumeric() || c == '_' || c == '-' || c == '\'');
                if is_ident {
                    Some(Ok(Value::str(format!("{}::{}", enum_type, k))))
                } else {
                    Some(Ok(Value::str(format!("{}::<{}>", enum_type, k))))
                }
            }
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
                Some(Ok(Value::pair(key.resolve(), val)))
            }
            "ACCEPTS" => {
                if args.is_empty() {
                    return Some(Err(RuntimeError::new("ACCEPTS requires an argument")));
                }
                let other = &args[0];
                Some(Ok(Value::truth(match other.view() {
                    ValueView::Enum {
                        enum_type: other_type,
                        key: other_key,
                        ..
                    } => enum_type == other_type && key == other_key,
                    _ => false,
                })))
            }
            "pred" => {
                if index == 0 {
                    // .pred on first element returns itself
                    return Some(Ok(target.clone()));
                }
                if let Some(variants) = self.registry().enum_types.get(&enum_type.resolve())
                    && let Some((prev_key, prev_val)) = variants.get(index - 1)
                {
                    return Some(Ok(Value::enum_parts(
                        enum_type,
                        Symbol::intern(prev_key),
                        prev_val.clone(),
                        index - 1,
                    )));
                }
                // Fallback: return self if variants not found
                Some(Ok(target.clone()))
            }
            "succ" => {
                if let Some(variants) = self.registry().enum_types.get(&enum_type.resolve()) {
                    if let Some((next_key, next_val)) = variants.get(index + 1) {
                        return Some(Ok(Value::enum_parts(
                            enum_type,
                            Symbol::intern(next_key),
                            next_val.clone(),
                            index + 1,
                        )));
                    }
                    // .succ on last element returns itself
                    return Some(Ok(target.clone()));
                }
                // Fallback: return self if variants not found
                Some(Ok(target.clone()))
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
                    .map(|(k, v)| Value::pair(k.clone(), v.to_value()))
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
                    .map(|(k, v)| Value::value_pair(v.to_value(), Value::str(k.clone())))
                    .collect();
                Ok(Value::array(pairs))
            }
            "invert" => {
                let mut pairs = Vec::new();
                for (k, v) in variants {
                    let val = v.to_value();
                    match val.view() {
                        ValueView::Array(items, _) => {
                            for item in items.as_ref() {
                                pairs.push(Value::value_pair(item.clone(), Value::str(k.clone())));
                            }
                        }
                        _ => {
                            pairs.push(Value::value_pair(val.clone(), Value::str(k.clone())));
                        }
                    }
                }
                Ok(Value::array(pairs))
            }
            _ => Ok(Value::array(Vec::new())),
        }
    }
}
