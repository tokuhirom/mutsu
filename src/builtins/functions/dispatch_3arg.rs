#![allow(clippy::result_large_err)]
use crate::value::{RuntimeError, Value};

pub(crate) fn native_function_3arg(
    name: &str,
    arg1: &Value,
    arg2: &Value,
    arg3: &Value,
) -> Option<Result<Value, RuntimeError>> {
    match name {
        "expmod" => Some(crate::builtins::expmod(arg1, arg2, arg3)),
        "unimatch" => {
            // unimatch(target, property_value, property_name)
            let prop_value = arg2.to_string_value();
            let prop_name = arg3.to_string_value();
            match arg1 {
                Value::Int(i) => {
                    let cp = *i as u32;
                    Some(Ok(crate::builtins::uniprop::unimatch_for_codepoint(
                        cp,
                        &prop_value,
                        Some(&prop_name),
                    )))
                }
                _ => {
                    let s = arg1.to_string_value();
                    if s.is_empty() {
                        return Some(Ok(Value::Nil));
                    }
                    let ch = s.chars().next().unwrap();
                    Some(Ok(Value::Bool(crate::builtins::uniprop::unimatch(
                        ch,
                        &prop_value,
                        Some(&prop_name),
                    ))))
                }
            }
        }
        "substr" => {
            if matches!(arg1, Value::Junction { .. })
                || matches!(arg2, Value::Junction { .. })
                || matches!(arg3, Value::Junction { .. })
            {
                return None;
            }
            crate::builtins::substr::native_substr_slice(&arg1.to_string_value(), arg2, Some(arg3))
        }
        _ => None,
    }
}
