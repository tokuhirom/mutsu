use super::*;

impl EnumValue {
    /// Return the integer value, or 0 for string/generic enums.
    pub fn as_i64(&self) -> i64 {
        match self {
            EnumValue::Int(i) => *i,
            EnumValue::Str(_) | EnumValue::Generic(_) => 0,
        }
    }

    /// Return the string representation for `.Str` coercion.
    pub fn to_str_value(&self) -> String {
        match self {
            EnumValue::Int(i) => i.to_string(),
            EnumValue::Str(s) => s.clone(),
            EnumValue::Generic(v) => v.to_str_context(),
        }
    }

    /// Convert to a runtime Value.
    pub fn to_value(&self) -> Value {
        match self {
            EnumValue::Int(i) => Value::Int(*i),
            EnumValue::Str(s) => Value::str(s.clone()),
            EnumValue::Generic(v) => v.as_ref().clone(),
        }
    }
}
