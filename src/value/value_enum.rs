use super::*;

impl EnumValue {
    /// Return the integer value, or 0 for string enums.
    ///
    /// A `Generic` enum value (any non-Int/Bool/Str variant initializer —
    /// e.g. a `BigInt` that fit back into `i64` after arithmetic, or a
    /// `Rat`/`Num`-valued enum) numifies its wrapped `Value` rather than
    /// blindly returning 0: `enum E (B => 9223372036854775808 - 2**64)`
    /// (one past `i64::MAX`, negated back into range) stores as `Generic`
    /// even though its value is a perfectly ordinary `i64`.
    pub fn as_i64(&self) -> i64 {
        match self {
            EnumValue::Int(i) => *i,
            EnumValue::Str(_) => 0,
            EnumValue::Generic(v) => match v.view() {
                ValueView::Int(i) => i,
                ValueView::BigInt(n) => n.to_i64().unwrap_or(0),
                ValueView::Num(f) => f as i64,
                ValueView::Bool(b) => i64::from(b),
                _ => 0,
            },
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
