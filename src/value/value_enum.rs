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

    /// The integer to use when this enum value is a *subscript*, or `None` when
    /// it has no numeric value at all.
    ///
    /// Distinct from [`EnumValue::as_i64`], which answers `0` for a string enum
    /// because its callers want a total function. A subscript must not: raku
    /// numifies an enum value used as a positional index (`@a[Green]` is
    /// `@a[1]`), but a STRING-valued enum (`enum E (S => 'x')`) numifies through
    /// `Str.Int` and dies with `X::Str::Numeric`. Answering `0` there would
    /// silently read element 0 instead, so the string case returns `None` and
    /// leaves the caller on its existing non-numeric path.
    pub fn as_index_i64(&self) -> Option<i64> {
        match self {
            EnumValue::Int(i) => Some(*i),
            EnumValue::Str(_) => None,
            EnumValue::Generic(v) => match v.view() {
                ValueView::Int(i) => Some(i),
                ValueView::BigInt(n) => n.to_i64(),
                ValueView::Num(f) => Some(f as i64),
                ValueView::Bool(b) => Some(i64::from(b)),
                _ => None,
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
