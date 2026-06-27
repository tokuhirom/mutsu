#![allow(clippy::result_large_err)]
//! Date, Instant, and Duration arithmetic helpers.

use super::rat::to_big_rat_parts;
use crate::symbol::Symbol;
use crate::value::{Value, make_big_rat_arith};

/// Check if a value is a Date, Instant, or Duration instance (temporal operand for arithmetic).
pub(crate) fn is_temporal_operand(value: &Value) -> bool {
    matches!(value, Value::Instance { class_name, .. }
        if class_name == "Date" || class_name == "Instant" || class_name == "Duration")
}

pub(crate) fn instance_days(value: &Value) -> Option<i64> {
    match value {
        Value::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Date" => match attributes.as_map().get("days") {
            Some(Value::Int(days)) => Some(*days),
            _ => None,
        },
        _ => None,
    }
}

pub(crate) fn instance_instant_value(value: &Value) -> Option<f64> {
    match value {
        Value::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Instant" => attributes
            .as_map()
            .get("value")
            .and_then(crate::runtime::to_float_value),
        _ => None,
    }
}

pub(crate) fn instance_instant_raw(value: &Value) -> Option<Value> {
    match value {
        Value::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Instant" => attributes.as_map().get("value").cloned(),
        _ => None,
    }
}

pub(crate) fn value_sub(a: Value, b: Value) -> Value {
    let (l, r) = crate::runtime::coerce_numeric(a, b);
    if let (Some((an, ad)), Some((bn, bd))) = (to_big_rat_parts(&l), to_big_rat_parts(&r)) {
        return make_big_rat_arith(an * &bd - bn * &ad, ad * bd);
    }
    Value::Num(
        crate::runtime::to_float_value(&l).unwrap_or(0.0)
            - crate::runtime::to_float_value(&r).unwrap_or(0.0),
    )
}

pub(crate) fn instance_duration_value(value: &Value) -> Option<f64> {
    match value {
        Value::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Duration" => attributes
            .as_map()
            .get("value")
            .and_then(crate::runtime::to_float_value),
        _ => None,
    }
}

/// Return the raw stored `value` of a Duration instance (a Rational), if any.
pub(crate) fn instance_duration_raw_value(value: &Value) -> Option<Value> {
    match value {
        Value::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Duration" => attributes.as_map().get("value").cloned(),
        _ => None,
    }
}

/// Build a Duration instance storing the given (already Rational) value.
pub(crate) fn make_duration_from_value(val: Value) -> Value {
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("value".to_string(), val);
    Value::make_instance(Symbol::intern("Duration"), attrs)
}

pub(crate) fn instance_datetime_parts(
    value: &Value,
) -> Option<(i64, i64, i64, i64, i64, f64, i64)> {
    match value {
        Value::Instance { attributes, .. }
            if attributes.contains_key("year")
                && attributes.contains_key("month")
                && attributes.contains_key("day")
                && attributes.contains_key("hour")
                && attributes.contains_key("minute")
                && attributes.contains_key("second")
                && attributes.contains_key("timezone") =>
        {
            Some(crate::builtins::methods_0arg::temporal::datetime_attrs(
                &(attributes).as_map(),
            ))
        }
        _ => None,
    }
}

pub(crate) fn make_duration_value(secs: f64) -> Value {
    make_duration(secs)
}

pub(crate) fn make_duration(secs: f64) -> Value {
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("value".to_string(), Value::Num(secs));
    Value::make_instance(Symbol::intern("Duration"), attrs)
}
