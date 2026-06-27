//! `RuntimeError` constructors for numeric, range, and assignment errors.
use super::{RuntimeError, Value};
use crate::symbol::Symbol;
use std::collections::HashMap;

impl RuntimeError {
    pub(crate) fn illegal_on_fixed_dimension_array(operation: &str) -> Self {
        let mut attrs = HashMap::new();
        attrs.insert("operation".to_string(), Value::str(operation.to_string()));
        attrs.insert(
            "message".to_string(),
            Value::str(format!("Cannot {} a fixed-dimension array", operation)),
        );
        let ex = Value::make_instance(Symbol::intern("X::IllegalOnFixedDimensionArray"), attrs);
        let mut err = Self::new(format!("Cannot {} a fixed-dimension array", operation));
        err.exception = Some(Box::new(ex));
        err
    }

    pub(crate) fn attribute_required(name: &str, why: Option<&str>) -> Self {
        let mut attrs = HashMap::new();
        attrs.insert("name".to_string(), Value::str(name.to_string()));
        let why_str = why.unwrap_or("Required").to_string();
        attrs.insert("why".to_string(), Value::str(why_str.clone()));
        attrs.insert(
            "message".to_string(),
            Value::str(format!(
                "The attribute '{}' is required, but you did not provide a value for it.",
                name
            )),
        );
        let ex = Value::make_instance(Symbol::intern("X::Attribute::Required"), attrs);
        let mut err = Self::new(format!(
            "The attribute '{}' is required, but you did not provide a value for it.",
            name
        ));
        err.exception = Some(Box::new(ex));
        err
    }

    pub(crate) fn numeric_divide_by_zero() -> Self {
        Self::numeric_divide_by_zero_with(None)
    }

    pub(crate) fn numeric_divide_by_zero_with(numerator: Option<Value>) -> Self {
        Self::numeric_divide_by_zero_full(numerator, None)
    }

    pub(crate) fn numeric_divide_by_zero_full(
        numerator: Option<Value>,
        using: Option<&str>,
    ) -> Self {
        let mut attrs = HashMap::new();
        let mut msg = "Attempt to divide".to_string();
        if let Some(ref n) = numerator {
            msg.push_str(&format!(" {} by zero", n.to_string_value()));
        } else {
            msg.push_str(" by zero");
        }
        if let Some(u) = using {
            msg.push_str(&format!(" using {}", u));
            attrs.insert("using".to_string(), Value::str_from(u));
        }
        attrs.insert("message".to_string(), Value::str_from(&msg));
        if let Some(n) = numerator {
            attrs.insert("numerator".to_string(), n);
        }
        let ex = Value::make_instance(Symbol::intern("X::Numeric::DivideByZero"), attrs);
        // Use the descriptive message (not the bare type name) so an uncaught
        // divide-by-zero prints like Rakudo (`Attempt to divide 1 by zero
        // using %`) instead of `X::Numeric::DivideByZero`.
        let mut err = Self::new(&msg);
        err.exception = Some(Box::new(ex));
        err
    }

    /// Create a Failure value wrapping an X::Numeric::DivideByZero exception
    /// for a method call on a zero-denominator Rational (e.g. `.floor`, `.Int`).
    pub(crate) fn divide_by_zero_failure_for_method(method: &str, type_name: &str) -> Value {
        let mut attrs = HashMap::new();
        let msg = format!(
            "Attempt to divide by zero when calling .{} on {}",
            method, type_name
        );
        attrs.insert("message".to_string(), Value::str_from(&msg));
        let ex = Value::make_instance(Symbol::intern("X::Numeric::DivideByZero"), attrs);
        let mut failure_attrs = HashMap::new();
        failure_attrs.insert("exception".to_string(), ex);
        Value::make_instance(Symbol::intern("Failure"), failure_attrs)
    }

    /// Create a Failure value wrapping an X::Numeric::DivideByZero exception.
    /// In Raku, `div` and `%` by zero return a Failure (lazy exception)
    /// instead of throwing immediately.
    pub(crate) fn divide_by_zero_failure(numerator: Option<Value>, using: Option<&str>) -> Value {
        let mut attrs = HashMap::new();
        let mut msg = "Attempt to divide".to_string();
        if let Some(ref n) = numerator {
            msg.push_str(&format!(" {} by zero", n.to_string_value()));
        } else {
            msg.push_str(" by zero");
        }
        if let Some(u) = using {
            msg.push_str(&format!(" using {}", u));
            attrs.insert("using".to_string(), Value::str_from(u));
        }
        attrs.insert("message".to_string(), Value::str_from(&msg));
        if let Some(n) = numerator {
            attrs.insert("numerator".to_string(), n);
        }
        let ex = Value::make_instance(Symbol::intern("X::Numeric::DivideByZero"), attrs);
        let mut failure_attrs = HashMap::new();
        failure_attrs.insert("exception".to_string(), ex);
        Value::make_instance(Symbol::intern("Failure"), failure_attrs)
    }

    /// Create a typed exception error with the given class name and attributes.
    /// This is the general-purpose constructor for structured exceptions.
    pub(crate) fn typed(class_name: &str, attrs: HashMap<String, Value>) -> Self {
        let msg = attrs
            .get("message")
            .map(|v| v.to_string_value())
            .unwrap_or_else(|| class_name.to_string());
        let ex = Value::make_instance(Symbol::intern(class_name), attrs);
        let mut err = Self::new(msg);
        err.exception = Some(Box::new(ex));
        err
    }

    /// Create a typed exception with just a message attribute.
    pub(crate) fn typed_msg(class_name: &str, message: impl Into<String>) -> Self {
        let message = message.into();
        let mut attrs = HashMap::new();
        attrs.insert("message".to_string(), Value::str(message.clone()));
        let ex = Value::make_instance(Symbol::intern(class_name), attrs);
        let mut err = Self::new(message);
        err.exception = Some(Box::new(ex));
        err
    }

    /// X::Assignment::RO - Cannot modify an immutable value
    pub(crate) fn assignment_ro(value: Option<&str>) -> Self {
        let msg = if let Some(v) = value {
            format!("Cannot modify an immutable value ({})", v)
        } else {
            "Cannot modify an immutable value".to_string()
        };
        let mut attrs = HashMap::new();
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        if let Some(v) = value {
            attrs.insert("value".to_string(), Value::str(v.to_string()));
        }
        Self::typed("X::Assignment::RO", attrs)
    }

    /// X::Role::Initialization - supplied an initialization value to a role that
    /// does not have exactly one public attribute.
    pub(crate) fn role_initialization(role_name: &str) -> Self {
        let msg = format!(
            "Can only supply an initialization value for a role if it has a single public attribute, but this is not the case for '{}'",
            role_name
        );
        let mut attrs = HashMap::new();
        attrs.insert("role".to_string(), Value::str(role_name.to_string()));
        attrs.insert("message".to_string(), Value::str(msg));
        Self::typed("X::Role::Initialization", attrs)
    }

    /// X::Assignment::RO with typename - Cannot modify an immutable value of a given type
    pub(crate) fn assignment_ro_typename(typename: &str, repr: &str) -> Self {
        let msg = format!("Cannot modify an immutable {} ({})", typename, repr);
        let mut attrs = HashMap::new();
        attrs.insert("typename".to_string(), Value::str(typename.to_string()));
        attrs.insert("message".to_string(), Value::str(msg));
        Self::typed("X::Assignment::RO", attrs)
    }

    /// X::Str::Numeric - Cannot convert string to number
    #[allow(dead_code)]
    pub(crate) fn str_numeric(source: &str, reason: &str) -> Self {
        let msg = format!("Cannot convert string '{}' to number: {}", source, reason);
        let mut attrs = HashMap::new();
        attrs.insert("source".to_string(), Value::str(source.to_string()));
        attrs.insert("reason".to_string(), Value::str(reason.to_string()));
        attrs.insert("target-name".to_string(), Value::str("Numeric".to_string()));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::Str::Numeric", attrs)
    }

    /// X::OutOfRange - Index out of range
    #[allow(dead_code)]
    pub(crate) fn out_of_range(what: &str, got: Value, range: &str) -> Self {
        let msg = format!(
            "Index out of range. Is: {}, should be in {}",
            got.to_string_value(),
            range
        );
        let mut attrs = HashMap::new();
        attrs.insert("what".to_string(), Value::str(what.to_string()));
        attrs.insert("got".to_string(), got);
        attrs.insert("range".to_string(), Value::str(range.to_string()));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::OutOfRange", attrs)
    }

    /// Create a Failure wrapping an X::OutOfRange exception.
    /// Used when operations should soft-fail (return Failure) rather than throw.
    pub(crate) fn out_of_range_failure(what: &str, got: Value, range: &str) -> Value {
        let msg = format!(
            "Index out of range. Is: {}, should be in {}",
            got.to_string_value(),
            range
        );
        let mut attrs = HashMap::new();
        attrs.insert("what".to_string(), Value::str(what.to_string()));
        attrs.insert("got".to_string(), got);
        attrs.insert("range".to_string(), Value::str(range.to_string()));
        attrs.insert("message".to_string(), Value::str(msg));
        let ex = Value::make_instance(Symbol::intern("X::OutOfRange"), attrs);
        let mut failure_attrs = HashMap::new();
        failure_attrs.insert("exception".to_string(), ex);
        Value::make_instance(Symbol::intern("Failure"), failure_attrs)
    }

    /// Create a Failure wrapping an X::Numeric::Underflow exception.
    pub(crate) fn numeric_underflow_failure() -> Value {
        let mut attrs = HashMap::new();
        attrs.insert(
            "message".to_string(),
            Value::str("Numeric underflow".to_string()),
        );
        let ex = Value::make_instance(Symbol::intern("X::Numeric::Underflow"), attrs);
        let mut failure_attrs = HashMap::new();
        failure_attrs.insert("exception".to_string(), ex);
        Value::make_instance(Symbol::intern("Failure"), failure_attrs)
    }
}
