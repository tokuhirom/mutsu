use super::Value;
use crate::symbol::Symbol;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimeErrorCode {
    ParseUnparsed,
    ParseExpected,
    ParseGeneric,
}

impl std::fmt::Display for RuntimeErrorCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            RuntimeErrorCode::ParseUnparsed => "PARSE_UNPARSED",
            RuntimeErrorCode::ParseExpected => "PARSE_EXPECTED",
            RuntimeErrorCode::ParseGeneric => "PARSE_GENERIC",
        };
        write!(f, "{}", name)
    }
}

impl RuntimeErrorCode {
    pub fn is_parse(self) -> bool {
        matches!(
            self,
            RuntimeErrorCode::ParseUnparsed
                | RuntimeErrorCode::ParseExpected
                | RuntimeErrorCode::ParseGeneric
        )
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    pub message: String,
    pub code: Option<RuntimeErrorCode>,
    pub line: Option<usize>,
    pub column: Option<usize>,
    pub hint: Option<String>,
    pub return_value: Option<Value>,
    pub is_return: bool,
    pub is_last: bool,
    pub is_next: bool,
    pub is_redo: bool,
    pub is_goto: bool,
    pub is_proceed: bool,
    pub is_succeed: bool,
    pub is_fail: bool,
    /// When true, the Failure produced from this fail error should be marked as handled.
    /// Set when UNDO phasers run in response to the fail.
    pub fail_handled: bool,
    pub is_warn: bool,
    pub is_leave: bool,
    pub is_resume: bool,
    pub is_react_done: bool,
    pub label: Option<String>,
    pub leave_callable_id: Option<u64>,
    pub leave_routine: Option<String>,
    /// For non-local returns (CX::Return from a block), the callable ID of the
    /// lexically enclosing routine that the return targets.
    pub return_target_callable_id: Option<u64>,
    /// Container name for Scalar container binding (e.g. when/default returning $a)
    pub container_name: Option<String>,
    /// Structured exception object (e.g. X::AdHoc, X::Promise::Vowed)
    pub exception: Option<Box<Value>>,
}

impl RuntimeError {
    pub(crate) fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            code: None,
            line: None,
            column: None,
            hint: None,
            return_value: None,
            is_return: false,
            is_last: false,
            is_next: false,
            is_redo: false,
            is_goto: false,
            is_proceed: false,
            is_succeed: false,
            is_fail: false,
            fail_handled: false,
            is_warn: false,
            is_leave: false,
            is_resume: false,
            is_react_done: false,
            label: None,
            leave_callable_id: None,
            leave_routine: None,
            return_target_callable_id: None,
            container_name: None,
            exception: None,
        }
    }

    /// Create a RuntimeError from an exception Value.
    /// Extracts the message from the exception's attributes and wraps it.
    pub(crate) fn from_exception_value(ex: Value) -> Self {
        let msg = if let Value::Instance { attributes, .. } = &ex {
            attributes
                .get("message")
                .map(|v| v.to_string_value())
                .unwrap_or_else(|| ex.to_string_value())
        } else {
            ex.to_string_value()
        };
        let mut err = Self::new(msg);
        err.exception = Some(Box::new(ex));
        err
    }

    pub(crate) fn with_location(
        message: impl Into<String>,
        code: RuntimeErrorCode,
        line: usize,
        column: usize,
    ) -> Self {
        Self {
            message: message.into(),
            code: Some(code),
            line: Some(line),
            column: Some(column),
            hint: None,
            return_value: None,
            is_return: false,
            is_last: false,
            is_next: false,
            is_redo: false,
            is_goto: false,
            is_proceed: false,
            is_succeed: false,
            is_fail: false,
            fail_handled: false,
            is_warn: false,
            is_leave: false,
            is_resume: false,
            is_react_done: false,
            label: None,
            leave_callable_id: None,
            leave_routine: None,
            return_target_callable_id: None,
            container_name: None,
            exception: None,
        }
    }

    pub(crate) fn last_signal() -> Self {
        Self {
            message: "X::ControlFlow".to_string(),
            is_last: true,
            ..Self::new("")
        }
    }

    pub(crate) fn next_signal() -> Self {
        Self {
            message: "X::ControlFlow".to_string(),
            is_next: true,
            ..Self::new("")
        }
    }

    pub(crate) fn redo_signal() -> Self {
        Self {
            message: "X::ControlFlow".to_string(),
            is_redo: true,
            ..Self::new("")
        }
    }

    pub(crate) fn goto_signal(label: String) -> Self {
        Self {
            message: "X::ControlFlow".to_string(),
            is_goto: true,
            label: Some(label),
            ..Self::new("")
        }
    }

    pub(crate) fn proceed_signal() -> Self {
        Self {
            is_proceed: true,
            ..Self::new("")
        }
    }

    pub(crate) fn succeed_signal() -> Self {
        Self {
            is_succeed: true,
            ..Self::new("")
        }
    }

    pub(crate) fn resume_signal() -> Self {
        Self {
            is_resume: true,
            ..Self::new("")
        }
    }

    pub(crate) fn react_done_signal() -> Self {
        Self {
            is_react_done: true,
            ..Self::new("")
        }
    }

    pub(crate) fn return_signal(value: Value) -> Self {
        Self {
            message: "CX::Return".to_string(),
            return_value: Some(value),
            is_return: true,
            ..Self::new("")
        }
    }

    pub(crate) fn warn_signal(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            is_warn: true,
            ..Self::new("")
        }
    }

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
        let mut err = Self::new("X::Numeric::DivideByZero");
        err.exception = Some(Box::new(ex));
        err
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

    /// X::Undeclared - Undeclared name
    #[allow(dead_code)]
    pub(crate) fn undeclared(what: &str, name: &str) -> Self {
        let msg = format!("Undeclared {} '{}'", what, name);
        let mut attrs = HashMap::new();
        attrs.insert("what".to_string(), Value::str(what.to_string()));
        attrs.insert("symbol".to_string(), Value::str(name.to_string()));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::Undeclared", attrs)
    }

    /// X::Undeclared::Symbols - Undeclared name (symbols variant)
    pub(crate) fn undeclared_symbols(message: impl Into<String>) -> Self {
        Self::typed_msg("X::Undeclared::Symbols", message)
    }

    /// X::Redeclaration - Redeclared symbol
    #[allow(dead_code)]
    pub(crate) fn redeclaration(what: &str, name: &str) -> Self {
        let msg = format!("Redeclaration of {} '{}'", what, name);
        let mut attrs = HashMap::new();
        attrs.insert("what".to_string(), Value::str(what.to_string()));
        attrs.insert("symbol".to_string(), Value::str(name.to_string()));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::Redeclaration", attrs)
    }

    /// X::Redeclaration for routine - includes "multi" suggestion in message
    pub(crate) fn redeclaration_routine(name: &str) -> Self {
        let msg = format!(
            "Redeclaration of routine '{}'. Did you mean to declare a multi-sub?",
            name
        );
        let mut attrs = HashMap::new();
        attrs.insert("what".to_string(), Value::str("routine".to_string()));
        attrs.insert("symbol".to_string(), Value::str(name.to_string()));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::Redeclaration", attrs)
    }

    /// X::Method::NotFound - No such method
    #[allow(dead_code)]
    pub(crate) fn method_not_found(method: &str, typename: &str) -> Self {
        let msg = format!(
            "No such method '{}' for invocant of type '{}'",
            method, typename
        );
        let mut attrs = HashMap::new();
        attrs.insert("method".to_string(), Value::str(method.to_string()));
        attrs.insert("typename".to_string(), Value::str(typename.to_string()));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::Method::NotFound", attrs)
    }

    /// X::Obsolete - Obsolete syntax
    #[allow(dead_code)]
    pub(crate) fn obsolete(old: &str, replacement: &str) -> Self {
        let msg = format!(
            "Unsupported use of {}. In Raku please use: {}.",
            old, replacement
        );
        let mut attrs = HashMap::new();
        attrs.insert("old".to_string(), Value::str(old.to_string()));
        attrs.insert(
            "replacement".to_string(),
            Value::str(replacement.to_string()),
        );
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::Obsolete", attrs)
    }

    /// X::Immutable - Cannot modify an immutable value
    pub(crate) fn immutable(typename: &str, method: &str) -> Self {
        let msg = format!("Cannot call '{}' on an immutable '{}'", method, typename);
        let mut attrs = HashMap::new();
        attrs.insert("typename".to_string(), Value::str(typename.to_string()));
        attrs.insert("method".to_string(), Value::str(method.to_string()));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::Immutable", attrs)
    }

    /// X::Cannot::Lazy - Cannot .elems a lazy list
    pub(crate) fn cannot_lazy(action: &str) -> Self {
        let msg = format!("Cannot .{} a lazy list", action);
        let mut attrs = HashMap::new();
        attrs.insert("action".to_string(), Value::str(action.to_string()));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::Cannot::Lazy", attrs)
    }

    /// X::Cannot::Lazy with a `what` attribute (e.g., for coercion to Bag/Set/Mix)
    pub(crate) fn cannot_lazy_what(what: &str) -> Self {
        let msg = format!("Cannot coerce a lazy list to a {}", what);
        let mut attrs = HashMap::new();
        attrs.insert("action".to_string(), Value::str("coerce".to_string()));
        attrs.insert("what".to_string(), Value::str(what.to_string()));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::Cannot::Lazy", attrs)
    }

    /// X::Syntax::Missing - Missing required syntax element
    #[allow(dead_code)]
    pub(crate) fn syntax_missing(what: &str) -> Self {
        let msg = format!("Missing {}", what);
        let mut attrs = HashMap::new();
        attrs.insert("what".to_string(), Value::str(what.to_string()));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::Syntax::Missing", attrs)
    }

    /// X::Syntax::Confused - Confused parse error
    #[allow(dead_code)]
    pub(crate) fn syntax_confused(message: impl Into<String>) -> Self {
        Self::typed_msg("X::Syntax::Confused", message)
    }

    /// X::Syntax::Malformed - Malformed syntax
    #[allow(dead_code)]
    pub(crate) fn syntax_malformed(what: &str, message: impl Into<String>) -> Self {
        let message = message.into();
        let mut attrs = HashMap::new();
        attrs.insert("what".to_string(), Value::str(what.to_string()));
        attrs.insert("message".to_string(), Value::str(message.clone()));
        Self::typed("X::Syntax::Malformed", attrs)
    }

    /// X::ControlFlow::Return - Return outside of routine
    #[allow(dead_code)]
    pub(crate) fn controlflow_return(out_of_dynamic_scope: bool) -> Self {
        let msg = "Attempt to return outside of any Routine".to_string();
        let mut attrs = HashMap::new();
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        attrs.insert(
            "out-of-dynamic-scope".to_string(),
            Value::Bool(!out_of_dynamic_scope),
        );
        Self::typed("X::ControlFlow::Return", attrs)
    }

    /// X::TypeCheck::Assignment - Type check failed in assignment (with optional symbol)
    pub(crate) fn typecheck_assignment(expected: &str, got: &str, symbol: Option<&str>) -> Self {
        let msg = if let Some(sym) = symbol {
            format!(
                "Type check failed in assignment to {}; expected {}, got {}",
                sym, expected, got
            )
        } else {
            format!(
                "Type check failed in assignment; expected {}, got {}",
                expected, got
            )
        };
        let mut attrs = HashMap::new();
        attrs.insert("expected".to_string(), Value::str(expected.to_string()));
        attrs.insert("got".to_string(), Value::str(got.to_string()));
        if let Some(sym) = symbol {
            attrs.insert("symbol".to_string(), Value::str(sym.to_string()));
        }
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::TypeCheck::Assignment", attrs)
    }

    /// X::Parameter::InvalidConcreteness - Invocant concreteness mismatch
    pub(crate) fn parameter_invalid_concreteness(
        expected: &str,
        got: &str,
        routine: &str,
        param: &str,
        should_be_concrete: bool,
        param_is_invocant: bool,
    ) -> Self {
        let kind = if should_be_concrete {
            "an object instance"
        } else {
            "a type object"
        };
        let actual_kind = if should_be_concrete {
            "a type object"
        } else {
            "an object instance"
        };
        let msg = format!(
            "Invocant of method '{}' must be {} of type\n'{}', not {} of type '{}'.  Did you forget a '.new'?",
            routine, kind, expected, actual_kind, got
        );
        let mut attrs = HashMap::new();
        attrs.insert("expected".to_string(), Value::str(expected.to_string()));
        attrs.insert("got".to_string(), Value::str(got.to_string()));
        attrs.insert("routine".to_string(), Value::str(routine.to_string()));
        attrs.insert("param".to_string(), Value::str(param.to_string()));
        attrs.insert(
            "should-be-concrete".to_string(),
            Value::Bool(should_be_concrete),
        );
        attrs.insert(
            "param-is-invocant".to_string(),
            Value::Bool(param_is_invocant),
        );
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::Parameter::InvalidConcreteness", attrs)
    }

    /// X::IO::Closed - IO::Handle is closed
    pub(crate) fn io_closed(trying: &str) -> Self {
        let msg = format!("Cannot do '{}' on a closed handle", trying);
        let mut attrs = HashMap::new();
        attrs.insert("trying".to_string(), Value::str(trying.to_string()));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::IO::Closed", attrs)
    }

    /// X::Bind - Cannot bind to a thing
    #[allow(dead_code)]
    pub(crate) fn bind(target: &str) -> Self {
        let msg = format!("Cannot bind to {}", target);
        let mut attrs = HashMap::new();
        attrs.insert("target".to_string(), Value::str(target.to_string()));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::Bind", attrs)
    }
}

#[cfg(test)]
mod tests {
    use super::RuntimeErrorCode;

    #[test]
    fn runtime_error_code_display_names_are_stable() {
        assert_eq!(
            RuntimeErrorCode::ParseUnparsed.to_string(),
            "PARSE_UNPARSED"
        );
        assert_eq!(
            RuntimeErrorCode::ParseExpected.to_string(),
            "PARSE_EXPECTED"
        );
        assert_eq!(RuntimeErrorCode::ParseGeneric.to_string(), "PARSE_GENERIC");
    }

    #[test]
    fn runtime_error_code_parse_classification() {
        assert!(RuntimeErrorCode::ParseUnparsed.is_parse());
        assert!(RuntimeErrorCode::ParseExpected.is_parse());
        assert!(RuntimeErrorCode::ParseGeneric.is_parse());
    }
}
