use super::Value;
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
    pub is_last: bool,
    pub is_next: bool,
    pub is_redo: bool,
    pub is_proceed: bool,
    pub is_succeed: bool,
    pub is_fail: bool,
    pub is_warn: bool,
    pub label: Option<String>,
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
            is_last: false,
            is_next: false,
            is_redo: false,
            is_proceed: false,
            is_succeed: false,
            is_fail: false,
            is_warn: false,
            label: None,
            exception: None,
        }
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
            is_last: false,
            is_next: false,
            is_redo: false,
            is_proceed: false,
            is_succeed: false,
            is_fail: false,
            is_warn: false,
            label: None,
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

    pub(crate) fn warn_signal(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            is_warn: true,
            ..Self::new("")
        }
    }

    pub(crate) fn illegal_on_fixed_dimension_array(operation: &str) -> Self {
        let mut attrs = HashMap::new();
        attrs.insert("operation".to_string(), Value::Str(operation.to_string()));
        attrs.insert(
            "message".to_string(),
            Value::Str(format!("Cannot {} a fixed-dimension array", operation)),
        );
        let ex = Value::make_instance("X::IllegalOnFixedDimensionArray".to_string(), attrs);
        let mut err = Self::new(format!("Cannot {} a fixed-dimension array", operation));
        err.exception = Some(Box::new(ex));
        err
    }

    pub(crate) fn numeric_divide_by_zero() -> Self {
        let mut attrs = HashMap::new();
        attrs.insert(
            "message".to_string(),
            Value::Str("Attempt to divide by zero".to_string()),
        );
        let ex = Value::make_instance("X::Numeric::DivideByZero".to_string(), attrs);
        let mut err = Self::new("X::Numeric::DivideByZero");
        err.exception = Some(Box::new(ex));
        err
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
