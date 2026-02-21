use super::Value;

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
            label: None,
            exception: None,
        }
    }

    pub(crate) fn last_signal() -> Self {
        Self {
            message: "X::ControlFlow".to_string(),
            code: None,
            line: None,
            column: None,
            hint: None,
            return_value: None,
            is_last: true,
            is_next: false,
            is_redo: false,
            is_proceed: false,
            is_succeed: false,
            is_fail: false,
            label: None,
            exception: None,
        }
    }

    pub(crate) fn next_signal() -> Self {
        Self {
            message: "X::ControlFlow".to_string(),
            code: None,
            line: None,
            column: None,
            hint: None,
            return_value: None,
            is_last: false,
            is_next: true,
            is_redo: false,
            is_proceed: false,
            is_succeed: false,
            is_fail: false,
            label: None,
            exception: None,
        }
    }

    pub(crate) fn redo_signal() -> Self {
        Self {
            message: "X::ControlFlow".to_string(),
            code: None,
            line: None,
            column: None,
            hint: None,
            return_value: None,
            is_last: false,
            is_next: false,
            is_redo: true,
            is_proceed: false,
            is_succeed: false,
            is_fail: false,
            label: None,
            exception: None,
        }
    }

    pub(crate) fn proceed_signal() -> Self {
        Self {
            message: String::new(),
            code: None,
            line: None,
            column: None,
            hint: None,
            return_value: None,
            is_last: false,
            is_next: false,
            is_redo: false,
            is_proceed: true,
            is_succeed: false,
            is_fail: false,
            label: None,
            exception: None,
        }
    }

    pub(crate) fn succeed_signal() -> Self {
        Self {
            message: String::new(),
            code: None,
            line: None,
            column: None,
            hint: None,
            return_value: None,
            is_last: false,
            is_next: false,
            is_redo: false,
            is_proceed: false,
            is_succeed: true,
            is_fail: false,
            label: None,
            exception: None,
        }
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
