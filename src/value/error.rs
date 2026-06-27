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

/// A non-error control-flow signal carried by an `Err(RuntimeError)`.
///
/// Raku implements `return`/`last`/`next`/`take`/`emit`/... as control
/// exceptions that unwind the Rust call stack via `Result::Err`. Historically
/// each was a separate `bool` on `RuntimeError`; they are mutually exclusive (an
/// error carries at most one control signal), so they are being consolidated
/// into this single enum (ANALYSIS §2.2 / §7-4). Migration is incremental — only
/// the variants below have moved off their `bool` fields so far; the rest remain
/// `is_*: bool` on `RuntimeError` until later slices.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Control {
    /// `return` — non-local return carrying `return_value`.
    Return,
    /// `goto` (`is_goto`) — labelled jump; the label is in `RuntimeError::label`.
    Goto,
    /// `proceed` — fall through to the next `when` in a `given`.
    Proceed,
    /// `take` — yield a value to the enclosing `gather` (`return_value`).
    Take,
    /// `emit` — emit a value to the enclosing `supply`/`react` (`return_value`).
    Emit,
    /// `redo` — re-run the current loop iteration.
    Redo,
    /// `next` — skip to the next loop iteration.
    Next,
    /// `succeed` — leave the enclosing `when`/`given` successfully.
    Succeed,
    /// A resumable control signal (e.g. a resumed `warn`/CONTROL handler).
    Resume,
    /// `done` for a `react`/`supply` (consumed by the react runtime).
    ReactDone,
    /// `warn` — emit a warning (resumable); message in `RuntimeError::message`.
    Warn,
    /// `last` — break out of the enclosing loop. (A `LEAVE`/routine unwind also
    /// sets the separate `is_leave` flag on top of this.)
    Last,
    /// `fail` — produce a Failure (see also `fail_handled`).
    Fail,
    /// A user `X::Control`-doing exception thrown as a control signal so CONTROL
    /// blocks (not CATCH) handle it.
    Done,
}

#[derive(Debug)]
pub struct RuntimeError {
    pub message: String,
    pub code: Option<RuntimeErrorCode>,
    pub line: Option<usize>,
    pub column: Option<usize>,
    pub hint: Option<String>,
    pub return_value: Option<Value>,
    /// The control-flow signal this error carries, if any (see `Control`).
    /// Replaces the former `is_*` bools for the migrated signals; read via the
    /// `is_*()` accessor methods. The remaining `is_*: bool` fields below carry
    /// signals not yet migrated to the enum (later slices).
    pub control: Option<Control>,
    /// When true, the Failure produced from a `Control::Fail` error should be
    /// marked as handled. Set when UNDO phasers run in response to the fail.
    /// (A modifier on `Control::Fail`, kept as a separate flag.)
    pub fail_handled: bool,
    /// A `LEAVE`/routine unwind sets this *in addition to* `Control::Last` (it
    /// breaks loops like `last` but also targets a routine frame via
    /// `leave_callable_id`/`leave_routine`/`label`), so it cannot live in the
    /// single-signal `control` enum and stays a separate flag.
    pub is_leave: bool,
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
    /// Formatted backtrace string from the call stack at the point of error.
    pub backtrace: Option<String>,
}

/// The runtime type-object Value for a type *name*, used as the `.expected`
/// attribute of type-check exceptions. Most types map to `Package(name)`
/// (`Package("Int") ~~ Int` is True), but a few core type objects have a
/// dedicated Value representation — notably `Nil`, whose type object is
/// `Value::Nil` (so a `Package("Nil")` would fail `~~ Nil` / `=== Nil`).
pub(crate) fn expected_type_object(name: &str) -> Value {
    match name {
        "Nil" => Value::Nil,
        _ => Value::Package(Symbol::intern(name)),
    }
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
            control: None,
            fail_handled: false,
            is_leave: false,
            label: None,
            leave_callable_id: None,
            leave_routine: None,
            return_target_callable_id: None,
            container_name: None,
            exception: None,
            backtrace: None,
        }
    }

    /// `return` control signal (non-local return carrying `return_value`).
    pub(crate) fn is_return(&self) -> bool {
        self.control == Some(Control::Return)
    }
    /// `goto` control signal (labelled jump; label in `self.label`).
    pub(crate) fn is_goto(&self) -> bool {
        self.control == Some(Control::Goto)
    }
    /// `proceed` control signal (fall through to the next `when`).
    pub(crate) fn is_proceed(&self) -> bool {
        self.control == Some(Control::Proceed)
    }
    /// `take` control signal (yield to the enclosing `gather`).
    pub(crate) fn is_take(&self) -> bool {
        self.control == Some(Control::Take)
    }
    /// `emit` control signal (emit to the enclosing `supply`/`react`).
    pub(crate) fn is_emit(&self) -> bool {
        self.control == Some(Control::Emit)
    }
    /// `redo` control signal (re-run the current loop iteration).
    pub(crate) fn is_redo(&self) -> bool {
        self.control == Some(Control::Redo)
    }
    /// `next` control signal (skip to the next loop iteration).
    pub(crate) fn is_next(&self) -> bool {
        self.control == Some(Control::Next)
    }
    /// `succeed` control signal (leave the enclosing `when`/`given`).
    pub(crate) fn is_succeed(&self) -> bool {
        self.control == Some(Control::Succeed)
    }
    /// Resumable control signal (resumed `warn`/CONTROL handler).
    pub(crate) fn is_resume(&self) -> bool {
        self.control == Some(Control::Resume)
    }
    /// `done` control signal for a `react`/`supply`.
    pub(crate) fn is_react_done(&self) -> bool {
        self.control == Some(Control::ReactDone)
    }
    /// `warn` control signal (resumable warning).
    pub(crate) fn is_warn(&self) -> bool {
        self.control == Some(Control::Warn)
    }
    /// `last` control signal (break out of the enclosing loop).
    pub(crate) fn is_last(&self) -> bool {
        self.control == Some(Control::Last)
    }
    /// `fail` control signal (produces a Failure).
    pub(crate) fn is_fail(&self) -> bool {
        self.control == Some(Control::Fail)
    }
    /// User `X::Control`-doing exception thrown as a control signal.
    pub(crate) fn is_done(&self) -> bool {
        self.control == Some(Control::Done)
    }

    /// Check if this error represents an X::CompUnit::UnsatisfiedDependency error.
    pub(crate) fn is_unsatisfied_dependency(&self) -> bool {
        if let Some(ref ex) = self.exception
            && let Value::Instance { class_name, .. } = ex.as_ref()
        {
            return class_name.resolve() == "X::CompUnit::UnsatisfiedDependency";
        }
        false
    }

    /// Check if this error represents a method-not-found error.
    pub(crate) fn is_method_not_found(&self) -> bool {
        if let Some(ref ex) = self.exception
            && let Value::Instance { class_name, .. } = ex.as_ref()
        {
            return class_name.resolve() == "X::Method::NotFound";
        }
        self.message.contains("X::Method::NotFound")
            || self.message.contains("No such method '")
            || self.message.contains("No such private method '")
    }

    /// Check if this error represents a multi-dispatch no-match
    /// (`X::Multi::NoMatch`). Used by constructor/accessor dispatch to decide
    /// whether to fall back to a default candidate (e.g. `Mu.new`).
    pub(crate) fn is_multi_no_match(&self) -> bool {
        if let Some(ref ex) = self.exception
            && let Value::Instance { class_name, .. } = ex.as_ref()
        {
            return class_name.resolve() == "X::Multi::NoMatch";
        }
        let msg = self.message.to_lowercase();
        msg.contains("no matching candidates") || msg.contains("none of these signatures match")
    }

    /// Create a RuntimeError from an exception Value.
    /// Extracts the message from the exception's attributes and wraps it.
    pub(crate) fn from_exception_value(ex: Value) -> Self {
        let msg = if let Value::Instance { attributes, .. } = &ex {
            attributes
                .as_map()
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
            control: None,
            fail_handled: false,
            is_leave: false,
            label: None,
            leave_callable_id: None,
            leave_routine: None,
            return_target_callable_id: None,
            container_name: None,
            exception: None,
            backtrace: None,
        }
    }

    pub(crate) fn last_signal() -> Self {
        Self {
            message: "X::ControlFlow".to_string(),
            control: Some(Control::Last),
            ..Self::new("")
        }
    }

    pub(crate) fn next_signal() -> Self {
        Self {
            message: "X::ControlFlow".to_string(),
            control: Some(Control::Next),
            ..Self::new("")
        }
    }

    pub(crate) fn redo_signal() -> Self {
        Self {
            message: "X::ControlFlow".to_string(),
            control: Some(Control::Redo),
            ..Self::new("")
        }
    }

    pub(crate) fn goto_signal(label: String) -> Self {
        Self {
            message: "X::ControlFlow".to_string(),
            control: Some(Control::Goto),
            label: Some(label),
            ..Self::new("")
        }
    }

    pub(crate) fn proceed_signal() -> Self {
        Self {
            control: Some(Control::Proceed),
            ..Self::new("")
        }
    }

    pub(crate) fn succeed_signal() -> Self {
        Self {
            control: Some(Control::Succeed),
            ..Self::new("")
        }
    }

    /// A benign control signal that unwinds a synchronously-running on-demand
    /// `supply { ... }` body when it emits to a consumer that has already
    /// signalled `done`. Carries `is_react_done` (so the react runtime treats it
    /// as normal completion) but no `X::ControlFlow` exception, so it never
    /// surfaces to user code.
    pub(crate) fn supply_terminate_signal() -> Self {
        Self {
            control: Some(Control::ReactDone),
            ..Self::new("")
        }
    }

    pub(crate) fn resume_signal() -> Self {
        Self {
            control: Some(Control::Resume),
            ..Self::new("")
        }
    }

    pub(crate) fn react_done_signal() -> Self {
        // When `done` propagates unhandled (no enclosing supply/react), it
        // becomes X::ControlFlow with illegal=>"done", enclosing=>"supply or
        // react". Pre-set the exception so throws-like / CATCH can match the
        // correct type; the supply/react runtime consumes the `is_react_done`
        // flag before this exception is ever observed.
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("illegal".to_string(), Value::str("done".to_string()));
        attrs.insert(
            "enclosing".to_string(),
            Value::str("supply or react".to_string()),
        );
        attrs.insert(
            "message".to_string(),
            Value::str("done without supply or react".to_string()),
        );
        let xcf = Self::typed("X::ControlFlow", attrs);
        Self {
            control: Some(Control::ReactDone),
            exception: xcf.exception,
            ..Self::new("")
        }
    }

    pub(crate) fn return_signal(value: Value) -> Self {
        Self {
            message: "CX::Return".to_string(),
            return_value: Some(value),
            control: Some(Control::Return),
            ..Self::new("")
        }
    }

    pub(crate) fn warn_signal(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            control: Some(Control::Warn),
            ..Self::new("")
        }
    }

    pub(crate) fn take_signal(value: Value) -> Self {
        // When CX::Take propagates unhandled (no CONTROL block catches it),
        // it becomes X::ControlFlow with illegal=>"take", enclosing=>"gather".
        // Pre-set the exception so throws-like can match the correct type.
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("illegal".to_string(), Value::str("take".to_string()));
        attrs.insert("enclosing".to_string(), Value::str("gather".to_string()));
        attrs.insert(
            "message".to_string(),
            Value::str("take without gather".to_string()),
        );
        let xcf = Self::typed("X::ControlFlow", attrs);
        Self {
            message: "CX::Take".to_string(),
            control: Some(Control::Take),
            return_value: Some(value),
            exception: xcf.exception,
            ..Self::new("")
        }
    }

    pub(crate) fn emit_signal(value: Value) -> Self {
        // When CX::Emit propagates unhandled (no enclosing supply/react), it
        // becomes X::ControlFlow with illegal=>"emit", enclosing=>"supply or
        // react". Pre-set the exception so throws-like / CATCH can match the
        // correct type; the supply/react runtime consumes the `is_emit` flag
        // before this exception is ever observed.
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("illegal".to_string(), Value::str("emit".to_string()));
        attrs.insert(
            "enclosing".to_string(),
            Value::str("supply or react".to_string()),
        );
        attrs.insert(
            "message".to_string(),
            Value::str("emit without supply or react".to_string()),
        );
        let xcf = Self::typed("X::ControlFlow", attrs);
        Self {
            message: "CX::Emit".to_string(),
            control: Some(Control::Emit),
            return_value: Some(value),
            exception: xcf.exception,
            ..Self::new("")
        }
    }

    /// Warn signal with a resume value stored in return_value.
    pub(crate) fn warn_signal_with_resume(message: impl Into<String>, resume_value: Value) -> Self {
        Self {
            message: message.into(),
            control: Some(Control::Warn),
            return_value: Some(resume_value),
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

    /// X::Undeclared::Symbols for a routine, carrying a `routine_suggestion`
    /// hash `{ name => [close names] }` (empty list when there is no match).
    pub(crate) fn undeclared_routine_symbols(
        name: &str,
        message: impl Into<String>,
        suggestions: Vec<String>,
    ) -> Self {
        let mut attrs = HashMap::new();
        attrs.insert("message".to_string(), Value::str(message.into()));
        attrs.insert("symbol".to_string(), Value::str(name.to_string()));
        let arr: Vec<Value> = suggestions.iter().cloned().map(Value::str).collect();
        let mut map = HashMap::new();
        map.insert(name.to_string(), Value::array(arr.clone()));
        attrs.insert("routine_suggestion".to_string(), Value::hash(map));
        // Also expose a flat `suggestions` list so `.suggestions` is uniformly
        // available on X::Undeclared::Symbols (some call sites check it directly).
        attrs.insert("suggestions".to_string(), Value::array(arr));
        Self::typed("X::Undeclared::Symbols", attrs)
    }

    /// X::Undeclared::Symbols for a type/bareword, carrying a `type_suggestion`
    /// hash `{ name => [close names] }` plus a flat `suggestions` list.
    pub(crate) fn undeclared_type_symbols(
        name: &str,
        message: impl Into<String>,
        suggestions: Vec<String>,
    ) -> Self {
        let mut attrs = HashMap::new();
        attrs.insert("message".to_string(), Value::str(message.into()));
        attrs.insert("symbol".to_string(), Value::str(name.to_string()));
        let arr: Vec<Value> = suggestions.iter().cloned().map(Value::str).collect();
        let mut map = HashMap::new();
        map.insert(name.to_string(), Value::array(arr.clone()));
        attrs.insert("type_suggestion".to_string(), Value::hash(map));
        attrs.insert("suggestions".to_string(), Value::array(arr));
        Self::typed("X::Undeclared::Symbols", attrs)
    }

    /// X::CompUnit::UnsatisfiedDependency - a required module could not be found.
    pub(crate) fn unsatisfied_dependency(module: &str) -> Self {
        let msg = format!("Could not find {} in:\n    (module repositories)", module);
        let mut attrs = HashMap::new();
        attrs.insert("specification".to_string(), Value::str(module.to_string()));
        attrs.insert("message".to_string(), Value::str(msg));
        Self::typed("X::CompUnit::UnsatisfiedDependency", attrs)
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
        use crate::runtime::did_you_mean::{known_methods_for_type, suggest_method};

        let mut msg = format!(
            "No such method '{}' for invocant of type '{}'",
            method, typename
        );

        let candidates = known_methods_for_type(typename);
        if let Some(suggestion) = suggest_method(method, candidates) {
            msg.push_str(&format!("\nDid you mean '{}'?", suggestion));
        }

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

    /// X::Cannot::Lazy with an action string and "onto" type (e.g., for .Capture on lazy lists)
    pub(crate) fn cannot_lazy_with_action(action: &str, onto: &str) -> Self {
        let msg = format!("Cannot {} a lazy list onto a {}", action, onto);
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

    /// X::Syntax::Confused with a reason attribute (for "Two terms in a row" etc.)
    pub(crate) fn syntax_confused_with_reason(reason: impl Into<String>) -> Self {
        let reason = reason.into();
        let mut attrs = HashMap::new();
        attrs.insert("message".to_string(), Value::str(reason.clone()));
        attrs.insert("reason".to_string(), Value::str(reason));
        Self::typed("X::Syntax::Confused", attrs)
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
    pub(crate) fn controlflow_return(out_of_dynamic_scope: bool) -> Self {
        let msg = "Attempt to return outside of any Routine".to_string();
        let mut attrs = HashMap::new();
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        attrs.insert(
            "out-of-dynamic-scope".to_string(),
            Value::Bool(out_of_dynamic_scope),
        );
        Self::typed("X::ControlFlow::Return", attrs)
    }

    /// X::TypeCheck::Assignment - Type check failed in assignment (with optional symbol).
    /// raku exposes `.expected` as the expected type OBJECT and `.got` as the
    /// offending VALUE; the message still names the value's type.
    pub(crate) fn typecheck_assignment(
        expected: &str,
        got_value: &Value,
        symbol: Option<&str>,
    ) -> Self {
        let got_type = crate::value::types::what_type_name(got_value);
        let msg = if let Some(sym) = symbol {
            format!(
                "Type check failed in assignment to {}; expected {}, got {}",
                sym, expected, got_type
            )
        } else {
            format!(
                "Type check failed in assignment; expected {}, got {}",
                expected, got_type
            )
        };
        let mut attrs = HashMap::new();
        attrs.insert("expected".to_string(), expected_type_object(expected));
        attrs.insert("got".to_string(), got_value.clone());
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

    /// X::IllegalDimensionInShape - Illegal dimension in shaped array declaration
    pub(crate) fn illegal_dimension_in_shape(dim: i64) -> Self {
        let msg = format!(
            "Illegal dimension in shape: {}. All dimensions must be integers bigger than 0",
            dim
        );
        let mut attrs = HashMap::new();
        attrs.insert("dim".to_string(), Value::Int(dim));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::IllegalDimensionInShape", attrs)
    }

    /// X::Adverb - Unsupported adverb combination on subscript access
    pub(crate) fn x_adverb(
        what: &str,
        source: &str,
        nogo: &[String],
        unexpected: &[String],
    ) -> Self {
        let nogo_display = nogo
            .iter()
            .map(|s| format!("'{}'", s))
            .collect::<Vec<_>>()
            .join(", ");
        let msg = format!(
            "Unsupported combination of adverbs ({}) passed to {}\non '{}'.",
            nogo_display, what, source
        );
        let mut attrs = HashMap::new();
        attrs.insert("what".to_string(), Value::str(what.to_string()));
        attrs.insert("source".to_string(), Value::str(source.to_string()));
        attrs.insert(
            "nogo".to_string(),
            Value::array(nogo.iter().map(|s| Value::str(s.to_string())).collect()),
        );
        // `.unexpected` is a list of the unknown adverb names (Raku returns a
        // Seq), so smartmatch against `<foo>` / a regex over its elements works.
        attrs.insert(
            "unexpected".to_string(),
            Value::array(
                unexpected
                    .iter()
                    .map(|s| Value::str(s.to_string()))
                    .collect(),
            ),
        );
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::Adverb", attrs)
    }

    /// X::TypeCheck::Binding::Parameter - Type check failed in binding to parameter
    pub(crate) fn typecheck_binding_parameter(
        param: &str,
        expected: &str,
        got: &str,
        message: Option<String>,
    ) -> Self {
        let msg = message.unwrap_or_else(|| {
            format!(
                "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; expected {}, got {}",
                param, expected, got
            )
        });
        let mut attrs = HashMap::new();
        attrs.insert("parameter".to_string(), Value::str(param.to_string()));
        // raku exposes `.expected` as the expected type OBJECT.
        attrs.insert("expected".to_string(), expected_type_object(expected));
        attrs.insert("got".to_string(), Value::str(got.to_string()));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        Self::typed("X::TypeCheck::Binding::Parameter", attrs)
    }

    /// Serialize this error as a JSON document of the form
    /// `{"<ClassName>":{<attr>:<value>, ...}}`.
    ///
    /// This is the format used by the `RAKU_EXCEPTIONS_HANDLER=JSON`
    /// environment-variable exception handler. The object always contains a
    /// `message` key (null when the exception has no message attribute).
    pub fn to_json_exception(&self) -> String {
        let (class_name, attrs): (String, HashMap<String, Value>) = match &self.exception {
            Some(boxed) => match boxed.as_ref() {
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } => (
                    class_name.resolve(),
                    attributes
                        .as_map()
                        .iter()
                        .map(|(k, v)| (k.clone(), v.clone()))
                        .collect(),
                ),
                other => ("X::AdHoc".to_string(), {
                    let mut m = HashMap::new();
                    m.insert("message".to_string(), Value::str(other.to_string_value()));
                    m
                }),
            },
            None => ("X::AdHoc".to_string(), {
                let mut m = HashMap::new();
                m.insert("message".to_string(), Value::str(self.message.clone()));
                m
            }),
        };

        // Emit attributes in a stable order: `message` first, then the rest
        // sorted by name. Always include `message` (null if absent).
        let mut keys: Vec<&String> = attrs.keys().collect();
        keys.sort();
        let mut parts: Vec<String> = Vec::new();
        parts.push(format!(
            "\"message\":{}",
            match attrs.get("message") {
                Some(v) => json_value(v),
                None => "null".to_string(),
            }
        ));
        for k in keys {
            if k == "message" {
                continue;
            }
            parts.push(format!("{}:{}", json_string(k), json_value(&attrs[k])));
        }

        format!("{{{}:{{{}}}}}", json_string(&class_name), parts.join(","))
    }
}

/// Encode a Rust string as a JSON string literal (with surrounding quotes).
fn json_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => out.push_str(&format!("\\u{:04x}", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

/// Encode a runtime Value as a JSON value for the exception handler.
fn json_value(v: &Value) -> String {
    match v {
        Value::Nil => "null".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Int(n) => n.to_string(),
        Value::Num(f) => {
            if f.is_finite() {
                f.to_string()
            } else {
                "null".to_string()
            }
        }
        Value::Str(s) => json_string(s),
        Value::Array(items, _) => {
            let elems: Vec<String> = items.iter().map(json_value).collect();
            format!("[{}]", elems.join(","))
        }
        Value::Hash(map) => {
            let mut keys: Vec<&String> = map.keys().collect();
            keys.sort();
            let pairs: Vec<String> = keys
                .iter()
                .map(|k| format!("{}:{}", json_string(k), json_value(&map[*k])))
                .collect();
            format!("{{{}}}", pairs.join(","))
        }
        other => json_string(&other.to_string_value()),
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
