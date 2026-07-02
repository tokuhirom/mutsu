use super::Value;
use crate::symbol::Symbol;

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

/// Cold, rarely-set routing/diagnostic fields of a [`RuntimeError`], boxed
/// behind a single `Option<Box<..>>` so the common error carries none of them
/// inline. Keeping these out of `RuntimeError` shrinks it below Clippy's
/// `result_large_err` threshold (was 272 B), so the ~1280 `Result<_,
/// RuntimeError>` signatures no longer need `#[allow(clippy::result_large_err)]`
/// (ANALYSIS §2.2 / §7-4). Every field is accessed through the getter/setter
/// methods on `RuntimeError`; the box is allocated lazily on first write.
#[derive(Debug, Default)]
pub struct RuntimeErrorCold {
    /// Parse-error classification (set only for parse failures).
    pub code: Option<RuntimeErrorCode>,
    /// 1-based source line of a parse failure.
    pub line: Option<usize>,
    /// 1-based source column of a parse failure.
    pub column: Option<usize>,
    /// A human-readable hint appended to the rendered error.
    pub hint: Option<String>,
    /// A `LEAVE`/routine unwind targets a routine frame via this callable ID.
    pub leave_callable_id: Option<u64>,
    /// A `LEAVE`/routine unwind targets a routine frame via this `Pkg::name`.
    pub leave_routine: Option<String>,
    /// For non-local returns (CX::Return from a block), the callable ID of the
    /// lexically enclosing routine that the return targets.
    pub return_target_callable_id: Option<u64>,
    /// Container name for Scalar container binding (e.g. when/default returning $a)
    pub container_name: Option<String>,
    /// Formatted backtrace string from the call stack at the point of error.
    pub backtrace: Option<String>,
}

#[derive(Debug)]
pub struct RuntimeError {
    pub message: String,
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
    /// Structured exception object (e.g. X::AdHoc, X::Promise::Vowed)
    pub exception: Option<Box<Value>>,
    /// Cold routing/diagnostic fields, allocated lazily. Accessed via the
    /// getter/setter methods below; `pub(crate)` only so the `..RuntimeError::new()`
    /// functional-update idiom keeps working at construction sites.
    pub(crate) cold: Option<Box<RuntimeErrorCold>>,
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
            return_value: None,
            control: None,
            fail_handled: false,
            is_leave: false,
            label: None,
            exception: None,
            cold: None,
        }
    }

    // ----- cold-field accessors (read) -----
    //
    // These mirror the former public fields; a `None` cold box reads as `None`.

    pub fn code(&self) -> Option<RuntimeErrorCode> {
        self.cold.as_ref().and_then(|c| c.code)
    }
    pub fn line(&self) -> Option<usize> {
        self.cold.as_ref().and_then(|c| c.line)
    }
    pub fn column(&self) -> Option<usize> {
        self.cold.as_ref().and_then(|c| c.column)
    }
    pub fn hint(&self) -> Option<&str> {
        self.cold.as_ref().and_then(|c| c.hint.as_deref())
    }
    pub fn leave_callable_id(&self) -> Option<u64> {
        self.cold.as_ref().and_then(|c| c.leave_callable_id)
    }
    pub fn leave_routine(&self) -> Option<&str> {
        self.cold.as_ref().and_then(|c| c.leave_routine.as_deref())
    }
    pub fn return_target_callable_id(&self) -> Option<u64> {
        self.cold.as_ref().and_then(|c| c.return_target_callable_id)
    }
    pub fn container_name(&self) -> Option<&str> {
        self.cold.as_ref().and_then(|c| c.container_name.as_deref())
    }
    pub fn backtrace(&self) -> Option<&str> {
        self.cold.as_ref().and_then(|c| c.backtrace.as_deref())
    }

    /// Move the container name out of the error, leaving it `None`.
    pub(crate) fn take_container_name(&mut self) -> Option<String> {
        self.cold.as_mut().and_then(|c| c.container_name.take())
    }
    /// Move the hint out of the error, leaving it `None`.
    pub(crate) fn take_hint(&mut self) -> Option<String> {
        self.cold.as_mut().and_then(|c| c.hint.take())
    }

    // ----- cold-field accessors (write) -----
    //
    // Each lazily allocates the cold box on first write via `cold_mut`.

    fn cold_mut(&mut self) -> &mut RuntimeErrorCold {
        self.cold.get_or_insert_with(Default::default)
    }
    pub(crate) fn set_code(&mut self, v: Option<RuntimeErrorCode>) {
        self.cold_mut().code = v;
    }
    pub(crate) fn set_line(&mut self, v: Option<usize>) {
        self.cold_mut().line = v;
    }
    pub(crate) fn set_hint(&mut self, v: Option<String>) {
        self.cold_mut().hint = v;
    }
    pub(crate) fn set_leave_callable_id(&mut self, v: Option<u64>) {
        self.cold_mut().leave_callable_id = v;
    }
    pub(crate) fn set_leave_routine(&mut self, v: Option<String>) {
        self.cold_mut().leave_routine = v;
    }
    pub(crate) fn set_return_target_callable_id(&mut self, v: Option<u64>) {
        self.cold_mut().return_target_callable_id = v;
    }
    pub(crate) fn set_container_name(&mut self, v: Option<String>) {
        self.cold_mut().container_name = v;
    }
    pub(crate) fn set_backtrace(&mut self, v: Option<String>) {
        self.cold_mut().backtrace = v;
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
            cold: Some(Box::new(RuntimeErrorCold {
                code: Some(code),
                line: Some(line),
                column: Some(column),
                ..Default::default()
            })),
            return_value: None,
            control: None,
            fail_handled: false,
            is_leave: false,
            label: None,
            exception: None,
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
