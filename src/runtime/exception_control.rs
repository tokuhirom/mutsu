//! `Exception`'s and `Failure`'s control methods (`throw`, `rethrow`, `die`,
//! `fail`, `resume`, `Failure`), the handlers of their rows (ADR-11276 §9.163).

use super::*;

/// The owners whose control-method rows an exception or a `Failure` reaches.
const CONTROL_OWNERS: [&str; 2] = ["Exception", "Failure"];

impl Interpreter {
    /// Answer a control method of `Exception` or `Failure` from its row, for
    /// an instance or type object whose MRO reaches that owner (a user
    /// subclass included). `None` when neither owner has an interpreter row
    /// for the call, or the receiver is neither.
    // Cost: O(k) to find the row, k = rows named `method`; then one MRO read,
    // O(d), d = its length, plus the handler's own cost.
    pub(super) fn exception_control_by_mro(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let class = match target.view() {
            ValueView::Instance { class_name, .. } if !target.is_match_instance() => class_name,
            ValueView::Package(name) => name,
            _ => return None,
        };
        let method_sym = Symbol::intern(method);
        let has_row = |owner: &str| {
            crate::builtins::method_table::owner_row(Symbol::intern(owner), method_sym, args.len())
                .is_some_and(|id| {
                    matches!(
                        crate::builtins::method_table::row(id).handler,
                        crate::builtins::method_table::Handler::Interp(_)
                    )
                })
        };
        if !CONTROL_OWNERS.into_iter().any(has_row) {
            return None;
        }
        let class = class.resolve();
        let mro = self.class_mro(&class);
        // Some built-in `X::`/`CX::` classes have catalog MROs (such as
        // `CX::Take`) that do not list Exception.
        let builtin_exception = class.starts_with("X::") || class.starts_with("CX::");
        let owner = CONTROL_OWNERS.into_iter().find(|&owner| {
            has_row(owner)
                && (class == owner
                    || (owner == "Exception" && builtin_exception)
                    || mro.iter().any(|p| p == owner))
        })?;
        crate::builtins::method_table::invoke_mro_late(self, &[owner], method, args, target)
    }

    /// Answer `message`, `gist`, `Str`, `Stringy`, `backtrace` or `payload` of a
    /// built-in exception instance from its owner's row
    /// (`method_table::exception`). A class with a method of its own, or one
    /// whose message the interpreter must compute, keeps its ordinary dispatch.
    // Cost: O(1) to decline a method with no row; otherwise one class-registry
    // probe plus the row's handler.
    pub(super) fn exception_rows_by_owner(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !args.is_empty()
            || !matches!(
                method,
                "message" | "gist" | "Str" | "Stringy" | "backtrace" | "payload"
            )
            || !target.instance_is_exception_by_name()
        {
            return None;
        }
        let ValueView::Instance { class_name, .. } = target.view() else {
            return None;
        };
        let cn = class_name.resolve();
        if self.has_user_method(&cn, method)
            || (matches!(method, "gist" | "Str" | "Stringy")
                && self.exception_render_needs_interpreter(target, &cn))
        {
            return None;
        }
        crate::builtins::method_table::exception::answer(target, method)
    }

    /// `Exception.throw`, `rethrow`, `die`, `fail` and `resume` on an exception
    /// instance, a user subclass's included: raise it (as a control exception
    /// for a class doing `X::Control`, as a warning for a `CX::Warn`), return a
    /// `Failure` carrying it, or signal its handler to resume.
    ///
    /// The text is `$exc.message`, never the raw attribute; a user
    /// `method message` is not run at the throw site, since Rakudo evaluates it
    /// lazily (`X.new.throw` under `dies-ok` never runs it, #12625).
    // Cost: O(d + m + s), d = the class's MRO length, m = chars of the message,
    // s = routine-stack depth (the backtrace of a built-in exception thrown
    // without one).
    /// `$exception.throw($backtrace)`: throw it carrying the given backtrace,
    /// not the throw site's (a mapped driver exception keeps the original one).
    // Cost: O(1) plus [`Self::exception_control`].
    pub(crate) fn exception_throw_with_backtrace(
        &mut self,
        target: &Value,
        backtrace: &Value,
    ) -> Result<Value, RuntimeError> {
        let target = match target.view() {
            ValueView::Instance {
                class_name,
                attributes,
                id,
            } if target.instance_is_exception_by_name() && !backtrace.is_nil() => {
                let mut attrs = attributes.as_map().clone();
                attrs.insert("backtrace".to_string(), backtrace.clone());
                Value::write_back_sharing(&attributes, class_name, attrs, id)
            }
            _ => target.clone(),
        };
        self.exception_control(&target, "throw")
    }

    pub(crate) fn exception_control(
        &mut self,
        target: &Value,
        method: &str,
    ) -> Result<Value, RuntimeError> {
        if method == "resume" {
            return Err(RuntimeError::resume_signal());
        }
        // `$exc.fail` is `fail $exc`: a Failure carrying the exception, which
        // throws only when sunk or used unhandled.
        if method == "fail" {
            return self.builtin_fail(std::slice::from_ref(target));
        }
        let target = self.with_throw_site_backtrace(target, method);
        let cn = match target.view() {
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            _ => String::new(),
        };
        let does_x_control =
            cn == "X::Control" || self.class_mro(&cn).iter().any(|p| p == "X::Control");
        let target = self.stamp_thrown_exception_dynamics(&target);
        let defer_message = self.has_user_method(&cn, "message");
        let msg = if defer_message {
            format!("Died with {cn}")
        } else {
            self.exception_message_or_died_with(&target)
        };
        let mut err = RuntimeError::new(msg);
        // A class doing `X::Control` throws as a control exception, so a
        // `CONTROL` block catches it instead of `CATCH`.
        if does_x_control {
            err.control = Some(crate::value::Control::Done);
        }
        // Throwing a `CX::Warn` re-raises it as a warning: the default handler
        // writes it to stderr and execution continues.
        if cn == "CX::Warn" {
            err.control = Some(crate::value::Control::Warn);
        }
        err.exception = Some(Box::new(if defer_message {
            target
        } else {
            Self::stamp_thrown_exception_message(&target, &err.message)
        }));
        // A control exception of a user-declared class runs the active CONTROL
        // handlers at the throw site, so a handler's `.resume` continues after
        // the `.throw` (the built-in `CX::*` signals unwind as before).
        if does_x_control
            && method == "throw"
            && !self.control.control_handlers.is_empty()
            && self.registry().classes.contains_key(&cn)
        {
            return self.try_control_inline_signal(err);
        }
        Err(err)
    }

    /// `Failure.rethrow`, `die` and `fail`: Rakudo's `Failure` answers each
    /// through its `FALLBACK`, which throws the exception it carries,
    /// handled or not (`open(...) orelse .die`; DBDish's
    /// `with self!handle-errors { ... } else { .fail }`). `None` for a Failure
    /// with no exception.
    // Cost: O(m), m = chars of the message.
    pub(crate) fn failure_control(&mut self, target: &Value) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Instance { attributes, .. } = target.view() else {
            return None;
        };
        let exc = attributes.as_map().get("exception").cloned()?;
        let msg = self.exception_message_or_died_with(&exc);
        let mut err = RuntimeError::new(msg);
        err.exception = Some(Box::new(exc));
        Some(Err(err))
    }

    /// A built-in exception raised by a method call carries the frames of the
    /// call site, written into the exception itself (Rakudo's `throw` sets
    /// the invocant's backtrace, so `$e.throw; $e === $!`), as the `die`/`fail` opcodes give theirs (so
    /// `.backtrace.list` is not empty). One that already has a backtrace (a
    /// rethrow) keeps it.
    // Cost: O(s), s = routine-stack depth, when a backtrace is built.
    fn with_throw_site_backtrace(&mut self, target: &Value, method: &str) -> Value {
        if !target.instance_is_exception_by_name() {
            return target.clone();
        }
        let ValueView::Instance {
            class_name,
            attributes,
            id,
        } = target.view()
        else {
            return target.clone();
        };
        if attributes.as_map().contains_key("backtrace") {
            return target.clone();
        }
        let mut attrs = attributes.as_map().clone();
        attrs.insert(
            "backtrace".to_string(),
            self.build_backtrace_value_with_leading(&[method]),
        );
        if let Some(line) = self.current_source_line() {
            attrs
                .entry("line".to_string())
                .or_insert_with(|| Value::int(line as i64));
        }
        if let Some(file) = self.current_source_file() {
            attrs
                .entry("file".to_string())
                .or_insert_with(|| Value::str_from(&file));
        }
        Value::write_back_sharing(&attributes, class_name, attrs, id)
    }
}
