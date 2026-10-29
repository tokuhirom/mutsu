//! Running a `CONTROL` handler INLINE at a `warn` raise site (#9469, #9510).
//!
//! Rakudo runs a CONTROL handler on top of the stack, in the dynamic scope of
//! the raise, and unwinds only when the handler does not resume. mutsu's VM
//! recurses on the Rust stack for every call, so unwinding to the region that
//! owns the handler destroys every frame between the raise site and it: an
//! op-raised warning (`"a" ~ Any`) has no resume point at all, and one raised
//! in a callee cannot be resumed into. So every CONTROL handler runs here, at
//! the raise site, before anything unwinds.
//!
//! A `resume_safe` handler (`control_block_is_resume_safe`) provably always
//! resumes, so a run that falls through counts as resuming with `Nil`. For any
//! other handler the outcome is read off the run, exactly as ADR-0072 does for
//! `CATCH` (`runtime/catch_inline.rs`).
//!
//! Handlers are tried innermost first. One whose `when` arms match nothing
//! declines, and the warning moves on to the next outer handler, still at the
//! raise site. One that matches without resuming ends its region: the raise
//! site returns the warn signal stamped with that region's token and
//! `Handled`, and the region (`exec_try_catch_op_inner`) applies the verdict
//! without running its handler again. When every handler declines, the default
//! handler prints the warning and resumes.

use super::*;
use crate::value::CatchInlineVerdict;

/// What one inline handler run decided.
enum ControlInlineOutcome {
    /// The warn resumes; the raise site continues.
    Resumed,
    /// A `when`/`default` arm matched without resuming: the region ends.
    Handled,
    /// No arm matched: the next outer handler sees the warning.
    Declined,
    /// The handler raised something else; it replaces the warning.
    Raised(RuntimeError),
}

impl Interpreter {
    /// Register a CONTROL handler for the protected body of a region. The
    /// handler carries its own bytecode and function table so the raise site
    /// can run it inline.
    // Cost: O(1), plus O(c + f) the first time a code object (c = ops +
    // constants) or function-table version (f entries) installs a handler.
    /// `range` is the handler's `control_begin..end` op range; `resume_safe`
    /// comes from the region's `OpCode::TryCatch`.
    pub(crate) fn push_control_handler(
        &mut self,
        code: &CompiledCode,
        range: (usize, usize),
        resume_safe: bool,
        handles_take: bool,
        token: u64,
        compiled_fns: &CompiledFns,
    ) {
        let (control_begin, end) = range;
        self.control.control_handler_depth += 1;
        let handler = crate::vm::ControlHandlerCode {
            code: code.shared_snapshot(),
            control_begin,
            end,
            compiled_fns: self.shared_fns_snapshot(compiled_fns),
        };
        self.control
            .control_handlers
            .push(crate::vm::ControlHandlerEntry {
                resume_safe,
                handler,
                handles_take,
                token,
                installing_base: self.locals.base(),
                installing_call_depth: self.call_frames.len(),
                installing_fixtures: self.capture_fixtures(),
            });
    }

    /// Offer a warning to the active CONTROL handlers at its raise site.
    ///
    /// - `Ok(_)`: the warning was resumed (by a handler, or by the default
    ///   handler after every handler declined); the raise expression continues.
    /// - `Err(e)`: a handler ended its region without resuming, or raised
    ///   a new error. `e` carries the verdict stamp where one applies.
    pub(crate) fn try_control_inline(&mut self, message: &str) -> Result<Value, RuntimeError> {
        // A `warn` raised inside a suppressed region (`quietly`, the Hash
        // hyper) must never reach a CONTROL handler registered outside it --
        // rakudo's `quietly` installs its own resume-everything CONTROL, so an
        // outer handler never sees the warning at all (#9607). A handler
        // declared *inside* the region is still above this floor and gets
        // first look, matching rakudo's nesting order.
        let floor = self.warn_control_handler_floor();
        let mut idx = self.control.control_handlers.len();
        while idx > floor {
            idx -= 1;
            let entry = &self.control.control_handlers[idx];
            let resume_safe = entry.resume_safe;
            let token = entry.token;
            let code = entry.handler.code.clone();
            let control_begin = entry.handler.control_begin;
            let end = entry.handler.end;
            let fns = entry.handler.compiled_fns.clone();
            let installing = (entry.installing_base, entry.installing_call_depth);
            let fixtures = entry.installing_fixtures.clone();
            // The handler runs with only the handlers outside it registered, so
            // a `warn` inside it goes outward rather than back into itself.
            // `control_handler_depth` deliberately stays put: the run loop
            // prints-and-resumes a warn signal on the spot when the depth is 0,
            // which would swallow a `.rethrow` inside this handler instead of
            // letting it reach the outcome below. A `warn` raised in the handler
            // with no outer handler registered reaches the default handler at
            // the end of this function.
            let inner = self.control.control_handlers.split_off(idx);
            let mark = fixtures.map(|f| self.push_scoped_fixtures(f));
            let outcome = self.run_control_handler_inline(
                &code,
                (control_begin, end),
                &fns,
                &RuntimeError::warn_signal(message.to_string()),
                resume_safe,
                installing,
            );
            if let Some(mark) = mark {
                self.end_scoped_fixtures(mark);
            }
            self.control.control_handlers.extend(inner);
            match outcome {
                ControlInlineOutcome::Resumed => return Ok(Value::NIL),
                ControlInlineOutcome::Handled => {
                    let mut err = RuntimeError::warn_signal(message.to_string());
                    err.set_catch_inline_verdict(Some((token, CatchInlineVerdict::Handled)));
                    return Err(err);
                }
                ControlInlineOutcome::Declined => {}
                ControlInlineOutcome::Raised(e) => return Err(e),
            }
        }
        // Every handler declined (or, inside a handler run, none is registered
        // outside it): the default handler prints and resumes.
        if !self.warning_suppressed() {
            self.write_warn_to_stderr(message);
        }
        Ok(Value::NIL)
    }

    /// Offer a user-defined control exception (a class doing `X::Control`,
    /// thrown with `.throw`) to the active CONTROL handlers at its throw site,
    /// so a handler's `.resume` continues right after the `.throw`, as in
    /// rakudo (Red's `CX::Red::Bool` relies on it).
    ///
    /// - `Ok(_)`: a handler resumed; the `.throw` returns.
    /// - `Err(e)`: a handler ended its region without resuming (stamped with
    ///   that region's token), raised something else, or every handler
    ///   declined and `signal` unwinds as an ordinary control exception.
    // Cost: O(h * b), h = active CONTROL handlers, b = a handler's body.
    pub(crate) fn try_control_inline_signal(
        &mut self,
        signal: RuntimeError,
    ) -> Result<Value, RuntimeError> {
        let mut idx = self.control.control_handlers.len();
        while idx > 0 {
            idx -= 1;
            let entry = &self.control.control_handlers[idx];
            let resume_safe = entry.resume_safe;
            let token = entry.token;
            let code = entry.handler.code.clone();
            let control_begin = entry.handler.control_begin;
            let end = entry.handler.end;
            let fns = entry.handler.compiled_fns.clone();
            let installing = (entry.installing_base, entry.installing_call_depth);
            let fixtures = entry.installing_fixtures.clone();
            let inner = self.control.control_handlers.split_off(idx);
            let mark = fixtures.map(|f| self.push_scoped_fixtures(f));
            let outcome = self.run_control_handler_inline(
                &code,
                (control_begin, end),
                &fns,
                &signal,
                resume_safe,
                installing,
            );
            if let Some(mark) = mark {
                self.end_scoped_fixtures(mark);
            }
            self.control.control_handlers.extend(inner);
            match outcome {
                ControlInlineOutcome::Resumed => return Ok(Value::NIL),
                ControlInlineOutcome::Handled => {
                    let mut err = signal;
                    err.set_catch_inline_verdict(Some((token, CatchInlineVerdict::Handled)));
                    return Err(err);
                }
                ControlInlineOutcome::Declined => {}
                ControlInlineOutcome::Raised(e) => return Err(e),
            }
        }
        Err(signal)
    }

    /// Run `code[control_begin..end]` as a CONTROL handler for a warning, with
    /// a `CX::Warn` as the topic.
    fn run_control_handler_inline(
        &mut self,
        code: &CompiledCode,
        (control_begin, end): (usize, usize),
        fns: &CompiledFns,
        signal: &RuntimeError,
        resume_safe: bool,
        installing: (usize, usize),
    ) -> ControlInlineOutcome {
        // Present the signal to the handler as its topic in `$_` (a `CX::Warn`
        // for a warning, the exception itself for a user control exception),
        // mirroring the unwinding path in `exec_try_catch_op_inner`.
        let topic = Self::control_signal_topic_value(signal);
        let saved_topic = self.env().get("_").cloned();
        // The handler's body belongs to the frame that registered it, so it
        // runs on that activation's env, as an inline `CATCH` handler does
        // (`enter_installing_env`): its by-name reads resolve its own
        // lexicals, and a same-named lexical of a frame between it and the
        // raise site neither shadows them nor takes its writes.
        let handler_env = self.enter_handler_env(code, installing);
        let on_installing = handler_env.as_ref().is_some_and(|h| h.on_installing);
        if let Some(t) = topic.clone() {
            self.env_mut().insert("_".to_string(), t);
        }
        let saved_when = self.when_matched();
        self.set_when_matched(false);

        // The handler's bytecode belongs to the CONTROL-installing frame and
        // addresses *that* frame's local slots, but `self.locals` is the raise
        // site's. `enter_installing_frame` seeds the handler's slots from the
        // installing frame's live slots (from `env` for a warn raised in that
        // frame itself), and `leave_installing_frame` writes the slots the
        // handler changed back, so the installing frame observes them.
        let frame = self.enter_installing_frame(code, Some(installing));
        // The handler runs on the raise site's operand stack; isolate its
        // effects so the suspended computation's stack is left untouched.
        let saved_stack = self.stack.len();
        let carrier = self.suspend_carrier_for_handler(handler_env.is_some());
        let result = if on_installing {
            self.run_handler_range(code, control_begin, end, fns, topic)
        } else {
            // No call frame of the installing activation to take its env
            // from: the handler runs on the raise site's env (or a private
            // overlay of it), where that activation is a caller, so its
            // lexicals are reachable only through the caller chain and the
            // static links in between are ignored (ADR-12529 phase 3).
            // TODO: give the light call paths a record of their caller's env,
            // so the handler always runs on the installing activation's own;
            // the caller chain also shows it the names of the frames in
            // between.
            crate::env::with_static_links_suppressed(|| {
                self.run_handler_range(code, control_begin, end, fns, topic)
            })
        };
        self.resume_carrier_after_handler(carrier);
        self.stack.truncate(saved_stack);
        let matched = self.when_matched();
        self.leave_installing_frame(code, frame, on_installing);
        if let Some(st) = handler_env {
            self.leave_installing_env(code, installing.1, st);
        }

        self.set_when_matched(saved_when);
        if let Some(v) = saved_topic {
            self.env_mut().insert("_".to_string(), v);
        } else {
            self.env_mut().remove("_");
        }

        match result {
            Err(ce) if ce.is_resume() => ControlInlineOutcome::Resumed,
            // The handler re-threw the warn (`warn`/`.rethrow` of CX::Warn):
            // act like the default handler — print and resume.
            Err(ce) if ce.is_warn() => {
                if !self.warning_suppressed() {
                    self.write_warn_to_stderr(&ce.message);
                }
                ControlInlineOutcome::Resumed
            }
            // A `resume_safe` handler that falls through resumes with Nil.
            Ok(()) if resume_safe => ControlInlineOutcome::Resumed,
            Err(ce) if ce.is_succeed() => ControlInlineOutcome::Handled,
            Ok(()) if matched => ControlInlineOutcome::Handled,
            Ok(()) => ControlInlineOutcome::Declined,
            Err(ce) => ControlInlineOutcome::Raised(ce),
        }
    }
}
