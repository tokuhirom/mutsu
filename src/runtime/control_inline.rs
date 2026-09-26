//! Running a `CONTROL` handler INLINE at a `warn` raise site (#9469).
//!
//! Rakudo runs a CONTROL handler on top of the stack, in the dynamic scope of
//! the raise, and unwinds only when the handler does not resume. mutsu's VM
//! recurses on the Rust stack for every call, so unwinding to the region that
//! owns the handler destroys every frame between the raise site and it: an
//! op-raised warning (`"a" ~ Any`) has no resume point at all, and one raised
//! in a callee cannot be resumed into. So a handler that can resume runs here,
//! at the raise site, before anything unwinds.
//!
//! Two kinds of handler are eligible:
//!
//! - `resume_safe` (`control_block_is_resume_safe`): provably always resumes,
//!   so a run that falls through counts as resuming with `Nil`.
//! - resume-*capable* (`OpCode::TryCatch::control_resume_capable`): its
//!   bytecode calls `.resume` somewhere. It may not resume, so the outcome is
//!   read off the run, exactly as ADR-0072 does for `CATCH`
//!   (`runtime/catch_inline.rs`).
//!
//! Handlers are tried innermost first. One whose `when` arms match nothing
//! declines, and the warning moves on to the next outer handler, still at the
//! raise site. One that matches without resuming ends its region: the raise
//! site returns the warn signal stamped with that region's token and
//! `Handled`, and the region (`exec_try_catch_op_inner`) applies the verdict
//! without running its handler again. When every handler declines, the default
//! handler prints the warning and resumes. Reaching a handler that is not
//! eligible falls back to the unwinding path; the regions that already
//! declined are stamped `Unhandled` so they do not run a second time.

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
    /// Register a CONTROL handler for the protected body of a region. A
    /// `resume_safe` or resume-capable handler carries its own bytecode and
    /// function table so the raise site can run it inline.
    // Cost: O(1), except an inline-eligible handler deep-clones the enclosing
    // `CompiledCode` (c = ops + constants) and `CompiledFns` (f entries): O(c + f).
    // Rakudo: O(1) -- see #9172.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn push_control_handler(
        &mut self,
        code: &CompiledCode,
        control_begin: usize,
        end: usize,
        resume_safe: bool,
        resume_capable: bool,
        handles_take: bool,
        token: u64,
        compiled_fns: &CompiledFns,
    ) {
        self.control_handler_depth += 1;
        let handler = (resume_safe || resume_capable).then(|| crate::vm::ControlHandlerCode {
            code: std::sync::Arc::new(code.clone()),
            control_begin,
            end,
            compiled_fns: compiled_fns.clone(),
        });
        self.control_handlers.push(crate::vm::ControlHandlerEntry {
            resume_safe,
            handler,
            handles_take,
            token,
        });
    }

    /// Offer a warning to the active CONTROL handlers at its raise site.
    ///
    /// - `Some(Ok(_))`: the warning was resumed (by a handler, or by the default
    ///   handler after every handler declined); the raise expression continues.
    /// - `Some(Err(e))`: a handler ended its region without resuming, or raised
    ///   a new error. `e` carries the verdict stamp where one applies.
    /// - `None`: the innermost handler cannot run inline and nothing ran; take
    ///   the ordinary unwinding path.
    pub(crate) fn try_control_inline(
        &mut self,
        message: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        let mut idx = self.control_handlers.len();
        // Token of the outermost handler that has declined so far.
        let mut declined: Option<u64> = None;
        while idx > 0 {
            idx -= 1;
            let entry = &self.control_handlers[idx];
            let Some(handler) = entry.handler.as_ref() else {
                // Not eligible: unwind to it. The handlers inside it already
                // ran; the stamp keeps their regions from running them again.
                let token = declined?;
                let mut err = RuntimeError::warn_signal(message.to_string());
                err.set_catch_inline_verdict(Some((token, CatchInlineVerdict::Unhandled)));
                return Some(Err(err));
            };
            let resume_safe = entry.resume_safe;
            let token = entry.token;
            let code = handler.code.clone();
            let control_begin = handler.control_begin;
            let end = handler.end;
            let fns = handler.compiled_fns.clone();
            // The handler runs with only the handlers outside it registered, so
            // a `warn` inside it goes outward rather than back into itself.
            // `control_handler_depth` deliberately stays put: the run loop
            // prints-and-resumes a warn signal on the spot when the depth is 0,
            // which would swallow a `.rethrow` inside this handler instead of
            // letting it reach the outcome below. A `warn` raised in the handler
            // with no outer handler registered reaches the default handler at
            // the end of this function.
            let inner = self.control_handlers.split_off(idx);
            let outcome = self.run_control_handler_inline(
                &code,
                control_begin,
                end,
                &fns,
                message,
                resume_safe,
            );
            self.control_handlers.extend(inner);
            match outcome {
                ControlInlineOutcome::Resumed => return Some(Ok(Value::NIL)),
                ControlInlineOutcome::Handled => {
                    let mut err = RuntimeError::warn_signal(message.to_string());
                    err.set_catch_inline_verdict(Some((token, CatchInlineVerdict::Handled)));
                    return Some(Err(err));
                }
                ControlInlineOutcome::Declined => declined = Some(token),
                ControlInlineOutcome::Raised(e) => return Some(Err(e)),
            }
        }
        // Every handler declined (or, inside a handler run, none is registered
        // outside it): the default handler prints and resumes.
        if !self.warning_suppressed() {
            self.write_warn_to_stderr(message);
        }
        Some(Ok(Value::NIL))
    }

    /// Run `code[control_begin..end]` as a CONTROL handler for a warning, with
    /// a `CX::Warn` as the topic.
    fn run_control_handler_inline(
        &mut self,
        code: &CompiledCode,
        control_begin: usize,
        end: usize,
        fns: &CompiledFns,
        message: &str,
        resume_safe: bool,
    ) -> ControlInlineOutcome {
        // Present the warning to the handler as a `CX::Warn` topic in `$_`,
        // mirroring the unwinding path in `exec_try_catch_op_inner`.
        let warn_signal = RuntimeError::warn_signal(message.to_string());
        let topic = Self::control_signal_topic_value(&warn_signal);
        let saved_topic = self.env().get("_").cloned();
        if let Some(t) = topic {
            self.env_mut().insert("_".to_string(), t);
        }
        let saved_when = self.when_matched();
        self.set_when_matched(false);

        // The handler's bytecode belongs to the CONTROL-installing frame and
        // addresses *that* frame's local slots, but `self.locals` is the raise
        // site's. `enter_installing_frame` reconstructs the installing frame's
        // locals from `env`, and `leave_installing_frame` flushes the slots the
        // handler changed back, so the installing frame (and any intervening
        // by-name reader) observes the handler's writes.
        let frame = self.enter_installing_frame(code);
        // The handler runs on the raise site's operand stack; isolate its
        // effects so the suspended computation's stack is left untouched.
        let saved_stack = self.stack.len();
        let result = self.run_range(code, control_begin, end, fns);
        self.stack.truncate(saved_stack);
        let matched = self.when_matched();
        self.leave_installing_frame(code, frame);

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
