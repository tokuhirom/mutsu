//! ADR-0072: running a resume-capable `CATCH` handler INLINE at the throw site.
//!
//! mutsu's VM recurses on the Rust stack for every call, so by the time a `die`
//! raised inside a nested sub reaches the region that owns the `CATCH`, every
//! frame between the throw and the handler has already been popped by `?`. There
//! is nothing left to resume into, which is why `resume_ip` is deliberately
//! frame-local (`take_resume_ip_for`) and why `.resume` only ever worked when the
//! throw and the handler shared one `CompiledCode`.
//!
//! Rakudo does not save a continuation either: it runs the handler in the dynamic
//! scope of the throw, *before* unwinding, so `.resume` is nothing more than the
//! handler returning and the `die` evaluating to `Any`. This module implements
//! that for `CATCH`, mirroring what `try_control_inline` has done for
//! `CONTROL`/`warn` since the cross-frame resumable-warn work.
//!
//! A handler that runs inline and does *not* resume still has to transfer control
//! to the end of its own block, which does require unwinding — so the throw site
//! returns the exception tagged with the region's token and the handler's
//! verdict, and `dispatch_to_catch_handler` recognises its own token and applies
//! the verdict rather than running the handler a second time.

use super::*;
use crate::value::{CatchInlinePayload, CatchInlineVerdict};

impl Interpreter {
    /// Whether `err` is an ordinary (catchable) exception rather than a control
    /// signal. Control signals — `return`, `last`, `next`, `warn`, `take`, `fail`,
    /// `succeed` — have their own routing and must never be diverted into a CATCH
    /// handler here.
    pub(crate) fn is_inline_catchable(err: &RuntimeError) -> bool {
        err.control.is_none() && err.return_value.is_none() && !err.is_leave
    }

    /// Whether the method named by `name_idx` is a *resumable throw site* — a
    /// `.throw`, which raises a brand-new exception and so resumes at its own
    /// call site. Deliberately NOT `.rethrow`: that re-raises an exception whose
    /// original throw point is already gone, so resuming it at the `.rethrow`
    /// would resume somewhere rakudo does not (ADR-0072 Slice 2).
    pub(crate) fn method_name_is_resumable_throw(
        &self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> bool {
        matches!(
            code.constants.get(name_idx as usize).map(|c| c.view()),
            Some(ValueView::Str(s)) if s.as_str() == "throw"
        )
    }

    /// ADR-0072 throw-site hook. Runs the active regions' `CATCH` handlers
    /// inline, innermost first, in the dynamic scope of the throw (Slices 2-3),
    /// and reports what to do next:
    ///
    /// - `Ok(value)` — a handler called `.resume`; the throw expression yields
    ///   `value` (`Any`) and the throwing frame continues, all Rust frames intact.
    /// - `Err(e)` — the exception has to unwind. When a handler already ran, `e`
    ///   carries a verdict stamp: the stamping region applies it without running
    ///   its handler again, and every region nested inside it (a larger token)
    ///   declines on the way out, because its handler ran too.
    ///
    /// Each handler runs with only the regions outside it registered, so a
    /// `die` inside it goes outward rather than back into itself. A handler
    /// that matches nothing, or re-throws an ordinary exception (`.rethrow`),
    /// passes it on to the next outer handler, still at the throw site — which
    /// is what lets an outer `.resume` reach the original `die` (row 13). The
    /// chain stops at a region that cannot run inline (a `try` with no `CATCH`,
    /// or one installed by the throwing code object itself); that region and
    /// everything outside it then see the exception by unwinding, as before.
    pub(crate) fn try_catch_inline(&mut self, err: RuntimeError) -> Result<Value, RuntimeError> {
        if !Self::is_inline_catchable(&err) || err.catch_inline_verdict().is_some() {
            return Err(err);
        }
        let mut err = err;
        // The outermost region whose handler ran without ending the chain, and
        // how it disposed of the exception. Stamped onto the error if the chain
        // falls back to unwinding, so those handlers do not run a second time.
        let mut passed: Option<(u64, CatchInlineVerdict)> = None;
        let mut idx = self.control.catch_handlers.len();
        while idx > 0 {
            idx -= 1;
            let entry = &self.control.catch_handlers[idx];
            let Some(handler) = entry.handler.as_ref() else {
                break;
            };
            // A throw in the activation that installed the region runs the
            // handler against the live `self.locals`: its bytecode addresses
            // exactly those slots. Any other frame gets an env reconstruction
            // of the installing frame (`enter_installing_frame`).
            let same_frame = entry.installing_code == self.current_code
                && entry.installing_base == self.locals.base();
            let token = entry.token;
            let return_target = entry.return_target;
            let installing_base = entry.installing_base;
            let installing_call_depth = entry.installing_call_depth;
            let routine_depth = entry.installing_routine_depth;
            let method_depth = entry.installing_method_depth;
            let installing_package = entry.installing_package;
            let code = handler.code.clone();
            let catch_begin = handler.catch_begin;
            let control_begin = handler.control_begin;
            let fns = handler.compiled_fns.clone();
            let inner = self.control.catch_handlers.split_off(idx);
            let throw_package = self.current_package_sym();
            let routine_len = self.routine_stack.len();
            let method_tail = if same_frame {
                None
            } else {
                if routine_depth > 0 && routine_len > routine_depth {
                    let mut frame = self.routine_stack[routine_depth - 1];
                    // The frame at the installing depth is often the `try`
                    // block's own, which carries no `def_file`. Left bare, the
                    // executing-unit walk skips it and lands on the dying
                    // routine's frames, so the handler resolved its imports
                    // against the throw site's compunit (`Unknown function`).
                    // Stamp the installing unit's file from the nearest
                    // enclosing frame that has one.
                    if frame.def_file.is_none() {
                        frame.def_file = self.routine_stack[..routine_depth]
                            .iter()
                            .rev()
                            .find_map(|f| f.def_file);
                    }
                    self.routine_stack.push_copy(frame);
                }
                (self.types.method_class_stack.len() > method_depth)
                    .then(|| self.types.method_class_stack.split_off(method_depth))
            };
            if installing_package != throw_package {
                self.set_current_package_sym(installing_package);
            }
            let outcome = self.run_catch_handler_inline(
                &code,
                (catch_begin, control_begin),
                (same_frame, installing_base, installing_call_depth),
                &fns,
                err,
            );
            self.control.catch_handlers.extend(inner);
            self.routine_stack.truncate(routine_len);
            if let Some(tail) = method_tail {
                self.types.method_class_stack.truncate(method_depth);
                self.types.method_class_stack.extend(tail);
            }
            if installing_package != throw_package {
                self.set_current_package_sym(throw_package);
            }
            match outcome {
                CatchRunOutcome::Resumed => return Ok(Value::package(crate::symbol::wk::any())),
                CatchRunOutcome::Handled(mut e, value) => {
                    e.set_catch_inline_verdict(Some((token, CatchInlineVerdict::Handled)));
                    e.set_catch_inline_payload(value.map(CatchInlinePayload::Value));
                    return Err(e);
                }
                CatchRunOutcome::Declined(e) => {
                    passed = Some((token, CatchInlineVerdict::Unhandled));
                    err = e;
                }
                // A `die` inside the handler already offered its exception to
                // the outer handlers at its own throw site, and one of them
                // stamped it: it belongs to that region now.
                CatchRunOutcome::Raised(e) if e.catch_inline_verdict().is_some() => {
                    return Err(e);
                }
                CatchRunOutcome::Raised(e) if Self::is_inline_catchable(&e) => {
                    passed = Some((token, CatchInlineVerdict::Rethrown));
                    err = e;
                }
                // A control signal (`return`, `next`, ...) is raised from here,
                // on top of the stack, exactly as rakudo raises it: a `next` in
                // a handler reaches the loop innermost at the *throw*.
                // A `return` still targets the routine that installed the
                // handler, not the first routine it meets on the way out.
                CatchRunOutcome::Raised(mut signal) => {
                    if signal.is_return()
                        && signal.return_target_callable_id().is_none()
                        && !same_frame
                    {
                        signal.set_return_target_callable_id(return_target);
                    }
                    return Err(signal);
                }
            }
        }
        if passed.is_some() {
            err.set_catch_inline_verdict(passed);
        }
        Err(err)
    }

    /// Run `code[catch_begin..control_begin]` as a CATCH handler for `err`, with
    /// the exception as the topic.
    fn run_catch_handler_inline(
        &mut self,
        code: &CompiledCode,
        (catch_begin, control_begin): (usize, usize),
        (same_frame, installing_base, installing_call_depth): (bool, usize, usize),
        fns: &CompiledFns,
        err: RuntimeError,
    ) -> CatchRunOutcome {
        let err_val = err.exception_value_with_backtrace(|| {
            err.backtrace()
                .map(|bt| Self::backtrace_value_from_string_with_runtime(bt, true))
        });

        let saved_topic = self.env().get("_").cloned();
        // Per Raku semantics the handler sees its own `$!`, starting out `Nil`:
        // inside the handler the exception is the topic, and the enclosing scope's
        // `$!` has not been written yet.
        let handler_env = (!same_frame)
            .then(|| self.enter_handler_env(code, (installing_base, installing_call_depth)));
        let handler_env = handler_env.flatten();
        self.env_mut().assign_error_var(Value::NIL);
        self.env_mut().insert("_".to_string(), err_val.clone());
        let saved_when = self.when_matched();
        self.set_when_matched(false);

        let frame = (!same_frame).then(|| {
            self.enter_installing_frame(code, Some((installing_base, installing_call_depth)))
        });
        // The handler runs on the throw site's operand stack; isolate its effects
        // so the suspended computation's stack is left exactly as it was.
        let saved_stack = self.stack.len();
        let carrier = self.suspend_carrier_for_handler(handler_env.is_some());
        let result = self.run_handler_range(code, catch_begin, control_begin, fns, Some(err_val.clone()));
        self.resume_carrier_after_handler(carrier);
        self.stack.truncate(saved_stack);
        let handled = self.when_matched();
        if let Some(frame) = frame {
            let on_installing = handler_env.as_ref().is_some_and(|h| h.on_installing);
            self.leave_installing_frame(code, frame, on_installing);
        }
        if let Some(st) = handler_env {
            self.leave_installing_env(code, installing_call_depth, st);
        }

        self.set_when_matched(saved_when);
        if let Some(v) = saved_topic {
            self.env_mut().insert("_".to_string(), v);
        } else {
            self.env_mut().remove("_");
        }

        // A handled (or resumed) exception is published in `$!`, as rakudo's
        // `try` does (#12904).
        match result {
            // `.resume` — the throw resumes at its own call site.
            Err(ce) if ce.is_resume() => {
                self.env_mut().assign_error_var(err_val);
                CatchRunOutcome::Resumed
            }
            // A `when`/`default` matched and exited via succeed: handled, but not
            // resumed — the region must still be abandoned, which needs unwinding.
            Err(ce) if ce.is_succeed() => {
                self.env_mut().assign_error_var(err_val);
                CatchRunOutcome::Handled(err, ce.return_value)
            }
            Ok(()) if handled => {
                self.env_mut().assign_error_var(err_val);
                CatchRunOutcome::Handled(err, None)
            }
            Ok(()) => CatchRunOutcome::Declined(err),
            // The handler itself threw (an explicit `die`, a `.rethrow`, or a
            // control signal such as `return`).
            Err(ce) => CatchRunOutcome::Raised(ce),
        }
    }
}

/// What one inline `CATCH` handler run decided.
enum CatchRunOutcome {
    /// `.resume`: the throw site continues.
    Resumed,
    /// A `when`/`default` matched without resuming: the region ends, with the
    /// value the arm succeeded with, if any.
    Handled(RuntimeError, Option<Value>),
    /// No arm matched: the exception is still live.
    Declined(RuntimeError),
    /// The handler raised something (a new or re-thrown exception, or a
    /// control signal); it replaces the original.
    Raised(RuntimeError),
}
