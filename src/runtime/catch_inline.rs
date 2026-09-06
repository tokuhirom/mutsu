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
//! that for `CATCH`, mirroring what `try_resume_safe_control_inline` has done for
//! `CONTROL`/`warn` since the cross-frame resumable-warn work.
//!
//! A handler that runs inline and does *not* resume still has to transfer control
//! to the end of its own block, which does require unwinding — so the throw site
//! returns the exception tagged with the region's token and the handler's
//! verdict, and `dispatch_to_catch_handler` recognises its own token and applies
//! the verdict rather than running the handler a second time.

use super::*;
use crate::value::CatchInlineVerdict;

/// Saved execution state for a handler run against the *installing* frame's
/// lexicals while `self.locals` belongs to the deep throw/raise site.
pub(crate) struct InstallingFrame {
    saved_locals: Vec<Value>,
    saved_upvalues: Vec<Option<Value>>,
    /// The reconstructed locals as seeded, so the flush writes back only slots
    /// the handler actually changed.
    seeded: Vec<Value>,
}

impl Interpreter {
    /// Swap `self.locals` for the locals of the frame that installed `code`,
    /// reconstructed from `env` by name (`code.locals[i]` names slot `i`).
    ///
    /// A handler's bytecode addresses the *installing* frame's slots, but at an
    /// inline run `self.locals` is the deep raise/throw site's array. `env` is the
    /// cross-frame-visible store, so it is where the installing frame's values can
    /// be found; this mirrors `reconcile_locals_from_env_at_site`. The upvalue
    /// array is cleared for the same reason: a `GetUpvalue` in the handler range
    /// must fall back to the env read rather than index the wrong frame's array.
    pub(crate) fn enter_installing_frame(&mut self, code: &CompiledCode) -> InstallingFrame {
        let handler_locals: Vec<Value> = code
            .locals
            .iter()
            .map(|name| {
                self.env().get(name).cloned().unwrap_or_else(|| {
                    name.strip_prefix('$')
                        .or_else(|| name.strip_prefix('@'))
                        .or_else(|| name.strip_prefix('%'))
                        .or_else(|| name.strip_prefix('&'))
                        .and_then(|bare| self.env().get(bare).cloned())
                        .unwrap_or(Value::NIL)
                })
            })
            .collect();
        let seeded = handler_locals.clone();
        let saved_locals = std::mem::replace(&mut self.locals, handler_locals);
        let saved_upvalues = std::mem::take(&mut self.upvalues);
        InstallingFrame {
            saved_locals,
            saved_upvalues,
            seeded,
        }
    }

    /// Restore the raise-site registers and flush back every slot the handler
    /// changed, so the installing frame (and any intervening by-name reader)
    /// observes the handler's writes. Only changed slots are written, to keep the
    /// blast radius minimal.
    pub(crate) fn leave_installing_frame(&mut self, code: &CompiledCode, st: InstallingFrame) {
        let InstallingFrame {
            saved_locals,
            saved_upvalues,
            seeded,
        } = st;
        let handler_locals = std::mem::replace(&mut self.locals, saved_locals);
        self.upvalues = saved_upvalues;
        for (i, name) in code.locals.iter().enumerate() {
            if name.is_empty() {
                continue;
            }
            if handler_locals[i] != seeded[i] {
                self.env_mut()
                    .insert(name.clone(), handler_locals[i].clone());
                // The handler mutated the installing frame's lexical `name` by
                // writing `env` here. Record the name for the precise drain
                // (`apply_pending_rw_writeback`) every call site runs: drop-on-miss
                // for the same frame, plus retain-on-miss so a deeper raise site
                // carries it up to the installing frame.
                self.pending_rw_writeback_sources.push(name.clone());
                self.record_caller_var_writeback(name);
                // This env write happened without a call opcode, so a leaf closure
                // between the raise site and the installing frame would otherwise
                // drop it on return. See `inline_control_env_writes`.
                self.inline_control_env_writes
                    .push(crate::symbol::Symbol::intern(name));
            }
        }
    }

    /// Whether `err` is an ordinary (catchable) exception rather than a control
    /// signal. Control signals — `return`, `last`, `next`, `warn`, `take`, `fail`,
    /// `succeed` — have their own routing and must never be diverted into a CATCH
    /// handler here.
    fn is_inline_catchable(err: &RuntimeError) -> bool {
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

    /// ADR-0072 throw-site hook. Runs the innermost active region's `CATCH`
    /// handler inline when that handler can resume, and reports what to do next:
    ///
    /// - `Ok(value)` — the handler called `.resume`; the throw expression yields
    ///   `value` (`Any`) and the throwing frame continues, all Rust frames intact.
    /// - `Err(e)` — no inline handling happened (nothing eligible), or the handler
    ///   ran and did not resume, in which case `e` carries this region's verdict
    ///   so the region applies it without re-running the handler.
    pub(crate) fn try_catch_inline(&mut self, err: RuntimeError) -> Result<Value, RuntimeError> {
        if !Self::is_inline_catchable(&err) || err.catch_inline_verdict().is_some() {
            return Err(err);
        }
        // Only the INNERMOST region is eligible. A nearer region that cannot
        // resume still blocks the inline path: running an outer resuming handler
        // instead would silently skip the nearer one, which is a worse answer than
        // not resuming (see ADR-0072 "Blocking markers").
        let Some(entry) = self.catch_handlers.last() else {
            return Err(err);
        };
        let Some(handler) = entry.handler.as_ref() else {
            return Err(err);
        };
        // Same-frame throws keep the pre-existing frame-local `resume_ip` path,
        // which already resumes them correctly and — unlike the inline path —
        // runs the handler against the live `self.locals` rather than an env
        // reconstruction of the very same frame. See `installing_code`.
        if entry.installing_code == self.current_code {
            return Err(err);
        }
        let token = entry.token;
        let code = handler.code.clone();
        let catch_begin = handler.catch_begin;
        let control_begin = handler.control_begin;
        let fns = handler.compiled_fns.clone();
        // Take the entry off the stack for the duration of the run so a `die`
        // inside the handler is not routed straight back into it.
        let entry = self.catch_handlers.pop().expect("checked above");

        let outcome = self.run_catch_handler_inline(&code, catch_begin, control_begin, &fns, err);

        self.catch_handlers.push(entry);

        match outcome {
            Ok(()) => Ok(Value::package(crate::symbol::Symbol::intern("Any"))),
            Err((verdict, mut e)) => {
                e.set_catch_inline_verdict(Some((token, verdict)));
                Err(e)
            }
        }
    }

    /// Run `code[catch_begin..control_begin]` as a CATCH handler for `err`, with
    /// the exception as the topic. `Ok(())` means the handler resumed.
    fn run_catch_handler_inline(
        &mut self,
        code: &CompiledCode,
        catch_begin: usize,
        control_begin: usize,
        fns: &CompiledFns,
        err: RuntimeError,
    ) -> Result<(), (CatchInlineVerdict, RuntimeError)> {
        let bt_value = err
            .backtrace()
            .map(|bt| Self::backtrace_value_from_string_with_runtime(bt, true));
        let err_val = err.exception_value_with_backtrace(bt_value);

        let saved_topic = self.env().get("_").cloned();
        // Per Raku semantics the handler sees its own `$!`, starting out `Nil`:
        // inside the handler the exception is the topic, and the enclosing scope's
        // `$!` has not been written yet.
        let prior_bang = self.env().get("!").cloned();
        self.env_mut().insert("!".to_string(), Value::NIL);
        self.env_mut().insert("_".to_string(), err_val);
        let saved_when = self.when_matched();
        self.set_when_matched(false);

        let frame = self.enter_installing_frame(code);
        // The handler runs on the throw site's operand stack; isolate its effects
        // so the suspended computation's stack is left exactly as it was.
        let saved_stack = self.stack.len();
        let result = self.run_range(code, catch_begin, control_begin, fns);
        self.stack.truncate(saved_stack);
        let handled = self.when_matched();
        self.leave_installing_frame(code, frame);

        self.set_when_matched(saved_when);
        if let Some(v) = saved_topic {
            self.env_mut().insert("_".to_string(), v);
        } else {
            self.env_mut().remove("_");
        }

        match result {
            // `.resume` — the throw resumes at its own call site.
            Err(ce) if ce.is_resume() => {
                // A resumed exception is handled: `$!` keeps its pre-throw value.
                self.env_mut()
                    .insert("!".to_string(), prior_bang.unwrap_or(Value::NIL));
                Ok(())
            }
            // A `when`/`default` matched and exited via succeed: handled, but not
            // resumed — the region must still be abandoned, which needs unwinding.
            Err(ce) if ce.is_succeed() => {
                let _ = ce;
                self.env_mut()
                    .insert("!".to_string(), prior_bang.unwrap_or(Value::NIL));
                Err((CatchInlineVerdict::Handled, err))
            }
            Ok(()) if handled => {
                self.env_mut()
                    .insert("!".to_string(), prior_bang.unwrap_or(Value::NIL));
                Err((CatchInlineVerdict::Handled, err))
            }
            Ok(()) => Err((CatchInlineVerdict::Unhandled, err)),
            // The handler itself threw (an explicit `die`, a `.rethrow`): that new
            // error replaces the original and propagates past the region.
            Err(ce) => Err((CatchInlineVerdict::Rethrown, ce)),
        }
    }
}
