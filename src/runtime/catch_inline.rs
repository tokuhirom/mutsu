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

/// Saved execution state for a handler run against the *installing* frame's
/// lexicals while `self.locals` belongs to the deep throw/raise site.
pub(crate) struct InstallingFrame {
    /// Absolute slot index of the installing frame's slot 0 when its live
    /// slots seeded the run, so the flush writes the handler's changes
    /// straight back into them.
    live_base: Option<usize>,
    saved_locals_base: crate::runtime::locals::CallerFrame,
    saved_upvalues: Vec<Option<Value>>,
    /// The reconstructed locals as seeded, so the flush writes back only slots
    /// the handler actually changed.
    seeded: Vec<Value>,
}

impl Interpreter {
    /// Swap `self.locals` for the locals of the frame that installed `code`,
    /// reconstructed from `env` by name (`code.locals[i]` names slot `i`).
    ///
    /// `installing_base` is the installing frame's base on the shared slot
    /// stack, when the caller knows it (a CATCH region records it). That
    /// frame is suspended below the throw site with its slots intact, so a
    /// non-`Nil` slot is taken from there: an env lookup by name at the throw
    /// site would find a same-named lexical of the dying routine instead.
    ///
    /// A handler's bytecode addresses the *installing* frame's slots, but at an
    /// inline run `self.locals` is the deep raise/throw site's array. `env` is the
    /// cross-frame-visible store, so it is where the installing frame's values can
    /// be found; this mirrors `reconcile_locals_from_env_at_site`. The upvalue
    /// array is cleared for the same reason: a `GetUpvalue` in the handler range
    /// must fall back to the env read rather than index the wrong frame's array.
    // Cost: O(l), l = the installing code's locals.
    pub(crate) fn enter_installing_frame(
        &mut self,
        code: &CompiledCode,
        installing: Option<(usize, usize)>,
    ) -> InstallingFrame {
        let installing_base = installing.map(|(base, _)| base);
        // The installing activation's upvalue array, saved by the call frame
        // it pushed next -- when that frame really is its own (its caller
        // base is the installing base). Without it a `GetUpvalue` in the
        // handler falls back to an env read at the throw site, where a
        // same-named lexical of the dying routine shadows the capture.
        let installing_upvalues = installing.and_then(|(base, depth)| {
            self.installing_call_frame(base, depth)
                .map(|f| f.saved_upvalues.clone())
        });
        // The installing frame lies wholly below the executing one, or its
        // region cannot be trusted (a code object that grew after install).
        let live_base = installing_base.filter(|&b| b + code.locals.len() <= self.locals.base());
        let handler_locals: Vec<Value> = code
            .locals
            .iter()
            .enumerate()
            .map(|(i, name)| {
                if let Some(b) = live_base {
                    let v = &self.locals.all_slots()[b + i];
                    if !v.is_nil() {
                        return v.clone();
                    }
                }
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
        let saved_locals_base = self.locals.push_frame_from(&handler_locals);
        let saved_upvalues =
            std::mem::replace(&mut self.upvalues, installing_upvalues.unwrap_or_default());
        InstallingFrame {
            live_base,
            saved_locals_base,
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
            live_base,
            saved_locals_base,
            saved_upvalues,
            seeded,
        } = st;
        // The flush below reads the handler's slots, so copy them out before
        // closing the frame — a frame's slots do not outlive it (ADR-0077).
        let handler_locals = self.locals.to_vec();
        self.locals.pop_frame(saved_locals_base);
        self.upvalues = saved_upvalues;
        for (i, name) in code.locals.iter().enumerate() {
            if name.is_empty() {
                continue;
            }
            if handler_locals[i] != seeded[i] {
                if let Some(b) = live_base {
                    *self.locals.absolute_slot_mut(b + i) = handler_locals[i].clone();
                }
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

    /// The call frame the installing activation (slot base `base`) pushed
    /// when it made its call at depth `depth`, if that is really its own.
    // Cost: O(1).
    fn installing_call_frame(&self, base: usize, depth: usize) -> Option<&crate::vm::VmCallFrame> {
        self.call_frames
            .get(depth)
            .filter(|f| f.saved_locals_base.as_ref().map(|c| c.base()) == Some(base))
    }

    /// Make the installing activation's env the executing one for an inline
    /// handler run, so the handler's by-name reads (`GetGlobal`,
    /// `GetHashVar`, ...) resolve its own lexicals: at the throw site a
    /// same-named lexical of the dying routine would shadow them. That env is
    /// the one the installing activation saved in the call frame it pushed
    /// next (`call_frames[depth]`, when its caller base proves it is that
    /// activation's). The dynamics declared on the way to the throw stay
    /// visible, copied into the handler's overlay: rakudo runs the handler in
    /// the dynamic scope of the throw. `None` when there is no such frame; the
    /// handler then runs in the throw site's env, as before.
    // Cost: O(e), e = entries in the throw site's env tiers.
    fn enter_installing_env(&mut self, base: usize, depth: usize) -> Option<HandlerEnv> {
        let installing = self.installing_call_frame(base, depth)?.saved_env.clone();
        let dynamics = self.env().tier_dynamic_entries();
        let mut env = crate::env::Env::scoped_child(installing);
        for (k, v) in &dynamics {
            env.insert_sym(*k, v.clone());
        }
        let throw_env = std::mem::replace(self.env_mut(), env);
        Some(HandlerEnv {
            throw_env,
            dynamics,
        })
    }

    /// Undo [`Self::enter_installing_env`] and route the handler's writes. A
    /// write to a lexical the throw site sees as the same variable also goes
    /// to the throw site's env (the by-name store the frames in between hand
    /// back up on return, as before); one the throw site shadows goes only to
    /// the installing activation's saved env. Dynamics go to the throw site.
    // Cost: O(w), w = names the handler wrote.
    fn leave_installing_env(&mut self, depth: usize, st: HandlerEnv) {
        let HandlerEnv {
            throw_env,
            dynamics,
        } = st;
        let handler_env = std::mem::replace(self.env_mut(), throw_env);
        let topic = crate::symbol::Symbol::intern("_");
        let bang = crate::symbol::Symbol::intern("!");
        for (k, v) in handler_env.overlay_iter() {
            if *k == topic || *k == bang {
                continue;
            }
            if k.is_dynamic_var_env_key() {
                if dynamics.iter().any(|(dk, dv)| dk == k && dv == v) {
                    continue;
                }
                self.env_mut().insert_sym(*k, v.clone());
                continue;
            }
            let installing = self.call_frames.get(depth).map(|f| &f.saved_env);
            let same_var = match (
                self.env().get_sym(*k),
                installing.and_then(|e| e.get_sym(*k)),
            ) {
                (None, _) => true,
                (Some(a), Some(b)) => a == b,
                (Some(_), None) => false,
            };
            if same_var {
                self.env_mut().insert_sym(*k, v.clone());
            }
            if let Some(f) = self.call_frames.get_mut(depth) {
                f.saved_env.insert_sym(*k, v.clone());
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

    /// Run `f` with no `CATCH` region registered, restoring the caller's
    /// regions afterwards. For code that runs on this thread only as an
    /// optimization of running it elsewhere — a `.then` callback on an
    /// already-resolved promise, which Rakudo always cues on the scheduler —
    /// so a `die` inside it is captured by the Rust caller (it breaks the
    /// derived promise) and must never be handled inline by a `CATCH` of the
    /// frame that merely registered the callback. Otherwise that handler runs
    /// once at the `die` and again when the broken promise is awaited.
    /// Cost: O(1).
    pub(crate) fn with_catch_regions_isolated<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let saved = std::mem::take(&mut self.catch_handlers);
        let out = f(self);
        self.catch_handlers = saved;
        out
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
        let mut idx = self.catch_handlers.len();
        while idx > 0 {
            idx -= 1;
            let entry = &self.catch_handlers[idx];
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
            let inner = self.catch_handlers.split_off(idx);
            let throw_package = self.current_package_sym();
            let routine_len = self.routine_stack.len();
            let method_tail = if same_frame {
                None
            } else {
                if routine_depth > 0 && routine_len > routine_depth {
                    let frame = self.routine_stack[routine_depth - 1];
                    self.routine_stack.push(frame);
                }
                (self.method_class_stack.len() > method_depth)
                    .then(|| self.method_class_stack.split_off(method_depth))
            };
            if installing_package != throw_package {
                self.set_current_package_with_sym(
                    installing_package.resolve().to_string(),
                    installing_package,
                );
            }
            let outcome = self.run_catch_handler_inline(
                &code,
                (catch_begin, control_begin),
                (same_frame, installing_base, installing_call_depth),
                &fns,
                err,
            );
            self.catch_handlers.extend(inner);
            self.routine_stack.truncate(routine_len);
            if let Some(tail) = method_tail {
                self.method_class_stack.truncate(method_depth);
                self.method_class_stack.extend(tail);
            }
            if installing_package != throw_package {
                self.set_current_package_with_sym(
                    throw_package.resolve().to_string(),
                    throw_package,
                );
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
        let prior_bang = self.env().get("!").cloned();
        let handler_env = (!same_frame)
            .then(|| self.enter_installing_env(installing_base, installing_call_depth));
        let handler_env = handler_env.flatten();
        self.env_mut().insert("!".to_string(), Value::NIL);
        self.env_mut().insert("_".to_string(), err_val);
        let saved_when = self.when_matched();
        self.set_when_matched(false);

        let frame = (!same_frame).then(|| {
            self.enter_installing_frame(code, Some((installing_base, installing_call_depth)))
        });
        // The handler runs on the throw site's operand stack; isolate its effects
        // so the suspended computation's stack is left exactly as it was.
        let saved_stack = self.stack.len();
        let result = self.run_range(code, catch_begin, control_begin, fns);
        self.stack.truncate(saved_stack);
        let handled = self.when_matched();
        if let Some(frame) = frame {
            self.leave_installing_frame(code, frame);
        }
        if let Some(st) = handler_env {
            self.leave_installing_env(installing_call_depth, st);
        }

        self.set_when_matched(saved_when);
        if let Some(v) = saved_topic {
            self.env_mut().insert("_".to_string(), v);
        } else {
            self.env_mut().remove("_");
        }

        // A handled (or resumed) exception leaves `$!` at its pre-throw value.
        let mut restore_bang = || {
            self.env_mut()
                .insert("!".to_string(), prior_bang.clone().unwrap_or(Value::NIL));
        };
        match result {
            // `.resume` — the throw resumes at its own call site.
            Err(ce) if ce.is_resume() => {
                restore_bang();
                CatchRunOutcome::Resumed
            }
            // A `when`/`default` matched and exited via succeed: handled, but not
            // resumed — the region must still be abandoned, which needs unwinding.
            Err(ce) if ce.is_succeed() => {
                restore_bang();
                CatchRunOutcome::Handled(err, ce.return_value)
            }
            Ok(()) if handled => {
                restore_bang();
                CatchRunOutcome::Handled(err, None)
            }
            Ok(()) => CatchRunOutcome::Declined(err),
            // The handler itself threw (an explicit `die`, a `.rethrow`, or a
            // control signal such as `return`).
            Err(ce) => CatchRunOutcome::Raised(ce),
        }
    }
}

/// The throw site's env, set aside while an inline handler runs in the
/// installing activation's (see `Interpreter::enter_installing_env`).
struct HandlerEnv {
    throw_env: crate::env::Env,
    /// The throw site's dynamics as copied into the handler's overlay, so only
    /// the ones the handler changed are written back.
    dynamics: Vec<(crate::symbol::Symbol, Value)>,
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
