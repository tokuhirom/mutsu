//! Routing an exception to a region's `CATCH` handler, and finishing one whose
//! handler already ran inline at the throw site (ADR-0072).
//!
//! Split out of `vm_try_catch_ops.rs`, which owns the `TryCatch` opcode itself.

use super::*;

impl Interpreter {
    /// ADR-0072: finish an exception whose CATCH handler already ran inline at
    /// the throw site. The handler body is NOT re-run; only this region's own
    /// disposition (end the region, swallow into `$!`, or keep propagating) is
    /// applied, exactly as the tail of `dispatch_to_catch_handler` would.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn apply_inline_catch_verdict(
        &mut self,
        e: RuntimeError,
        verdict: crate::value::CatchInlineVerdict,
        explicit_catch: bool,
        traps: bool,
        end: usize,
        saved_depth: usize,
        ip: &mut usize,
    ) -> Result<(), RuntimeError> {
        use crate::value::CatchInlineVerdict as V;
        match verdict {
            // The handler threw (or re-threw): that error replaces the original
            // and propagates past this region, exactly as the non-inline path's
            // `Err(catch_err) => return Err(catch_err)` does.
            V::Rethrown => {
                self.stack.truncate(saved_depth);
                Err(e)
            }
            V::Handled => {
                // Mirror the non-inline path's `set_when_matched(saved_when ||
                // when_handled)`, so an enclosing CATCH region can still see that
                // a nested one handled the exception.
                self.set_when_matched(true);
                // A handled exception ends the region with `Nil`, the same value
                // the non-inline path leaves (the compiled CATCH range ends in
                // `LoadNil`). `$!` was already restored to its pre-throw value by
                // the inline runner.
                self.stack.truncate(saved_depth);
                self.stack.push(Value::NIL);
                *ip = end;
                Ok(())
            }
            V::Unhandled => {
                // Nothing matched, so the exception is still live. Publish it in
                // `$!` and dispose of it the way this region would: an explicit
                // CATCH re-throws what it did not handle, and an implicit wrapper
                // around a block that merely *contains* a phaser is not a trap
                // either — only a genuine `try` swallows.
                let err_val = e.exception_value_with_backtrace(
                    e.backtrace()
                        .map(|bt| Self::backtrace_value_from_string_with_runtime(bt, true)),
                );
                self.env_mut().insert("!".to_string(), err_val);
                self.stack.truncate(saved_depth);
                if explicit_catch || !traps {
                    return Err(e);
                }
                self.stack.push(Value::NIL);
                *ip = end;
                Ok(())
            }
        }
    }

    /// Route an exception to this region's `CATCH` handler (the bytecode range
    /// `catch_begin..control_begin`) and decide what happens to it afterwards:
    /// handled by a matching `when`/`default`, re-thrown, or swallowed into `$!`.
    ///
    /// Reached both from the ordinary exception path and from a `CONTROL` block
    /// that *declined* an illegal control signal (`next` with no enclosing loop),
    /// which Raku turns into a catchable `X::ControlFlow` here.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn dispatch_to_catch_handler(
        &mut self,
        code: &CompiledCode,
        e: RuntimeError,
        catch_begin: usize,
        control_begin: usize,
        end: usize,
        explicit_catch: bool,
        traps: bool,
        catch_token: Option<u64>,
        saved_depth: usize,
        ip: &mut usize,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        // ADR-0072: this region's own CATCH handler already ran, INLINE at the
        // throw site, and did not `.resume` — so the exception is unwinding to
        // here only to have the region abandoned. Apply the recorded verdict
        // instead of running the handler a second time.
        if let Some((token, verdict)) = e.catch_inline_verdict()
            && Some(token) == catch_token
        {
            return self.apply_inline_catch_verdict(
                e,
                verdict,
                explicit_catch,
                traps,
                end,
                saved_depth,
                ip,
            );
        }
        if catch_begin >= control_begin {
            return Err(e);
        }
        self.stack.truncate(saved_depth);
        // Build a Backtrace object from the string for legacy errors
        // that only have a string backtrace.
        //
        // A *compile-time* diagnosis (anything doing `X::Comp`) reaches here
        // with no backtrace at all, which used to leave `.backtrace` answering
        // the empty-string placeholder — a `Str`, so `.is-runtime` could not be
        // asked of it. rakudo always hands back a real `Backtrace` there, with
        // `is-runtime` False; synthesize one from the live stack of the code
        // that triggered the compilation (an `EVAL`, a `use`), which is exactly
        // the non-setting frame rakudo's own compile-time backtrace ends with.
        let is_comp = e
            .exception
            .as_deref()
            .is_some_and(|ex| self.type_matches_value("X::Comp", ex));
        let bt_value = match e.backtrace() {
            Some(bt) => Some(Self::backtrace_value_from_string_with_runtime(bt, !is_comp)),
            None if is_comp => Some(self.build_backtrace_value_with_runtime(false)),
            None => None,
        };
        let err_val = e.exception_value_with_backtrace(bt_value);
        let saved_topic = self.env().get("_").cloned();
        // Per Raku semantics `$!` is only *updated* to the exception when it
        // propagates out of the `try` unhandled (swallowed by the implicit
        // trap). A CATCH that handles the exception (a matching
        // `when`/`default`, or `.resume`) leaves `$!` at whatever it held
        // before the `try`. Remember that prior value so the handled paths
        // below can restore it.
        let prior_bang = self.env().get("!").cloned();
        // The CATCH block gets its own `$!`, which starts out `Nil`: inside
        // the handler the exception is the *topic* (`$_`), and the enclosing
        // scope's `$!` has not been written yet. It is only updated below,
        // once the handler is done and the exception turns out to be
        // unhandled (an implicit `try` trap swallows it into `$!`).
        self.env_mut().insert("!".to_string(), Value::NIL);
        self.env_mut().insert("_".to_string(), err_val.clone());
        let saved_when = self.when_matched();
        loan_env!(self, set_when_matched(false));
        let catch_stack_base = self.stack.len();
        let when_handled = match self.run_range(code, catch_begin, control_begin, compiled_fns) {
            Ok(()) => self.when_matched(),
            // succeed from `when` inside CATCH means exception was handled
            Err(catch_err) if catch_err.is_succeed() => {
                // Truncate values left by default body, then push Nil
                // (Raku: try { die; CATCH { default { "caught" } } } returns Nil)
                self.stack.truncate(catch_stack_base);
                self.stack.push(Value::NIL);
                true
            }
            // .resume called inside CATCH: resume execution after the die
            Err(catch_err) if catch_err.is_resume() => {
                self.stack.truncate(catch_stack_base);
                loan_env!(self, set_when_matched(saved_when));
                if let Some(v) = saved_topic {
                    self.env_mut().insert("_".to_string(), v);
                } else {
                    self.env_mut().remove("_");
                }
                // A resumed exception is handled: restore `$!` to its
                // pre-`try` value so the resumed body and the code after
                // the `try` see the prior `$!`, not the handled exception.
                self.env_mut()
                    .insert("!".to_string(), prior_bang.unwrap_or(Value::NIL));
                // Resume from the instruction after die
                if let Some(resume_point) = self.take_resume_ip_for(code) {
                    // Run from the resume point to the end of the try body
                    match self.run_range(code, resume_point, catch_begin, compiled_fns) {
                        Ok(()) => {}
                        Err(resume_err) => return Err(resume_err),
                    }
                }
                *ip = end;
                return Ok(());
            }
            Err(catch_err) => return Err(catch_err),
        };
        // Propagate when_handled upward so an enclosing CATCH region
        // can detect that this nested CATCH (e.g., a CATCH inside a
        // CATCH) handled the exception.
        self.set_when_matched(saved_when || when_handled);
        if let Some(v) = saved_topic {
            self.env_mut().insert("_".to_string(), v);
        } else {
            self.env_mut().remove("_");
        }
        // A handled exception (a matching `when`/`default`) leaves `$!` at
        // its pre-`try` value. When nothing matched, `$!` keeps the
        // exception: an explicit CATCH re-throws (below), and an implicit
        // `try` trap swallows it with the exception left in `$!`.
        if when_handled {
            self.env_mut()
                .insert("!".to_string(), prior_bang.unwrap_or(Value::NIL));
        } else {
            // Nothing matched: the exception is still live, so publish it in
            // the enclosing `$!` now that the handler (which saw `Nil`) is
            // done. An explicit CATCH re-throws just below; an implicit
            // `try` trap swallows it with the exception left in `$!`.
            self.env_mut().insert("!".to_string(), err_val);
        }
        // Nothing matched, so the exception is still live. Only a
        // genuine `try` without a CATCH swallows it into `$!`: an
        // explicit CATCH re-throws what it did not handle, and an
        // implicit wrapper around a block that merely *contains* a
        // CATCH/CONTROL phaser is not a trap at all, so
        // `{ die "x"; CONTROL { } }` must propagate.
        if !when_handled && (explicit_catch || !traps) {
            return Err(e);
        }
        *ip = end;
        Ok(())
    }
}
