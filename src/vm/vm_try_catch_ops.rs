use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_try_catch_op(
        &mut self,
        code: &CompiledCode,
        catch_start: u32,
        control_start: u32,
        body_end: u32,
        explicit_catch: bool,
        resume_safe: bool,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        // Reset any leftover resume_ip from previous exception handling so a
        // later .resume cannot accidentally jump into a sibling scope.
        let saved_resume_ip = self.resume_ip.take();
        let result = self.exec_try_catch_op_inner(
            code,
            catch_start,
            control_start,
            body_end,
            explicit_catch,
            resume_safe,
            ip,
            compiled_fns,
        );
        // Restore the outer resume_ip so an outer .resume sees the right
        // point. When the inner block escaped with an error (e.g. rethrow
        // of CX::Warn), keep the inner resume_ip so an outer handler can
        // resume at the original call site.
        if result.is_ok() {
            self.resume_ip = saved_resume_ip;
        }
        result
    }

    #[allow(clippy::too_many_arguments)]
    fn exec_try_catch_op_inner(
        &mut self,
        code: &CompiledCode,
        catch_start: u32,
        control_start: u32,
        body_end: u32,
        explicit_catch: bool,
        resume_safe: bool,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let saved_depth = self.stack.len();
        let let_mark = self.let_saves_len();
        let body_start = *ip + 1;
        let catch_begin = catch_start as usize;
        let control_begin = control_start as usize;
        let end = body_end as usize;
        let has_control = control_begin < end;
        if has_control {
            self.control_handler_depth += 1;
            // Register this CONTROL handler so a `warn` raised deep inside the
            // protected body (on the Rust call stack, several frames down) can
            // find it. A `resume_safe` handler carries its own bytecode +
            // function table so `builtin_warn` can run it INLINE at the raise
            // site and resume the deep computation, instead of unwinding the
            // Rust stack (which would lose every frame between the warn and
            // here). The depth invariant `control_handlers.len() ==
            // control_handler_depth` is preserved so the innermost handler is
            // always `control_handlers.last()`.
            let handler = if resume_safe {
                Some(crate::vm::ControlHandlerCode {
                    code: std::sync::Arc::new(code.clone()),
                    control_begin,
                    end,
                    compiled_fns: compiled_fns.clone(),
                })
            } else {
                None
            };
            self.control_handlers.push(crate::vm::ControlHandlerEntry {
                resume_safe,
                handler,
            });
        }
        // Guard the protected body with a panic->X:: boundary so an internal
        // Rust panic (overflow/OOB/unwrap) raised anywhere inside it becomes a
        // catchable exception routed to the CATCH handler, instead of crashing.
        let body_result = self.run_range_guarded(code, body_start, catch_begin, compiled_fns);
        if has_control {
            self.control_handler_depth -= 1;
            self.control_handlers.pop();
        }
        match body_result {
            Ok(()) => {
                self.discard_let_saves(let_mark);
                // A `try` that completes normally but yields a soft Failure value
                // (e.g. the result of an expression that returned a Failure rather
                // than throwing) handles that Failure: its value is now "caught",
                // so subsequent uses must not re-throw the stored exception. Per
                // Raku semantics `$!` is then set to that Failure's exception
                // (a successful try with a non-Failure result resets `$!` to Nil).
                let mut failure_exception = None;
                if let Some(top) = self.stack.last()
                    && matches!(top, Value::Instance { class_name, .. } if class_name == "Failure")
                {
                    top.mark_failure_handled();
                    if let Value::Instance { attributes, .. } = top
                        && let Some(exc) = attributes.as_map().get("exception")
                    {
                        failure_exception = Some(exc.clone());
                    }
                }
                self.env_mut()
                    .insert("!".to_string(), failure_exception.unwrap_or(Value::Nil));
                *ip = end;
                Ok(())
            }
            Err(e) if e.is_return() => {
                self.discard_let_saves(let_mark);
                if control_begin < end {
                    self.stack.truncate(saved_depth);
                    let saved_topic = self.env().get("_").cloned();
                    if let Some(signal_topic) = Self::control_signal_topic_value(&e) {
                        self.env_mut().insert("_".to_string(), signal_topic);
                    }
                    let saved_when = self.when_matched();
                    loan_env!(self, set_when_matched(false));
                    match self.run_range(code, control_begin, end, compiled_fns) {
                        Ok(()) => {
                            self.stack.truncate(saved_depth);
                            self.stack.push(Value::Nil);
                        }
                        Err(control_err) if control_err.is_succeed() => {
                            self.stack.truncate(saved_depth);
                            self.stack.push(Value::Nil);
                        }
                        Err(control_err) => return Err(control_err),
                    }
                    loan_env!(self, set_when_matched(saved_when));
                    if let Some(v) = saved_topic {
                        self.env_mut().insert("_".to_string(), v);
                    } else {
                        self.env_mut().remove("_");
                    }
                    *ip = end;
                    Ok(())
                } else {
                    Err(e)
                }
            }
            Err(e)
                if e.return_value.is_some()
                    && !e.is_succeed()
                    && !e.is_warn()
                    && !e.is_take()
                    && !e.is_emit() =>
            {
                self.discard_let_saves(let_mark);
                Err(e)
            }
            // Control signals (warn, last, next, redo, etc.) without a CONTROL
            // block must propagate up — `try` alone does not catch them.
            Err(e)
                if (e.is_last()
                    || e.is_next()
                    || e.is_redo()
                    || e.is_proceed()
                    || e.is_succeed()
                    || e.is_warn()
                    || e.is_take()
                    || e.is_emit()
                    || e.is_done()
                    || e.is_react_done())
                    && control_begin >= end =>
            {
                self.discard_let_saves(let_mark);
                Err(e)
            }
            Err(e)
                if (e.is_last()
                    || e.is_next()
                    || e.is_redo()
                    || e.is_proceed()
                    || e.is_succeed()
                    || e.is_warn()
                    || e.is_take()
                    || e.is_emit()
                    || e.is_done()
                    || e.is_react_done())
                    && control_begin < end =>
            {
                self.discard_let_saves(let_mark);
                self.stack.truncate(saved_depth);
                let saved_topic = self.env().get("_").cloned();
                let saved_when = self.when_matched();
                let mut pending_err = e;
                loop {
                    if let Some(signal_topic) = Self::control_signal_topic_value(&pending_err) {
                        self.env_mut().insert("_".to_string(), signal_topic);
                    }
                    loan_env!(self, set_when_matched(false));
                    let control_result = self.run_range(code, control_begin, end, compiled_fns);
                    let next_resume = match control_result {
                        Ok(()) => None,
                        Err(ref ce) if ce.is_succeed() => None,
                        // Re-throwing a CX::Warn from CONTROL acts like the
                        // default warn handler: print to stderr and resume.
                        Err(ce) if ce.is_warn() => {
                            if !self.warning_suppressed() {
                                self.write_warn_to_stderr(&ce.message);
                            }
                            self.resume_ip.take()
                        }
                        Err(ce) if ce.is_resume() => {
                            let _ = ce;
                            self.resume_ip.take()
                        }
                        Err(ce) => {
                            loan_env!(self, set_when_matched(saved_when));
                            if let Some(v) = saved_topic {
                                self.env_mut().insert("_".to_string(), v);
                            } else {
                                self.env_mut().remove("_");
                            }
                            return Err(ce);
                        }
                    };
                    match next_resume {
                        None => break,
                        Some(resume_point) => {
                            loan_env!(self, set_when_matched(saved_when));
                            if let Some(v) = saved_topic.clone() {
                                self.env_mut().insert("_".to_string(), v);
                            } else {
                                self.env_mut().remove("_");
                            }
                            // A resumable *warn* raised mid-expression (e.g. a Nil
                            // coercion: `Nil.ords`, `Nil.Int`) carries the value
                            // the suspended call should evaluate to. The stack was
                            // truncated to the block base above, so push that value
                            // back so execution continues from the call site with
                            // the call's result in place. Gate strictly on `is_warn`:
                            // other control signals (take/emit/done) also carry a
                            // `return_value` for their own machinery, which must NOT
                            // land on the Interpreter operand stack here.
                            if pending_err.is_warn()
                                && let Some(rv) = pending_err.return_value.take()
                            {
                                self.stack.push(rv);
                            }
                            if has_control {
                                self.control_handler_depth += 1;
                                let handler = if resume_safe {
                                    Some(crate::vm::ControlHandlerCode {
                                        code: std::sync::Arc::new(code.clone()),
                                        control_begin,
                                        end,
                                        compiled_fns: compiled_fns.clone(),
                                    })
                                } else {
                                    None
                                };
                                self.control_handlers.push(crate::vm::ControlHandlerEntry {
                                    resume_safe,
                                    handler,
                                });
                            }
                            let body_result =
                                self.run_range(code, resume_point, catch_begin, compiled_fns);
                            if has_control {
                                self.control_handler_depth -= 1;
                                self.control_handlers.pop();
                            }
                            match body_result {
                                Ok(()) => {
                                    *ip = end;
                                    return Ok(());
                                }
                                Err(new_err)
                                    if new_err.is_last()
                                        || new_err.is_next()
                                        || new_err.is_redo()
                                        || new_err.is_proceed()
                                        || new_err.is_succeed()
                                        || new_err.is_warn()
                                        || new_err.is_take()
                                        || new_err.is_emit()
                                        || new_err.is_done()
                                        || new_err.is_react_done() =>
                                {
                                    pending_err = new_err;
                                    continue;
                                }
                                Err(new_err) => return Err(new_err),
                            }
                        }
                    }
                }
                loan_env!(self, set_when_matched(saved_when));
                if let Some(v) = saved_topic {
                    self.env_mut().insert("_".to_string(), v);
                } else {
                    self.env_mut().remove("_");
                }
                *ip = end;
                Ok(())
            }
            Err(e) => {
                // Exception — restore let saves
                loan_env!(self, restore_let_saves(let_mark));
                // The restore wrote the saved values back into `env` only and
                // recorded each restored name precisely (`restore_let_value`); drain
                // it so this frame's slots refresh.
                self.apply_pending_rw_writeback(code);
                if catch_begin >= control_begin {
                    return Err(e);
                }
                self.stack.truncate(saved_depth);
                let err_val = if let Some(ex) = e.exception.as_ref() {
                    *ex.clone()
                } else {
                    let mut exc_attrs = std::collections::HashMap::new();
                    exc_attrs.insert("message".to_string(), Value::str(e.message.clone()));
                    if let Some(line) = e.line {
                        exc_attrs.insert("line".to_string(), Value::Int(line as i64));
                    }
                    if let Some(ref bt) = e.backtrace {
                        // Build a Backtrace object from the string for
                        // legacy errors that only have a string backtrace.
                        let bt_val = Self::backtrace_value_from_string(bt);
                        exc_attrs.insert("backtrace".to_string(), bt_val);
                    }
                    // An untyped runtime error surfaces as X::AdHoc in Raku (the
                    // class a bare `die "msg"` produces), not the abstract base
                    // Exception. X::AdHoc IS-A Exception, so `throws-like
                    // ..., Exception` / `isa-ok $!, Exception` still match.
                    Value::make_instance(Symbol::intern("X::AdHoc"), exc_attrs)
                };
                let saved_topic = self.env().get("_").cloned();
                self.env_mut().insert("!".to_string(), err_val.clone());
                self.env_mut().insert("_".to_string(), err_val);
                let saved_when = self.when_matched();
                loan_env!(self, set_when_matched(false));
                let catch_stack_base = self.stack.len();
                let when_handled =
                    match self.run_range(code, catch_begin, control_begin, compiled_fns) {
                        Ok(()) => self.when_matched(),
                        // succeed from `when` inside CATCH means exception was handled
                        Err(catch_err) if catch_err.is_succeed() => {
                            // Truncate values left by default body, then push Nil
                            // (Raku: try { die; CATCH { default { "caught" } } } returns Nil)
                            self.stack.truncate(catch_stack_base);
                            self.stack.push(Value::Nil);
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
                            // Resume from the instruction after die
                            if let Some(resume_point) = self.resume_ip.take() {
                                // Run from the resume point to the end of the try body
                                match self.run_range(code, resume_point, catch_begin, compiled_fns)
                                {
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
                // If there's an explicit CATCH block but no `when`/`default`
                // matched, re-throw the exception (Raku semantics).
                if explicit_catch && !when_handled {
                    return Err(e);
                }
                *ip = end;
                Ok(())
            }
        }
    }
}
