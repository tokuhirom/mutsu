//! The body loop of a compiled method frame, shared by the fast binder
//! (`call_compiled_method_fast_in_unit`) and the lean planned frame
//! (`vm_method_lean_frame`), so the two cannot disagree on how a `return`,
//! `leave`, `fail` or JIT entry ends the body.

use super::*;

/// The frame facts a method body run needs.
pub(super) struct MethodBodyFrame {
    /// The class that declares the method and the method's name: a bare
    /// `leave` targets `owner::method`.
    pub(super) owner_sym: Symbol,
    pub(super) method_sym: Symbol,
    /// The frame's callable id, which a `return` / `fail` names.
    pub(super) method_callable_id: u64,
    /// The value-stack depth at frame entry.
    pub(super) saved_stack_depth: usize,
    /// The `let`/`temp` journal length at body entry.
    pub(super) let_mark: usize,
}

/// How a method body run ended.
pub(super) struct MethodBodyOutcome {
    /// `Ok` when the body completed (fell through, returned, left or turned a
    /// `fail` into a Failure); the error that escapes the frame otherwise.
    pub(super) result: Result<(), RuntimeError>,
    /// The value of an explicit `return` / `leave` that targeted this frame.
    pub(super) explicit_return: Option<Value>,
    /// A `fail` was turned into this frame's Failure result, which the
    /// return-type check must not see.
    pub(super) fail_bypass: bool,
}

impl Interpreter {
    /// Run `cc`'s ops as the body of the method `owner_sym.method_sym`
    /// (callable id `frame.method_callable_id`), entering it natively when the JIT
    /// has compiled it. Restores `current_code` to the caller's on exit.
    // Cost: O(1) per op beyond the ops themselves.
    pub(super) fn run_compiled_method_body(
        &mut self,
        cc: &CompiledCode,
        compiled_fns: &CompiledFns,
        frame: MethodBodyFrame,
    ) -> MethodBodyOutcome {
        let MethodBodyFrame {
            owner_sym,
            method_sym,
            method_callable_id,
            saved_stack_depth,
            let_mark,
        } = frame;
        let mut ip = 0;
        let mut result = Ok(());
        let mut explicit_return: Option<Value> = None;
        let mut fail_bypass = false;
        let polls = crate::vm::vm_poll::DispatchPolls::current();
        let frame_caller_code = self.current_code;
        while ip < cc.ops.len() {
            // JIT entry (ADR-0004 J2): same hook as vm_call_light.rs — at body
            // start, run the whole body natively when the chunk is hot and
            // Tier A-compilable; the native outcome threads through the same
            // match arms below.
            let step = if ip == 0
                && let Some(r) = crate::vm::vm_jit::try_enter(self, cc, compiled_fns)
            {
                ip = cc.ops.len();
                r
            } else {
                self.exec_one_backedge_polled(cc, &mut ip, compiled_fns, polls)
            };
            match step {
                Ok(()) => {}
                Err(mut e) if e.is_leave => {
                    let routine_key = crate::qualified::qualified(owner_sym, method_sym);
                    let matches_frame = if let Some(_target_id) = e.leave_callable_id() {
                        false
                    } else if let Some(target_routine) = e.leave_routine() {
                        target_routine == routine_key.as_str()
                    } else {
                        e.label.is_none()
                    };
                    if matches_frame {
                        e.is_leave = false;
                        e.control = None;
                        let ret_val = e.return_value.unwrap_or(Value::NIL);
                        explicit_return = Some(ret_val.clone());
                        self.stack.truncate(saved_stack_depth);
                        self.stack.push(ret_val.clone());
                        self.resolve_frame_let_saves(let_mark, &ret_val);
                        result = Ok(());
                        break;
                    }
                    loan_env!(self, restore_let_saves(let_mark));
                    result = Err(e);
                    break;
                }
                Err(e)
                    if e.return_value.is_some()
                        && !e.is_yield_signal()
                        && !cc.lets_succeed_through(&e) =>
                {
                    if let Some(target_id) = e.return_target_callable_id()
                        && target_id != method_callable_id
                    {
                        loan_env!(self, restore_let_saves(let_mark));
                        result = Err(e);
                        break;
                    }
                    let ret_val = e.return_value.unwrap();
                    explicit_return = Some(ret_val.clone());
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(ret_val.clone());
                    self.resolve_frame_let_saves(let_mark, &ret_val);
                    result = Ok(());
                    break;
                }
                Err(e) if e.is_fail() => {
                    if e.fail_passes_frame(Some(method_callable_id)) {
                        loan_env!(self, restore_let_saves(let_mark));
                        result = Err(e);
                        break;
                    }
                    fail_bypass = true;
                    let failure = self.fail_error_to_failure_value(&e);
                    loan_env!(self, restore_let_saves(let_mark));
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(failure);
                    result = Ok(());
                    break;
                }
                Err(e) => {
                    loan_env!(self, restore_let_saves(let_mark));
                    result = Err(e);
                    break;
                }
            }
            if self.is_halted() {
                break;
            }
        }
        self.current_code = frame_caller_code;
        MethodBodyOutcome {
            result,
            explicit_return,
            fail_bypass,
        }
    }

    /// The value a finished method frame hands back for `base`: a result
    /// that is the invocant itself carries the frame's reconciled attributes
    /// (when `:=` recovery adjusted them beyond the cell) or shares the
    /// invocant's live cell.
    // Cost: O(1), plus a write-back of the reconciled map when there is one.
    pub(super) fn method_frame_result_sharing(
        base: &Value,
        v: Value,
        reconciled: &Option<crate::value::AttrMap>,
    ) -> Value {
        match (base.view(), v.view()) {
            (
                ValueView::Instance {
                    class_name,
                    attributes: base_attrs,
                    id: base_id,
                },
                ValueView::Instance { id: ret_id, .. },
            ) if base_id == ret_id => {
                if let Some(m) = reconciled {
                    Value::write_back_sharing(&base_attrs, class_name, m.clone(), base_id)
                } else {
                    Value::instance_sharing_cell(&base_attrs, class_name, base_id)
                }
            }
            _ => v,
        }
    }
}
