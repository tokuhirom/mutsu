//! Running a straight-line role method on a mixin without a method frame.
//!
//! `CArray[num64]`'s `ASSIGN-POS` / `AT-POS` are `nqp::bindpos_n(self, $pos,
//! $value)`-sized bodies: local reads and `nqp::` ops, nothing else
//! ([`CompiledCode::ignores_role_context`]). Such a body has no use for the
//! method frame machinery -- the scoped env, the routine frame, the attribute
//! snapshot, the parameter binder -- so once the winner is known (and the
//! winner memo has already proved every parameter's built-in type constraint)
//! it is run here: `self` and the positional arguments go straight into the
//! locals, the ops run, the locals frame is popped (#16215).

use super::*;

impl Interpreter {
    /// Run `method_def` -- a role multi's winner, straight-line, key-decided --
    /// on `target` with `args`. `None` when the call is not of the shape this
    /// lane handles (nothing has been touched then); the caller takes the
    /// general method path.
    ///
    /// The lane handles a method whose compiled body only reads locals that
    /// are `self` or a plain positional parameter, over positional arguments
    /// that are neither pairs nor `Proxy`s, with no return type, no `is rw`,
    /// no defaults and no `Positional` or narrow native-integer parameter (those
    /// bind through a coercion the lane does not repeat; `int` / `int64` bind
    /// an `Int` argument as it is).
    // Cost: O(n + p), n = ops of the body, p = parameters; the body's ops dominate.
    pub(crate) fn run_straight_line_method(
        &mut self,
        method_def: &crate::runtime::MethodDef,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let cc = method_def.compiled_code.as_deref()?;
        if method_def.delegation.is_some()
            || method_def.return_type.is_some()
            || method_def.deprecated_message.is_some()
            || method_def.captured_env.is_some()
            || method_def.is_rw
            || method_def.is_submethod
            || cc.may_observe_named_slurpy
            || cc.uses_dispatcher
            || cc.uses_callframe
            || !cc.state_locals.is_empty()
        {
            return None;
        }
        // The positional parameters, in order, against the arguments.
        let names = method_def.param_storage_names();
        let mut positional = 0usize;
        for (idx, pd) in method_def.param_defs.iter().enumerate() {
            if pd.is_invocant {
                continue;
            }
            if pd.slurpy && pd.name == "%_" {
                continue;
            }
            if pd.named
                || pd.slurpy
                || pd.sigilless
                || pd.optional_marker
                || pd.default.is_some()
                || !pd.traits.is_empty()
                || pd.name.starts_with(['@', '%', '&', '!', '.'])
                || names.get(idx).is_none()
            {
                return None;
            }
            if let Some(tc) = pd.type_constraint.as_deref() {
                if tc.starts_with("Positional") {
                    return None;
                }
                if let Some(native) = self.native_int_binding_type(tc) {
                    // A 64-bit signed native binds an `Int` argument as it is;
                    // every other width wraps or range-checks it, which the
                    // lane does not repeat.
                    let plain = matches!(native, "int" | "int64")
                        && args
                            .get(positional)
                            .is_some_and(|a| matches!(a.unwrap_varref().view(), ValueView::Int(_)));
                    if !plain {
                        return None;
                    }
                }
            }
            positional += 1;
        }
        if args.len() != positional
            || args.iter().any(|a| {
                let a = a.unwrap_varref();
                a.is_any_pair_value() || a.is_proxy_value() || a.is_container_ref()
            })
        {
            return None;
        }
        // Every local is `self`, a bound parameter, or a frame fixture the body
        // cannot read (`ignores_role_context`, `may_observe_named_slurpy`).
        let invocant_name = method_def
            .param_defs
            .iter()
            .position(|pd| pd.is_invocant)
            .and_then(|idx| names.get(idx));
        let local_ok = |name: &str| {
            matches!(name, "self" | "__ANON_STATE#" | "?CLASS" | "?ROLE" | "%_")
                || invocant_name.is_some_and(|n| n.as_str() == name)
                || method_def
                    .param_defs
                    .iter()
                    .zip(names.iter())
                    .any(|(pd, n)| !pd.is_invocant && !pd.slurpy && n.as_str() == name)
        };
        if !cc.locals.iter().all(|name| local_ok(name.as_str())) {
            return None;
        }

        if self.stack_check_due() {
            crate::grow_and_reenter!(self.run_straight_line_method(method_def, target, args));
            if let Err(e) = crate::vm::vm_stack_guard::check_now() {
                return Some(Err(e));
            }
        }
        let unit = self.unit_of_source_sym(cc.source_file);
        let saved_unit = std::mem::replace(&mut self.current_unit, unit);
        let saved_stack_depth = self.stack.len();
        let caller_frame = self.locals.push_frame(cc.locals.len());
        for (i, local) in cc.locals.iter().enumerate() {
            let name = local.as_str();
            let value = if matches!(name, "self" | "__ANON_STATE#" | "?CLASS" | "?ROLE" | "%_")
                || invocant_name.is_some_and(|n| n.as_str() == name)
            {
                target.clone()
            } else {
                // The n-th positional parameter takes the n-th argument.
                let mut arg = 0usize;
                let mut found = Value::NIL;
                for (pd, n) in method_def.param_defs.iter().zip(names.iter()) {
                    if pd.is_invocant || pd.slurpy {
                        continue;
                    }
                    if n.as_str() == name {
                        found = Self::itemize_fast_method_arg(Some(pd), args[arg].clone());
                        break;
                    }
                    arg += 1;
                }
                found
            };
            self.locals[i] = value;
        }

        let compiled_fns = method_def.compiled_fns.as_deref();
        let empty_fns = crate::opcode::CompiledFns::default();
        let compiled_fns = compiled_fns.unwrap_or(&empty_fns);
        let polls = crate::vm::vm_poll::DispatchPolls::current();
        let frame_caller_code = self.current_code;
        let mut ip = 0;
        let mut raised: Option<Result<Value, RuntimeError>> = None;
        while ip < cc.ops.len() {
            if let Err(e) = self.exec_one_backedge_polled(cc, &mut ip, compiled_fns, polls) {
                raised = Some(if e.is_fail() {
                    Ok(self.fail_error_to_failure_value(&e))
                } else {
                    Err(e)
                });
                break;
            }
        }
        self.current_code = frame_caller_code;
        let outcome = raised.unwrap_or_else(|| {
            Ok(if self.stack.len() > saved_stack_depth {
                self.stack.pop().unwrap_or(Value::NIL)
            } else {
                Value::NIL
            })
        });
        self.stack.truncate(saved_stack_depth);
        self.locals.pop_frame(caller_frame);
        self.current_unit = saved_unit;
        Some(self.finalize_method_result_fast(cc, outcome, None, None, None))
    }
}
