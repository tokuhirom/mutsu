use super::*;

impl Interpreter {
    /// `OpCode::CallFunc` whose callee is a native atomic helper
    /// (`flags::NATIVE_ATOMIC_HELPER`): the `⚛`-operators and `cas` lower to
    /// these, and the call goes straight to `try_native_atomic_function`.
    ///
    /// The helpers live in a RESERVED namespace the compiler emits and no user
    /// routine is declared in, so none of the machinery `exec_call_func_op_inner`
    /// runs for an ordinary call can answer differently: the frame-lexical
    /// routine, `nqp::`, increment, NativeCall and empty-proto gates, the
    /// light-call and OTF cache probes, the `CALL-ME` mixin probe, junction
    /// auto-threading (it skips every `__mutsu_` name itself), the wrap chain, the
    /// lexical override, `has_proto` / `find_compiled_function_memo`,
    /// `user_only_sub_hides_builtin` and the `imported_env_aliases` probe all
    /// miss for an unregistered name. In `roast/S17-lowlevel/cas-int.t` they were
    /// ~5-8k instructions per `cas` (#12120), against 4-6k for the helper itself.
    ///
    /// The arguments are prepared exactly as the general path prepared them:
    ///
    /// * the first argument of an `_var` helper keeps its `VarRef` tag, since it
    ///   names the target binding and its frame slot (`normalize_call_args_for_target`),
    ///   and every other argument unwraps to its value;
    /// * the synthetic callsite-line marker is stripped and recorded;
    /// * `Proxy` arguments are FETCHed, except in lvalue-assignment context.
    ///
    /// After the call the `is rw` writeback drains into the caller's slots, on
    /// the error path too, as the general path does.
    ///
    /// Only a call with no argument-source descriptor comes here (the compiler
    /// emits none for these helpers): a `|EXPR` or named argument keeps the
    /// general path, which is what decodes it. A name the helper table does not
    /// answer is an internal error (`NATIVE_ATOMIC_HELPER_NAMES` is pinned
    /// against it by a unit test).
    // Cost: O(a), a = arity (argument drain, unwrap and proxy check), plus the
    // helper's own cost.
    pub(super) fn exec_atomic_helper_call_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
    ) -> Result<(), RuntimeError> {
        let arity = arity as usize;
        if self.stack.len() < arity {
            return Err(RuntimeError::new("Interpreter stack underflow in CallFunc"));
        }
        let keep_target =
            code.const_sym(name_idx).flags() & crate::symbol::flags::ATOMIC_TARGET_HELPER != 0;
        let start = self.stack.len() - arity;
        let mut args: Vec<Value> = self.stack.drain(start..).collect();
        for (i, arg) in args.iter_mut().enumerate() {
            if i > 0 || !keep_target {
                *arg = Self::unwrap_var_ref_value(std::mem::replace(arg, Value::NIL));
            }
        }
        let (args, callsite_line) = self.sanitize_call_args_owned(args);
        let args = if self.in_lvalue_assignment {
            args
        } else {
            self.auto_fetch_proxy_args(args)?
        };
        loan_env!(self, set_pending_callsite_line(callsite_line));
        // The general path leaves no call-argument sources pending.
        self.set_pending_call_arg_sources(None);
        let name = Self::const_str(code, name_idx);
        let result = match self.try_native_atomic_function(name, &args) {
            Some(Ok(result)) => result,
            Some(Err(e)) => {
                self.apply_pending_rw_writeback(code);
                return Err(e);
            }
            None => {
                return Err(RuntimeError::new(format!(
                    "internal error: '{name}' is marked a native atomic helper but has no handler"
                )));
            }
        };
        self.apply_pending_rw_writeback(code);
        self.stack.push(result);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// `NATIVE_ATOMIC_HELPER_NAMES` may only name helpers
    /// `try_native_atomic_function` answers, or the shortcut would turn a working
    /// call into an internal error. No arguments at all, so every handler fails
    /// fast (an `Err`, or a panic on a missing one: either way it was reached,
    /// which is all this asks) rather than running a retry loop.
    #[test]
    fn every_flagged_atomic_helper_has_a_handler() {
        for name in crate::symbol::NATIVE_ATOMIC_HELPER_NAMES {
            let handled = std::panic::catch_unwind(|| {
                let mut interp = Interpreter::new();
                interp.try_native_atomic_function(name, &[]).is_some()
            });
            assert!(
                handled.unwrap_or(true),
                "{name} is in NATIVE_ATOMIC_HELPER_NAMES but try_native_atomic_function declines it"
            );
        }
        // And the flag is what the call op reads.
        for name in crate::symbol::NATIVE_ATOMIC_HELPER_NAMES {
            let sym = crate::symbol::Symbol::intern(name);
            assert_ne!(
                sym.flags() & crate::symbol::flags::NATIVE_ATOMIC_HELPER,
                0,
                "{name} lost its NATIVE_ATOMIC_HELPER flag"
            );
        }
        let plain = crate::symbol::Symbol::intern("__mutsu_cas_not_a_helper");
        assert_eq!(
            plain.flags() & crate::symbol::flags::NATIVE_ATOMIC_HELPER,
            0
        );
    }

    /// Only the `_var` helpers keep the target `VarRef` on their first argument.
    #[test]
    fn only_var_helpers_keep_their_target_tag() {
        let tagged = |n: &str| {
            crate::symbol::Symbol::intern(n).flags() & crate::symbol::flags::ATOMIC_TARGET_HELPER
                != 0
        };
        assert!(tagged("__mutsu_cas_var"));
        assert!(tagged("__mutsu_cas_add_var"));
        assert!(tagged("__mutsu_atomic_post_inc_var"));
        assert!(!tagged("__mutsu_cas_array_elem"));
        assert!(!tagged("__mutsu_atomic_elem"));
        assert!(!tagged("say"));
    }
}
