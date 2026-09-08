use super::*;

impl Interpreter {
    /// `OpCode::CallFunc` whose callee is an `nqp::` op.
    ///
    /// `nqp::` is a RESERVED namespace: no user routine can be declared there,
    /// so none of the machinery `exec_call_func_op` runs for an ordinary call
    /// — the increment/NativeCall/empty-proto gates, the light-call and OTF
    /// cache probes, `normalize_call_args_for_target`'s registry probes,
    /// `decode_arg_sources` — can ever change what the op table answers. The
    /// op table already sat at the end of that chain (it was reached only after
    /// every cache had missed); this entry, keyed on the callee symbol's
    /// memoized `flags::NQP_OP` bit, takes it first. The vendored `Test.rakumod`
    /// runs five `nqp::` ops per assertion (`nqp::time` twice, `nqp::join` /
    /// `nqp::split` twice each, `nqp::iseq_i`), and each paid ~4k instructions
    /// of call-dispatch overhead for a ~1k op
    /// (vendor-real-test-module-flip, #7554).
    ///
    /// The arguments are prepared exactly as the general path prepared them
    /// before it reached the op table, so the ops see the same values:
    ///
    /// * `|EXPR` positions spread (`spread_call_args_by_syntax`);
    /// * a `VarRef` capture unwraps to its value — the general path did this
    ///   through `normalize_call_args_for_target`, whose registry probes all
    ///   miss for an unregistered name, so it always chose the plain arguments
    ///   (`nqp::eqaddr(Int, Int)` depends on it);
    /// * the synthetic callsite-line marker is stripped and recorded;
    /// * `Proxy` arguments are FETCHed (`nqp::` is not in
    ///   `callee_takes_arg_containers`, and lvalue-assignment context never
    ///   targets an nqp op).
    ///
    /// The rw-source descriptor is NOT decoded: an `nqp::` op binds no `is rw`
    /// parameter, so it never contributes a writeback source, and the
    /// companion slot map is repopulated (cleared first) by the next call that
    /// does decode it.
    pub(super) fn exec_nqp_call_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        arg_sources_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let arity = arity as usize;
        if self.stack.len() < arity {
            return Err(RuntimeError::new("Interpreter stack underflow in CallFunc"));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        let (args, _) = Self::spread_call_args_by_syntax(code, raw_args, arg_sources_idx, None);
        let args: Vec<Value> = args.into_iter().map(Self::unwrap_var_ref_value).collect();
        // Unconditional, exactly as the general path: a `None` here clears a
        // marker line a preceding call left pending.
        let (args, callsite_line) = self.sanitize_call_args_owned(args);
        loan_env!(self, set_pending_callsite_line(callsite_line));
        let args = self.auto_fetch_proxy_args(args)?;
        let name = Self::const_str(code, name_idx);
        let op = name
            .strip_prefix(crate::symbol::NQP_OP_PREFIX)
            .expect("exec_nqp_call_op reached with a non-nqp:: callee");
        let result = self.dispatch_nqp_op(op, &args)?;
        self.stack.push(result);
        Ok(())
    }
}
