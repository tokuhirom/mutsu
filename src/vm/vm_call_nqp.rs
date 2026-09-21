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

    /// `OpCode::NqpOp` — an `nqp::` VALUE op whose identity and operand count
    /// the compiler already settled (`try_compile_nqp_value_op`).
    ///
    /// This is the same work `exec_nqp_call_op` above does, with everything
    /// that was re-deriving a compile-time fact removed. Each step it drops is
    /// dropped because the compiler only emits this opcode for the shape that
    /// makes the step a no-op, not because the step was found unnecessary in
    /// practice:
    ///
    /// * `spread_call_args_by_syntax` — a `|EXPR` position keeps the
    ///   `CallFunc` path, so `arity` operands on the stack ARE the operands;
    /// * `sanitize_call_args_owned` — that scan looks for the synthetic
    ///   callsite-line `Pair` the test-assertion lowering injects, and no nqp
    ///   op site carries one. Clearing a line a PRECEDING call left pending is
    ///   still done unconditionally, exactly as before: rakudo's own
    ///   `Test.rakumod` runs several nqp ops per assertion, so this opcode
    ///   sits between a test call and the next one;
    /// * the `Vec`s — the operands move into a buffer reused across ops
    ///   (`nqp_arg_scratch`), and the `VarRef` unwrap and `Proxy` FETCH happen
    ///   in place in it rather than each rebuilding the list;
    /// * the callee-string lookup and the walk down the table chain — the id
    ///   names the op and its owning table (`dispatch_nqp_op_by_id`).
    ///
    /// `literal_native_args` is cleared for the duration rather than carried:
    /// it ranks a `multi`'s native-vs-boxed candidates for the CALL it belongs
    /// to, an nqp op ranks no candidates, and leaving the enclosing call's
    /// mask visible would offer it to whatever an op dispatches into (an
    /// `AT-KEY` override reached through `nqp::atkey`).
    pub(super) fn exec_nqp_op(&mut self, id: u16, arity: usize) -> Result<(), RuntimeError> {
        if self.stack.len() < arity {
            return Err(RuntimeError::new("Interpreter stack underflow in NqpOp"));
        }
        if self.exec_nqp_pure_op(id, arity) {
            return Ok(());
        }
        let mut args = std::mem::take(&mut self.nqp_arg_scratch);
        args.clear();
        let start = self.stack.len() - arity;
        args.extend(self.stack.drain(start..).map(Self::unwrap_var_ref_value));
        loan_env!(self, set_pending_callsite_line(None));
        let saved_literals = std::mem::replace(&mut self.literal_native_args, 0);
        // Keep the `MUTSU_VM_STATS` dispatch tally comparable across this
        // change: an nqp op used to reach `exec_call_func_op_inner`, which
        // counted it here.
        crate::vm::vm_stats::record_function_dispatch();
        let result = self
            .fetch_nqp_proxy_operands(&mut args)
            .and_then(|()| self.dispatch_nqp_op_by_id(id, &args));
        self.literal_native_args = saved_literals;
        args.clear();
        self.nqp_arg_scratch = args;
        self.stack.push(result?);
        Ok(())
    }

    /// The direct form of [`Self::exec_nqp_op`] for the `nqp::` ops that are
    /// pure functions of native operands — `add_i`, `iseq_i`, `bitand_i`,
    /// `add_n`, ... (`crate::runtime::nqp_pure`).
    ///
    /// `true` means the op ran and its result is on the stack. `false` means
    /// this is not one of those ops, or its operands are not already native,
    /// and the caller keeps the general path.
    ///
    /// Everything the general path does around the op body is dropped here,
    /// and each one because the op's OWN definition makes it a no-op rather
    /// than because it was found unnecessary in practice:
    ///
    /// * the argument list — the operands are read where the caller's opcodes
    ///   left them, on the stack, and a pure op keeps no reference to them;
    /// * the `VarRef` unwrap, the container deref and the `Proxy` FETCH —
    ///   `try_eval_native` declines every one of those operand shapes, so the
    ///   general path is still the only place they are normalized;
    /// * `set_pending_callsite_line(None)` — that clears a marker line so the
    ///   next test assertion does not inherit it, and a pure op cannot reach
    ///   an assertion: it dispatches nowhere. The general path still clears it
    ///   for every op that can;
    /// * the `literal_native_args` save/clear/restore — that mask ranks a
    ///   `multi`'s native-vs-boxed candidates for whatever an op dispatches
    ///   into, and a pure op dispatches into nothing.
    ///
    /// The `MUTSU_VM_STATS` dispatch tally is still bumped, so the counter
    /// stays comparable across this change.
    fn exec_nqp_pure_op(&mut self, id: u16, arity: usize) -> bool {
        let Some(op) = crate::runtime::nqp_pure::pure_op(id) else {
            return false;
        };
        let start = self.stack.len() - arity;
        let Some(result) = crate::runtime::nqp_pure::try_eval_native(op, &self.stack[start..])
        else {
            return false;
        };
        crate::vm::vm_stats::record_function_dispatch();
        self.stack.truncate(start);
        self.stack.push(result);
        true
    }

    /// FETCH any `Proxy` operand in place. The general call path rebuilt the
    /// whole argument list to do this (`auto_fetch_proxy_args`); an nqp op's
    /// operands are almost never `Proxy`, so probe and only write back the
    /// ones that are.
    fn fetch_nqp_proxy_operands(&mut self, args: &mut [Value]) -> Result<(), RuntimeError> {
        for arg in args {
            if arg.is_proxy_value() {
                let fetched = loan_env!(self, auto_fetch_proxy(arg))?;
                *arg = fetched;
            }
        }
        Ok(())
    }
}
