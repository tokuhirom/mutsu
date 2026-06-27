use super::*;

impl Interpreter {
    /// Interpreter-native Stdout emit (③後段 PR-C), mirroring `Interpreter::emit_output`:
    /// bump the Stdout-target handle's `bytes_written`, then push to the sink
    /// (immediate real-stdout flush / buffer / thread-clone shared buffer per the
    /// sink's decision). `subtest_active` comes from the interpreter (TAP state
    /// stays interpreter-owned). Build the payload before calling — no guard is
    /// held across re-entrant work.
    pub(crate) fn vm_emit_stdout(&mut self, text: &str) {
        let byte_count = text.len() as i64;
        {
            let mut table = self.io_handles_mut();
            if let Some(h) = table.map.values_mut().find(|h| h.is_stdout_target()) {
                h.add_bytes_written(byte_count);
            }
        }
        let subtest_active = self.subtest_active();
        self.output_sink_mut().emit(text, subtest_active);
    }

    /// Interpreter-native Stderr emit (③後段 PR-C), mirroring the `Stderr` branch of
    /// `write_to_handle_value_trying` (immediate real-stderr flush or the stderr
    /// buffer; no `bytes_written` scan, no `output_emitted`).
    pub(crate) fn vm_emit_stderr(&mut self, text: &str) {
        let subtest_active = self.subtest_active();
        self.output_sink_mut().emit_stderr(text, subtest_active);
    }

    /// Invoke a callable value using the Interpreter fast paths when available and
    /// return the interpreter state to the caller.
    pub(crate) fn call_value(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        self.vm_call_on_value(target, args, None)
    }

    // (CP-3 collapse) The Interpreter's env / env_mut / clone_env / set_env / take_env
    // accessors are gone — they duplicated the canonical `Interpreter` methods
    // (env now lives on the merged struct), so callers reach those directly.

    /// env-loan (CP-1 1e): swap the Interpreter-owned env into the interpreter's loan
    /// slot, run `f` (a carrier that reads `self.env`), then swap the
    /// env back. The interpreter sees the live env for the duration of the
    /// carrier; the nested ping-pong (`run_block_raw` → `mem::take(self)` →
    /// `Interpreter::new`) carries the loaned env into the inner Interpreter and back, so the swap
    /// nests correctly. Returns whatever the carrier returns.
    #[inline]
    pub(crate) fn loan_env_for<R>(&mut self, f: impl FnOnce(&mut Interpreter) -> R) -> R {
        // CP-3 collapse: the Interpreter dissolved into the Interpreter, so there is no
        // separate interpreter to lend the env to — env is just `self.env`. This
        // is now a thin self-call kept so the existing call sites need no edit.
        f(self)
    }

    #[inline]
    pub(crate) fn vm_call_function(
        &mut self,
        name: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        self.loan_env_for(|i| i.call_function(name, args))
    }

    #[inline]
    pub(crate) fn vm_call_sub_value(
        &mut self,
        func: Value,
        args: Vec<Value>,
        merge_all: bool,
    ) -> Result<Value, RuntimeError> {
        self.loan_env_for(|i| i.call_sub_value(func, args, merge_all))
    }

    #[inline]
    pub(crate) fn vm_call_function_fallback(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        self.loan_env_for(|i| i.call_function_fallback(name, args))
    }

    #[inline]
    pub(crate) fn vm_call_method_with_values(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        self.loan_env_for(|i| i.call_method_with_values(target, method, args))
    }

    #[inline]
    #[allow(clippy::type_complexity)]
    pub(crate) fn vm_run_instance_method(
        &mut self,
        receiver_class_name: &str,
        attributes: HashMap<String, Value>,
        method_name: &str,
        args: Vec<Value>,
        invocant: Option<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        self.loan_env_for(|i| {
            i.run_instance_method(receiver_class_name, attributes, method_name, args, invocant)
        })
    }

    #[inline]
    pub(crate) fn vm_run_block_raw(&mut self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        if stmts.is_empty() {
            return Ok(());
        }
        // CP-3 collapse PoC: instead of bouncing to `Interpreter::run_block_raw`
        // (which spins up a fresh sub-Interpreter via `mem::take`/`Interpreter::new` — the
        // ping-pong), compile the block (pure, no env) and run it in-place on
        // this Interpreter via `run_nested`, sharing the same interpreter + env directly.
        let (code, compiled_fns) = self.compile_block_raw(stmts);
        let result = self.run_nested(&code, &compiled_fns);
        // Slice F (env<->locals coherence): a deferred role body that mutates an
        // outer lexical (`role R { $side = @outer.elems * 100 }`) writes it into
        // `env` by name; record those names so the caller (the role-registration
        // opcode, which holds the outer `code`) writes them through to the outer
        // frame's local slots, dropping the dependency on the reverse pull. The
        // topic is excluded as a per-call alias.
        for sym in &code.free_var_writes {
            sym.with_str(|fname| {
                if fname != "_" && fname != "@_" && fname != "%_" {
                    self.pending_rw_writeback_sources.push(fname.to_string());
                }
            });
        }
        // Mirror `run_block_raw`'s trailing DESTROY pass (may run user code, so
        // it needs the env loaned).
        self.loan_env_for(|i| i.run_pending_instance_destroys())?;
        result.map(|_| ())
    }

    pub(crate) fn vm_eval_block_value(&mut self, body: &[Stmt]) -> Result<Value, RuntimeError> {
        if body.is_empty() {
            return Ok(Value::Nil);
        }
        // CP-3 collapse: when the block is pure expression statements (no sub/
        // proto/operator declarations and no trailing-sub value), the registry +
        // code-env save/restore that `Interpreter::eval_block_value` performs is a
        // no-op, so run the block in-place via `run_nested` (no `mem::take`/
        // `Interpreter::new` ping-pong) and only replicate the cheap scope bookkeeping
        // (block-scope depth + let/temp restore + DESTROY pass). All current
        // callers pass a single `Stmt::Expr` (registration-time default / enum /
        // role-arg evaluation). Any other shape falls back to the interpreter.
        if body.iter().all(|s| matches!(s, Stmt::Expr(_))) {
            let (code, compiled_fns) = self.compile_block_value(body);
            let let_mark = self.let_saves_len();
            self.push_block_scope_depth();
            let result = self.run_nested(&code, &compiled_fns);
            self.pop_block_scope_depth();
            self.restore_let_saves(let_mark);
            self.loan_env_for(|i| i.run_pending_instance_destroys())?;
            return result.map(|v| v.unwrap_or(Value::Nil));
        }
        self.loan_env_for(|i| i.eval_block_value(body))
    }

    #[inline]
    pub(crate) fn vm_use_module_with_tags(
        &mut self,
        module: &str,
        tags: &[String],
    ) -> Result<(), RuntimeError> {
        self.loan_env_for(|i| i.use_module_with_tags(module, tags))
    }

    #[inline]
    pub(crate) fn vm_call_method_mut_with_values(
        &mut self,
        target_var: &str,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        self.loan_env_for(|i| i.call_method_mut_with_values(target_var, target, method, args))
    }

    #[inline]
    pub(crate) fn vm_set_var_type_constraint(&mut self, name: &str, constraint: Option<String>) {
        self.loan_env_for(|i| i.set_var_type_constraint(name, constraint))
    }

    pub(crate) fn last_stack_value(&self) -> Option<&Value> {
        if self.stack.len() == 1 {
            self.stack.last()
        } else {
            None
        }
    }

    /// Override the source variable used when mutating `$_` in Interpreter execution.
    pub(crate) fn set_topic_source_var(&mut self, name: Option<String>) {
        self.topic_source_var = name;
    }
}
