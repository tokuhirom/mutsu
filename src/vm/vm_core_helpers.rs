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
        attributes: AttrMap,
        method_name: &str,
        args: Vec<Value>,
        invocant: Option<Value>,
    ) -> Result<(Value, AttrMap), RuntimeError> {
        // ADR-0019 Phase E box E7, first sub-slice: this is the carrier's ONLY
        // two live callers (`CallDefined`'s user `.defined`, `SinkPop`'s user
        // `.sink` in `vm_exec_dispatch.rs`), so tag them with a dedicated
        // measurement site -- see `Interpreter::run_instance_method_at`'s doc
        // comment for what the tag enables and why every other
        // `run_instance_method` caller stays untagged (`""`).
        self.loan_env_for(|i| {
            i.run_instance_method_at(
                "run_instance_method:vm-carrier",
                receiver_class_name,
                attributes,
                method_name,
                args,
                invocant,
            )
        })
    }

    pub(crate) fn vm_eval_block_value(&mut self, body: &[Stmt]) -> Result<Value, RuntimeError> {
        if body.is_empty() {
            return Ok(Value::NIL);
        }
        // CP-3 collapse: when the block is pure expression statements (no sub/
        // proto/operator declarations and no trailing-sub value), the registry +
        // code-env save/restore that `Interpreter::eval_block_value` performs is a
        // no-op, so run the block in-place via `run_nested` (no `mem::take`/
        // `Interpreter::new` ping-pong) and only replicate the cheap scope bookkeeping
        // (block-scope depth + let/temp restore + DESTROY pass). All current
        // callers pass a single `Stmt::Expr` (registration-time default / enum /
        // role-arg evaluation). Any other shape falls back to the interpreter.
        // The block is compiled for its VALUE, which makes its last expression a
        // `SetTopic` — but this helper runs at *declaration* time, inside whatever
        // frame happens to be constructing, so that topic write escaped to the
        // caller. `class S { has Bool $.b }; $_ = 'x'; S.new` left `$_` holding
        // `Bool`, because seeding the unset typed attribute evaluates its type
        // constraint through here. Cro hit it in a loop body — `for $resp.cookies
        // { $state = CookieState.new(...); self!get-cookie-lifetime($_, $state) }`
        // passed a `Bool` where a `Cro::HTTP::Cookie` was expected. `run_decl_expr`
        // below shares this fix — see its own doc comment.
        let saved_topic = self.save_decl_expr_topic();
        if body.iter().all(|s| matches!(s, Stmt::Expr(_))) {
            let (code, compiled_fns) = self.compile_block_value(body);
            let let_mark = self.let_saves_len();
            self.push_block_scope_depth();
            let result = self.run_nested(&code, &compiled_fns);
            self.pop_block_scope_depth();
            self.restore_let_saves(let_mark);
            self.restore_decl_expr_topic(saved_topic);
            self.loan_env_for(|i| i.run_pending_instance_destroys())?;
            return result.map(|v| v.unwrap_or(Value::NIL));
        }
        let result = self.loan_env_for(|i| i.eval_block_value(body));
        self.restore_decl_expr_topic(saved_topic);
        result
    }

    /// Save `$_` before a declaration-time value block/chunk runs — see
    /// `vm_eval_block_value`'s doc comment for why this is needed. Paired
    /// with [`Self::restore_decl_expr_topic`].
    fn save_decl_expr_topic(&mut self) -> Option<Value> {
        self.env().get("_").cloned()
    }

    fn restore_decl_expr_topic(&mut self, saved_topic: Option<Value>) {
        match saved_topic {
            Some(v) => {
                self.env_mut().insert("_".to_string(), v);
            }
            None => {
                self.env_mut().remove("_");
            }
        }
    }

    /// Run a declaration-time expression chunk (ADR-0019 C5).
    ///
    /// The chunk was lowered by the compiler, so this is the `vm_eval_block_value`
    /// fast path with the on-demand compile removed: the same re-entrant bytecode
    /// entry and the same scope bookkeeping, minus rebuilding the bytecode at
    /// every registration. It needs the identical topic save/restore
    /// `vm_eval_block_value` carries (see the comment there): the chunk is
    /// compiled for its VALUE, so its last expression is a `SetTopic` that
    /// would otherwise escape to whatever frame is constructing — exactly the
    /// `class S { has Bool $.b }; $_ = 'x'; S.new` shape #6071 fixed for the
    /// `Ast`/on-demand-compile path, now reachable here too once a
    /// declaration-time expression (e.g. an attribute default, ADR-0019
    /// D2c-4) is precompiled to `DeclTraitArg::Compiled`.
    pub(crate) fn run_decl_expr(
        &mut self,
        chunk: &crate::opcode::CompiledDeclExpr,
    ) -> Result<Value, RuntimeError> {
        self.run_decl_code(&chunk.code, &chunk.fns)
    }

    /// The body of [`Self::run_decl_expr`], taking the code/fns pair directly
    /// instead of a `CompiledDeclExpr` — shared with
    /// [`Self::vm_call_on_value`]'s declaration-expression-thunk arm, which
    /// reads the same pair off a `SubData`'s `compiled_code`/`compiled_fns`
    /// fields rather than a `CompiledDeclExpr`.
    pub(crate) fn run_decl_code(
        &mut self,
        code: &CompiledCode,
        fns: &CompiledFns,
    ) -> Result<Value, RuntimeError> {
        let saved_topic = self.save_decl_expr_topic();
        let let_mark = self.let_saves_len();
        self.push_block_scope_depth();
        let result = self.run_nested(code, fns);
        self.pop_block_scope_depth();
        self.restore_let_saves(let_mark);
        self.restore_decl_expr_topic(saved_topic);
        self.run_pending_instance_destroys()?;
        result.map(|v| v.unwrap_or(Value::NIL))
    }

    /// Evaluate a declaration trait's argument, whichever form it carries.
    pub(crate) fn eval_decl_trait_arg(
        &mut self,
        arg: &crate::opcode::DeclTraitArg,
    ) -> Result<Value, RuntimeError> {
        match arg {
            crate::opcode::DeclTraitArg::Literal(value) => Ok(value.clone()),
            crate::opcode::DeclTraitArg::Compiled(chunk) => self.run_decl_expr(chunk),
            crate::opcode::DeclTraitArg::Ast(expr) => {
                let body = [Stmt::Expr((**expr).clone())];
                self.vm_eval_block_value(&body)
            }
        }
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

    /// A `with LITERAL { ... }` block desugars to `$_ = (literal marked
    /// __mutsu_topic_ro__)`, which *establishes a fresh topic scope* rather than
    /// mutating an outer `given`/`with`'s aliased topic. When such an assignment
    /// runs inside an enclosing `given $x` (whose `topic_source_var` is `$x`), the
    /// topic-source writeback must be suppressed — otherwise the inner literal
    /// topic leaks back into the outer source variable (e.g. nested
    /// `with $x { with 12345 { } }` would clobber `$x` with `12345`).
    pub(super) fn is_topic_ro_assignment(val: &Value) -> bool {
        matches!(val.view(), ValueView::Mixin(_, ov) if ov.contains_key("__mutsu_topic_ro__"))
    }
}
