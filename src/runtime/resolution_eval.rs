use super::*;

type CompiledFnMap = std::collections::HashMap<String, crate::opcode::CompiledFunction>;
type ProtectBlockCompiled = std::sync::Arc<crate::opcode::CompiledCode>;
type ProtectBlockCompiledFns = std::sync::Arc<CompiledFnMap>;
type ProtectBlockCapturedBindings = std::sync::Arc<Vec<(usize, String)>>;
type ProtectBlockWritebackBindings = std::sync::Arc<Vec<(usize, String)>>;
type ProtectBlockCapturedNames = std::sync::Arc<Vec<String>>;

impl Interpreter {
    pub(crate) fn composed_result_to_args(value: Value, prefer_single: bool) -> Vec<Value> {
        if prefer_single {
            return match value {
                Value::Seq(items) | Value::Slip(items) => vec![Value::array(items.to_vec())],
                other => vec![other],
            };
        }
        match value {
            Value::Array(items, _) => items.to_vec(),
            Value::Seq(items) => items.to_vec(),
            Value::Slip(items) => items.to_vec(),
            Value::Capture { positional, named } => {
                let mut args = *positional;
                for (k, v) in *named {
                    args.push(Value::Pair(k, Box::new(v)));
                }
                args
            }
            other => vec![other],
        }
    }

    /// Like `eval_block_value` but handles PRE/POST phasers in the body.
    /// Used for function call evaluation where the body may contain PRE/POST.
    pub(crate) fn eval_block_value_with_pre_post(
        &mut self,
        body: &[Stmt],
    ) -> Result<Value, RuntimeError> {
        let (pre_ph, _enter_ph, _success_ph, _failure_ph, post_ph, body_main) =
            self.split_block_phasers(body);
        if pre_ph.is_empty() && post_ph.is_empty() {
            return self.eval_block_value(body);
        }
        // Run PRE phasers
        for pre in &pre_ph {
            let result = self.eval_block_value(std::slice::from_ref(pre))?;
            if !result.truthy() {
                return Err(Self::make_phaser_prepost_error(true));
            }
        }
        // Run main body
        let result = self.eval_block_value(&body_main);
        // Run POST phasers (with $_ set to return value for POST checks)
        let ret_val = match &result {
            Ok(v) => v.clone(),
            Err(e) => e.return_value.clone().unwrap_or(Value::Nil),
        };
        let saved_topic = self.env.get("_").cloned();
        self.env.insert("_".to_string(), ret_val);
        for post in &post_ph {
            let post_result = self.eval_block_value(std::slice::from_ref(post));
            match post_result {
                Ok(v) if !v.truthy() => {
                    if let Some(t) = saved_topic {
                        self.env.insert("_".to_string(), t);
                    }
                    return Err(Self::make_phaser_prepost_error(false));
                }
                Err(e) => {
                    if let Some(t) = saved_topic {
                        self.env.insert("_".to_string(), t);
                    }
                    return Err(e);
                }
                _ => {}
            }
        }
        if let Some(t) = saved_topic {
            self.env.insert("_".to_string(), t);
        }
        result
    }

    /// Compile a block with `eval_block_value`'s compiler context (routine
    /// scope, `$?PACKAGE`/`$?DISTRIBUTION`) without executing it. Pure
    /// compilation — touches no `env`, runs no user code — so the VM can call it
    /// (no env loan) and run the result in-place via `VM::run_nested` instead of
    /// the `mem::take`/`VM::new` ping-pong (CP-3 collapse).
    pub(crate) fn compile_block_value(
        &self,
        body: &[Stmt],
    ) -> (
        crate::opcode::CompiledCode,
        std::collections::HashMap<String, crate::opcode::CompiledFunction>,
    ) {
        let mut compiler = crate::compiler::Compiler::new();
        compiler.is_routine = !self.routine_stack.is_empty();
        compiler.lexically_in_routine = !self.routine_stack.is_empty();
        let scope = if let Some(frame) = self.routine_stack.last() {
            // Set enclosing_package to the clean package name so that
            // $?PACKAGE resolves to the package, not the mangled routine
            // scope (e.g., "PackageTest" instead of "PackageTest::&pkg").
            compiler.enclosing_package = Some(frame.package.clone());
            format!("{}::&{}", frame.package, frame.name)
        } else {
            self.current_package()
        };
        compiler.set_current_package(scope);
        // Resolve distribution context for $?DISTRIBUTION
        compiler.current_distribution = self.current_distribution.clone().or_else(|| {
            self.package_distributions
                .get(&self.current_package())
                .cloned()
        });
        compiler.compile(body)
    }

    pub(crate) fn eval_block_value(&mut self, body: &[Stmt]) -> Result<Value, RuntimeError> {
        if body.is_empty() {
            return Ok(Value::Nil);
        }
        let let_mark = self.let_saves_len();
        let saved_functions = self.registry().functions.clone();
        let saved_proto_subs = self.registry().proto_subs.clone();
        let saved_proto_functions = self.registry().proto_functions.clone();
        let saved_operator_assoc = self.operator_assoc.clone();
        let saved_code_env: std::collections::HashMap<Symbol, Value> = self
            .env
            .iter()
            .filter(|(k, _)| k.starts_with("&") || k.starts_with("__mutsu_callable_id::"))
            .map(|(k, v)| (*k, v.clone()))
            .collect();
        let (code, compiled_fns) = self.compile_block_value(body);
        // Multi-frame coherence (env_dirty-deletion path): box any captured-outer
        // scalar this carrier body writes into a shared cell across env + saved
        // frames, so the by-name write survives the owner frame's env restore.
        // No-op in the default build (gated on cell_boxing_active).
        self.box_carrier_free_var_writes(&code);
        self.block_scope_depth += 1;
        let result = self.run_compiled_block(&code, &compiled_fns);
        let trailing_sub_value = match body.last() {
            Some(Stmt::SubDecl {
                name,
                params,
                param_defs,
                body,
                is_rw,
                ..
            }) => Some(Value::make_sub(
                Symbol::intern(&self.current_package()),
                *name,
                params.clone(),
                param_defs.clone(),
                body.clone(),
                *is_rw,
                self.env.clone(),
            )),
            _ => None,
        };
        self.block_scope_depth = self.block_scope_depth.saturating_sub(1);
        // Sub/proto declarations in block scope are lexical; restore registries on block exit.
        self.registry_mut().functions = saved_functions;
        self.registry_mut().proto_subs = saved_proto_subs;
        self.registry_mut().proto_functions = saved_proto_functions;
        self.operator_assoc = saved_operator_assoc;
        self.env
            .retain(|k, _| !(k.starts_with("&") || k.starts_with("__mutsu_callable_id::")));
        for (k, v) in saved_code_env {
            self.env.insert_sym(k, v);
        }
        // Blocks are scope boundaries for temp/let saves.
        self.restore_let_saves(let_mark);
        self.run_pending_instance_destroys()?;
        result.map(|value| {
            // When the block's last statement is a sub declaration, its value is
            // the declared sub — not a leftover topic/`$_` value from an earlier
            // statement (e.g. a Failure that propagated through a sink). Prefer
            // the trailing sub unconditionally in that case.
            if let Some(sub) = trailing_sub_value {
                return sub;
            }
            value
        })
    }

    /// Fast path for simple closures (e.g. sequence generators) that don't
    /// declare subs or modify proto registries. Takes pre-compiled bytecode
    /// to avoid recompilation on every call.
    pub(crate) fn eval_precompiled_block_fast(
        &mut self,
        code: &crate::opcode::CompiledCode,
        compiled_fns: &std::collections::HashMap<String, crate::opcode::CompiledFunction>,
    ) -> Result<Value, RuntimeError> {
        self.block_scope_depth += 1;
        // CP-3 collapse: run the precompiled block re-entrantly in place.
        let result = self.run_nested(code, compiled_fns);
        for (slot, key) in &code.state_locals {
            if let Some(name) = code.locals.get(*slot)
                && let Some(val) = self.env().get(name).cloned()
            {
                self.set_state_var(key.clone(), val);
            }
        }
        let value = self.env().get("_").cloned().unwrap_or(Value::Nil);
        self.block_scope_depth = self.block_scope_depth.saturating_sub(1);
        result.map(|last_value| last_value.unwrap_or(value))
    }

    /// Fast path for `Lock::Async.protect { ... }` — executes a bare block
    /// directly in the current env without the full env save/restore overhead
    /// of `call_sub_value`.  Shared vars must already be synced to env by the
    /// caller.
    pub(crate) fn call_protect_block(&mut self, code: &Value) -> Result<Value, RuntimeError> {
        if let Value::Sub(data) = code {
            let (compiled, compiled_fns, _captured_bindings, _writeback_bindings, captured_names) =
                self.get_or_compile_protect_block_with_slots(data);
            self.sync_shared_vars_for_names(captured_names.iter().map(|name| name.as_str()));
            self.run_compiled_block(&compiled, compiled_fns.as_ref())
        } else {
            self.call_sub_value(code.clone(), Vec::new(), true)
        }
    }

    pub(crate) fn get_or_compile_protect_block_with_slots(
        &mut self,
        data: &std::sync::Arc<crate::value::SubData>,
    ) -> (
        ProtectBlockCompiled,
        ProtectBlockCompiledFns,
        ProtectBlockCapturedBindings,
        ProtectBlockWritebackBindings,
        ProtectBlockCapturedNames,
    ) {
        let entry = self.protect_block_cache.entry(data.id).or_insert_with(|| {
            let (compiled, compiled_fns) = if let Some(ref cc) = data.compiled_code {
                (
                    cc.clone(),
                    std::sync::Arc::new(std::collections::HashMap::new()),
                )
            } else {
                let compiler = crate::compiler::Compiler::new();
                let (compiled, compiled_fns) = compiler.compile(&data.body);
                (
                    std::sync::Arc::new(compiled),
                    std::sync::Arc::new(compiled_fns),
                )
            };
            let captured_bindings: Vec<(usize, String)> = compiled
                .locals
                .iter()
                .enumerate()
                .filter(|(_, name)| data.env.contains_key(name))
                .map(|(idx, name)| (idx, name.clone()))
                .collect();
            let mut assigned_slots = std::collections::HashSet::new();
            for op in &compiled.ops {
                match op {
                    crate::opcode::OpCode::SetLocal(slot)
                    | crate::opcode::OpCode::AssignExprLocal(slot)
                    | crate::opcode::OpCode::PreIncrement(slot)
                    | crate::opcode::OpCode::PreDecrement(slot)
                    | crate::opcode::OpCode::PostIncrement(slot)
                    | crate::opcode::OpCode::PostDecrement(slot) => {
                        assigned_slots.insert(*slot as usize);
                    }
                    _ => {}
                }
            }
            let writeback_bindings: Vec<(usize, String)> = captured_bindings
                .iter()
                .filter(|(slot, _)| assigned_slots.contains(slot))
                .cloned()
                .collect();
            let mut captured_names: Vec<String> = captured_bindings
                .iter()
                .map(|(_, name)| name.clone())
                .collect();
            for op in &compiled.ops {
                let name_idx = match op {
                    crate::opcode::OpCode::GetGlobal(idx)
                    | crate::opcode::OpCode::SetGlobal(idx)
                    | crate::opcode::OpCode::GetArrayVar(idx)
                    | crate::opcode::OpCode::GetHashVar(idx)
                    | crate::opcode::OpCode::CheckReadOnly(idx) => Some(*idx as usize),
                    _ => None,
                };
                let Some(idx) = name_idx else {
                    continue;
                };
                let Some(crate::value::Value::Str(name)) = compiled.constants.get(idx) else {
                    continue;
                };
                if data.env.contains_key(name.as_str()) && !captured_names.contains(name) {
                    captured_names.push(name.to_string());
                }
            }
            (
                compiled,
                compiled_fns,
                std::sync::Arc::new(captured_bindings),
                std::sync::Arc::new(writeback_bindings),
                std::sync::Arc::new(captured_names),
            )
        });
        (
            entry.0.clone(),
            entry.1.clone(),
            entry.2.clone(),
            entry.3.clone(),
            entry.4.clone(),
        )
    }

    /// Run pre-compiled bytecode and return the `$_` topic value.
    fn run_compiled_block(
        &mut self,
        code: &crate::opcode::CompiledCode,
        compiled_fns: &std::collections::HashMap<String, crate::opcode::CompiledFunction>,
    ) -> Result<Value, RuntimeError> {
        // CP-3 collapse: run the compiled block re-entrantly in place.
        let result = self.run_nested(code, compiled_fns);
        // Persist state variables for top-level compiled blocks too.
        for (slot, key) in &code.state_locals {
            if let Some(name) = code.locals.get(*slot)
                && let Some(val) = self.env().get(name).cloned()
            {
                self.set_state_var(key.clone(), val);
            }
        }
        let value = self.env().get("_").cloned().unwrap_or(Value::Nil);
        result.map(|last_value| last_value.unwrap_or(value))
    }

    /// Slice F (env<->locals coherence): record an eager block-iteration's
    /// captured-outer free-var writes so the call-site method op drains each new
    /// value straight through to the caller's local slot, keeping it coherent
    /// without the reverse `sync_locals_from_env` pull.
    ///
    /// The eager `.map`/`.grep` loops (`(1,2,3).map({ $sum += $_ })`) run the
    /// block body via `run_reuse` rather than `call_compiled_closure_with_topic`,
    /// so they bypass the closure-dispatch recording (#3307): a mutated captured
    /// lexical lands in `env` but is never recorded for write-through. Replay the
    /// block's compile-time `free_var_writes` here. Topic/param names are
    /// loop-local (restored by the caller) and excluded.
    pub(crate) fn record_eager_block_free_var_writeback(
        &mut self,
        code: &crate::opcode::CompiledCode,
        params: &[String],
    ) {
        for sym in &code.free_var_writes {
            sym.with_str(|fname| {
                if fname != "_"
                    && fname != "$_"
                    && fname != "@_"
                    && fname != "%_"
                    && !params.iter().any(|p| p == fname)
                {
                    self.pending_rw_writeback_sources.push(fname.to_string());
                }
            });
        }
    }
}
