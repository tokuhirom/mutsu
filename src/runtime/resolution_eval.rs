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
            if let ValueView::Seq(items) | ValueView::Slip(items) = value.view() {
                return vec![Value::array(items.to_vec())];
            }
            return vec![value];
        }
        let converted = match value.view() {
            ValueView::Array(items, _) => Some(items.to_vec()),
            ValueView::Seq(items) => Some(items.to_vec()),
            ValueView::Slip(items) => Some(items.to_vec()),
            ValueView::Capture { positional, named } => {
                let mut args = positional.clone();
                for (k, v) in named {
                    args.push(Value::pair(k.clone(), v.clone()));
                }
                Some(args)
            }
            _ => None,
        };
        converted.unwrap_or_else(|| vec![value])
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
            Err(e) => e.return_value.clone().unwrap_or(Value::NIL),
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
            return Ok(Value::NIL);
        }
        let let_mark = self.let_saves_len();
        let saved_functions = self.registry().functions.clone();
        let saved_proto_subs = self.registry().proto_subs.clone();
        let saved_proto_functions = self.registry().proto_functions.clone();
        let saved_operator_assoc = self.operator_assoc.clone();
        let saved_user_declared_infix_ops = self.user_declared_infix_ops.clone();
        // Sub/proto/operator declarations in block scope are lexical, so the
        // registry is snapshotted here and restored on block exit. The restore
        // reassigns the three registry maps through `registry_mut()`, which bumps
        // the per-interpreter snapshot generation (invalidating the #4408
        // regex/grammar snapshot cache). But the overwhelmingly common block —
        // a grammar `token` body that is just a regex literal, run ~55x/parse —
        // writes nothing to the registry, so that restore (and its generation
        // bump) is pure overhead. Every registry write goes through
        // `registry_mut()`, which bumps `registry_write_gen`; snapshot the
        // generation here and skip the restore entirely when it is unchanged
        // after the block (nothing was declared — including via a `require`/`use`
        // or any other call, since those bump the generation too). This is sound
        // regardless of what the block does: only an actual registry write can
        // change the generation.
        let registry_gen_before = self
            .registry_write_gen
            .load(std::sync::atomic::Ordering::Relaxed);
        // `&`-code vars and their `__mutsu_callable_id::` markers are lexical to
        // this block too; snapshot them so a block-local `sub`/`my &foo` binding
        // does not leak its env entries into the caller.
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
        // Restore the routine registry only if the block actually wrote to it
        // (tracked by the monotonic `registry_write_gen`, bumped by every
        // `registry_mut()`). The common declaration-free block leaves the
        // generation unchanged, so it skips the whole restore — no lock
        // acquisition and, crucially, no generation bump that would invalidate
        // the #4408 snapshot cache. When it did change, one write guard restores
        // all three maps (one lock acquisition instead of three) — the moves
        // cannot re-enter user code, so holding the guard across them is safe.
        if self
            .registry_write_gen
            .load(std::sync::atomic::Ordering::Relaxed)
            != registry_gen_before
        {
            {
                let mut reg = self.registry_mut();
                reg.functions = saved_functions;
                reg.proto_subs = saved_proto_subs;
                reg.proto_functions = saved_proto_functions;
            }
            self.operator_assoc = saved_operator_assoc;
            self.user_declared_infix_ops = saved_user_declared_infix_ops;
        }
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

    /// Like `eval_block_value`, but for a block passed as a *test-assertion
    /// argument* (`dies-ok { ... }`, `lives-ok { ... }`, `throws-like { ... }`,
    /// `fails-like { ... }`, `warns-like { ... }`): such a block is a genuine
    /// lexical closure, so a `my $x` it declares must not leak into the
    /// caller's shared `env` once it returns — mirroring how `EVAL 'my $x'`
    /// already isolates its lexicals via `is_plain_user_lexical` in
    /// `parse_and_eval_with_operators`. Without this, a `my TYPE $var;`
    /// reached inside the block (e.g. `dies-ok { ...; my Int $x; }`) leaves
    /// `$x`'s value in `env` after the call, which can silently overwrite an
    /// unrelated same-named outer variable (roast S04-declarations/my-6e.t,
    /// "Can access variable returned from a named closure ..." — the mainline
    /// `my $x` must stay untouched by a `my $x` inside an unrelated,
    /// previously-run test-assertion block).
    pub(crate) fn eval_test_block_value(&mut self, body: &[Stmt]) -> Result<Value, RuntimeError> {
        let pre_lexicals: std::collections::HashSet<Symbol> = self
            .env
            .keys()
            .filter(|k| k.with_str(|s| crate::env::is_plain_user_lexical(s) && !s.starts_with('&')))
            .copied()
            .collect();
        // `our @e1 = ...` inside the block is package-scoped and must persist
        // after the block returns (unlike a `my` lexical); the plain-lexical
        // heuristic below cannot tell `our` and `my` apart by name alone, so
        // collect the block's own `our`-declared names to exclude them from
        // the leaked-lexical cleanup (roast S04-declarations/our.t,
        // "our-scoped array/hash has correct value").
        let mut our_names = std::collections::HashSet::new();
        Self::collect_our_decl_names(body, &mut our_names);
        // A lexical pragma such as `use fatal` inside a test-assertion block
        // (`throws-like { use fatal; ... }`) is scoped to that block. This Rust
        // entry point runs the body directly without the compiler's block-level
        // import scope, so a `use fatal` *raised* here would otherwise leak into
        // the tests that follow. Clear only a block-local raise (off -> on);
        // never force `fatal_mode` back on, so a block that legitimately lowered
        // an already-on fatal keeps it off (roast S04-exceptions/fail.t, where an
        // earlier sub leaked fatal on and a later subtest turns it back off).
        let saved_fatal_mode = self.fatal_mode;
        let result = self.eval_block_value(body);
        // Whether `use fatal` was in effect at the block's end (before the
        // scope-restore below) drives the trailing-Failure check further down.
        let block_fatal_mode = self.fatal_mode;
        if !saved_fatal_mode && block_fatal_mode {
            self.fatal_mode = false;
        }
        let leaked: Vec<Symbol> = self
            .env
            .keys()
            .filter(|k| {
                !pre_lexicals.contains(k)
                    && k.with_str(|s| {
                        crate::env::is_plain_user_lexical(s)
                            && !s.starts_with('&')
                            && !our_names.contains(s)
                    })
            })
            .copied()
            .collect();
        for key in leaked {
            self.env.remove_sym(key);
        }
        // Under `use fatal` (in the block's lexical scope), a trailing reified
        // list/Seq whose element is an unhandled Failure must throw so a
        // `throws-like { use fatal; "a".map: *.Int }` assertion sees the
        // exception. A bare trailing Failure is already surfaced by the block's
        // own return handling; this covers the list-wrapped case that the map's
        // per-element WhateverCode produces. Gated on the block's own fatal
        // state (captured above, before the scope-restore).
        if block_fatal_mode
            && let Ok(ref val) = result
            && let Some(err) = self.unhandled_failure_in_list_for_fatal(val)
        {
            return Err(err);
        }
        result
    }

    /// Collect the plain-lexical env keys (`@e1` -> `"e1"`/`"@e1"` depending
    /// on the caller's convention — here the *bare* `VarDecl.name`, matching
    /// what `is_plain_user_lexical` checks) declared with `our` anywhere in
    /// `stmts`, recursing into the statement containers a test-assertion
    /// block commonly nests declarations in. Not exhaustive over every `Stmt`
    /// variant with a nested body, but covers the realistic shapes; missing a
    /// deeply-nested case only reverts to the pre-existing behavior.
    fn collect_our_decl_names(stmts: &[Stmt], out: &mut std::collections::HashSet<String>) {
        for stmt in stmts {
            match stmt {
                Stmt::VarDecl {
                    name, is_our: true, ..
                } => {
                    out.insert(name.clone());
                }
                Stmt::Block(inner) | Stmt::SyntheticBlock(inner) => {
                    Self::collect_our_decl_names(inner, out);
                }
                Stmt::If {
                    then_branch,
                    else_branch,
                    ..
                } => {
                    Self::collect_our_decl_names(then_branch, out);
                    Self::collect_our_decl_names(else_branch, out);
                }
                Stmt::While { body, .. }
                | Stmt::Loop { body, .. }
                | Stmt::React { body, .. }
                | Stmt::Whenever { body, .. }
                | Stmt::Given { body, .. }
                | Stmt::When { body, .. }
                | Stmt::For { body, .. } => {
                    Self::collect_our_decl_names(body, out);
                }
                _ => {}
            }
        }
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
        let value = self.env().get("_").cloned().unwrap_or(Value::NIL);
        self.block_scope_depth = self.block_scope_depth.saturating_sub(1);
        result.map(|last_value| last_value.unwrap_or(value))
    }

    /// Fast path for `Lock::Async.protect { ... }` — executes a bare block
    /// directly in the current env without the full env save/restore overhead
    /// of `call_sub_value`.  Shared vars must already be synced to env by the
    /// caller.
    pub(crate) fn call_protect_block(&mut self, code: &Value) -> Result<Value, RuntimeError> {
        if let ValueView::Sub(data) = code.view() {
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
        data: &crate::gc::Gc<crate::value::SubData>,
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
                    | crate::opcode::OpCode::AssignExprLocal(slot) => {
                        assigned_slots.insert(*slot as usize);
                    }
                    // Inc/dec carry (name_idx, slot); keep the pre-existing field-0
                    // behavior for this EVAL-writeback detection (unchanged by the
                    // §1.5 inc/dec slot-baking slices).
                    crate::opcode::OpCode::PreIncrement(name_idx, _)
                    | crate::opcode::OpCode::PreDecrement(name_idx, _)
                    | crate::opcode::OpCode::PostIncrement(name_idx, _)
                    | crate::opcode::OpCode::PostDecrement(name_idx, _) => {
                        assigned_slots.insert(*name_idx as usize);
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
                let Some(ValueView::Str(name)) = compiled.constants.get(idx).map(Value::view)
                else {
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
        let value = self.env().get("_").cloned().unwrap_or(Value::NIL);
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
