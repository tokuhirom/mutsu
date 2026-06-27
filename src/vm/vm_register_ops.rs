use super::*;
use crate::compiler::Compiler;
use crate::symbol::Symbol;

impl Interpreter {
    /// Get the current source line number from the interpreter env.
    pub(super) fn current_source_line(&self) -> Option<u32> {
        self.env().get("?LINE").and_then(|v| match v {
            Value::Int(n) => Some(*n as u32),
            _ => None,
        })
    }

    /// Get the current source file from the interpreter env.
    pub(super) fn current_source_file(&self) -> Option<String> {
        self.env().get("?FILE").and_then(|v| match v {
            Value::Str(s) => Some(s.to_string()),
            _ => None,
        })
    }

    pub(super) fn exec_make_gather_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::Block(body) = stmt {
            let mut env = self.env().clone();
            env.insert(
                "__mutsu_lazylist_from_gather".to_string(),
                Value::Bool(true),
            );
            // Compile the gather body to bytecode for Interpreter-native forcing
            let compiler = Compiler::new();
            let (compiled_code, compiled_fns) = compiler.compile(body);
            let list = LazyList {
                body: body.clone(),
                env,
                cache: std::sync::Mutex::new(None),
                compiled_code: Some(std::sync::Arc::new(compiled_code)),
                compiled_fns: Some(std::sync::Arc::new(compiled_fns)),
                elems_count: None,
                scan_spec: None,
                sequence_spec: None,
                coroutine: Some(std::sync::Mutex::new(crate::value::GatherCoroutineState {
                    ip: 0,
                    locals: Vec::new(),
                    stack: Vec::new(),
                    env: crate::env::Env::new(),
                    finished: false,
                    for_loop_resume: None,
                })),
                lazy_pipe: None,
                closure_seq: None,
            };
            let val = Value::LazyList(std::sync::Arc::new(list));
            self.stack.push(val);
            Ok(())
        } else {
            Err(RuntimeError::new("MakeGather expects Block"))
        }
    }

    fn resolve_closure_code(
        code: &CompiledCode,
        cc_idx: Option<u32>,
    ) -> Option<std::sync::Arc<CompiledCode>> {
        cc_idx.map(|i| code.closure_compiled_codes[i as usize].clone())
    }

    pub(super) fn exec_make_anon_sub_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
        cc_idx: Option<u32>,
        is_block: bool,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::Block(body) = stmt {
            let params = crate::ast::collect_placeholders_shallow(body);
            let compiled_code = Self::resolve_closure_code(code, cc_idx);
            self.box_captured_lexicals(code, &compiled_code);
            let owned_captures = self.compute_owned_captures(&compiled_code);
            let (captured_upvalues, captured_upvalues_from_local) =
                self.capture_closure_upvalues(code, &compiled_code);
            let cc_source_line = compiled_code
                .as_ref()
                .and_then(|cc| cc.source_line)
                .map(|l| l as u32)
                .or_else(|| self.current_source_line());
            let val = Value::Sub(std::sync::Arc::new(crate::value::SubData {
                package: Symbol::intern(&self.current_package()),
                name: Symbol::intern(""),
                params,
                param_defs: Vec::new(),
                body: body.clone(),
                is_rw: false,
                is_raw: false,
                // Upvalue snapshot (single-store Slice E): capture only free vars,
                // shadow-meta, and system names; see `capture_closure_env`.
                env: self.capture_closure_env(code, &compiled_code),
                assumed_positional: Vec::new(),
                assumed_named: std::collections::HashMap::new(),
                id: crate::value::next_instance_id(),
                empty_sig: false,
                is_bare_block: is_block,
                owned_captures,
                compiled_code,
                deprecated_message: None,
                source_line: cc_source_line,
                source_file: self.current_source_file(),
                captured_upvalues,
                captured_upvalues_from_local,
            }));
            self.stack.push(val);
            Ok(())
        } else {
            Err(RuntimeError::new("MakeAnonSub expects Block"))
        }
    }

    pub(super) fn exec_make_anon_sub_params_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
        cc_idx: Option<u32>,
        is_whatever_code: bool,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::SubDecl {
            params,
            param_defs,
            return_type,
            body,
            is_rw,
            is_raw,
            ..
        } = stmt
        {
            let compiled_code = Self::resolve_closure_code(code, cc_idx);
            self.box_captured_lexicals(code, &compiled_code);
            let owned_captures = self.compute_owned_captures(&compiled_code);
            let (captured_upvalues, captured_upvalues_from_local) =
                self.capture_closure_upvalues(code, &compiled_code);
            // Upvalue snapshot (single-store Slice E); see `capture_closure_env`.
            let mut env = self.capture_closure_env(code, &compiled_code);
            if let Some(rt) = return_type {
                env.insert("__mutsu_return_type".to_string(), Value::str(rt.clone()));
            }
            if is_whatever_code {
                env.insert(
                    "__mutsu_callable_type".to_string(),
                    Value::str_from("WhateverCode"),
                );
            }
            let cc_source_line = compiled_code
                .as_ref()
                .and_then(|cc| cc.source_line)
                .map(|l| l as u32)
                .or_else(|| self.current_source_line());
            let val = Value::Sub(std::sync::Arc::new(crate::value::SubData {
                package: Symbol::intern(&self.current_package()),
                name: Symbol::intern(""),
                params: params.clone(),
                param_defs: param_defs.clone(),
                body: body.clone(),
                is_rw: *is_rw,
                is_raw: *is_raw,
                env,
                assumed_positional: Vec::new(),
                assumed_named: std::collections::HashMap::new(),
                id: crate::value::next_instance_id(),
                empty_sig: params.is_empty() && param_defs.is_empty(),
                is_bare_block: false,
                owned_captures,
                compiled_code,
                deprecated_message: None,
                source_line: cc_source_line,
                source_file: self.current_source_file(),
                captured_upvalues,
                captured_upvalues_from_local,
            }));
            self.stack.push(val);
            Ok(())
        } else {
            Err(RuntimeError::new("MakeAnonSubParams expects SubDecl"))
        }
    }

    /// Free variables of a closure being created that were declared in an
    /// enclosing loop body (see `Interpreter::loop_local_vars`). These become the
    /// closure's `owned_captures`: read at call time from its own frozen captured
    /// env so each loop iteration's closure sees its own value (Raku
    /// per-iteration binding), immune to the dual-store slot re-injection.
    fn compute_owned_captures(
        &self,
        compiled_code: &Option<std::sync::Arc<CompiledCode>>,
    ) -> Vec<Symbol> {
        if self.loop_local_vars.is_empty() {
            return Vec::new();
        }
        let Some(cc) = compiled_code else {
            return Vec::new();
        };
        cc.free_var_syms
            .iter()
            .filter(|sym| sym.with_str(|s| self.loop_local_vars.iter().any(|set| set.contains(s))))
            .copied()
            .collect()
    }

    /// Capture the closure's environment as an *upvalue snapshot* (single-store
    /// Slice E): instead of flattening the whole lexical env into the closure
    /// (`clone_env`), capture only the names the closure body (and its nested
    /// closures) can actually observe.
    ///
    /// The invariant that makes this safe: a closure body references an outer
    /// **user lexical** only through a `GetGlobal`-family opcode, so the compiler's
    /// `free_var_syms` set already lists every such name. The *other* names a body
    /// can read — `self` (attribute access), special vars (`$_`, `$/`, `$!`,
    /// `$?FILE`, …), dynamic vars (`$*…`), match captures, `&?ROUTINE`/`&?BLOCK`,
    /// and type names — go through dedicated opcodes the free-var scan cannot see,
    /// but they are all *system* names rather than plain user lexicals. So the
    /// capture keeps: free variables, the `__mutsu_*` shadow-meta, and every name
    /// that is not a plain user lexical ([`crate::env::is_plain_user_lexical`]). It
    /// drops only the bulk non-free plain user lexicals, which the body provably
    /// cannot reference.
    ///
    /// Two cases keep the whole-env snapshot (`clone_env`) because `free_var_syms`
    /// is *not* a complete account of the names the body reads:
    /// - **reflective** programs (`EVAL` / `CALLER::` / symbolic deref) can read a
    ///   caller lexical under any name (process-global flag, set at finalize);
    /// - **`captures_env_by_name`** frames run an inline body that reads lexicals
    ///   by name through a path the op-scan misses (`whenever`/`gather` bodies
    ///   stashed in the `stmt_pool`, loop/block control temps) — see the field on
    ///   [`CompiledCode`].
    ///
    /// **Slice E Part 2 (the upvalue read):** a free variable that is one of *this*
    /// frame's own locals is read straight from the slot store
    /// (`self.locals[slot]`, the live upvalue), not from `env`. This is what lets
    /// `compute_needs_env_sync` drop its closure-driven flush (branch #2): the
    /// closure no longer depends on the parent frame mirroring that local into
    /// `env` before capture. Ancestor free variables (no slot here) and the system
    /// names still come from the flattened env, where they always live. Mutation
    /// *propagation* back to the parent is unchanged — it flows through the reverse
    /// `env_dirty` path and, for captured-and-mutated locals, the shared
    /// `ContainerRef` cell that `box_captured_lexicals` installs in both the slot
    /// and `env`.
    fn capture_closure_upvalues(
        &self,
        code: &CompiledCode,
        cc: &Option<std::sync::Arc<CompiledCode>>,
    ) -> (Vec<Option<Value>>, Vec<bool>) {
        let Some(cc) = cc else {
            return (Vec::new(), Vec::new());
        };
        cc.free_var_syms.iter().fold(
            (Vec::with_capacity(cc.free_var_syms.len()), Vec::with_capacity(cc.free_var_syms.len())),
            |(mut values, mut from_local), sym| {
                if !sym.with_str(crate::env::is_plain_user_lexical) {
                    values.push(None);
                    from_local.push(false);
                    return (values, from_local);
                }
                if let Some(slot) = sym.with_str(|s| code.locals.iter().rposition(|n| n == s))
                    && let Some(val) = self.locals.get(slot)
                {
                    values.push(Some(val.clone()));
                    from_local.push(true);
                    return (values, from_local);
                }
                values.push(Some(self.env().get_sym(*sym).cloned().unwrap_or(Value::Nil)));
                from_local.push(false);
                (values, from_local)
            },
        )
    }

    fn capture_closure_env(
        &self,
        _code: &CompiledCode,
        cc: &Option<std::sync::Arc<CompiledCode>>,
    ) -> Env {
        let Some(cc) = cc else {
            return self.clone_env();
        };
        if cc.captures_env_by_name || crate::opcode::reflective_name_access_possible() {
            return self.clone_env();
        }
        let free: std::collections::HashSet<Symbol> = cc.free_var_syms.iter().copied().collect();
        // Keep only non-free names here: system/meta names still flow through
        // env, while lexical free vars live in `captured_upvalues`.
        let mut keep = |k: Symbol, _v: &Value| {
            !free.contains(&k) || !k.with_str(crate::env::is_plain_user_lexical)
        };
        let map = self.env().filtered_symbol_map(&mut keep);
        crate::env::Env::from_symbol_map(map)
    }

    /// Box-on-capture (lever C Slice 2): a closure captures the *container* of a
    /// closed-over lexical scalar, not a frozen value — but only for the lexicals
    /// that actually need it: an enclosing-scope local that is BOTH captured by a
    /// closure AND mutated after declaration (`code.captured_mutated_locals`,
    /// computed by the compiler). Before snapshotting the env into the new
    /// closure's `data.env`, replace such a free variable (which has a slot in
    /// `code.locals`) with a shared `ContainerRef` in BOTH the slot and the env.
    /// The env snapshot then shares the same `Arc`, so:
    ///
    /// - mutation of the lexical *after* capture is visible to the closure
    ///   (`my $x=1; my $c={$x}; $x=2; $c()` -> 2, in or out of a loop), and
    /// - sibling closures share one cell
    ///   (`my $v=0; my $g={$v}; my $s=->$n{$v=$n}; $s(42); $g()` -> 42).
    ///
    /// Per-iteration freshness is preserved because a loop-body `my` redeclaration
    /// resets the stale ContainerRef in the slot+env each iteration (see
    /// exec_set_local_op vardecl handling), so the next closure boxes a fresh
    /// cell. Read-only / declaration-only captures are deliberately NOT boxed:
    /// they don't need container identity, and boxing them (e.g. Test's
    /// `lives-ok {...}` closing over a surrounding `$obj` / type object / Mix)
    /// would hide the value behind a ContainerRef and trip the many code paths
    /// that don't yet deref one (immutability, type-object dispatch, `.kv` rw
    /// writeback). Arrays / hashes / subs / type objects are reference-shared
    /// already and untouched.
    fn box_captured_lexicals(
        &mut self,
        code: &CompiledCode,
        cc: &Option<std::sync::Arc<CompiledCode>>,
    ) {
        let Some(cc) = cc else { return };
        // Box captured-and-mutated `$` scalar locals into a shared `ContainerRef`
        // cell so the closure observes mutations and siblings share one cell. Two
        // narrow triggers (deliberately NOT "every captured-mutated local" — that
        // broad form regressed perf and correctness, see #2749 / docs):
        //   (A) loop-body locals (`loop_local_vars`): per-iteration binding, the
        //       original lever-C path — kept byte-for-byte.
        //   (B) `needs_cell_locals`: locals captured by a child closure whose
        //       value ESCAPES the creating frame (escape analysis — stored,
        //       returned, or bound, not immediately invoked). These genuinely
        //       need a shared cell even in non-loop frames (e.g. a getter+setter
        //       factory, or a single `&f = sub {...}` assigned closure). The
        //       escape signal excludes the immediately-invoked closure
        //       (`lives-ok {...}` / `map {...}`, call args / control blocks),
        //       bounding boxing cost and avoiding the broad-boxing perf blowup.
        // Read-only loop captures are handled by `owned_captures` (value-freeze).
        if code.captured_mutated_locals.is_empty()
            || (self.loop_local_vars.is_empty() && code.needs_cell_locals.is_empty())
        {
            return;
        }
        for sym in &cc.free_var_syms {
            if !code.captured_mutated_locals.contains(sym) {
                continue;
            }
            let needs_cell = code.needs_cell_locals.contains(sym);
            let Some(idx) = sym.with_str(|s| {
                if s.starts_with('@') || s.starts_with('%') || s.starts_with('&') {
                    return None;
                }
                let is_loop_local = self.loop_local_vars.iter().any(|set| set.contains(s));
                if !is_loop_local {
                    // Non-loop escaping path (B) only.
                    if !needs_cell {
                        return None;
                    }
                    // A type/`where`-constrained scalar must keep flowing through
                    // the assignment chokepoint so each mutation re-checks the
                    // constraint; the ContainerRef write-through bypasses it. Skip
                    // boxing it (inline `where` desugars to an anonymous subset, so
                    // var_type_constraint catches block/whatever/`&pred` forms).
                    // Applied to (B) only — the loop path (A) is left unchanged.
                    // EXCEPTION: `Mu` is the universal type — every value satisfies
                    // it, so the ContainerRef write-through bypasses no real check.
                    // Box `my Mu $s` so captured-outer thunks (metaop `Xxx`/`Zand`)
                    // share its cell and stay coherent without the blanket reconcile.
                    let mut tc = loan_env!(self, var_type_constraint(s));
                    if tc.is_none() {
                        tc = loan_env!(self, var_type_constraint(s.trim_start_matches('$')));
                    }
                    if matches!(tc.as_deref(), Some(t) if t != "Mu") {
                        return None;
                    }
                }
                code.locals.iter().rposition(|n| n == s)
            }) else {
                continue;
            };
            let cur = &self.locals[idx];
            // Already a shared cell -> a sibling closure (or earlier capture)
            // boxed it; reuse the same Arc.
            if cur.is_container_ref() {
                continue;
            }
            // Only box plain scalar containers. Reference types share already;
            // type objects / proxies must not be hidden behind a ContainerRef.
            if matches!(
                cur,
                Value::Package(_)
                    | Value::Array(..)
                    | Value::Hash(..)
                    | Value::Sub(..)
                    | Value::Instance { .. }
                    | Value::Proxy { .. }
            ) {
                continue;
            }
            let container = cur.clone().into_container_ref();
            self.locals[idx] = container.clone();
            sym.with_str(|s| {
                self.env_mut().insert(s.to_string(), container.clone());
                // Track C: if a thread is already running (shared_vars active) and a
                // stale plain snapshot of this name lives in `shared_vars` (seeded
                // by an earlier `start` before this local was boxed), replace it
                // with the cell. Otherwise the stale value, marked dirty, would be
                // written back over the cell by `sync_shared_vars_to_env` after the
                // next await — disconnecting the parent from the shared cell.
                // `set_shared_var` only updates entries that already exist, so this
                // is a no-op when the name was never snapshotted.
                if self.shared_vars_active {
                    loan_env!(self, set_shared_var(s, container.clone()));
                }
            });
        }
    }

    pub(super) fn exec_make_lambda_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
        cc_idx: Option<u32>,
        is_whatever_code: bool,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::SubDecl {
            params,
            param_defs,
            return_type,
            body,
            is_rw,
            is_raw,
            ..
        } = stmt
        {
            let compiled_code = Self::resolve_closure_code(code, cc_idx);
            self.box_captured_lexicals(code, &compiled_code);
            let owned_captures = self.compute_owned_captures(&compiled_code);
            let (captured_upvalues, captured_upvalues_from_local) =
                self.capture_closure_upvalues(code, &compiled_code);
            // Upvalue snapshot (single-store Slice E); see `capture_closure_env`.
            let mut env = self.capture_closure_env(code, &compiled_code);
            if let Some(rt) = return_type {
                env.insert("__mutsu_return_type".to_string(), Value::str(rt.clone()));
            }
            if is_whatever_code {
                env.insert(
                    "__mutsu_callable_type".to_string(),
                    Value::str_from("WhateverCode"),
                );
            }
            let cc_source_line = compiled_code
                .as_ref()
                .and_then(|cc| cc.source_line)
                .map(|l| l as u32)
                .or_else(|| self.current_source_line());
            let val = Value::Sub(std::sync::Arc::new(crate::value::SubData {
                package: Symbol::intern(&self.current_package()),
                name: Symbol::intern(""),
                params: params.clone(),
                param_defs: param_defs.clone(),
                body: body.clone(),
                is_rw: *is_rw,
                is_raw: *is_raw,
                env,
                assumed_positional: Vec::new(),
                assumed_named: std::collections::HashMap::new(),
                id: crate::value::next_instance_id(),
                empty_sig: params.is_empty() && param_defs.is_empty(),
                is_bare_block: false,
                owned_captures,
                compiled_code,
                deprecated_message: None,
                source_line: cc_source_line,
                source_file: self.current_source_file(),
                captured_upvalues,
                captured_upvalues_from_local,
            }));
            self.stack.push(val);
            Ok(())
        } else {
            Err(RuntimeError::new("MakeLambda expects SubDecl"))
        }
    }

    pub(super) fn exec_make_block_closure_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
        cc_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::Block(body) = stmt {
            let compiled_code = Self::resolve_closure_code(code, cc_idx);
            self.box_captured_lexicals(code, &compiled_code);
            let owned_captures = self.compute_owned_captures(&compiled_code);
            let (captured_upvalues, captured_upvalues_from_local) =
                self.capture_closure_upvalues(code, &compiled_code);
            let cc_source_line = compiled_code
                .as_ref()
                .and_then(|cc| cc.source_line)
                .map(|l| l as u32)
                .or_else(|| self.current_source_line());
            let val = Value::Sub(std::sync::Arc::new(crate::value::SubData {
                package: Symbol::intern(&self.current_package()),
                name: Symbol::intern(""),
                params: vec![],
                param_defs: Vec::new(),
                body: body.clone(),
                is_rw: false,
                is_raw: false,
                // Upvalue snapshot (single-store Slice E); see capture_closure_env.
                env: self.capture_closure_env(code, &compiled_code),
                assumed_positional: Vec::new(),
                assumed_named: std::collections::HashMap::new(),
                id: crate::value::next_instance_id(),
                empty_sig: false,
                is_bare_block: true,
                owned_captures,
                compiled_code,
                deprecated_message: None,
                source_line: cc_source_line,
                source_file: self.current_source_file(),
                captured_upvalues,
                captured_upvalues_from_local,
            }));
            self.stack.push(val);
            Ok(())
        } else {
            Err(RuntimeError::new("MakeBlockClosure expects Block"))
        }
    }

    pub(super) fn exec_register_sub_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::SubDecl {
            name,
            name_expr,
            params,
            param_defs,
            return_type,
            associativity,
            signature_alternates,
            body,
            multi,
            is_rw,
            is_raw,
            is_export,
            export_tags,
            is_test_assertion,
            supersede,
            custom_traits,
            ..
        } = stmt
        {
            let resolved_name = if let Some(expr) = name_expr {
                self.vm_eval_block_value(&[Stmt::Expr(expr.clone())])?
                    .to_string_value()
            } else {
                name.resolve()
            };
            self.loan_env_for(|i| {
                i.register_sub_decl(
                    &resolved_name,
                    params,
                    param_defs,
                    return_type.as_ref(),
                    associativity.as_ref(),
                    body,
                    *multi,
                    *is_rw,
                    *is_raw,
                    *is_test_assertion,
                    *supersede,
                    custom_traits,
                )
            })?;
            // If this sub carries the `is native(...)` trait, record its C-FFI
            // descriptor so calls route through NativeCall instead of the body.
            if custom_traits.iter().any(|(t, _)| t == "native") {
                self.register_native_call_sub(
                    &resolved_name,
                    param_defs,
                    return_type.as_ref(),
                    custom_traits,
                )?;
            }
            self.fn_resolve_gen += 1;
            self.method_resolve_cache.clear();
            self.last_method_resolve = None;
            self.fast_method_cache.clear();
            self.multi_resolve_cache.clear();
            self.multi_type_cacheable.clear();
            // Record `&`-sigil parameter names so calls to a same-named routine
            // inside this sub bypass the name-keyed light-call caches (the param
            // can shadow a package sub of the same name).
            for pd in param_defs {
                if let Some(bare) = pd.name.strip_prefix('&')
                    && !bare.is_empty()
                {
                    // Records both plain names (`foo`) and operator categories
                    // (`infix:<@@>`); both can shadow a same-named package routine.
                    self.amp_param_shadowed_names.insert(Symbol::intern(bare));
                }
            }
            if *is_export && !self.suppress_exports {
                let pkg = self.current_package().to_string();
                self.register_exported_sub(pkg.clone(), resolved_name.clone(), export_tags.clone());
                // If a custom `is` trait mixed a role into this routine, the
                // resulting Mixin lives in the lexical env as `&name` but would
                // be dropped when the module scope exits. Capture it so `import`
                // can restore the trait-modified value.
                let code_var_key = format!("&{}", resolved_name);
                if let Some(val @ Value::Mixin(..)) = self.env().get(&code_var_key) {
                    self.record_exported_sub_value(pkg, resolved_name.clone(), val.clone());
                }
            }
            for (alt_params, alt_param_defs) in signature_alternates {
                self.loan_env_for(|i| {
                    i.register_sub_decl(
                        &resolved_name,
                        alt_params,
                        alt_param_defs,
                        return_type.as_ref(),
                        associativity.as_ref(),
                        body,
                        *multi,
                        *is_rw,
                        *is_raw,
                        *is_test_assertion,
                        *supersede,
                        custom_traits,
                    )
                })?;
            }
            // Note: we intentionally do NOT push the Sub onto the stack or
            // store it in env here. The interpreter's trailing_sub_value
            // mechanism handles returning the Sub when it's the last statement
            // of a block. Pushing would interfere with stack depth tracking.
            Ok(())
        } else {
            Err(RuntimeError::new("RegisterSub expects SubDecl"))
        }
    }

    /// Build and store the NativeCall descriptor for an `is native(...)` sub.
    /// The library name comes from the `native` trait argument, the C symbol
    /// from an optional `is symbol('...')` trait (defaulting to the sub name),
    /// and the C signature from the parameter / return type constraints.
    fn register_native_call_sub(
        &mut self,
        name: &str,
        param_defs: &[crate::ast::ParamDef],
        return_type: Option<&String>,
        custom_traits: &[(String, Option<crate::ast::Expr>)],
    ) -> Result<(), RuntimeError> {
        use crate::runtime::nativecall::{CType, NativeCallSpec, ParamSpec};

        // Evaluate a trait's argument expression to a String, if present.
        let mut eval_trait_str = |trait_name: &str| -> Result<Option<String>, RuntimeError> {
            for (t, arg) in custom_traits {
                if t == trait_name {
                    return Ok(match arg {
                        Some(expr) => Some(
                            self.vm_eval_block_value(&[Stmt::Expr(expr.clone())])?
                                .to_string_value(),
                        ),
                        None => None,
                    });
                }
            }
            Ok(None)
        };

        let library = eval_trait_str("native")?;
        let symbol = eval_trait_str("symbol")?.unwrap_or_else(|| name.to_string());

        // Map each parameter's type constraint to a C type. An unmapped /
        // missing type means we cannot marshal it — skip native registration so
        // the failure surfaces clearly rather than mis-calling.
        let mut params = Vec::with_capacity(param_defs.len());
        for pd in param_defs {
            let Some(tc) = pd.type_constraint.as_deref() else {
                return Ok(());
            };
            let Some(ct) = CType::from_type_name(tc) else {
                return Ok(());
            };
            let is_rw = pd.traits.iter().any(|t| t == "rw");
            params.push(ParamSpec { ct, is_rw });
        }

        let ret = match return_type {
            None => CType::Void,
            Some(rt) => match CType::from_type_name(rt) {
                Some(ct) => ct,
                None => return Ok(()),
            },
        };

        self.native_call_specs.insert(
            name.to_string(),
            NativeCallSpec {
                library,
                symbol,
                params,
                ret,
            },
        );
        Ok(())
    }

    pub(super) fn exec_register_token_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        match stmt {
            Stmt::TokenDecl {
                name,
                params,
                param_defs,
                body,
                multi,
                ..
            }
            | Stmt::RuleDecl {
                name,
                params,
                param_defs,
                body,
                multi,
            } => {
                self.register_token_decl(&name.resolve(), params, param_defs, body, *multi);
                Ok(())
            }
            _ => Err(RuntimeError::new(
                "RegisterToken expects TokenDecl/RuleDecl",
            )),
        }
    }

    pub(super) fn exec_register_proto_sub_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::ProtoDecl {
            name,
            params,
            param_defs,
            body,
            is_export,
            custom_traits,
            ..
        } = stmt
        {
            let name_str = name.resolve();
            self.register_proto_decl(&name_str, params, param_defs, body)?;
            if *is_export {
                self.register_proto_decl_as_global(&name_str, params, param_defs, body)?;
            }
            // Apply custom trait_mod:<is> for each non-builtin trait (only if defined)
            if !custom_traits.is_empty() {
                let has_trait_mod =
                    self.has_proto("trait_mod:<is>") || self.has_multi_candidates("trait_mod:<is>");
                for trait_name in custom_traits.iter().filter(|t| {
                    !t.starts_with("__")
                        && *t != "default"
                        && !t.starts_with("DEPRECATED")
                        && *t != "deep"
                }) {
                    if !has_trait_mod {
                        return Err(RuntimeError::new(format!(
                            "Can't use unknown trait 'is' -> '{}' in sub declaration.",
                            trait_name
                        )));
                    }
                    let sub_val = Value::make_sub(
                        Symbol::intern(&self.current_package()),
                        Symbol::intern(&name_str),
                        params.clone(),
                        param_defs.clone(),
                        body.clone(),
                        false,
                        self.clone_env(),
                    );
                    let named_arg = Value::Pair(trait_name.clone(), Box::new(Value::Bool(true)));
                    let result = loan_env!(
                        self,
                        call_function("trait_mod:<is>", vec![sub_val, named_arg])
                    )?;
                    // If the trait_mod returned a modified sub (e.g. with CALL-ME mixed in),
                    // store it in the env so function dispatch can find it.
                    if matches!(result, Value::Mixin(..)) {
                        self.env_mut().insert(format!("&{}", name), result);
                    }
                }
            }
            Ok(())
        } else {
            Err(RuntimeError::new("RegisterProtoSub expects ProtoDecl"))
        }
    }

    pub(super) fn exec_register_proto_token_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::ProtoToken { name } = stmt {
            self.register_proto_token_decl(&name.resolve());
            Ok(())
        } else {
            Err(RuntimeError::new("RegisterProtoToken expects ProtoToken"))
        }
    }

    pub(super) fn exec_use_module_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        tags_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let module = Self::const_str(code, name_idx);
        let tags: Vec<String> = tags_idx
            .and_then(|idx| code.constants.get(idx as usize))
            .and_then(|v| match v {
                Value::Array(items, ..) => Some(
                    items
                        .iter()
                        .map(|v| v.to_string_value())
                        .collect::<Vec<String>>(),
                ),
                _ => None,
            })
            .unwrap_or_default();
        self.vm_use_module_with_tags(module, &tags)?;
        self.fn_resolve_gen += 1;
        self.method_resolve_cache.clear();
        self.last_method_resolve = None;
        self.fast_method_cache.clear();
        self.multi_resolve_cache.clear();
        self.multi_type_cacheable.clear();
        // A module load writes imported symbols into env by name; flag the env so
        // the next GetLocal barrier reconciles them into locals. (An eager
        // sync_locals_from_env here is unsafe: it can clobber a fresh in-place
        // cell mutation of a local that env does not yet reflect -- see the
        // cyclic-`:=`-bind regression in t/element-bind-cell.t. Only the
        // flag-deferred barrier pull, which runs once env is fresh, is correct.)
        Ok(())
    }

    pub(super) fn exec_import_module_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        tags_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let module = Self::const_str(code, name_idx);
        let tags = tags_idx
            .and_then(|idx| code.constants.get(idx as usize))
            .and_then(|v| match v {
                Value::Array(items, ..) => Some(
                    items
                        .iter()
                        .map(|v| v.to_string_value())
                        .collect::<Vec<String>>(),
                ),
                _ => None,
            })
            .unwrap_or_default();
        loan_env!(self, import_module(module, &tags))?;
        self.fn_resolve_gen += 1;
        self.method_resolve_cache.clear();
        self.last_method_resolve = None;
        self.fast_method_cache.clear();
        self.multi_resolve_cache.clear();
        self.multi_type_cacheable.clear();
        // Slice F: write imported symbols through to the caller's local slots
        // (import_module recorded their names); keeps an imported `constant c`
        // coherent without the reverse pull. This op holds the outer `code`.
        self.apply_pending_rw_writeback(code);
        // A module load writes imported symbols into env by name; flag the env so
        // the next GetLocal barrier reconciles them into locals. (An eager
        // sync_locals_from_env here is unsafe: it can clobber a fresh in-place
        // cell mutation of a local that env does not yet reflect -- see the
        // cyclic-`:=`-bind regression in t/element-bind-cell.t. Only the
        // flag-deferred barrier pull, which runs once env is fresh, is correct.)
        Ok(())
    }

    pub(super) fn exec_no_module_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let module = Self::const_str(code, name_idx);
        self.no_module(module)?;
        self.fn_resolve_gen += 1;
        self.method_resolve_cache.clear();
        self.last_method_resolve = None;
        self.fast_method_cache.clear();
        self.multi_resolve_cache.clear();
        self.multi_type_cacheable.clear();
        // A module load writes imported symbols into env by name; flag the env so
        // the next GetLocal barrier reconciles them into locals. (An eager
        // sync_locals_from_env here is unsafe: it can clobber a fresh in-place
        // cell mutation of a local that env does not yet reflect -- see the
        // cyclic-`:=`-bind regression in t/element-bind-cell.t. Only the
        // flag-deferred barrier pull, which runs once env is fresh, is correct.)
        Ok(())
    }

    pub(super) fn exec_need_module_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let module = Self::const_str(code, name_idx);
        self.need_module(module)?;
        self.fn_resolve_gen += 1;
        self.method_resolve_cache.clear();
        self.last_method_resolve = None;
        self.fast_method_cache.clear();
        self.multi_resolve_cache.clear();
        self.multi_type_cacheable.clear();
        // A module load writes imported symbols into env by name; flag the env so
        // the next GetLocal barrier reconciles them into locals. (An eager
        // sync_locals_from_env here is unsafe: it can clobber a fresh in-place
        // cell mutation of a local that env does not yet reflect -- see the
        // cyclic-`:=`-bind regression in t/element-bind-cell.t. Only the
        // flag-deferred barrier pull, which runs once env is fresh, is correct.)
        Ok(())
    }

    pub(super) fn exec_use_lib_path_op(
        &mut self,
        _code: &CompiledCode,
    ) -> Result<(), RuntimeError> {
        let value = self.stack.pop().unwrap_or(Value::Nil);
        let path = value.to_string_value();
        if path.is_empty() {
            return Err(RuntimeError::new(
                "X::LibEmpty: Repository specification can not be an empty string",
            ));
        }
        // An `inst#PREFIX` spec selects a CompUnit::Repository::Installation as
        // the current `$*REPO`, chained in front of whatever was there before.
        if let Some(prefix) = path.strip_prefix("inst#") {
            let prev = self.env().get("*REPO").cloned().unwrap_or(Value::Nil);
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("prefix".to_string(), Value::str(prefix.to_string()));
            attrs.insert("next-repo".to_string(), prev);
            let repo =
                Value::make_instance(Symbol::intern("CompUnit::Repository::Installation"), attrs);
            self.env_mut().insert("*REPO".to_string(), repo);
        }
        self.add_lib_path(path);
        Ok(())
    }

    pub(super) fn exec_register_var_export_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        tags_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx).to_string();
        let tags = tags_idx
            .and_then(|idx| code.constants.get(idx as usize))
            .and_then(|v| match v {
                Value::Array(items, ..) => Some(
                    items
                        .iter()
                        .map(|v| v.to_string_value())
                        .collect::<Vec<String>>(),
                ),
                _ => None,
            })
            .unwrap_or_else(|| vec!["DEFAULT".to_string()]);
        self.register_exported_var(self.current_package().to_string(), name, tags);
        Ok(())
    }

    pub(super) fn exec_apply_var_trait_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        trait_name_idx: u32,
        has_arg: bool,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx);
        let trait_name = Self::const_str(code, trait_name_idx).to_string();

        // Handle `is default(...)` as a built-in variable trait.
        if trait_name == "default" {
            let default_value = if has_arg {
                self.stack.pop().unwrap_or(Value::Nil)
            } else {
                Value::Bool(true)
            };
            let name = name.to_string();
            self.set_var_default(&name, default_value.clone());
            // For array/hash variables, also register the container default
            // so that element access on missing indices returns the default.
            // Fall back to the env value when the variable is not a local slot:
            // an anonymous `(my % is default(...))` in expression position is
            // stored via SetGlobal, not a local, so the embedded default would
            // otherwise be skipped and lost when the value flows out.
            if (name.starts_with('@') || name.starts_with('%'))
                && let Some(container) = self
                    .locals_get_by_name(code, &name)
                    .or_else(|| self.get_env_with_main_alias(&name))
            {
                let container = self.tag_container_default(container, default_value.clone());
                self.locals_set_by_name(code, &name, container.clone());
                self.set_env_with_main_alias(&name, container.clone());
                // Replace existing Nil and uninitialized (Package("Any"))
                // elements with the default value (Raku container semantics:
                // Nil/uninitialized slots in a defaulted container become
                // the default).
                if name.starts_with('@')
                    && let Value::Array(ref items, kind) = container
                {
                    let is_hole = |v: &Value| {
                        matches!(v, Value::Nil) || matches!(v, Value::Package(n) if n == "Any")
                    };
                    let has_holes = items.iter().any(is_hole);
                    if has_holes {
                        let replaced: Vec<Value> = items
                            .iter()
                            .map(|v| {
                                if is_hole(v) {
                                    default_value.clone()
                                } else {
                                    v.clone()
                                }
                            })
                            .collect();
                        let new_arr =
                            Value::Array(Arc::new(crate::value::ArrayData::new(replaced)), kind);
                        let new_arr = self.tag_container_default(new_arr, default_value.clone());
                        self.locals_set_by_name(code, &name, new_arr.clone());
                        self.set_env_with_main_alias(&name, new_arr);
                    }
                }
            }
            // If the variable is currently Nil (uninitialized scalar), set it to the default.
            if !name.starts_with('@') && !name.starts_with('%') {
                let current = self.locals_get_by_name(code, &name);
                if matches!(current, Some(Value::Nil) | None) {
                    self.locals_set_by_name(code, &name, default_value.clone());
                    self.set_env_with_main_alias(&name, default_value);
                }
            }
            return Ok(());
        }

        // Handle `is Buf/Blob/buf8/...` trait for array variables:
        // converts the array variable into a native typed buffer.
        if name.starts_with('@') {
            if trait_name == "List" {
                if has_arg {
                    self.stack.pop(); // discard unsupported trait argument
                }
                self.mark_readonly(name);
                return Ok(());
            }
            let is_buf_trait = matches!(
                trait_name.as_str(),
                "Buf"
                    | "Blob"
                    | "buf"
                    | "blob"
                    | "buf8"
                    | "buf16"
                    | "buf32"
                    | "buf64"
                    | "blob8"
                    | "blob16"
                    | "blob32"
                    | "blob64"
                    | "utf8"
            );
            if is_buf_trait {
                if has_arg {
                    self.stack.pop(); // discard unused arg
                }
                let buf_type = match trait_name.as_str() {
                    "buf" | "blob" => {
                        let resolved = self
                            .call_function("EVAL", vec![Value::str(trait_name.clone())])
                            .ok()
                            .or_else(|| self.get_env_with_main_alias(&trait_name))
                            .or_else(|| self.locals_get_by_name(code, &trait_name));
                        match resolved {
                            Some(Value::Package(sym)) => Value::Package(sym),
                            Some(Value::Scalar(inner)) => match *inner {
                                Value::Package(sym) => Value::Package(sym),
                                _ => Value::Package(crate::symbol::Symbol::intern(
                                    if trait_name == "buf" { "Buf" } else { "Blob" },
                                )),
                            },
                            _ => Value::Package(crate::symbol::Symbol::intern(
                                if trait_name == "buf" { "Buf" } else { "Blob" },
                            )),
                        }
                    }
                    _ => Value::Package(crate::symbol::Symbol::intern(&trait_name)),
                };
                // Get current value, convert to buf.
                // The value might be an Array (from the initializer) or already
                // a Buf/Blob Instance (if SetLocal coerced through an old Buf
                // container in the same slot, e.g. in a loop redeclaration).
                let current = self.locals_get_by_name(code, name).unwrap_or(Value::Nil);
                let items = match &current {
                    Value::Array(items, ..) => items
                        .iter()
                        .map(|v| Value::Int(crate::runtime::to_int(v)))
                        .collect(),
                    Value::Instance { attributes, .. } => {
                        // Extract items from an existing Buf/Blob instance
                        if let Some(Value::Array(items, ..)) = attributes.as_map().get("bytes") {
                            items
                                .iter()
                                .map(|v| Value::Int(crate::runtime::to_int(v)))
                                .collect()
                        } else {
                            Vec::new()
                        }
                    }
                    _ => Vec::new(),
                };
                let buf = self.try_compiled_method_or_interpret(buf_type, "new", items)?;
                let name_str = name.to_string();
                self.locals_set_by_name(code, &name_str, buf.clone());
                self.set_env_with_main_alias(&name_str, buf);
                return Ok(());
            }
        }

        // Handle `is Map` on hash variables: register the hash as a Map type
        // and mark it read-only (Maps are immutable in Raku).
        if name.starts_with('%') && trait_name == "Map" {
            if has_arg {
                self.stack.pop(); // discard unused trait argument
            }
            let name_str = name.to_string();
            // Register container type metadata with declared_type "Map"
            if let Some(container) = self.locals_get_by_name(code, &name_str) {
                let info = crate::runtime::ContainerTypeInfo {
                    value_type: String::new(),
                    key_type: None,
                    declared_type: Some("Map".to_string()),
                };
                // Hashes embed metadata in `HashData`; store the tagged value
                // back into both the local slot and env.
                let tagged = self.tag_container_metadata(container, info);
                self.locals_set_by_name(code, &name_str, tagged.clone());
                self.set_env_with_main_alias(&name_str, tagged);
            }
            // Mark the variable read-only to prevent mutation
            self.mark_readonly(&name_str);
            return Ok(());
        }

        // Handle `is BagHash`, `is SetHash`, `is MixHash`, `is Bag`, `is Set`, `is Mix`
        // (and parameterized versions like `is Set[Int]`, `is Bag[Str]`)
        // on hash variables: replace the variable with an instance of the appropriate type.
        if name.starts_with('%') {
            let base_trait = trait_name.split('[').next().unwrap_or(&trait_name);
            let is_hash_like_trait = matches!(
                base_trait,
                "BagHash" | "SetHash" | "MixHash" | "Bag" | "Set" | "Mix"
            );
            if is_hash_like_trait {
                if has_arg {
                    self.stack.pop(); // discard unused trait argument
                }
                let name_str = name.to_string();
                // Check if the variable already has initial values from the declaration.
                // If so, construct the QuantHash from those values instead of creating
                // an empty one. This handles `my %h is Bag = <a b b c>`.
                let current_val = self.locals_get_by_name(code, &name_str);
                let has_init_values = match &current_val {
                    Some(Value::Hash(h)) => !h.is_empty(),
                    Some(Value::Array(a, _)) => !a.is_empty(),
                    // A Seq/Slip initializer (e.g. `is SetHash = %h.map: {...}`)
                    // must be coerced, not treated as "no initializer".
                    Some(Value::Seq(s) | Value::Slip(s)) => !s.is_empty(),
                    Some(Value::LazyList(_)) => true,
                    // Already converted to a QuantHash by type constraint coercion
                    Some(Value::Set(_, _) | Value::Bag(_, _) | Value::Mix(_, _)) => true,
                    _ => false,
                };
                let mut instance = if has_init_values {
                    let init_val = current_val.unwrap();
                    // If already the target QuantHash type, use it directly
                    let already_target = matches!(
                        (&init_val, base_trait),
                        (Value::Mix(_, _), "MixHash" | "Mix")
                            | (Value::Bag(_, _), "BagHash" | "Bag")
                            | (Value::Set(_, _), "SetHash" | "Set")
                    );
                    if already_target {
                        // Ensure mutability flag matches the trait
                        match init_val {
                            Value::Mix(data, _) => {
                                let mutable = base_trait == "MixHash";
                                Value::Mix(data, mutable)
                            }
                            Value::Bag(data, _) => {
                                let mutable = base_trait == "BagHash";
                                Value::Bag(data, mutable)
                            }
                            Value::Set(data, _) => {
                                let mutable = base_trait == "SetHash";
                                Value::Set(data, mutable)
                            }
                            other => other,
                        }
                    } else {
                        // Convert initial values to the target QuantHash type
                        self.try_compiled_method_or_interpret(init_val, base_trait, vec![])?
                    }
                } else {
                    let type_obj = Value::Package(crate::symbol::Symbol::intern(base_trait));
                    self.try_compiled_method_or_interpret(type_obj, "new", vec![])?
                };
                // Type check for parameterized QuantHash (e.g. Mix[Int], Set[Str])
                // and store typed original_keys so .keys returns typed values
                if let Some(bracket_pos) = trait_name.find('[') {
                    let constraint = &trait_name[bracket_pos + 1..trait_name.len() - 1];
                    if constraint.starts_with(char::is_uppercase)
                        && constraint != "Any"
                        && constraint != "Mu"
                    {
                        let keys: Vec<String> = match &instance {
                            Value::Mix(m, _) => m.weights.keys().cloned().collect(),
                            Value::Bag(b, _) => b.counts.keys().cloned().collect(),
                            Value::Set(s, _) => s.elements.iter().cloned().collect(),
                            _ => vec![],
                        };
                        // Try to coerce keys to the constraint type for type checking
                        let mut typed_keys = std::collections::HashMap::new();
                        for key in &keys {
                            let coerced = self.try_coerce_str_to_type(key, constraint);
                            if let Some(ref typed_val) = coerced {
                                if !self.type_matches_value(constraint, typed_val) {
                                    let got_type = crate::value::what_type_name(typed_val);
                                    return Err(RuntimeError::typecheck_binding_parameter(
                                        key, constraint, &got_type, None,
                                    ));
                                }
                                typed_keys.insert(key.clone(), typed_val.clone());
                            } else {
                                // Can't coerce to type - check original string
                                let key_val = Value::str(key.clone());
                                if !self.type_matches_value(constraint, &key_val) {
                                    let got_type = crate::value::what_type_name(&key_val);
                                    return Err(RuntimeError::typecheck_binding_parameter(
                                        key, constraint, &got_type, None,
                                    ));
                                }
                            }
                        }
                        // Store typed keys in the QuantHash so .keys returns typed values
                        if !typed_keys.is_empty() {
                            instance = match instance {
                                Value::Mix(data, mutable) => {
                                    let new_data = crate::value::MixData::with_original_keys(
                                        data.weights.clone(),
                                        typed_keys,
                                    );
                                    Value::Mix(std::sync::Arc::new(new_data), mutable)
                                }
                                Value::Bag(data, mutable) => {
                                    let new_data = crate::value::BagData::with_original_keys(
                                        data.counts.clone(),
                                        typed_keys,
                                    );
                                    Value::Bag(std::sync::Arc::new(new_data), mutable)
                                }
                                Value::Set(data, mutable) => {
                                    let new_data = crate::value::SetData::with_original_keys(
                                        data.elements.clone(),
                                        typed_keys,
                                    );
                                    Value::Set(std::sync::Arc::new(new_data), mutable)
                                }
                                other => other,
                            };
                        }
                    }
                }
                // Embed container type metadata so assignment operations
                // know to coerce back to BagHash/SetHash/etc.
                let info = crate::runtime::ContainerTypeInfo {
                    value_type: String::new(),
                    key_type: None,
                    declared_type: Some(trait_name.clone()),
                };
                let instance = self.tag_container_metadata(instance, info);
                self.locals_set_by_name(code, &name_str, instance.clone());
                self.set_env_with_main_alias(&name_str, instance.clone());
                // Set type constraint so future assignments are coerced correctly
                self.vm_set_var_type_constraint(&name_str, Some(trait_name.clone()));
                return Ok(());
            }
        }

        // For array/hash variables with a class trait that inherits from Array[X] or Hash[V,K],
        // propagate the element type constraint so .of works correctly.
        if name.starts_with('@') || name.starts_with('%') {
            let element_type = self.find_parameterized_container_parent(&trait_name);
            if let Some(et) = element_type {
                if has_arg {
                    self.stack.pop(); // discard unused trait argument
                }
                let name_str = name.to_string();
                self.vm_set_var_type_constraint(&name_str, Some(et));
                return Ok(());
            }
        }

        if !(self.has_proto("trait_mod:<is>") || self.has_multi_candidates("trait_mod:<is>")) {
            // For uppercase type-like traits (e.g. `is Map`, `is Set`), silently
            // accept them even if trait_mod:<is> is not defined. These are type
            // container traits that may not have runtime trait_mod:<is> handlers.
            if trait_name
                .chars()
                .next()
                .is_some_and(|c| c.is_ascii_uppercase())
            {
                if has_arg {
                    self.stack.pop();
                }
                return Ok(());
            }
            return Err(RuntimeError::new(format!(
                "X::Comp::Trait::Unknown: Unknown variable trait 'is {}'",
                trait_name
            )));
        }
        let trait_value = if has_arg {
            self.stack.pop().unwrap_or(Value::Nil)
        } else {
            Value::Bool(true)
        };
        let target = self.env().get(name).cloned().unwrap_or(Value::Nil);
        // CARRIER: `.VAR` pseudo-method + `trait_mod:<is>` metaprogramming hook
        // (reflective container object + user trait handler). See ledger §C.
        let var_obj = loan_env!(
            self,
            call_method_mut_with_values(name, target, "VAR", vec![])
        )?;
        let named_arg = Value::Pair(trait_name, Box::new(trait_value));
        self.vm_call_function("trait_mod:<is>", vec![var_obj, named_arg])?;
        Ok(())
    }

    pub(super) fn exec_register_enum_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::EnumDecl {
            name,
            variants,
            is_export,
            base_type,
            language_version,
        } = stmt
        {
            let result = loan_env!(
                self,
                register_enum_decl(&name.resolve(), variants, *is_export, base_type.as_deref(),)
            )?;
            // Store language revision metadata from the version captured at parse time
            if !name.resolve().is_empty() {
                self.store_language_revision_from_version(&name.resolve(), language_version);
            }
            // For anonymous enums, push the Map result onto the stack
            if name.resolve().is_empty() {
                self.stack.push(result);
            }
            Ok(())
        } else {
            Err(RuntimeError::new("RegisterEnum expects EnumDecl"))
        }
    }

    pub(super) fn exec_register_class_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        // Registering a class can shadow a same-named earlier class (`my class A`
        // in a fresh lexical scope) with different method bodies/candidates, so the
        // method-resolution caches — keyed on the class NAME symbol — must be
        // invalidated, or a cached resolution from the old class would be reused for
        // the new one. (The multi-resolution cache made this observable:
        // S12-methods/multi.t reuses `my class A`/`B` with multi submethods.)
        self.method_resolve_cache.clear();
        self.last_method_resolve = None;
        self.fast_method_cache.clear();
        self.multi_resolve_cache.clear();
        self.multi_type_cacheable.clear();
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::ClassDecl {
            name,
            name_expr,
            parents,
            class_is_rw,
            is_hidden,
            is_lexical,
            hidden_parents,
            does_parents,
            repr,
            body,
            language_version,
            custom_traits,
            ..
        } = stmt
        {
            let resolved_name = if let Some(expr) = name_expr {
                self.vm_eval_block_value(&[Stmt::Expr(expr.clone())])?
                    .to_string_value()
            } else {
                name.resolve()
            };
            let current_package = self.current_package().to_string();
            let qualified_name = if let Some(stripped) = resolved_name.strip_prefix("GLOBAL::") {
                // `class GLOBAL::Foo` declares Foo in the global namespace
                stripped.to_string()
            } else if resolved_name.contains("::")
                || current_package == "GLOBAL"
                || resolved_name == current_package
            {
                resolved_name.clone()
            } else {
                format!("{current_package}::{resolved_name}")
            };
            // If the name was previously suppressed (e.g. by a `my class` in an
            // earlier block), clear the suppression before running the class body
            // so that references to the class name inside the body can resolve.
            self.unsuppress_name(&resolved_name);
            // TODO: Detect redeclaration of package-scoped classes across
            // EVAL boundaries (X::Redeclaration). Currently deferred because
            // distinguishing EVAL re-definitions from normal re-execution
            // (e.g., anonymous classes in loops, augment) requires tracking
            // compilation unit boundaries.
            let deferred_traits = loan_env!(
                self,
                register_class_decl(
                    &qualified_name,
                    parents,
                    crate::runtime::ClassDeclModifiers {
                        class_is_rw: *class_is_rw,
                        is_hidden: *is_hidden,
                        is_lexical: *is_lexical,
                        hidden_parents,
                        does_parents,
                        language_version,
                    },
                    body,
                )
            )?;
            // Check for assignment to native read-only params before
            // compiling (X::Assignment::RO::Comp).
            if let Some(err) = self.check_class_native_readonly_param_errors(&qualified_name) {
                return Err(err);
            }
            // Compile method bodies to bytecode for the fast path
            self.compile_class_methods(&qualified_name);
            // Register CUnion repr if present
            if let Some(repr_name) = repr
                && repr_name == "CUnion"
            {
                self.register_cunion_class(&qualified_name);
            }
            // Register the class name in the lexical env so that
            // ::("ClassName") indirect lookups can find it in the current scope.
            let env = self.env_mut();
            env.insert(
                "_".to_string(),
                Value::Package(Symbol::intern(&qualified_name)),
            );
            // Always insert the class type object so that class names take
            // precedence over same-named `$`-sigiled variables (whose stripped
            // name may already be in the env).
            env.insert(
                qualified_name.clone(),
                Value::Package(Symbol::intern(&qualified_name)),
            );
            // When a nested class is registered inside another class (e.g. class B inside class A
            // becomes A::B), suppress the short name (B) so it cannot be used outside.
            // Only suppress when the parent package is itself a class, not a module.
            // Also register the short name in the lexical env so it is available
            // within the enclosing class body and its methods.
            let parent_is_class = qualified_name
                .rsplit_once("::")
                .map(|(parent, _)| self.has_class(parent))
                .unwrap_or(false);
            if qualified_name != resolved_name && !resolved_name.contains("::") && parent_is_class {
                self.suppress_name(&resolved_name);
                // Register the short name in the lexical env so it resolves
                // within the enclosing class scope (e.g. `Frog` inside `Forest`).
                let env = self.env_mut();
                env.insert(
                    resolved_name.clone(),
                    Value::Package(Symbol::intern(&qualified_name)),
                );
            }
            // When a class is declared with an already-qualified name
            // (e.g. the compiler pre-qualified `class C1` inside
            // `unit module M` to `M::C1`), also register the short name
            // `C1` in the env so that subsequent code inside the same
            // module can refer to it bare. Skip this when the parent
            // package is a class (where suppress_name semantics apply).
            if qualified_name.contains("::") && !parent_is_class {
                let short = qualified_name
                    .rsplit_once("::")
                    .map(|(_, s)| s.to_string())
                    .unwrap_or_else(|| qualified_name.clone());
                // Do not shadow built-in types (e.g. `my class X::Roast::Channel`
                // must not make the bare name `Channel` resolve to the user class).
                if !short.is_empty() && short != qualified_name && !Self::is_builtin_type(&short) {
                    self.env_mut().entry_or_insert_with(short, || {
                        Value::Package(Symbol::intern(&qualified_name))
                    });
                }
            }
            // When `my class` is used, register the class name as lexically scoped
            // so it gets suppressed when the enclosing block scope exits.
            if *is_lexical {
                self.register_lexical_class(resolved_name.clone());
                // Also mark as my-scoped so it's excluded from the parent package stash
                self.mark_my_scoped_package_item(qualified_name.clone());
            }
            // Store language revision metadata from the version captured at parse time
            self.store_language_revision_from_version(&qualified_name, language_version);

            // Dispatch custom `is` traits via trait_mod:<is> if defined.
            // Merge explicitly parsed custom_traits with deferred_traits
            // (unknown lowercase parents deferred from register_class_decl).
            let has_trait_mod =
                self.has_proto("trait_mod:<is>") || self.has_multi_candidates("trait_mod:<is>");
            if has_trait_mod && (!custom_traits.is_empty() || !deferred_traits.is_empty()) {
                let type_obj = Value::Package(Symbol::intern(&qualified_name));
                // Dispatch explicitly parsed custom traits (with args)
                for (trait_name, trait_arg) in custom_traits {
                    let trait_value = if let Some(arg_expr) = trait_arg {
                        self.vm_eval_block_value(&[Stmt::Expr(arg_expr.clone())])?
                    } else {
                        Value::Bool(true)
                    };
                    let named_arg = Value::Pair(trait_name.clone(), Box::new(trait_value));
                    self.vm_call_function("trait_mod:<is>", vec![type_obj.clone(), named_arg])?;
                }
                // Dispatch deferred unknown parents as custom traits (no args)
                for trait_name in &deferred_traits {
                    let named_arg = Value::Pair(trait_name.clone(), Box::new(Value::Bool(true)));
                    self.vm_call_function("trait_mod:<is>", vec![type_obj.clone(), named_arg])?;
                }
            }

            // Slice F: write the deferred body's outer-lexical mutations through
            // to this caller frame's local slots (`register_class_decl` ran the
            // body via `run_block_raw`, which recorded them); keeps e.g.
            // `$tracker` coherent without the reverse pull. This op holds the
            // outer `code`.
            self.apply_pending_rw_writeback(code);

            Ok(())
        } else {
            Err(RuntimeError::new("RegisterClass expects ClassDecl"))
        }
    }

    pub(super) fn exec_augment_class_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::AugmentClass {
            name,
            body,
            is_role,
        } = stmt
        {
            let name_str = name.resolve();
            // Check MONKEY-TYPING pragma: we check if `use MONKEY-TYPING` or `use MONKEY`
            // was issued. Since the compiler simply ignores these `use` statements,
            // we track them at the interpreter level.
            if !self.monkey_typing_enabled() {
                return Err(RuntimeError::typed_msg(
                    "X::Syntax::Augment::WithoutMonkeyTyping",
                    "augment not allowed without 'use MONKEY-TYPING'",
                ));
            }
            if *is_role {
                return Err(self.augment_role_error(&name_str));
            }
            loan_env!(self, augment_class(&name_str, body))?;
            // Recompile augmented class methods for the fast path
            self.compile_class_methods(&name_str);
            Ok(())
        } else {
            Err(RuntimeError::new("AugmentClass expects AugmentClass stmt"))
        }
    }

    pub(super) fn exec_register_role_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::RoleDecl {
            name,
            type_params,
            type_param_defs,
            is_export,
            export_tags,
            body,
            is_rw,
            language_version,
            custom_traits,
        } = stmt
        {
            let name_str = name.resolve();
            let current_package = self.current_package().to_string();
            let qualified_name = if let Some(stripped) = name_str.strip_prefix("GLOBAL::") {
                stripped.to_string()
            } else if name_str.contains("::")
                || current_package == "GLOBAL"
                || name_str == current_package
            {
                name_str.clone()
            } else {
                format!("{current_package}::{name_str}")
            };
            // If the short name was suppressed by an earlier lexical type with
            // the same name, re-enable it before registering the new role.
            self.unsuppress_name(&name_str);
            loan_env!(
                self,
                register_role_decl(&qualified_name, type_params, type_param_defs, body, *is_rw,)
            )?;
            if *is_export && !self.suppress_exports {
                // The compiler may have pre-qualified the role name
                // (e.g. `R1` → `GH2613::R1`) when compiling under a
                // `unit module`. Exports use the short bare name and
                // the originating package, so split the qualified name.
                let (export_pkg, export_short) =
                    if let Some((pkg, short)) = name_str.rsplit_once("::") {
                        (pkg.to_string(), short.to_string())
                    } else {
                        (current_package.clone(), name_str.clone())
                    };
                self.register_exported_var(export_pkg, export_short, export_tags.clone());
            }
            // Store language revision metadata from the version captured at parse time
            self.store_language_revision_from_version(&qualified_name, language_version);
            // Compile role method bodies to bytecode
            self.compile_role_methods(&qualified_name);
            self.env_mut().insert(
                "_".to_string(),
                Value::Package(Symbol::intern(&qualified_name)),
            );
            self.env_mut().insert(
                qualified_name.clone(),
                Value::Package(Symbol::intern(&qualified_name)),
            );
            if qualified_name != name_str && !name_str.contains("::") {
                self.env_mut().insert(
                    name_str.clone(),
                    Value::Package(Symbol::intern(&qualified_name)),
                );
            }
            // When a role is declared with an already-qualified name
            // (e.g. the compiler pre-qualified `role R1` inside
            // `unit module GH2613` to `GH2613::R1`), also register the
            // short name `R1` in the env so subsequent code in the same
            // module can refer to it bare.
            if qualified_name.contains("::") && qualified_name == name_str {
                let short = qualified_name
                    .rsplit_once("::")
                    .map(|(_, s)| s.to_string())
                    .unwrap_or_else(|| qualified_name.clone());
                if !short.is_empty() && short != qualified_name {
                    self.env_mut().entry_or_insert_with(short, || {
                        Value::Package(Symbol::intern(&qualified_name))
                    });
                }
            }
            // Execute deferred non-declaration body statements now that the role
            // name is fully available in the environment.  This lets code like
            // `role R { method foo {}; R.foo }` work.
            if type_params.is_empty() {
                let deferred = self
                    .get_role_def(&qualified_name)
                    .map(|r| r.deferred_body_stmts.clone())
                    .unwrap_or_default();
                for stmt in &deferred {
                    self.vm_run_block_raw(std::slice::from_ref(stmt))?;
                }
                // Slice F: write the deferred body's outer-lexical mutations
                // through to this caller frame's local slots (vm_run_block_raw
                // recorded them); keeps `$side` coherent without the reverse pull.
                self.apply_pending_rw_writeback(code);
            }

            // Gather deferred custom traits from role registration
            let role_deferred = self
                .get_role_def(&qualified_name)
                .map(|r| r.deferred_custom_traits.clone())
                .unwrap_or_default();

            // Dispatch custom `is` traits via trait_mod:<is> if defined
            let has_trait_mod =
                self.has_proto("trait_mod:<is>") || self.has_multi_candidates("trait_mod:<is>");
            if has_trait_mod && (!custom_traits.is_empty() || !role_deferred.is_empty()) {
                let type_obj = Value::Package(Symbol::intern(&qualified_name));
                for (trait_name, trait_arg) in custom_traits {
                    // Skip internal markers (e.g. `__my_scoped`); they are not real `is` traits.
                    if trait_name.starts_with("__") {
                        continue;
                    }
                    let trait_value = if let Some(arg_expr) = trait_arg {
                        self.vm_eval_block_value(&[Stmt::Expr(arg_expr.clone())])?
                    } else {
                        Value::Bool(true)
                    };
                    let named_arg = Value::Pair(trait_name.clone(), Box::new(trait_value));
                    self.vm_call_function("trait_mod:<is>", vec![type_obj.clone(), named_arg])?;
                }
                // Dispatch deferred unknown parents as custom traits (no args)
                for trait_name in &role_deferred {
                    let named_arg = Value::Pair(trait_name.clone(), Box::new(Value::Bool(true)));
                    self.vm_call_function("trait_mod:<is>", vec![type_obj.clone(), named_arg])?;
                }
            }

            Ok(())
        } else {
            Err(RuntimeError::new("RegisterRole expects RoleDecl"))
        }
    }

    pub(super) fn exec_register_subset_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::SubsetDecl {
            name,
            base,
            predicate,
            version,
            is_export,
            export_tags,
        } = stmt
        {
            let resolved_name = name.resolve();
            loan_env!(
                self,
                register_subset_decl(&resolved_name, base, predicate.as_ref(), version,)
            );
            // When a subset is declared `is export` inside a module, record it
            // in the export table so `import M` (and `use M`) can find it.
            // The subset type itself is already registered under its bare name
            // in the global env by `register_subset_decl`, so importing only
            // needs to make `import M` succeed (and validate export tags).
            if *is_export && !self.suppress_exports {
                let (export_pkg, export_short) =
                    if let Some((pkg, short)) = resolved_name.rsplit_once("::") {
                        (pkg.to_string(), short.to_string())
                    } else {
                        (self.current_package().to_string(), resolved_name)
                    };
                self.register_exported_var(export_pkg, export_short, export_tags.clone());
            }
            Ok(())
        } else {
            Err(RuntimeError::new("RegisterSubset expects SubsetDecl"))
        }
    }

    pub(super) fn exec_subtest_scope_op(
        &mut self,
        code: &CompiledCode,
        body_end: u32,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let end = body_end as usize;
        let body_start = *ip + 1;
        let label = self.stack.pop().unwrap_or(Value::Nil).to_string_value();
        let ctx = self.begin_subtest();
        let saved_depth = self.stack.len();
        let run_result = self.run_range(code, body_start, end, compiled_fns);
        self.stack.truncate(saved_depth);
        self.finish_subtest(ctx, &label, run_result)?;
        *ip = end;
        Ok(())
    }

    pub(super) fn exec_react_scope_op(
        &mut self,
        code: &CompiledCode,
        body_end: u32,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let end = body_end as usize;
        let body_start = *ip + 1;

        // `whenever` callbacks run as compiled bytecode (the drive loop lives on
        // `impl Interpreter`, see `vm_react_loop.rs`) but still capture their lexicals from
        // env. First pull any pending env updates into locals (e.g. instance
        // attribute mutations written into the shared cell after bind-stdin), then
        // flush all locals to env so captured vars are visible/mutable from the
        // whenever callbacks.
        self.sync_env_from_locals(code);

        // Enter react mode: whenever blocks will register subscriptions
        self.enter_react();
        let saved_depth = self.stack.len();
        let run_result = self.run_range(code, body_start, end, compiled_fns);
        self.stack.truncate(saved_depth);

        // If `done;` was called in the react body, skip the event loop —
        // the body already signaled that no further events should be processed.
        let body_done = matches!(&run_result, Err(e) if e.is_react_done());
        // The react/supply drive loop runs Interpreter-side and dispatches every
        // whenever / LAST / QUIT / CLOSE callback as compiled bytecode
        // (Stage 2 #3038/#3039; QUIT handlers Interpreter-native in the Stage 3 follow-up).
        // No drive-loop callback routes back through the tree-walk interpreter.
        // The `whenever` callbacks mutate captured-outer lexicals by name in env,
        // with no per-write record this site can drain. Snapshot the caller frame's
        // slot-backing env values right before the event loop so that, after it,
        // only the slots whose env value actually changed are written through.
        let pre_env: Vec<Option<Value>> = code
            .locals
            .iter()
            .map(|n| {
                self.env().get(n).cloned().or_else(|| {
                    n.strip_prefix('$')
                        .or_else(|| n.strip_prefix('@'))
                        .or_else(|| n.strip_prefix('%'))
                        .or_else(|| n.strip_prefix('&'))
                        .and_then(|b| self.env().get(b).cloned())
                })
            })
            .collect();
        let event_result = if body_done {
            // Drain any queued subscriptions so they don't leak
            self.run_react_event_loop_drain();
            Ok(())
        } else {
            self.run_react_event_loop()
        };
        // Slice F (react/whenever coherence): the `whenever` callbacks ran as
        // compiled bytecode on *this* VM (synchronous `from-list` emit) and
        // mutated captured-outer caller lexicals (`my $i; whenever ... { $i++ }`)
        // straight into `env` by name. Reconcile the caller's local slots from
        // env so the slot stays coherent (same HashEntryRef / `!attr` per-slot
        // skips); this is what keeps `$i` correct.
        for (i, name) in code.locals.iter().enumerate() {
            if name.starts_with('!') || matches!(self.locals[i], Value::HashEntryRef { .. }) {
                continue;
            }
            let cur = self.env().get(name).cloned().or_else(|| {
                name.strip_prefix('$')
                    .or_else(|| name.strip_prefix('@'))
                    .or_else(|| name.strip_prefix('%'))
                    .or_else(|| name.strip_prefix('&'))
                    .and_then(|b| self.env().get(b).cloned())
            });
            if let Some(cur) = cur
                && pre_env.get(i).map(|p| p.as_ref()) != Some(Some(&cur))
            {
                self.locals[i] = cur;
            }
        }

        *ip = end;
        if let Err(err) = run_result
            && !err.is_react_done()
        {
            return Err(err);
        }
        if let Err(err) = event_result
            && !err.is_react_done()
        {
            // Wrap in X::React::Died if not already wrapped
            return Err(crate::runtime::Interpreter::wrap_react_died_if_needed(err));
        }
        Ok(())
    }

    pub(super) fn exec_whenever_scope_op(
        &mut self,
        code: &CompiledCode,
        body_idx: u32,
        param_idx: &Option<u32>,
        target_var_idx: &Option<u32>,
    ) -> Result<(), RuntimeError> {
        let supply_val = self.stack.pop().unwrap_or(Value::Nil);
        let param = param_idx.map(|idx| Self::const_str(code, idx).to_string());
        let target_var = target_var_idx.map(|idx| Self::const_str(code, idx));
        let stmt = &code.stmt_pool[body_idx as usize];
        if let Stmt::Block(body) = stmt {
            loan_env!(
                self,
                run_whenever_with_value(supply_val, target_var, &param, body)
            )?;
            // Slice F (env<->locals coherence): a `my $tap = do whenever $sup {…}`
            // binds the tap handle by writing `env[target_var]` directly (see
            // `run_whenever_with_value`), but never updates the caller's local
            // slot. With the reverse env->locals pull disabled, a later read of
            // that variable *within the same react block* (e.g.
            // `isa-ok $tap, Tap`) sees the stale slot (the `do` block's own
            // result) instead of the bound tap. Reconcile the caller's slots from
            // env here so the binding is visible immediately. Byte-identical with
            // the reverse pull enabled.
            //
            // env_dirty substrate (docs/captured-outer-cell-sharing.md §10): the
            // bound name is known exactly (`target_var`), so write just that slot
            // through from env — the precise form of the blanket reconcile below.
            // Armed only under boxing; the default build keeps the blanket pull.
            if let Some(name) = target_var
                && let Some(slot) = self.find_local_slot(code, name)
                && !matches!(self.locals[slot], Value::HashEntryRef { .. })
                && let Some(val) = self.env().get(name).cloned()
            {
                self.locals[slot] = val;
            }
            Ok(())
        } else {
            Err(RuntimeError::new("WheneverScope expects Block body"))
        }
    }

    /// Walk the MRO of `class_name` to find a parameterized Array or Hash parent.
    /// Returns the element type if found (e.g. "Str" for `Array[Str]`).
    fn find_parameterized_container_parent(&self, class_name: &str) -> Option<String> {
        let parents = self.class_parents_readonly(class_name);
        for parent in &parents {
            if let Some(inner) = parent
                .strip_prefix("Array[")
                .or_else(|| parent.strip_prefix("List["))
                .and_then(|s| s.strip_suffix(']'))
            {
                return Some(inner.trim().to_string());
            }
        }
        // Also check the class itself in case it IS a parameterized type
        if let Some(inner) = class_name
            .strip_prefix("Array[")
            .or_else(|| class_name.strip_prefix("List["))
            .and_then(|s| s.strip_suffix(']'))
        {
            return Some(inner.trim().to_string());
        }
        None
    }
}
