use super::*;
use std::sync::atomic::Ordering;

impl Compiler {
    /// Recursively allocate local variable slots for sub_signature parameters
    /// (array/hash unpacking in function signatures like `sub f([$a, *@b]) { ... }`).
    fn alloc_sub_signature_locals(compiler: &mut Compiler, sub_params: &[crate::ast::ParamDef]) {
        for sp in sub_params {
            if !sp.name.is_empty() {
                compiler.alloc_local(&sp.name);
            }
            if let Some(nested) = &sp.sub_signature {
                Self::alloc_sub_signature_locals(compiler, nested);
            }
        }
    }

    /// Convert `Stmt::Call`'s `Vec<CallArg>` to `Vec<Expr>` for expression-level
    /// compilation (needed when a Stmt::Call is the last statement in a sub body
    /// and its return value must be preserved as implicit return).
    pub(super) fn call_args_to_expr_args(args: &[crate::ast::CallArg]) -> Vec<Expr> {
        args.iter()
            .map(|arg| match arg {
                crate::ast::CallArg::Positional(e) | crate::ast::CallArg::Invocant(e) => e.clone(),
                crate::ast::CallArg::Named {
                    name,
                    value: Some(e),
                } => Expr::Binary {
                    left: Box::new(Expr::Literal(Value::str(name.clone()))),
                    op: crate::token_kind::TokenKind::FatArrow,
                    right: Box::new(e.clone()),
                },
                crate::ast::CallArg::Named { name, value: None } => Expr::Binary {
                    left: Box::new(Expr::Literal(Value::str(name.clone()))),
                    op: crate::token_kind::TokenKind::FatArrow,
                    right: Box::new(Expr::Literal(Value::Bool(true))),
                },
                crate::ast::CallArg::Slip(e) => Expr::Unary {
                    op: crate::token_kind::TokenKind::Pipe,
                    expr: Box::new(e.clone()),
                },
            })
            .collect()
    }

    /// Compile a SubDecl body to a CompiledFunction and store it.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn compile_sub_body(
        &mut self,
        name: &str,
        params: &[String],
        param_defs: &[crate::ast::ParamDef],
        return_type: Option<&String>,
        body: &[Stmt],
        multi: bool,
        state_group: Option<&str>,
        is_rw: bool,
        is_raw: bool,
    ) {
        self.compile_sub_body_with_deprecation(
            name,
            params,
            param_defs,
            return_type,
            body,
            multi,
            state_group,
            is_rw,
            is_raw,
            None,
        );
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn compile_sub_body_with_deprecation(
        &mut self,
        name: &str,
        params: &[String],
        param_defs: &[crate::ast::ParamDef],
        return_type: Option<&String>,
        body: &[Stmt],
        multi: bool,
        state_group: Option<&str>,
        is_rw: bool,
        is_raw: bool,
        deprecated_info: Option<(String, String, String, String)>,
    ) {
        // Before compiling the sub body, check for heredoc interpolations
        // that reference variables not visible at the outer scope (where the
        // heredoc terminator physically appears in Raku).
        if let Some(err) = self.check_heredoc_scope_errors(body) {
            let idx = self.code.add_constant(err);
            self.code.emit(OpCode::LoadConst(idx));
            self.code.emit(OpCode::Die);
            return;
        }
        let sink_last_expr = return_type
            .map(|s| Self::is_definite_return_spec(s))
            .unwrap_or(false);
        let mut sub_compiler = Compiler::new();
        sub_compiler.is_routine = true;
        sub_compiler.lexically_in_routine = true;
        // A method body carries the synthetic `?CLASS` parameter injected by the
        // MethodDecl lowering. Methods always provide an implicit `*%_` / `*@_`
        // slurpy, so `%_` / `@_` are valid lexicals anywhere in the body.
        sub_compiler.lexically_in_method = params.iter().any(|p| p == "?CLASS");
        // Propagate last_source_line so the sub body knows which line
        // the sub was defined at (for backtraces).
        sub_compiler.last_source_line = self.last_source_line;
        // Propagate distribution context so $?DISTRIBUTION works inside subs
        sub_compiler.current_distribution = self.current_distribution.clone();
        let arity = param_defs
            .iter()
            .filter(|p| !p.named && (!p.slurpy || p.name == "_capture"))
            .count();
        let state_scope = if multi {
            if let Some(group) = state_group {
                format!("{}::&{}/shared:{}", self.current_package, name, group)
            } else {
                let type_sig: Vec<String> = param_defs
                    .iter()
                    .filter(|pd| !pd.named && (!pd.slurpy || pd.name == "_capture"))
                    .map(|pd| pd.type_constraint.clone().unwrap_or_default())
                    .collect();
                if !type_sig.is_empty() {
                    format!(
                        "{}::&{}/{}:{}",
                        self.current_package,
                        name,
                        arity,
                        type_sig.join(",")
                    )
                } else {
                    format!("{}::&{}/{}", self.current_package, name, arity)
                }
            }
        } else {
            format!("{}::&{}/{}", self.current_package, name, arity)
        };
        // Preserve the enclosing package for $?PACKAGE resolution.
        // The state_scope assigned to current_package below is a mangled name
        // (e.g. `Test2::&pkg/0`) used to scope state variables; it is not the
        // package the sub was declared in. `$?PACKAGE` must resolve to the
        // declaring package, so capture that here before overwriting.
        sub_compiler.enclosing_package = Some(
            self.enclosing_package
                .clone()
                .unwrap_or_else(|| self.current_package.clone()),
        );
        sub_compiler.set_current_package(state_scope);
        // Pre-allocate locals for parameters
        for param in params {
            sub_compiler.alloc_local(param);
        }
        // Also allocate from param_defs in case param names differ
        for pd in param_defs {
            if !pd.name.is_empty() {
                sub_compiler.alloc_local(&pd.name);
                // Track sigilless parameters so BareWord resolution uses
                // GetLocal for them but not for `$`-sigiled params.
                if pd.sigilless {
                    sub_compiler.sigilless_locals.insert(pd.name.clone());
                }
            }
            // Also allocate locals for sub_signature parameters (array unpacking)
            // so that they get proper local slots and don't leak between recursive calls.
            if let Some(sub_params) = &pd.sub_signature {
                Self::alloc_sub_signature_locals(&mut sub_compiler, sub_params);
            }
            // Also allocate locals for outer_sub_signature parameters
            if let Some(outer_params) = &pd.outer_sub_signature {
                Self::alloc_sub_signature_locals(&mut sub_compiler, outer_params);
            }
        }
        // Hoist sub declarations within the sub body
        sub_compiler.hoist_sub_decls(body, true);
        // Emit SetSourceLine at the start of the function body so that
        // ?LINE is set to the sub's definition line on entry.
        if let Some(line) = sub_compiler.last_source_line {
            sub_compiler.code.emit(OpCode::SetSourceLine(line));
        }
        // If sub body contains CATCH/CONTROL, wrap in implicit try. compile_try
        // leaves the body's final-expression value on the stack; for a normal
        // value-producing sub that value is the implicit return, so keep it.
        // Only discard it when the return spec fixes the value (sink_last_expr).
        if Self::has_catch_or_control(body) {
            sub_compiler.compile_try(body, &None);
            if sink_last_expr {
                sub_compiler.code.emit(OpCode::Pop);
                let nil_idx = sub_compiler.code.add_constant(Value::Nil);
                sub_compiler.code.emit(OpCode::LoadConst(nil_idx));
            }
        } else if Self::has_block_enter_leave_phasers(body) {
            let idx = sub_compiler.code.emit(OpCode::BlockScope {
                pre_end: 0,
                enter_end: 0,
                body_end: 0,
                keep_start: 0,
                undo_start: 0,
                post_start: 0,
                end: 0,
            });
            // PRE phasers (forward order, before ENTER)
            Self::compile_pre_phasers(&mut sub_compiler, body);
            sub_compiler.code.patch_block_pre_end(idx);
            let mut body_main: Vec<Stmt> = Vec::new();
            // Collect all ENTER phaser bodies for potential value production
            let mut enter_bodies: Vec<Vec<Stmt>> = Vec::new();
            for stmt in body.iter() {
                match stmt {
                    Stmt::Phaser {
                        kind: PhaserKind::Enter,
                        body: enter_body,
                    } => {
                        enter_bodies.push(enter_body.clone());
                    }
                    Stmt::Phaser {
                        kind:
                            PhaserKind::Leave
                            | PhaserKind::Keep
                            | PhaserKind::Undo
                            | PhaserKind::Pre
                            | PhaserKind::Post,
                        ..
                    } => {}
                    _ => body_main.push(stmt.clone()),
                }
            }
            // When the textually-last statement of the routine body is an ENTER
            // phaser (value-producing context), its entry-time value becomes the
            // routine's implicit return value (Raku semantics). The value must be
            // captured in the ENTER section and re-materialized at the end of the
            // body section: the ENTER section runs before `stack_base` is recorded
            // in `exec_block_scope_op`, so a value left directly on the stack there
            // would not be detected as the block result. Use the ENTER-result stack
            // (PushEnterResult / LoadEnterResult) to bridge the two sections.
            let last_is_enter = matches!(
                body.iter().rev().find(|s| !matches!(s, Stmt::SetLine(_))),
                Some(Stmt::Phaser {
                    kind: PhaserKind::Enter,
                    ..
                })
            ) && !sink_last_expr;
            for (i, enter_body) in enter_bodies.iter().enumerate() {
                let is_last_enter = i == enter_bodies.len() - 1;
                if is_last_enter && last_is_enter {
                    if enter_body.is_empty() {
                        sub_compiler.compile_expr(&Expr::Literal(Value::Nil));
                    } else {
                        for (j, inner) in enter_body.iter().enumerate() {
                            if j == enter_body.len() - 1 {
                                match inner {
                                    Stmt::Expr(expr) => sub_compiler.compile_expr(expr),
                                    _ => {
                                        sub_compiler.compile_stmt(inner);
                                        sub_compiler
                                            .compile_expr(&Expr::Literal(Value::Bool(true)));
                                    }
                                }
                            } else {
                                sub_compiler.compile_stmt(inner);
                            }
                        }
                    }
                    sub_compiler.code.emit(OpCode::PushEnterResult);
                } else {
                    for inner in enter_body {
                        sub_compiler.compile_stmt(inner);
                    }
                }
            }
            sub_compiler.code.patch_block_enter_end(idx);
            if last_is_enter {
                // The trailing ENTER provides the return value; the (earlier)
                // non-phaser body statements run in sink context.
                for stmt in &body_main {
                    sub_compiler.compile_stmt(stmt);
                }
                sub_compiler.code.emit(OpCode::LoadEnterResult);
            } else {
                Self::compile_routine_body_stmts(&mut sub_compiler, &body_main, sink_last_expr);
            }
            sub_compiler.code.patch_block_body_end(idx);
            sub_compiler.code.patch_block_keep_start(idx);
            {
                let mut prev_guard: Option<usize> = None;
                for stmt in body.iter().rev() {
                    if let Stmt::Phaser { kind, body } = stmt
                        && matches!(kind, PhaserKind::Leave | PhaserKind::Keep)
                    {
                        if let Some(pg) = prev_guard {
                            sub_compiler.code.patch_leave_guard_next(pg);
                        }
                        let guard_idx = sub_compiler.code.emit(OpCode::LeaveGuard { next: 0 });
                        for inner in body {
                            sub_compiler.compile_stmt(inner);
                        }
                        prev_guard = Some(guard_idx);
                    }
                }
                if let Some(pg) = prev_guard {
                    sub_compiler.code.patch_leave_guard_next(pg);
                }
            }
            sub_compiler.code.patch_block_undo_start(idx);
            {
                let mut prev_guard: Option<usize> = None;
                for stmt in body.iter().rev() {
                    if let Stmt::Phaser { kind, body } = stmt
                        && matches!(kind, PhaserKind::Leave | PhaserKind::Undo)
                    {
                        if let Some(pg) = prev_guard {
                            sub_compiler.code.patch_leave_guard_next(pg);
                        }
                        let guard_idx = sub_compiler.code.emit(OpCode::LeaveGuard { next: 0 });
                        for inner in body {
                            sub_compiler.compile_stmt(inner);
                        }
                        prev_guard = Some(guard_idx);
                    }
                }
                if let Some(pg) = prev_guard {
                    sub_compiler.code.patch_leave_guard_next(pg);
                }
            }
            // POST phasers (reverse order, after LEAVE)
            sub_compiler.code.patch_block_post_start(idx);
            Self::compile_post_phasers(&mut sub_compiler, body);
            sub_compiler.code.patch_loop_end(idx);
        } else if sink_last_expr {
            Self::compile_routine_body_stmts(&mut sub_compiler, body, true);
        } else {
            Self::compile_routine_body_stmts(&mut sub_compiler, body, false);
        }

        let fingerprint = crate::ast::function_body_fingerprint(params, param_defs, body);
        let key = if multi {
            let type_sig: Vec<String> = param_defs
                .iter()
                .filter(|pd| !pd.named && (!pd.slurpy || pd.name == "_capture"))
                .map(|pd| pd.type_constraint.clone().unwrap_or_default())
                .collect();
            format!(
                "{}::{}{}",
                self.current_package,
                name,
                if !type_sig.is_empty() {
                    format!("/{}:{}", arity, type_sig.join(","))
                } else {
                    format!("/{}", arity)
                }
            )
        } else {
            // Include arity and fingerprint in key to avoid collisions between
            // same-named subs with different bodies in different scopes
            format!(
                "{}::{}/{}#{:x}",
                self.current_package, name, arity, fingerprint
            )
        };

        // Named subs are always routines — mark the compiled code so
        // call_compiled_closure catches CX::Return at the right boundary.
        sub_compiler.code.is_routine = true;
        sub_compiler.code.compute_needs_env_sync();
        let mut cf = CompiledFunction {
            code: sub_compiler.code,
            params: params.to_vec(),
            param_defs: param_defs.to_vec(),
            return_type: return_type.cloned(),
            fingerprint,
            // Named subs with no params and no param_defs that don't use @_/%_ have
            // explicit empty signature :() and should reject any arguments.
            empty_sig: params.is_empty()
                && param_defs.is_empty()
                && !Self::body_uses_legacy_args(body),
            is_rw,
            is_raw,
            param_local_slots: None,
            has_inner_subs: false,
            declares_inner_routines: false,
            named_param_slots: None,
            deprecated_info,
            declared_locals: None,
            // The declaring package, matching the package component of this
            // function's `compiled_fns` key (built from `self.current_package`).
            package: self.current_package.clone(),
        };
        cf.precompute_param_local_slots();
        cf.precompute_named_param_slots();
        cf.detect_inner_subs();
        cf.compute_declared_locals();
        // Contribute this directly-nested named sub's WRITE set to the enclosing
        // scope so `compute_free_vars` can flag the locals it mutates as
        // `needs_cell_named_sub` (the VM then boxes them at their declaration site
        // for cross-call accumulation through a shared cell — see
        // docs/captured-outer-cell-sharing.md). A named sub has no runtime creation
        // op, so this compile-time pass is the only place to surface its writes.
        // In addition to scalar/whole-container name rebinds (`free_var_writes`),
        // include `@`/`%` containers the sub mutates IN PLACE (push / element
        // assign — `free_var_container_writes`), so a captured outer container
        // (e.g. a user `trait_mod:<is>` pushing to an outer `@names`) is boxed too.
        let mut nf_writes = cf.code.free_var_writes.clone();
        nf_writes.extend(cf.code.free_var_container_writes.iter().copied());
        self.code
            .named_sub_captures
            .push((nf_writes, cf.code.needs_cell_named_sub_free.clone()));
        self.compiled_functions.insert(key, cf);
    }

    fn compile_routine_body_stmts(
        sub_compiler: &mut Compiler,
        body: &[Stmt],
        sink_last_expr: bool,
    ) {
        if sink_last_expr {
            for stmt in body {
                sub_compiler.compile_stmt(stmt);
            }
            let nil_idx = sub_compiler.code.add_constant(Value::Nil);
            sub_compiler.code.emit(OpCode::LoadConst(nil_idx));
            return;
        }
        // Compile body statements; last Stmt::Expr should NOT emit Pop (implicit return)
        for (i, stmt) in body.iter().enumerate() {
            let is_last = i == body.len() - 1;
            if is_last {
                match stmt {
                    Stmt::Expr(expr) => {
                        // Tail expression becomes the routine's value (implicit
                        // return) -> a closure here escapes the frame.
                        sub_compiler.with_escape(true, |c| c.compile_expr(expr));
                        // Don't emit Pop — leave value on stack as implicit return
                        continue;
                    }
                    // Stmt::Call as last statement: convert to expression-level
                    // Expr::Call so the return value is left on the stack.
                    Stmt::Call { name, args } => {
                        let expr_args: Vec<Expr> = Self::call_args_to_expr_args(args);
                        sub_compiler.compile_expr(&Expr::Call {
                            name: *name,
                            args: expr_args,
                        });
                        continue;
                    }
                    Stmt::If {
                        cond,
                        then_branch,
                        else_branch,
                        binding_var,
                    } if binding_var.is_none() => {
                        sub_compiler.compile_if_value(cond, then_branch, else_branch);
                        continue;
                    }
                    Stmt::Block(stmts) | Stmt::SyntheticBlock(stmts) => {
                        // Bare blocks in final statement position auto-execute and
                        // produce their final value.
                        sub_compiler.compile_block_inline(stmts);
                        continue;
                    }
                    Stmt::VarDecl { name, .. } => {
                        sub_compiler.compile_stmt(stmt);
                        // VarDecl as last statement returns the variable value
                        if let Some(&slot) = sub_compiler.local_map.get(name) {
                            sub_compiler.code.emit(OpCode::GetLocal(slot));
                        } else {
                            sub_compiler.emit_nil_value();
                        }
                        continue;
                    }
                    // ENTER phaser as last statement: compile body inline
                    // so the value is left on stack as implicit return
                    Stmt::Phaser {
                        kind: crate::ast::PhaserKind::Enter,
                        body: ph_body,
                    } => {
                        sub_compiler.compile_block_inline(ph_body);
                        continue;
                    }
                    Stmt::Assign { name, .. } => {
                        sub_compiler.compile_stmt(stmt);
                        // Assignment as last statement returns the assigned value
                        if let Some(&slot) = sub_compiler.local_map.get(name) {
                            sub_compiler.code.emit(OpCode::GetLocal(slot));
                        } else {
                            let idx = sub_compiler
                                .code
                                .add_constant(Value::str(sub_compiler.qualify_variable_name(name)));
                            sub_compiler.code.emit(OpCode::GetGlobal(idx));
                        }
                        continue;
                    }
                    _ => {}
                }
            }
            sub_compiler.compile_stmt(stmt);
        }
    }

    /// Compile a closure body to a `CompiledCode` (not stored in compiled_functions).
    /// Used for Lambda, AnonSub, AnonSubParams, and BlockClosure.
    pub(crate) fn compile_closure_body(
        &mut self,
        params: &[String],
        param_defs: &[crate::ast::ParamDef],
        body: &[Stmt],
    ) -> CompiledCode {
        self.compile_closure_body_with_routine_flag(params, param_defs, body, false)
    }

    pub(crate) fn compile_routine_closure_body(
        &mut self,
        params: &[String],
        param_defs: &[crate::ast::ParamDef],
        body: &[Stmt],
    ) -> CompiledCode {
        self.compile_closure_body_with_routine_flag(params, param_defs, body, true)
    }

    fn compile_closure_body_with_routine_flag(
        &mut self,
        params: &[String],
        param_defs: &[crate::ast::ParamDef],
        body: &[Stmt],
        is_routine: bool,
    ) -> CompiledCode {
        let mut sub_compiler = Compiler::new();
        sub_compiler.is_routine = is_routine;
        // Make the names of all constants visible at this closure's definition
        // point known to the child compiler, so a `constant X` inside the body
        // that shadows an outer constant is recognised as shadowing (and thus
        // kept purely lexical, not written back to the shared package store).
        sub_compiler.outer_constant_names = self
            .constant_vars_in_scope
            .iter()
            .chain(self.outer_constant_names.iter())
            .cloned()
            .collect();
        // A closure body is lexically inside a routine if either the parent
        // already is, or the parent itself is a routine.
        sub_compiler.lexically_in_routine =
            is_routine || self.is_routine || self.lexically_in_routine;
        // `%_` / `@_` from an enclosing method stay visible inside nested
        // closures (e.g. a `do {}` block inside a `.protect: { ... }` block),
        // so carry the method context down. A nested *named sub* is not a method
        // and gets a fresh compiler via compile_sub_body, so it correctly resets.
        sub_compiler.lexically_in_method = self.lexically_in_method;
        // Propagate last_source_line so closures inside blocks that
        // lack their own SetLine can still inherit the line from the
        // enclosing statement.
        sub_compiler.last_source_line = self.last_source_line;
        // Give closures a unique package name so their state variables don't
        // collide with state variables in the enclosing code that happen to
        // share the same variable name.
        let closure_id = STATE_COUNTER.fetch_add(1, Ordering::Relaxed);
        let closure_package = format!("{}::&<closure>/{}", self.current_package, closure_id);
        // Preserve the enclosing package for $?PACKAGE resolution.
        // If the parent already has an enclosing_package, propagate it;
        // otherwise use the parent's current_package.
        sub_compiler.enclosing_package = Some(
            self.enclosing_package
                .clone()
                .unwrap_or_else(|| self.current_package.clone()),
        );
        sub_compiler.set_current_package(closure_package);
        // Pre-allocate locals for parameters
        for param in params {
            sub_compiler.alloc_local(param);
        }
        for pd in param_defs {
            if !pd.name.is_empty() {
                sub_compiler.alloc_local(&pd.name);
                if pd.sigilless {
                    sub_compiler.sigilless_locals.insert(pd.name.clone());
                }
            }
        }
        // Hoist sub declarations within the closure body
        sub_compiler.hoist_sub_decls(body, true);
        // If body contains CATCH/CONTROL, wrap in implicit try. compile_try
        // leaves the body's final-expression value on the stack, which is the
        // closure's implicit return value, so keep it (do not Pop).
        if Self::has_catch_or_control(body) {
            sub_compiler.compile_try(body, &None);
        } else if Self::has_block_enter_leave_phasers(body) {
            // Body has ENTER/LEAVE/KEEP/UNDO/PRE/POST — wrap in BlockScope
            let idx = sub_compiler.code.emit(OpCode::BlockScope {
                pre_end: 0,
                enter_end: 0,
                body_end: 0,
                keep_start: 0,
                undo_start: 0,
                post_start: 0,
                end: 0,
            });
            Self::compile_pre_phasers(&mut sub_compiler, body);
            sub_compiler.code.patch_block_pre_end(idx);
            // A trailing ENTER phaser (ignoring `SetLine` markers) provides the
            // closure's implicit return value (Raku semantics). Capture it in the
            // ENTER section and re-materialize it on the value stack at the end of
            // the body (the closure returns its value via the stack, not the topic).
            let last_is_enter = matches!(
                body.iter().rev().find(|s| !matches!(s, Stmt::SetLine(_))),
                Some(Stmt::Phaser {
                    kind: PhaserKind::Enter,
                    ..
                })
            );
            let enter_last_idx = body
                .iter()
                .rposition(|s| !matches!(s, Stmt::SetLine(_)))
                .unwrap_or(usize::MAX);
            // ENTER phasers
            for (i, stmt) in body.iter().enumerate() {
                if let Stmt::Phaser {
                    kind: PhaserKind::Enter,
                    body: ph_body,
                } = stmt
                {
                    if last_is_enter && i == enter_last_idx {
                        if ph_body.is_empty() {
                            sub_compiler.emit_nil_value();
                        } else {
                            for (j, inner) in ph_body.iter().enumerate() {
                                if j == ph_body.len() - 1 {
                                    match inner {
                                        Stmt::Expr(expr) => {
                                            sub_compiler.with_escape(true, |c| c.compile_expr(expr))
                                        }
                                        _ => {
                                            sub_compiler.compile_stmt(inner);
                                            sub_compiler
                                                .compile_expr(&Expr::Literal(Value::Bool(true)));
                                        }
                                    }
                                } else {
                                    sub_compiler.compile_stmt(inner);
                                }
                            }
                        }
                        sub_compiler.code.emit(OpCode::PushEnterResult);
                    } else {
                        for inner in ph_body {
                            sub_compiler.compile_stmt(inner);
                        }
                    }
                }
            }
            sub_compiler.code.patch_block_enter_end(idx);
            // Body (excluding phasers)
            let body_stmts: Vec<&Stmt> = body
                .iter()
                .filter(|s| {
                    !matches!(
                        s,
                        Stmt::Phaser {
                            kind: PhaserKind::Enter
                                | PhaserKind::Leave
                                | PhaserKind::Keep
                                | PhaserKind::Undo
                                | PhaserKind::Pre
                                | PhaserKind::Post,
                            ..
                        }
                    )
                })
                .collect();
            // The value-producing statement is the last non-`SetLine` statement;
            // trailing markers must not become the closure's value.
            let last_value_idx = body_stmts
                .iter()
                .rposition(|s| !matches!(s, Stmt::SetLine(_)));
            for (i, stmt) in body_stmts.iter().enumerate() {
                let is_value = !last_is_enter && Some(i) == last_value_idx;
                if is_value && let Stmt::Expr(expr) = stmt {
                    // Tail expression = implicit return -> closure escapes.
                    sub_compiler.with_escape(true, |c| c.compile_expr(expr));
                    continue;
                }
                if is_value && let Stmt::Call { name, args } = stmt {
                    let positional: Option<Vec<Expr>> = args
                        .iter()
                        .map(|arg| match arg {
                            crate::ast::CallArg::Positional(expr) => Some(expr.clone()),
                            _ => None,
                        })
                        .collect();
                    if let Some(positional_args) = positional {
                        sub_compiler.compile_expr(&Expr::Call {
                            name: *name,
                            args: positional_args,
                        });
                        continue;
                    }
                }
                sub_compiler.compile_stmt(stmt);
            }
            if last_is_enter {
                sub_compiler.code.emit(OpCode::LoadEnterResult);
            } else if last_value_idx.is_none() {
                // No value-producing statement (phaser-only closure body): the
                // implicit return is Nil.
                sub_compiler.emit_nil_value();
            }
            sub_compiler.code.patch_block_body_end(idx);
            sub_compiler.code.patch_block_keep_start(idx);
            {
                let mut prev_guard: Option<usize> = None;
                for s in body.iter().rev() {
                    if let Stmt::Phaser {
                        kind,
                        body: ph_body,
                    } = s
                        && matches!(kind, PhaserKind::Leave | PhaserKind::Keep)
                    {
                        if let Some(pg) = prev_guard {
                            sub_compiler.code.patch_leave_guard_next(pg);
                        }
                        let guard_idx = sub_compiler.code.emit(OpCode::LeaveGuard { next: 0 });
                        for inner in ph_body {
                            sub_compiler.compile_stmt(inner);
                        }
                        prev_guard = Some(guard_idx);
                    }
                }
                if let Some(pg) = prev_guard {
                    sub_compiler.code.patch_leave_guard_next(pg);
                }
            }
            sub_compiler.code.patch_block_undo_start(idx);
            {
                let mut prev_guard: Option<usize> = None;
                for s in body.iter().rev() {
                    if let Stmt::Phaser {
                        kind,
                        body: ph_body,
                    } = s
                        && matches!(kind, PhaserKind::Leave | PhaserKind::Undo)
                    {
                        if let Some(pg) = prev_guard {
                            sub_compiler.code.patch_leave_guard_next(pg);
                        }
                        let guard_idx = sub_compiler.code.emit(OpCode::LeaveGuard { next: 0 });
                        for inner in ph_body {
                            sub_compiler.compile_stmt(inner);
                        }
                        prev_guard = Some(guard_idx);
                    }
                }
                if let Some(pg) = prev_guard {
                    sub_compiler.code.patch_leave_guard_next(pg);
                }
            }
            sub_compiler.code.patch_block_post_start(idx);
            Self::compile_post_phasers(&mut sub_compiler, body);
            sub_compiler.code.patch_loop_end(idx);
        } else {
            // Compile body; last Stmt::Expr leaves value on stack (implicit return)
            for (i, stmt) in body.iter().enumerate() {
                let is_last = i == body.len() - 1;
                if is_last {
                    match stmt {
                        Stmt::Expr(expr) => {
                            // Tail expression = implicit return -> closure escapes.
                            sub_compiler.with_escape(true, |c| c.compile_expr(expr));
                            continue;
                        }
                        Stmt::Call { name, args } => {
                            let expr_args = Self::call_args_to_expr_args(args);
                            sub_compiler.compile_expr(&Expr::Call {
                                name: *name,
                                args: expr_args,
                            });
                            continue;
                        }
                        Stmt::If {
                            cond,
                            then_branch,
                            else_branch,
                            binding_var,
                        } if binding_var.is_none() => {
                            sub_compiler.compile_if_value(cond, then_branch, else_branch);
                            continue;
                        }
                        Stmt::Block(stmts) | Stmt::SyntheticBlock(stmts) => {
                            if Self::has_block_placeholders(stmts) {
                                sub_compiler.compile_stmt(&Stmt::Die(Expr::Literal(Value::str(
                                    "Implicit placeholder parameters are not available in bare nested blocks"
                                        .to_string(),
                                ))));
                            } else {
                                sub_compiler.compile_block_inline(stmts);
                            }
                            continue;
                        }
                        Stmt::VarDecl { name, .. } => {
                            sub_compiler.compile_stmt(stmt);
                            // VarDecl as last statement returns the variable value
                            if let Some(&slot) = sub_compiler.local_map.get(name) {
                                sub_compiler.code.emit(OpCode::GetLocal(slot));
                            } else {
                                sub_compiler.emit_nil_value();
                            }
                            continue;
                        }
                        Stmt::Assign { name, .. } => {
                            sub_compiler.compile_stmt(stmt);
                            // Assignment as last statement returns the assigned value
                            if let Some(&slot) = sub_compiler.local_map.get(name) {
                                sub_compiler.code.emit(OpCode::GetLocal(slot));
                            } else {
                                let idx = sub_compiler.code.add_constant(Value::str(
                                    sub_compiler.qualify_variable_name(name),
                                ));
                                sub_compiler.code.emit(OpCode::GetGlobal(idx));
                            }
                            continue;
                        }
                        _ => {}
                    }
                }
                sub_compiler.compile_stmt(stmt);
            }
        }
        // Transfer any compiled functions from the closure to the parent
        for (k, v) in sub_compiler.compiled_functions {
            self.compiled_functions.insert(k, v);
        }
        sub_compiler.code.is_routine = is_routine;
        // Use the sub_compiler's source line if a SetLine was processed
        // within the body, otherwise fall back to the parent compiler's
        // last_source_line.
        sub_compiler.code.source_line = sub_compiler.last_source_line.or(self.last_source_line);
        sub_compiler.code.compute_needs_env_sync();
        // Promote read-only plain-lexical free variables to index-based upvalues.
        // Closure-only (this path compiles anonymous closures/blocks); named subs
        // and the top-level program never get an upvalue array at runtime.
        // Sub-signature capture params (`|c(Str $x)`) bind at call time but read
        // via GetGlobal, so they look free — exclude them so they are NOT
        // upvalue-promoted (they must read the runtime-bound env value).
        let mut runtime_bound: std::collections::HashSet<crate::symbol::Symbol> =
            std::collections::HashSet::new();
        for pd in param_defs {
            let mut names: std::collections::HashSet<String> = std::collections::HashSet::new();
            crate::runtime::Interpreter::collect_sub_signature_names(&pd.sub_signature, &mut names);
            for n in names {
                let bare = n.trim_start_matches(['$', '@', '%', '&']);
                runtime_bound.insert(crate::symbol::Symbol::intern(bare));
                runtime_bound.insert(crate::symbol::Symbol::intern(&n));
            }
        }
        sub_compiler.code.compute_upvalues(&runtime_bound);
        sub_compiler.code
    }
}
