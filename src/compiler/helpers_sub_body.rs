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
        // If sub body contains CATCH/CONTROL, wrap in implicit try
        if Self::has_catch_or_control(body) {
            sub_compiler.compile_try(body, &None);
            sub_compiler.code.emit(OpCode::Pop);
            if sink_last_expr {
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
            // Compile ENTER phaser bodies. If body_main is empty and
            // this is a value-producing context (not sink), compile
            // the last ENTER body to leave its value on the stack.
            let enter_is_return = body_main.is_empty() && !sink_last_expr;
            for (i, enter_body) in enter_bodies.iter().enumerate() {
                let is_last_enter = i == enter_bodies.len() - 1;
                if is_last_enter && enter_is_return {
                    sub_compiler.compile_block_inline(enter_body);
                } else {
                    for inner in enter_body {
                        sub_compiler.compile_stmt(inner);
                    }
                }
            }
            sub_compiler.code.patch_block_enter_end(idx);
            Self::compile_routine_body_stmts(&mut sub_compiler, &body_main, sink_last_expr);
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
        let cf = CompiledFunction {
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
        };
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
                        sub_compiler.compile_expr(expr);
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
            }
        }
        // Hoist sub declarations within the closure body
        sub_compiler.hoist_sub_decls(body, true);
        // If body contains CATCH/CONTROL, wrap in implicit try
        if Self::has_catch_or_control(body) {
            sub_compiler.compile_try(body, &None);
            sub_compiler.code.emit(OpCode::Pop);
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
            // ENTER phasers
            for stmt in body.iter() {
                if let Stmt::Phaser {
                    kind: PhaserKind::Enter,
                    body: ph_body,
                } = stmt
                {
                    for inner in ph_body {
                        sub_compiler.compile_stmt(inner);
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
            for (i, stmt) in body_stmts.iter().enumerate() {
                let is_last = i == body_stmts.len() - 1;
                if is_last && let Stmt::Expr(expr) = stmt {
                    sub_compiler.compile_expr(expr);
                    sub_compiler.code.emit(OpCode::SetTopic);
                    continue;
                }
                if is_last && let Stmt::Call { name, args } = stmt {
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
                        sub_compiler.code.emit(OpCode::SetTopic);
                        continue;
                    }
                }
                sub_compiler.compile_stmt(stmt);
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
                            sub_compiler.compile_expr(expr);
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
        sub_compiler.code
    }
}
