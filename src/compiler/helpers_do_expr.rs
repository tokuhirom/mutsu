use super::*;

impl Compiler {
    pub(super) fn compile_do_block_expr(&mut self, body: &[Stmt], label: &Option<String>) {
        // If the do block contains CATCH/CONTROL, compile as try so exceptions are handled.
        if Self::has_catch_or_control(body) {
            self.compile_try(body, &None);
            return;
        }
        // If the do block contains ENTER/LEAVE/KEEP/UNDO phasers, wrap in
        // DoBlockExpr + BlockScope so phaser semantics are preserved.
        if Self::has_block_enter_leave_phasers(body) {
            let do_idx = self.code.emit(OpCode::DoBlockExpr {
                body_end: 0,
                label: label.clone(),
                scope_isolate: false,
            });
            let saved = self.push_dynamic_scope_lexical();
            let idx = self.code.emit(OpCode::BlockScope {
                pre_end: 0,
                enter_end: 0,
                body_end: 0,
                keep_start: 0,
                undo_start: 0,
                post_start: 0,
                end: 0,
            });
            // PRE phasers
            Self::compile_pre_phasers(self, body);
            self.code.patch_block_pre_end(idx);
            // ENTER phasers
            for s in body {
                if let Stmt::Phaser {
                    kind: PhaserKind::Enter,
                    body: ph_body,
                } = s
                {
                    for inner in ph_body {
                        self.compile_stmt(inner);
                    }
                }
            }
            self.code.patch_block_enter_end(idx);
            // Body (filter out phasers)
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
            for (i, s) in body_stmts.iter().enumerate() {
                let is_last = i == body_stmts.len() - 1;
                if is_last {
                    self.compile_last_stmt_as_topic(s);
                } else {
                    self.compile_stmt(s);
                }
            }
            self.code.patch_block_body_end(idx);
            // KEEP phasers
            self.code.patch_block_keep_start(idx);
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
                            self.code.patch_leave_guard_next(pg);
                        }
                        let guard_idx = self.code.emit(OpCode::LeaveGuard { next: 0 });
                        for inner in ph_body {
                            self.compile_stmt(inner);
                        }
                        prev_guard = Some(guard_idx);
                    }
                }
                if let Some(pg) = prev_guard {
                    self.code.patch_leave_guard_next(pg);
                }
            }
            // UNDO phasers
            self.code.patch_block_undo_start(idx);
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
                            self.code.patch_leave_guard_next(pg);
                        }
                        let guard_idx = self.code.emit(OpCode::LeaveGuard { next: 0 });
                        for inner in ph_body {
                            self.compile_stmt(inner);
                        }
                        prev_guard = Some(guard_idx);
                    }
                }
                if let Some(pg) = prev_guard {
                    self.code.patch_leave_guard_next(pg);
                }
            }
            // POST phasers
            self.code.patch_block_post_start(idx);
            Self::compile_post_phasers(self, body);
            self.code.patch_loop_end(idx);
            self.pop_dynamic_scope_lexical(saved);
            self.code.patch_body_end(do_idx);
            return;
        }
        let idx = self.code.emit(OpCode::DoBlockExpr {
            body_end: 0,
            label: label.clone(),
            scope_isolate: false,
        });
        self.compile_block_inline(body);
        self.code.patch_body_end(idx);
    }

    pub(super) fn compile_do_block_expr_scoped(&mut self, body: &[Stmt], label: &Option<String>) {
        let idx = self.code.emit(OpCode::DoBlockExpr {
            body_end: 0,
            label: label.clone(),
            scope_isolate: true,
        });
        self.compile_block_inline(body);
        self.code.patch_body_end(idx);
    }

    pub(super) fn compile_do_if_expr(
        &mut self,
        cond: &Expr,
        then_branch: &[Stmt],
        else_branch: &[Stmt],
    ) {
        self.compile_expr(cond);
        let jump_else = self.code.emit(OpCode::JumpIfFalse(0));
        self.compile_block_inline(then_branch);
        let jump_end = self.code.emit(OpCode::Jump(0));
        self.code.patch_jump(jump_else);

        if else_branch.is_empty() {
            let empty_idx = self.code.add_constant(Value::slip(vec![]));
            self.code.emit(OpCode::LoadConst(empty_idx));
        } else if else_branch.len() == 1 {
            if let Stmt::If {
                cond: inner_cond,
                then_branch: inner_then,
                else_branch: inner_else,
                ..
            } = &else_branch[0]
            {
                self.compile_do_if_expr(inner_cond, inner_then, inner_else);
            } else {
                self.compile_block_inline(else_branch);
            }
        } else {
            self.compile_block_inline(else_branch);
        }
        self.code.patch_jump(jump_end);
    }

    fn compile_collected_loop_body(&mut self, body: &[Stmt]) {
        self.compile_stmts_value(body);
    }

    /// Compile `do for` expression: like a for loop but collects each iteration result.
    pub(super) fn compile_do_for_expr(
        &mut self,
        iterable: &Expr,
        param: &Option<String>,
        param_def: &Option<crate::ast::ParamDef>,
        params: &[String],
        body: &[Stmt],
        label: &Option<String>,
    ) {
        // Parser currently lowers labeled `do { ... }` / labeled bare blocks into
        // a dummy single-iteration `for Nil` with a label. Preserve block semantics
        // here so control flow like `LABEL.leave(...)` returns the block value.
        if param.is_none()
            && params.is_empty()
            && matches!(
                iterable,
                Expr::ArrayLiteral(items)
                    if items.len() == 1
                        && matches!(items[0], Expr::Literal(Value::Nil))
            )
        {
            self.compile_do_block_expr(body, label);
            return;
        }

        let (_pre_stmts, mut loop_body, _post_stmts) =
            self.expand_loop_phasers(body, label.as_deref());
        let param_idx = param
            .as_ref()
            .map(|p| self.code.add_constant(Value::str(p.clone())));
        let bind_stmts = Self::build_for_bind_stmts(param, param_def, param_idx, params);
        if !bind_stmts.is_empty() {
            let mut merged = bind_stmts;
            merged.extend(loop_body);
            loop_body = merged;
        }
        let has_rw = param_def
            .as_ref()
            .is_some_and(|def| def.traits.iter().any(|t| t == "rw"));
        let arity = if !params.is_empty() {
            params.len() as u32
        } else {
            1
        };
        let normalized_iterable = self.normalize_for_iterable(iterable);
        self.compile_expr(&normalized_iterable);
        if let Some(source_name) = Self::for_iterable_source_name(iterable) {
            let source_idx = self.code.add_constant(Value::str(source_name));
            self.code.emit(OpCode::TagContainerRef(source_idx));
        }
        let param_local = param
            .as_ref()
            .and_then(|p| self.local_map.get(p.as_str()).copied());
        let source_var_names = Self::for_iterable_var_names(iterable);
        let loop_idx = self.code.emit(OpCode::ForLoop {
            param_idx,
            param_local,
            body_end: 0,
            label: label.clone(),
            arity,
            collect: true,
            restore_topic: true,
            threaded: false,
            is_rw: has_rw,
            do_writeback: has_rw,
            rw_param_names: Vec::new(),
            kv_mode: false,
            source_var_names,
            autothread_junctions: false,
        });
        self.compile_collected_loop_body(&loop_body);
        self.code.patch_loop_end(loop_idx);
    }

    /// Compile `do while` / `do until` expression: collect each iteration value.
    pub(super) fn compile_do_while_expr(
        &mut self,
        cond: &Expr,
        body: &[Stmt],
        label: &Option<String>,
    ) {
        let (pre_stmts, loop_body, post_stmts) = self.expand_loop_phasers(body, label.as_deref());
        for stmt in &pre_stmts {
            self.compile_stmt(stmt);
        }
        let loop_idx = self.code.emit(OpCode::WhileLoop {
            cond_end: 0,
            body_end: 0,
            label: label.clone(),
            collect: true,
            isolate_topic: Self::body_mutates_topic(&loop_body),
        });
        self.compile_expr(cond);
        self.code.patch_while_cond_end(loop_idx);
        self.compile_collected_loop_body(&loop_body);
        self.code.patch_loop_end(loop_idx);
        for stmt in &post_stmts {
            self.compile_stmt(stmt);
        }
    }

    /// Compile `do loop (...) { ... }` expression: collect each iteration value.
    pub(super) fn compile_do_loop_expr(
        &mut self,
        init: &Option<Box<Stmt>>,
        cond: &Option<Expr>,
        step: &Option<Expr>,
        body: &[Stmt],
        label: &Option<String>,
    ) {
        let (pre_stmts, loop_body, post_stmts) = self.expand_loop_phasers(body, label.as_deref());
        if let Some(init_stmt) = init {
            self.compile_stmt(init_stmt);
        }
        for stmt in &pre_stmts {
            self.compile_stmt(stmt);
        }
        let loop_idx = self.code.emit(OpCode::CStyleLoop {
            cond_end: 0,
            step_start: 0,
            body_end: 0,
            label: label.clone(),
            collect: true,
        });
        if let Some(cond_expr) = cond {
            self.compile_expr(cond_expr);
        } else {
            self.code.emit(OpCode::LoadTrue);
        }
        self.code.patch_cstyle_cond_end(loop_idx);
        self.compile_collected_loop_body(&loop_body);
        self.code.patch_cstyle_step_start(loop_idx);
        if let Some(step_expr) = step {
            self.compile_expr(step_expr);
            self.code.emit(OpCode::Pop);
        }
        self.code.patch_loop_end(loop_idx);
        for stmt in &post_stmts {
            self.compile_stmt(stmt);
        }
    }

    pub(super) fn do_if_branch_supported(stmts: &[Stmt]) -> bool {
        if Self::has_phasers(stmts) {
            return false;
        }
        for stmt in stmts {
            match stmt {
                Stmt::Given { .. } => return false,
                Stmt::If {
                    then_branch,
                    else_branch,
                    ..
                } => {
                    if !Self::do_if_branch_supported(then_branch)
                        || !Self::do_if_branch_supported(else_branch)
                    {
                        return false;
                    }
                }
                _ => {}
            }
        }
        true
    }
}
