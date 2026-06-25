use super::*;

impl Compiler {
    pub(super) fn compile_do_block_expr(&mut self, body: &[Stmt], label: &Option<String>) {
        // A `do {}` block does not take a signature, so a placeholder variable
        // used directly inside it cannot be captured -> X::Placeholder::Block.
        if let Some(ph) = crate::ast::collect_unattached_placeholders(body)
            .into_iter()
            .next()
        {
            let err = Self::placeholder_scope_error("block", &ph);
            let idx = self.code.add_constant(err);
            self.code.emit(OpCode::LoadConst(idx));
            self.code.emit(OpCode::Die);
            return;
        }
        // DoBlocks from lifted CHECK phasers carry a sentinel label so we can
        // wrap them in CheckPhaserStart/CheckPhaserEnd, ensuring errors inside
        // are wrapped in X::Comp::BeginTime.
        if matches!(label, Some(l) if l == "__mutsu_check_phaser__") {
            let start_idx = self.code.emit(OpCode::CheckPhaserStart { end_ip: 0 });
            // Compile the inner DoBlock normally (without the sentinel label)
            self.compile_do_block_expr(body, &None);
            self.code.emit(OpCode::CheckPhaserEnd);
            let end_ip = self.code.ops.len() as u32;
            if let OpCode::CheckPhaserStart { end_ip: ref mut e } = self.code.ops[start_idx] {
                *e = end_ip;
            }
            return;
        }
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
            self.compile_phaser_block_scope(body, true);
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
        self.compile_do_if_expr_bound(cond, then_branch, else_branch, &None);
    }

    /// Like `compile_do_if_expr`, but honours a per-branch topic binding
    /// (`if EXPR -> $v { ... }`). When `binding_var` is `Some`, the condition
    /// value is bound to `$v` for the `then` branch — desugared exactly like the
    /// statement-form `if` (`{ my $v = EXPR; if $v { ... } }`) — so a value-mode
    /// `if`/`elsif` with a topic binding does not leave `$v` (or `$_`) reading
    /// the enclosing topic. Inner `elsif`s thread their own binding through the
    /// recursion.
    pub(super) fn compile_do_if_expr_bound(
        &mut self,
        cond: &Expr,
        then_branch: &[Stmt],
        else_branch: &[Stmt],
        binding_var: &Option<String>,
    ) {
        if let Some(var_name) = binding_var {
            let bare_name = var_name.trim_start_matches('$').to_string();
            let var_decl = Stmt::VarDecl {
                name: bare_name.clone(),
                expr: cond.clone(),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: vec![],
                custom_traits: Vec::new(),
                where_constraint: None,
            };
            self.compile_stmt(&var_decl);
            self.compile_expr(&Expr::Var(bare_name));
        } else {
            self.compile_expr(cond);
        }
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
                binding_var: inner_binding,
            } = &else_branch[0]
            {
                self.compile_do_if_expr_bound(inner_cond, inner_then, inner_else, inner_binding);
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
        // No params_def available on the lazy-for path; pass empty so all params
        // are treated as required (the pre-feature behavior). See the TODO at the
        // `inner_for` construction below.
        let bind_stmts = Self::build_for_bind_stmts(param, param_def, param_idx, params, &[]);
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
            explicit_zero_params: false,
            multi_param_names: Vec::new(),
            loop_var_wraps_element: Self::for_iterable_wraps_pair(iterable),
            values_mode: Self::for_iterable_is_values_alias(iterable),
        });
        self.compile_collected_loop_body(&loop_body);
        self.code.patch_loop_end(loop_idx);
        // Balance the ForLoop opcode's deferred param-restore push (see the
        // Stmt::For compile path). Required even though this collected form has
        // no post phasers, so the push/pop stay balanced.
        if param
            .as_ref()
            .is_some_and(|p| !p.starts_with('@') && !p.starts_with('%'))
        {
            self.code.emit(OpCode::RestoreForParam);
        }
    }

    /// Compile `lazy for` expression: lower to `gather { for @items -> $param { take do { body } } }`.
    /// This defers execution of the body until the resulting Seq is consumed.
    pub(super) fn compile_lazy_for_expr(
        &mut self,
        iterable: &Expr,
        param: &Option<String>,
        param_def: &Option<crate::ast::ParamDef>,
        params: &[String],
        body: &[Stmt],
        label: &Option<String>,
    ) {
        use crate::ast::{Expr as AExpr, Stmt as AStmt};
        // Build `take <last_expr>` body: replace the last expression with `take <expr>`
        // and wrap the body in a for loop inside a gather block.
        let take_body: Vec<AStmt> = {
            let mut stmts = body.to_vec();
            // Replace last expression statement with `take(expr)`
            let last_idx = stmts.iter().rposition(|s| !matches!(s, AStmt::SetLine(_)));
            if let Some(idx) = last_idx {
                if let AStmt::Expr(expr) = stmts[idx].clone() {
                    stmts[idx] = AStmt::Take(expr, false);
                } else {
                    // Wrap non-expr last stmt in Take(Nil) since we need a take
                    stmts.push(AStmt::Take(AExpr::Literal(crate::value::Value::Nil), false));
                }
            } else {
                stmts.push(AStmt::Take(AExpr::Literal(crate::value::Value::Nil), false));
            }
            stmts
        };
        // Build inner for loop with the take body
        let inner_for = AStmt::For {
            iterable: iterable.clone(),
            param: param.clone(),
            param_def: Box::new(param_def.clone()),
            params: params.to_vec(),
            // TODO: thread params_def through compile_lazy_for_expr so a
            // `lazy for ... -> $a, $b = 7 { }` gets the same arity/default
            // handling; empty here just preserves the pre-feature behavior.
            params_def: Vec::new(),
            body: take_body,
            label: label.clone(),
            mode: crate::ast::ForMode::Normal,
            rw_block: false,
            explicit_zero_params: false,
        };
        // Build gather block wrapping the for loop
        let gather_body = vec![inner_for];
        let gather_expr = AExpr::Gather(gather_body);
        self.compile_expr(&gather_expr);
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
        let last = stmts.len().saturating_sub(1);
        for (i, stmt) in stmts.iter().enumerate() {
            match stmt {
                // A `given` in final position is handled by compile_block_inline
                // (it leaves the block value on the stack); only a non-final
                // `given` is unsupported here.
                Stmt::Given { .. } if i != last => return false,
                Stmt::If {
                    then_branch,
                    else_branch,
                    ..
                } if !Self::do_if_branch_supported(then_branch)
                    || !Self::do_if_branch_supported(else_branch) =>
                {
                    return false;
                }
                _ => {}
            }
        }
        true
    }
}
