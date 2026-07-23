use super::*;

impl Compiler {
    pub(super) fn compile_do_block_expr(&mut self, body: &[Stmt], label: &Option<String>) {
        // A `do {}` block does not take a signature, so a placeholder variable
        // used directly inside it cannot be captured -> X::Placeholder::Block.
        // Exception: inside a method, the legacy argument variables `%_` / `@_`
        // refer to the method's implicit `*%_` / `*@_` slurpy and are valid here.
        if let Some(ph) = crate::ast::collect_unattached_placeholders(body)
            .into_iter()
            .find(|ph| !(self.lexically_in_method && (ph == "%_" || ph == "@_")))
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
                isolate_decls_idx: u32::MAX,
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
            isolate_decls_idx: u32::MAX,
        });
        self.compile_block_inline(body);
        self.code.patch_body_end(idx);
    }

    pub(super) fn compile_do_block_expr_scoped(&mut self, body: &[Stmt], label: &Option<String>) {
        let idx = self.code.emit(OpCode::DoBlockExpr {
            body_end: 0,
            label: label.clone(),
            scope_isolate: true,
            isolate_decls_idx: u32::MAX,
        });
        // Record every `my`/`state` declaration compiled in the body (including
        // ones nested in expressions like `(state $a)++` and ones shadowing an
        // outer same-name) so the scope-isolating exit reverts exactly those
        // while letting OUTER-variable mutations persist. A nested closure
        // compiles in a fresh `Compiler`, so it never contributes here.
        self.block_decl_tracker.push(Vec::new());
        self.compile_block_inline(body);
        let mut decls = self.block_decl_tracker.pop().unwrap_or_default();
        // Hashes are intentionally NOT isolated: a `my %h` (e.g.
        // `:into(my %h := :{})`) must survive into the enclosing scope.
        decls.retain(|n| !n.starts_with('%') && !n.starts_with('&'));
        if !decls.is_empty() {
            let decls_idx = self.code.add_constant(Value::array(
                decls.into_iter().map(Value::str).collect::<Vec<_>>(),
            ));
            if let OpCode::DoBlockExpr {
                isolate_decls_idx, ..
            } = &mut self.code.ops[idx]
            {
                *isolate_decls_idx = decls_idx;
            }
        }
        self.code.patch_body_end(idx);
    }

    /// Compile an `if`/`elsif` chain in value (expression) position, honouring an
    /// optional per-branch topic binding
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
        // A pointy `if EXPR -> $_ { }` binds a FRESH lexical `$_` (like `for ->
        // $_`), so its topic must NOT flow back to an enclosing `given $x`'s source
        // variable. `EnterPointyTopic` saves + clears `topic_source_var` for the
        // branch; `ExitPointyTopic` (at the end) restores it and the outer `$_`.
        let pointy_topic_scope = binding_var
            .as_deref()
            .is_some_and(|v| v.trim_start_matches('$') == "_");
        if pointy_topic_scope {
            self.code.emit(OpCode::EnterPointyTopic);
        }
        // A bare `do if EXPR { ... $^a ... }` / `(if EXPR { ... })` block receives
        // the condition value as `@_` and as a scalar placeholder (like `-> $a`),
        // so `do if 9 { $^a + 1 }` is 10. Mirrors `compile_if_value`.
        let needs_at_underscore = binding_var.is_none() && Self::body_uses_legacy_args(then_branch);
        let cond_placeholder: Option<String> = if binding_var.is_none() {
            crate::ast::collect_placeholders_shallow(then_branch)
                .into_iter()
                .find(|n| n.starts_with('^'))
        } else {
            None
        };
        let needs_cond_value = needs_at_underscore || cond_placeholder.is_some();
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
        if needs_cond_value {
            self.code.emit(OpCode::Dup);
        }
        let jump_else = self.code.emit(OpCode::JumpIfFalse(0));
        if needs_at_underscore {
            self.code.emit(OpCode::FlattenSlurpy);
            self.emit_set_named_var("@_");
        } else if let Some(ph) = &cond_placeholder {
            self.emit_set_named_var(ph);
        }
        self.compile_block_inline(then_branch);
        let jump_end = self.code.emit(OpCode::Jump(0));
        self.code.patch_jump(jump_else);
        if needs_cond_value {
            self.code.emit(OpCode::Pop);
        }

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
        if pointy_topic_scope {
            self.code.emit(OpCode::ExitPointyTopic);
        }
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
                        && matches!(&items[0], Expr::Literal(lit) if lit.is_nil())
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
        // A `for`-loop handles `is rw` write-back through its own
        // `TagContainerRef` mechanism, so the iterable's synthetic single-element
        // wrap (`for $a` -> `ArrayLiteral([$a])`) must NOT also box `$a` into an
        // aliasing `ContainerRef` cell — that shared cell would be written back
        // into `$a`, creating a self-referential cycle that deadlocks on the next
        // read (`try for $a -> $v is rw { $v++ }`). Mirror the statement-form
        // guard in `stmt.rs` for the for-as-expression path (a `for` used as the
        // tail value of `try`/`do`/a sub body).
        let saved_suppress = self.suppress_list_var_alias;
        self.suppress_list_var_alias = true;
        self.compile_expr(&normalized_iterable);
        self.suppress_list_var_alias = saved_suppress;
        if let Some(source_name) = Self::for_iterable_source_name(iterable) {
            let source_slot = self.local_map.get(source_name.as_str()).copied();
            let source_idx = self.code.add_constant(Value::str(source_name));
            self.code
                .emit(OpCode::TagContainerRef(source_idx, source_slot));
        }
        let param_local = param
            .as_ref()
            .and_then(|p| self.local_map.get(p.as_str()).copied());
        let source_var_names = Self::for_iterable_var_names(iterable);
        let source_var_locals = self.for_source_var_locals(&source_var_names);
        let loop_idx = self
            .code
            .emit(OpCode::ForLoop(Box::new(crate::opcode::ForLoopSpec {
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
                source_var_locals,
                autothread_junctions: false,
                explicit_zero_params: false,
                multi_param_names: Vec::new(),
                loop_var_wraps_element: Self::for_iterable_wraps_pair(iterable),
                values_mode: Self::for_iterable_is_values_alias(iterable),
                single_array_source: Self::for_single_array_source(iterable),
                single_array_source_local: self
                    .for_single_array_source_local(&Self::for_single_array_source(iterable)),
                body_declares_routines: Self::stmts_declare_routines(&loop_body),
            })));
        self.hoist_sub_decls(&loop_body, true);
        // A `for` body is its own Raku call frame (see `callframe_block_depth`).
        self.callframe_block_depth += 1;
        self.compile_collected_loop_body(&loop_body);
        self.callframe_block_depth -= 1;
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
        let take_body = Self::wrap_loop_body_last_in_take(body);
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
        // Build gather block wrapping the for loop, then mark it `.lazy` so the
        // body does not run until the resulting Seq is consumed (`lazy for`
        // semantics — S04 for.t "Lazy for loop does not execute until asked").
        let gather_body = vec![inner_for];
        let gather_expr = AExpr::Gather(gather_body);
        let lazy_expr = AExpr::MethodCall {
            target: Box::new(gather_expr),
            name: crate::symbol::Symbol::intern("lazy"),
            args: Vec::new(),
            modifier: None,
            quoted: false,
        };
        self.compile_expr(&lazy_expr);
    }

    /// Rewrite a loop body so its per-iteration value is `take`n: the last
    /// expression statement becomes `take <expr>`; a value-bearing `if`/`given`
    /// last statement is taken through `do` (so a false `if`-modifier yields
    /// `Empty`, which `take` slips away); any other shape appends `take Nil`.
    /// The loop's KEEP/UNDO result capture sees through the trailing `Take`
    /// (see `expand_loop_phasers`). Shared by the `lazy for` lowering and the
    /// `while`/`loop` expression forms.
    fn wrap_loop_body_last_in_take(body: &[Stmt]) -> Vec<Stmt> {
        use crate::ast::{Expr as AExpr, Stmt as AStmt};
        let mut stmts = body.to_vec();
        let last_idx = stmts.iter().rposition(|s| !matches!(s, AStmt::SetLine(_)));
        if let Some(idx) = last_idx {
            match stmts[idx].clone() {
                AStmt::Expr(expr) => stmts[idx] = AStmt::Take(expr, false),
                s @ (AStmt::If { .. } | AStmt::Given { .. }) => {
                    stmts[idx] = AStmt::Take(AExpr::DoStmt(Box::new(s)), false);
                }
                _ => {
                    stmts.push(AStmt::Take(AExpr::Literal(crate::value::Value::NIL), false));
                }
            }
        } else {
            stmts.push(AStmt::Take(AExpr::Literal(crate::value::Value::NIL), false));
        }
        stmts
    }

    /// Compile `do while` / `do until` (and parenthesized `(while ...)`)
    /// expression: lower to `gather { while COND { take do { body } } }` so
    /// the result is a lazy Seq pulled on demand, matching raku —
    /// `(while $++ < 2 { 42.say; 43 }).map: *.say` interleaves 42/43, and
    /// the whole loop only runs to completion when the Seq is reified.
    pub(super) fn compile_do_while_expr(
        &mut self,
        cond: &Expr,
        body: &[Stmt],
        label: &Option<String>,
    ) {
        use crate::ast::{Expr as AExpr, Stmt as AStmt};
        let inner = AStmt::While {
            cond: cond.clone(),
            body: Self::wrap_loop_body_last_in_take(body),
            label: label.clone(),
        };
        let gather_expr = AExpr::Gather(vec![inner]);
        self.compile_expr(&gather_expr);
    }

    /// Compile `do loop (...) { ... }` / `(loop { ... })` expression: lower to
    /// `gather { loop (...) { take do { body } } }` — a lazy Seq, so an
    /// infinite `(loop { 42.say })[2]` pulls exactly three iterations.
    /// The C-style init runs inside the gather, deferred until first pull.
    pub(super) fn compile_do_loop_expr(
        &mut self,
        init: &Option<Box<Stmt>>,
        cond: &Option<Expr>,
        step: &Option<Expr>,
        body: &[Stmt],
        label: &Option<String>,
    ) {
        use crate::ast::{Expr as AExpr, Stmt as AStmt};
        let inner = AStmt::Loop {
            init: init.clone(),
            cond: cond.clone(),
            step: step.clone(),
            body: Self::wrap_loop_body_last_in_take(body),
            repeat: false,
            label: label.clone(),
        };
        let gather_expr = AExpr::Gather(vec![inner]);
        self.compile_expr(&gather_expr);
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
