use super::*;

impl Compiler {
    /// Emit a post-tested loop, preserving the raw condition for its body.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn compile_repeat_loop(
        &mut self,
        init: &Option<Box<Stmt>>,
        cond: &Option<Expr>,
        step: &Option<Expr>,
        body: &[Stmt],
        label: &Option<String>,
        is_until: bool,
    ) {
        let (pre_stmts, loop_body, post_stmts) =
            self.expand_loop_phasers(body, label.as_deref(), false);
        if let Some(init_stmt) = init {
            self.compile_stmt(init_stmt);
        }
        for s in &pre_stmts {
            self.compile_stmt(s);
        }
        // ADR-0048 D4: a `repeat {} while/until` body is invoked with
        // `Mu` on the first pass — the condition has not run yet — and
        // with the raw condition value on every pass after that
        // (`ArgSupply::ConditionAfterFirstPass`). Seed the `Mu` ahead
        // of the loop; the per-iteration bind goes in the condition
        // region below, which is evaluated between passes.
        let binds_cond_placeholder =
            cond.is_some() && Self::inlined_body_binds_supplied_value(&loop_body);
        let condition_param = match init.as_deref() {
            Some(Stmt::VarDecl { name, .. }) => Some(name.as_str()),
            _ => None,
        };
        if binds_cond_placeholder {
            self.emit_repeat_first_pass_mu_seed(&loop_body);
        }
        // Layout: [RepeatLoop] [body..] [cond..]
        let loop_idx = self.code.emit(OpCode::RepeatLoop {
            cond_end: 0,
            body_end: 0,
            label: label.clone(),
        });
        // The arity failure is raised when the block is INVOKED, so it
        // belongs at the head of the body region.
        self.emit_inlined_body_placeholder_arity_die(
            &loop_body,
            ArgSupply::ConditionAfterFirstPass,
        );
        // Compile body. The parser inlines the `repeat { ... }` block's
        // statements directly into `body`, so a sole `{ ... }` here is
        // a NESTED bare block that re-clones per iteration — its
        // `state` restarts (raku: 1 1 1), no reset suppression.
        self.compile_scope_restored_loop_body(&loop_body, body, false);
        self.code.patch_repeat_cond_end(loop_idx);
        // Compile condition (or push True if none)
        if let Some(cond_expr) = cond {
            if binds_cond_placeholder || condition_param.is_some() {
                // `repeat {} until COND` is parsed as a negated `while`
                // condition; raku supplies the WRITTEN condition's
                // value (`repeat { $^c } until $i++ > 1` yields `Mu`,
                // `False`, `False`), so peel the synthetic `!` off the
                // copy that is bound and re-apply it to the copy the
                // loop tests.
                self.compile_condition_expr(Self::loop_supplied_condition(
                    cond_expr, is_until,
                ));
                self.code.emit(OpCode::Dup);
                if let Some(name) = condition_param {
                    self.emit_set_named_var(name);
                } else {
                    self.emit_inlined_body_placeholder_bind_value(
                        &loop_body,
                        ArgSupply::ConditionAfterFirstPass,
                    );
                }
                if is_until {
                    self.code.emit(OpCode::Not);
                }
            } else {
                self.compile_condition_expr(cond_expr);
            }
        } else {
            self.code.emit(OpCode::LoadTrue);
        }
        // Compile step (if any)
        if let Some(step_expr) = step {
            self.compile_expr(step_expr);
            self.code.emit(OpCode::Pop);
        }
        self.code.patch_loop_end(loop_idx);
        for s in &post_stmts {
            self.compile_stmt(s);
        }
    }
}
