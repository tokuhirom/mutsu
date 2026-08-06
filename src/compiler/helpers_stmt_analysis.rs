use super::*;

impl Compiler {
    /// Whether `stmts` declares a `state` variable at its OWN statement level.
    ///
    /// Deliberately shallow: a `state` inside a nested loop, `if` branch or bare
    /// block belongs to that construct's clone and is reset at ITS entry (see
    /// `OpCode::ResetStateLocals` / `reset_state_locals_in_range`), so descending
    /// would only make this block emit a redundant reset. Drives whether an
    /// inline nested block needs a `ResetStateLocals` at all, so the common
    /// state-free `if` keeps its current bytecode.
    pub(super) fn stmts_declare_state(stmts: &[Stmt]) -> bool {
        stmts.iter().any(|s| match s {
            Stmt::VarDecl { is_state, expr, .. } => *is_state || Self::expr_has_state_decl(expr),
            Stmt::Expr(expr) => Self::expr_has_state_decl(expr),
            _ => false,
        })
    }

    /// Emit a [`OpCode::ResetStateLocals`] for an inline nested block body that
    /// declares `state` at its own level, returning the index to patch once the
    /// body is compiled. `None` (nothing emitted) otherwise.
    pub(super) fn emit_nested_block_state_reset(&mut self, stmts: &[Stmt]) -> Option<usize> {
        Self::stmts_declare_state(stmts)
            .then(|| self.code.emit(OpCode::ResetStateLocals { body_end: 0 }))
    }

    /// Whether a loop body consists of exactly one source `{ ... }` block
    /// (the statement-modifier form `{ ... } for @xs` parses that way, with
    /// only `SetLine` markers beside it). That block IS the loop's body — the
    /// loop statement clones it once and its iterations share the clone, so
    /// its `state` must persist across iterations (raku: `{ state $n = 0;
    /// $n = $n + 1; say $n } for 1..3` prints 1 2 3). The compile sites set
    /// [`Compiler::suppress_loop_block_state_reset`] from this so the block's
    /// per-execution `ResetStateLocals` is skipped; the loop-entry reset
    /// already restarts the state when the loop STATEMENT re-executes.
    pub(super) fn loop_body_is_sole_block(body: &[Stmt]) -> bool {
        let mut semantic = body.iter().filter(|s| !matches!(s, Stmt::SetLine(_)));
        matches!(
            (semantic.next(), semantic.next()),
            (Some(Stmt::Block(_)), None)
        )
    }

    /// [`Self::emit_nested_block_state_reset`] for an `if`/`unless` branch: a
    /// postfix statement MODIFIER introduces no block, so the statement it gates
    /// belongs to the enclosing block and its `state` must not restart
    /// (`sub f { state $n = 0 if 1; ++$n }` counts across calls).
    pub(super) fn emit_branch_state_reset(
        &mut self,
        stmts: &[Stmt],
        is_statement_modifier: bool,
    ) -> Option<usize> {
        (!is_statement_modifier)
            .then(|| self.emit_nested_block_state_reset(stmts))
            .flatten()
    }

    /// Patch the [`OpCode::ResetStateLocals`] emitted by
    /// [`Self::emit_nested_block_state_reset`] to end at the current position.
    pub(super) fn patch_nested_block_state_reset(&mut self, idx: Option<usize>) {
        if let Some(idx) = idx {
            self.code.patch_reset_state_locals_end(idx);
        }
    }

    /// Check if a statement list contains `let` or `temp` statements (not inside sub/lambda bodies).
    pub(super) fn has_let_deep(stmts: &[Stmt]) -> bool {
        for s in stmts {
            match s {
                Stmt::Let { .. } | Stmt::TempMethodAssign { .. } => return true,
                Stmt::Block(inner) => {
                    if Self::has_let_deep(inner) {
                        return true;
                    }
                }
                Stmt::If {
                    then_branch,
                    else_branch,
                    ..
                } => {
                    if Self::has_let_deep(then_branch) || Self::has_let_deep(else_branch) {
                        return true;
                    }
                }
                Stmt::Expr(expr) => {
                    if Self::expr_has_let_deep(expr) {
                        return true;
                    }
                }
                Stmt::Call { args, .. } => {
                    for arg in args {
                        if let crate::ast::CallArg::Positional(expr) = arg
                            && Self::expr_has_let_deep(expr)
                        {
                            return true;
                        }
                    }
                }
                Stmt::Say(exprs) | Stmt::Print(exprs) | Stmt::Note(exprs) => {
                    for expr in exprs {
                        if Self::expr_has_let_deep(expr) {
                            return true;
                        }
                    }
                }
                _ => {}
            }
        }
        false
    }

    /// Check if a statement list contains actual `let` (not `temp`) statements.
    /// Used to decide whether the block's return value matters for save/restore.
    pub(super) fn has_real_let_deep(stmts: &[Stmt]) -> bool {
        for s in stmts {
            match s {
                Stmt::Let { is_temp: false, .. } => return true,
                Stmt::Block(inner) => {
                    if Self::has_real_let_deep(inner) {
                        return true;
                    }
                }
                Stmt::If {
                    then_branch,
                    else_branch,
                    ..
                } => {
                    if Self::has_real_let_deep(then_branch) || Self::has_real_let_deep(else_branch)
                    {
                        return true;
                    }
                }
                Stmt::Expr(expr) => {
                    if Self::expr_has_real_let_deep(expr) {
                        return true;
                    }
                }
                Stmt::Call { args, .. } => {
                    for arg in args {
                        if let crate::ast::CallArg::Positional(expr) = arg
                            && Self::expr_has_real_let_deep(expr)
                        {
                            return true;
                        }
                    }
                }
                _ => {}
            }
        }
        false
    }

    /// Check if an expression contains actual `let` (not `temp`) deep inside.
    fn expr_has_real_let_deep(expr: &Expr) -> bool {
        match expr {
            Expr::DoBlock { body, .. } => Self::has_real_let_deep(body),
            Expr::Try { body, .. } => Self::has_real_let_deep(body),
            Expr::Call { args, .. } | Expr::UserRoutineCall { args, .. } => {
                args.iter().any(Self::expr_has_real_let_deep)
            }
            Expr::MethodCall { args, target, .. }
            | Expr::DynamicMethodCall { args, target, .. }
            | Expr::HyperMethodCall { args, target, .. }
            | Expr::HyperMethodCallDynamic { args, target, .. } => {
                Self::expr_has_real_let_deep(target)
                    || args.iter().any(Self::expr_has_real_let_deep)
            }
            _ => false,
        }
    }

    /// Check if a block directly contains a `use`/`no` statement (non-recursive).
    pub(super) fn has_use_stmt(stmts: &[Stmt]) -> bool {
        stmts
            .iter()
            .any(|s| matches!(s, Stmt::Use { .. } | Stmt::Import { .. } | Stmt::No { .. }))
    }

    pub(super) fn expr_has_let_deep(expr: &Expr) -> bool {
        match expr {
            Expr::DoBlock { body, .. } => Self::has_let_deep(body),
            Expr::DoStmt(stmt) => Self::has_let_deep(&[*stmt.clone()]),
            Expr::Try { body, .. } => Self::has_let_deep(body),
            Expr::Grouped(inner) => Self::expr_has_let_deep(inner),
            Expr::IndexAssign {
                target,
                index,
                value,
                ..
            } => {
                Self::expr_has_let_deep(target)
                    || Self::expr_has_let_deep(index)
                    || Self::expr_has_let_deep(value)
            }
            // Detect `undefine temp $var`: Call("undefine", [Call("temp", ...)])
            // The compiler expands this to LetSave + assign, so the enclosing block
            // needs LetBlock wrapping for proper save/restore.
            Expr::Call { name, args, .. } => {
                if name.resolve() == "undefine"
                    && args.len() == 1
                    && matches!(
                        &args[0],
                        Expr::Call { name: inner, .. }
                            if inner.resolve() == "temp"
                    )
                {
                    return true;
                }
                args.iter().any(Self::expr_has_let_deep)
            }
            Expr::MethodCall { args, target, .. }
            | Expr::DynamicMethodCall { args, target, .. }
            | Expr::HyperMethodCall { args, target, .. }
            | Expr::HyperMethodCallDynamic { args, target, .. } => {
                Self::expr_has_let_deep(target) || args.iter().any(Self::expr_has_let_deep)
            }
            _ => false,
        }
    }

    pub(super) fn next_tmp_name(&mut self, prefix: &str) -> String {
        let name = format!("${}{}", prefix, self.tmp_counter);
        self.tmp_counter += 1;
        name
    }
}
