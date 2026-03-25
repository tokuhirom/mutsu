use super::*;

impl Compiler {
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
            Expr::Call { args, .. } => args.iter().any(Self::expr_has_real_let_deep),
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
            Expr::Try { body, .. } => Self::has_let_deep(body),
            Expr::Call { args, .. } => args.iter().any(Self::expr_has_let_deep),
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
