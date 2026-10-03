use super::*;

impl Compiler {
    /// `(cond ?? $a !! $b)++` and its `--` / prefix forms.
    ///
    /// A ternary yields the *container* of the branch it selects, so an
    /// increment through it updates that variable:
    /// `($test.ok ?? $!passed !! $!failed)++` (TAP's `State.handle-entry`).
    /// Desugar to `cond ?? $a++ !! $b++`, mirroring the assignment form
    /// (`(cond ?? $a !! $b) = rhs`, see `ternary_branch_assign`): only the
    /// selected branch is touched, and a nested selector on a branch is
    /// distributed the same way. Returns false when `expr` is no ternary.
    // Cost: O(n), n = ternary nesting depth (compile time).
    pub(super) fn compile_incdec_through_ternary(
        &mut self,
        expr: &Expr,
        op: TokenKind,
        postfix: bool,
    ) -> bool {
        let Some(desugared) = Self::distribute_incdec(expr.peel_parens(), &op, postfix) else {
            return false;
        };
        self.compile_expr(&desugared);
        true
    }

    // Cost: O(n), n = ternary nesting depth (compile time).
    fn distribute_incdec(expr: &Expr, op: &TokenKind, postfix: bool) -> Option<Expr> {
        let Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } = expr
        else {
            return None;
        };
        let branch = |e: &Expr| {
            let e = e.peel_parens();
            Self::distribute_incdec(e, op, postfix).unwrap_or_else(|| {
                if postfix {
                    Expr::PostfixOp {
                        op: op.clone(),
                        expr: Box::new(e.clone()),
                    }
                } else {
                    Expr::Unary {
                        op: op.clone(),
                        expr: Box::new(e.clone()),
                    }
                }
            })
        };
        Some(Expr::Ternary {
            cond: cond.clone(),
            then_expr: Box::new(branch(then_expr)),
            else_expr: Box::new(branch(else_expr)),
        })
    }
}
