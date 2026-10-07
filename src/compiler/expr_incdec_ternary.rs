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
    // Cost: O(1) per selector level (compile time).
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

    // Cost: O(1) (compile time).
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
            // A nested selector on a branch is distributed when this
            // wrapped branch is compiled in turn.
            let e = Box::new(e.peel_parens().clone());
            if postfix {
                Expr::PostfixOp {
                    op: op.clone(),
                    expr: e,
                }
            } else {
                Expr::Unary {
                    op: op.clone(),
                    expr: e,
                    word: false,
                }
            }
        };
        Some(Expr::Ternary {
            cond: cond.clone(),
            then_expr: Box::new(branch(then_expr)),
            else_expr: Box::new(branch(else_expr)),
        })
    }
}
