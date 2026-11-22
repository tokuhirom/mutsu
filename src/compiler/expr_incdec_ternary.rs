use super::*;

impl Compiler {
    /// `(cond ?? $a !! $b)++` and `($x = EXPR)++`, and their `--` / prefix
    /// forms.
    ///
    /// A ternary yields the *container* of the branch it selects, so an
    /// increment through it updates that variable:
    /// `($test.ok ?? $!passed !! $!failed)++` (TAP's `State.handle-entry`).
    /// Desugar to `cond ?? $a++ !! $b++`, mirroring the assignment form
    /// (`(cond ?? $a !! $b) = rhs`, see `ternary_branch_assign`): only the
    /// selected branch is touched, and a nested selector on a branch is
    /// distributed the same way. Returns false when `expr` is neither form.
    // Cost: O(1) per selector level (compile time).
    pub(super) fn compile_incdec_through_ternary(
        &mut self,
        expr: &Expr,
        op: TokenKind,
        postfix: bool,
    ) -> bool {
        if let Some(name) = Self::assigned_scalar_name(expr) {
            // An assignment yields the scalar it assigned to, so
            // `($x = 6)++` / `++($x = 6)` / `($ = 1)++` run the assignment and
            // then increment that scalar (#18840), like the method-call form
            // (`($x = EXPR).push`).
            let var = Box::new(Expr::Var(name.to_string()));
            self.compile_expr(expr);
            self.code.emit(OpCode::Pop);
            self.compile_expr(&Self::incdec_of(var, &op, postfix));
            return true;
        }
        let Some(desugared) = Self::distribute_incdec(expr.peel_parens(), &op, postfix) else {
            return false;
        };
        self.compile_expr(&desugared);
        true
    }

    /// `expr++` / `++expr` (or the `--` forms) as an expression.
    // Cost: O(1) (compile time).
    fn incdec_of(expr: Box<Expr>, op: &TokenKind, postfix: bool) -> Expr {
        if postfix {
            Expr::PostfixOp {
                op: op.clone(),
                expr,
            }
        } else {
            Expr::Unary {
                op: op.clone(),
                expr,
                word: false,
            }
        }
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
            Self::incdec_of(Box::new(e.peel_parens().clone()), op, postfix)
        };
        Some(Expr::Ternary {
            cond: cond.clone(),
            then_expr: Box::new(branch(then_expr)),
            else_expr: Box::new(branch(else_expr)),
        })
    }
}
