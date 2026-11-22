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
        if let Some(desugared) = Self::element_assign_incdec(expr.peel_parens(), &op, postfix) {
            self.compile_expr(&desugared);
            return true;
        }
        let Some(desugared) = Self::distribute_incdec(expr.peel_parens(), &op, postfix) else {
            return false;
        };
        self.compile_expr(&desugared);
        true
    }

    /// `(@a[i] = v)++` / `(%h<k> = v)++` and the other forms: an element
    /// assignment yields the element's container (#18884). Desugars to
    /// `do { my $idx = i; @a[$idx] = v; @a[$idx]++ }`, so the subscript is
    /// evaluated once (`(@a[$i++] = 1)++` moves `$i` once). The target must
    /// be a plain variable, which naming twice cannot change; any other
    /// target is left to the general path.
    // Cost: O(1) (compile time).
    fn element_assign_incdec(expr: &Expr, op: &TokenKind, postfix: bool) -> Option<Expr> {
        let Expr::IndexAssign {
            target,
            index,
            value,
            is_positional,
            spelling,
        } = expr
        else {
            return None;
        };
        if !matches!(
            target.as_ref(),
            Expr::ArrayVar(_) | Expr::HashVar(_) | Expr::Var(_)
        ) {
            return None;
        }
        // A literal subscript (`%h<k>`, `@a[0]`) reads the same twice; any
        // other is bound once to a temporary, unique per compile so a nested
        // form cannot capture an outer one's.
        static NEXT_TEMP: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        let (index_expr, decl) = if matches!(index.as_ref(), Expr::Literal(_)) {
            (index.as_ref().clone(), None)
        } else {
            let id = NEXT_TEMP.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            let name = format!("__mutsu_incdec_index_{id}");
            let decl = Stmt::VarDecl {
                marks: Default::default(),
                anonymous: true,
                name: name.clone(),
                expr: index.as_ref().clone(),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                has_initializer: true,
                constant: None,
                bind: None,
                where_constraint: None,
            };
            (Expr::Var(name), Some(decl))
        };
        let assign = Expr::IndexAssign {
            target: target.clone(),
            index: Box::new(index_expr.clone()),
            value: value.clone(),
            is_positional: *is_positional,
            spelling: *spelling,
        };
        let element = Expr::Index {
            target: target.clone(),
            index: Box::new(index_expr),
            is_positional: *is_positional,
            spelling: *spelling,
        };
        let mut body: Vec<Stmt> = decl.into_iter().collect();
        body.push(Stmt::Expr(assign));
        body.push(Stmt::Expr(Self::incdec_of(Box::new(element), op, postfix)));
        Some(Expr::DoBlock {
            body,
            label: None,
            origin: crate::ast::DoBlockOrigin::SourceBlock,
        })
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
