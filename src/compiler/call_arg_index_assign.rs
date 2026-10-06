use super::*;

impl Compiler {
    /// `f(@a[1] = v)` / `f(%h<k><j> = v)`: an indexed assignment passed as a
    /// call argument. In Raku the assignment yields the element's own
    /// container, so an `is rw` parameter binds the caller's storage (Path::Map's
    /// `$p.value.(%!vcache{$k}{$key} = $key)` hands a constraint callback its
    /// cache slot). Compiled as an ordinary expression it leaves the stored
    /// *value* on the stack and the callee rejects it as "a value without a
    /// container".
    ///
    /// Returns the plain read `Expr::Index` of the assigned element when the
    /// assignment can be split into "perform it, then pass the element" — the
    /// same shape `f($x = v)` already takes through `Expr::Var`. The split
    /// evaluates the target chain twice, so it is taken only when that chain is
    /// made of variables, literals and zero-argument method calls (accessors).
    /// Anything else stays on the value path, where a read-only parameter works
    /// as before and an `is rw` one still reports the error.
    // Cost: O(d), d = subscript chain depth.
    pub(super) fn index_assign_arg_element(arg: &Expr) -> Option<Expr> {
        let Expr::IndexAssign {
            target,
            index,
            is_positional,
            ..
        } = arg
        else {
            return None;
        };
        if !Self::is_repeatable_subscript_part(target) || !Self::is_repeatable_subscript_part(index)
        {
            return None;
        }
        Some(Expr::Index {
            target: target.clone(),
            index: index.clone(),
            is_positional: *is_positional,
        })
    }

    /// [`Self::index_assign_arg_element`] for the callee-less `CallOn` forms
    /// (`$f(...)`, `&f(...)`), which also need a scalar assignment argument
    /// (`$f($x = v)`) split into "assign, then pass `$x`": unlike a named call,
    /// they have no per-argument `VarRef` of their own for the assignment result.
    // Cost: O(d), d = subscript chain depth.
    pub(super) fn assign_arg_element(arg: &Expr) -> Option<Expr> {
        match arg {
            Expr::AssignExpr {
                name,
                is_bind: false,
                ..
            } if !name.starts_with(['@', '%', '&', '$']) && !name.contains("::") => {
                Some(Expr::Var(name.clone()))
            }
            _ => Self::index_assign_arg_element(arg),
        }
    }

    // Cost: O(d), d = expression depth.
    fn is_repeatable_subscript_part(expr: &Expr) -> bool {
        let mut check = RepeatableSubscript { repeatable: true };
        crate::ast_visit::Visit::visit_expr(&mut check, expr);
        check.repeatable
    }
}

/// Whether evaluating a subscript chain twice is indistinguishable from once:
/// it contains only variables, literals, `self`, subscripts and zero-argument
/// method calls (accessors).
struct RepeatableSubscript {
    repeatable: bool,
}

impl<'ast> crate::ast_visit::Visit<'ast> for RepeatableSubscript {
    fn visit_expr(&mut self, expr: &'ast Expr) {
        match expr {
            Expr::Var(_)
            | Expr::ArrayVar(_)
            | Expr::HashVar(_)
            | Expr::Literal(_)
            | Expr::Index { .. } => crate::ast_visit::walk_expr(self, expr),
            Expr::BareWord(name) if name == "self" => {}
            Expr::MethodCall {
                args,
                modifier: None,
                ..
            } if args.is_empty() => crate::ast_visit::walk_expr(self, expr),
            _ => self.repeatable = false,
        }
    }
}
