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
        // Only a plain `@`/`%` aggregate (possibly nested): a scalar-rooted
        // chain may be a user object whose `ASSIGN-POS` / `ASSIGN-KEY` result is
        // the expression's value, which re-reading the element would discard.
        let mut root = target.as_ref();
        while let Expr::Index { target, .. } = root {
            root = target;
        }
        if !matches!(root, Expr::ArrayVar(_) | Expr::HashVar(_))
            || !Self::is_repeatable_subscript_part(target)
            || !Self::is_repeatable_subscript_part(index)
        {
            return None;
        }
        Some(Expr::Index {
            target: target.clone(),
            index: index.clone(),
            is_positional: *is_positional,
            spelling: Default::default(),
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

    /// The two container-candidate producers of a `CallOn` argument: a plain
    /// variable/accessor source and a subscript source.
    // Cost: O(1).
    pub(super) fn mark_call_on_arg(
        &mut self,
        callee: crate::opcode::RwArgCallee,
        positional: Option<u32>,
        stack_offset: u32,
        arg: &Expr,
    ) {
        self.mark_arg_as_rw_container_candidate_callee(
            callee.clone(),
            positional,
            stack_offset,
            arg,
        );
        self.mark_arg_index_as_container_candidate_callee(callee, positional, stack_offset, arg);
    }

    /// Emit the two-way compile of an assigned-element argument (see
    /// [`Self::index_assign_arg_element`]), decided at run time on the real
    /// callee: when it binds this positional to the caller's container,
    /// `split` runs (the assignment, then the element itself); every other
    /// callee gets `ordinary`, the assignment expression's own value (a native
    /// array's `@a[0] = -1` is `-1`, a user `ASSIGN-POS`'s result is its own).
    ///
    /// `stack_offset` is the number of argument values already above the
    /// callee, which only a [`crate::opcode::RwArgCallee::Code`] gate reads. That
    /// gate expects one more value on top, so a placeholder is pushed for it.
    // Cost: O(1) plus the two compiled arms.
    pub(super) fn compile_gated_assigned_arg(
        &mut self,
        arg: &Expr,
        callee: crate::opcode::RwArgCallee,
        positional: Option<u32>,
        stack_offset: u32,
        ordinary: impl FnOnce(&mut Self),
        split: impl FnOnce(&mut Self),
    ) {
        let placeholder = matches!(callee, crate::opcode::RwArgCallee::Code);
        if placeholder {
            self.code.emit(OpCode::LoadNil);
        }
        self.code.emit(OpCode::RwArgCalleeBindsContainer(Box::new(
            crate::opcode::RwArgCalleeMark {
                positional: positional.unwrap_or(crate::opcode::RWARG_POSITIONAL_UNKNOWN),
                stack_offset,
                callee,
            },
        )));
        // `JumpIfTrue` only peeks; negate and branch on the popping form.
        self.code.emit(OpCode::Not);
        let to_split = self.code.emit(OpCode::JumpIfFalse(0));
        if placeholder {
            self.code.emit(OpCode::Pop);
        }
        ordinary(self);
        let to_end = self.code.emit(OpCode::Jump(0));
        self.code.patch_jump(to_split);
        if placeholder {
            self.code.emit(OpCode::Pop);
        }
        self.compile_expr(arg);
        self.code.emit(OpCode::Pop);
        split(self);
        self.code.patch_jump(to_end);
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
