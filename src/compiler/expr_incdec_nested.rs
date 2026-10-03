use super::*;

/// The expression at the bottom of an element chain, looking through both
/// subscripts and parentheses: `f()` for `f()[0]<k>`, the declaration for
/// `(constant w = [1, 2])[0]`.
// Cost: O(d), d = subscript and paren depth.
fn elem_chain_root(expr: &Expr) -> &Expr {
    let mut expr = expr;
    loop {
        match expr {
            Expr::Index { target, .. } | Expr::Grouped(target) => expr = target,
            other => return other,
        }
    }
}

/// Whether parentheses appear anywhere along `expr`'s element chain.
// Cost: O(d), d = subscript and paren depth.
fn elem_chain_has_parens(expr: &Expr) -> bool {
    let mut expr = expr;
    loop {
        match expr {
            Expr::Index { target, .. } => expr = target,
            Expr::Grouped(_) => return true,
            _ => return false,
        }
    }
}

/// `expr`'s element chain with its root (see [`elem_chain_root`]) replaced by
/// `root`; the parentheses around the old root go with it.
// Cost: O(d), d = subscript and paren depth.
fn with_elem_chain_root(expr: &Expr, root: &Expr) -> Expr {
    let mut subscripts = Vec::new();
    let mut expr = expr;
    loop {
        match expr {
            Expr::Index {
                target,
                index,
                is_positional,
            } => {
                subscripts.push((index, *is_positional));
                expr = target;
            }
            Expr::Grouped(inner) => expr = inner,
            _ => break,
        }
    }
    subscripts
        .into_iter()
        .rev()
        .fold(root.clone(), |target, (index, is_positional)| Expr::Index {
            target: Box::new(target),
            index: index.clone(),
            is_positional,
        })
}

impl Compiler {
    /// `++`/`--` on an element whose chain is rooted in something other than
    /// a variable or a method call: a call (`f()[0]++`) or a declaration
    /// (`(constant w = [1, 2])[0]++`).
    ///
    /// The nested increment reads the element and then writes it back with an
    /// `IndexAssign`, so it compiles the element's container twice. For a
    /// variable that is only two lookups, but any other root ran twice: a call
    /// had its side effects doubled, and a `constant` declaration was
    /// redeclared, which is a compile-time error (#10582). Evaluate the root
    /// once instead, binding it to a temporary, and increment the same chain
    /// rooted at that temporary. Binding (not assigning) keeps the root's own
    /// container, so the write lands in it.
    ///
    /// `wrap` rebuilds the increment around the rewritten element. Returns
    /// `false`, emitting nothing, when the root is a variable that no
    /// parentheses wrap.
    // Cost: O(d) compile time, d = subscript and paren depth.
    pub(super) fn compile_incdec_through_bound_root(
        &mut self,
        expr: &Expr,
        wrap: impl FnOnce(Expr) -> Expr,
    ) -> bool {
        let root = elem_chain_root(expr);
        // A method root (`$o.h<k>++`) keeps the two-evaluation path: its
        // `IndexAssign` writes back through the method-lvalue machinery, which
        // a write through a bound temporary bypasses -- a typed-key hash
        // attribute then drops later writes made through the accessor (#10803).
        if matches!(root, Expr::MethodCall { .. }) {
            return false;
        }
        if root.container_var_key().is_some() {
            // A variable root needs no temporary, but parentheses around it
            // (`(%h)<a><b>++`) hid it from the named-variable paths, and the
            // write-back then went nowhere. Drop them and compile the plain
            // chain.
            if !elem_chain_has_parens(expr) {
                return false;
            }
            self.compile_expr(&wrap(with_elem_chain_root(expr, root)));
            return true;
        }
        let tmp = format!("__mutsu_incdec_root_{}", self.code.constants.len());
        let bind_decl = Stmt::VarDecl {
            name: tmp.clone(),
            expr: root.clone(),
            type_constraint: None,
            is_state: false,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: vec![("__scalar_bind".to_string(), None)],
            where_constraint: None,
        };
        let incdec = wrap(with_elem_chain_root(expr, &Expr::Var(tmp)));
        self.compile_expr(&Expr::desugar_block(vec![
            Stmt::MarkBind,
            bind_decl,
            Stmt::Expr(incdec),
        ]));
        true
    }

    /// Compile prefix ++/-- on a nested index expression (e.g. `++$foo[0][0]`).
    /// `expr` is the full Index expression (the operand of UnaryOp).
    /// `increment` is true for ++, false for --.
    ///
    /// Strategy:
    /// 1. Read old value into tmp_val
    /// 2. PreIncrement/PreDecrement on tmp_val (returns new value, stores new)
    /// 3. Write back tmp_val via IndexAssign
    /// 4. Return the new value
    pub(super) fn compile_nested_prefix_incdec(&mut self, expr: &Expr, increment: bool) {
        let op = if increment {
            TokenKind::PlusPlus
        } else {
            TokenKind::MinusMinus
        };
        if self.compile_incdec_through_bound_root(expr, |elem| Expr::Unary {
            op,
            expr: Box::new(elem),
        }) {
            return;
        }
        if let Expr::Index {
            target,
            index,
            is_positional,
            ..
        } = expr
        {
            // Evaluate a non-trivial subscript once (`++@$a[$++]`), since both
            // the read and the write-back use it.
            if !matches!(index.as_ref(), Expr::Literal(_) | Expr::Var(_)) {
                let tmp_key = format!("__mutsu_nested_preincdec_key_{}", self.code.constants.len());
                let tmp_key_idx = self.code.add_constant(Value::str(tmp_key.clone()));
                self.compile_expr(index);
                self.code.emit(OpCode::SetGlobal(tmp_key_idx));
                let hoisted = Expr::Index {
                    target: target.clone(),
                    index: Box::new(Expr::Var(tmp_key)),
                    is_positional: *is_positional,
                };
                return self.compile_nested_prefix_incdec_hoisted(&hoisted, increment);
            }
            self.compile_nested_prefix_incdec_hoisted(expr, increment);
        }
    }

    fn compile_nested_prefix_incdec_hoisted(&mut self, expr: &Expr, increment: bool) {
        if let Expr::Index {
            target,
            index,
            is_positional,
            ..
        } = expr
        {
            let tmp_val = format!("__mutsu_nested_preincdec_val_{}", self.code.constants.len());
            let tmp_val_idx = self.code.add_constant(Value::str(tmp_val.clone()));

            // 1. Read current value and store in tmp_val
            self.compile_expr(expr);
            self.code.emit(OpCode::SetGlobal(tmp_val_idx));

            // 2. PreIncrement/PreDecrement on tmp_val:
            //    - modifies tmp_val in place
            //    - pushes new value on stack
            if increment {
                self.code.emit(OpCode::PreIncrement(tmp_val_idx, None));
            } else {
                self.code.emit(OpCode::PreDecrement(tmp_val_idx, None));
            }
            // Stack now has new value; tmp_val also has new value
            // Save new value, we'll push it back at the end
            let tmp_new = format!("__mutsu_nested_preincdec_new_{}", self.code.constants.len());
            let tmp_new_idx = self.code.add_constant(Value::str(tmp_new));
            self.code.emit(OpCode::SetGlobal(tmp_new_idx));

            // 3. Write back the new value via IndexAssign (preserve subscript kind
            //    so `%h<a><b>` autovivifies a Hash, `@a[0][1]` an Array).
            let assign_expr = Expr::IndexAssign {
                target: target.clone(),
                index: index.clone(),
                value: Box::new(Expr::Var(tmp_val)),
                is_positional: *is_positional,
            };
            self.compile_expr(&assign_expr);
            self.code.emit(OpCode::Pop);

            // 4. Push the new value as the result
            self.code.emit(OpCode::GetGlobal(tmp_new_idx));
        }
    }
}
