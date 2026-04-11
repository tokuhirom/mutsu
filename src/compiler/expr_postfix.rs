use super::*;
use crate::symbol::Symbol;

impl Compiler {
    /// Compile postfix ++ on variable/index/method target.
    pub(super) fn compile_expr_postfix_inc(&mut self, expr: &Expr) {
        if let Expr::Var(name) = expr {
            let name_idx = self.code.add_constant(Value::str(name.clone()));
            self.code.emit(OpCode::PostIncrement(name_idx));
        } else if let Some(var_name) = Self::extract_vardecl_name(expr) {
            // state/my declarator in expression position: `state $x++`, `my $x.++`
            self.compile_expr(expr);
            self.code.emit(OpCode::Pop);
            let name_idx = self.code.add_constant(Value::str(var_name));
            self.code.emit(OpCode::PostIncrement(name_idx));
        } else if let Expr::Index { target, index, .. } = expr {
            if let Some(name) = Self::postfix_index_name(target) {
                self.compile_expr(index);
                let name_idx = self.code.add_constant(Value::str(name));
                self.code.emit(OpCode::PostIncrementIndex(name_idx));
            } else {
                // Nested index (e.g. $foo[0][0]++): read old value, increment,
                // write back via IndexAssign, and return old value.
                self.compile_nested_postfix_incdec(expr, true);
            }
        } else if let Expr::MethodCall {
            target,
            name,
            args,
            modifier: _,
            quoted: _,
        } = expr
        {
            // Extract the variable name from Var, ArrayVar, or HashVar targets
            let target_var_name = match target.as_ref() {
                Expr::Var(name) => Some(name.clone()),
                Expr::ArrayVar(name) => Some(format!("@{}", name)),
                Expr::HashVar(name) => Some(format!("%{}", name)),
                _ => None,
            };
            if let Some(target_var) = target_var_name {
                let tmp_value_name =
                    format!("__mutsu_tmp_method_inc_{}", self.code.constants.len());
                let tmp_result_name = format!(
                    "__mutsu_tmp_method_inc_result_{}",
                    self.code.constants.len()
                );
                let tmp_value_idx = self.code.add_constant(Value::str(tmp_value_name.clone()));
                let tmp_result_idx = self.code.add_constant(Value::str(tmp_result_name.clone()));
                self.compile_expr(expr);
                self.code.emit(OpCode::SetGlobal(tmp_value_idx));
                self.code.emit(OpCode::PostIncrement(tmp_value_idx));
                self.code.emit(OpCode::SetGlobal(tmp_result_idx));
                let assign_expr = Expr::Call {
                    name: Symbol::intern("__mutsu_assign_method_lvalue"),
                    args: vec![
                        Expr::Var(target_var.clone()),
                        Expr::Literal(Value::str(name.resolve())),
                        Expr::ArrayLiteral(args.clone()),
                        Expr::Var(tmp_value_name),
                        Expr::Literal(Value::str(target_var.clone())),
                    ],
                };
                self.compile_expr(&assign_expr);
                self.code.emit(OpCode::Pop);
                self.compile_expr(&Expr::Var(tmp_result_name));
            } else {
                self.compile_expr(&Expr::Call {
                    name: Symbol::intern("__mutsu_incdec_nomatch"),
                    args: vec![Expr::Literal(Value::str_from("postfix:<++>"))],
                });
            }
        } else {
            self.compile_expr(&Expr::Call {
                name: Symbol::intern("__mutsu_incdec_nomatch"),
                args: vec![Expr::Literal(Value::str_from("postfix:<++>"))],
            });
        }
    }

    /// Compile postfix -- on variable/index/method target.
    pub(super) fn compile_expr_postfix_dec(&mut self, expr: &Expr) {
        if let Expr::Var(name) = expr {
            let name_idx = self.code.add_constant(Value::str(name.clone()));
            self.code.emit(OpCode::PostDecrement(name_idx));
        } else if let Some(var_name) = Self::extract_vardecl_name(expr) {
            self.compile_expr(expr);
            self.code.emit(OpCode::Pop);
            let name_idx = self.code.add_constant(Value::str(var_name));
            self.code.emit(OpCode::PostDecrement(name_idx));
        } else if let Expr::Index { target, index, .. } = expr {
            if let Some(name) = Self::postfix_index_name(target) {
                self.compile_expr(index);
                let name_idx = self.code.add_constant(Value::str(name));
                self.code.emit(OpCode::PostDecrementIndex(name_idx));
            } else {
                // Nested index (e.g. $foo[0][0]--): read old value, decrement,
                // write back via IndexAssign, and return old value.
                self.compile_nested_postfix_incdec(expr, false);
            }
        } else if let Expr::MethodCall {
            target,
            name,
            args,
            modifier: _,
            quoted: _,
        } = expr
        {
            // Extract the variable name from Var, ArrayVar, or HashVar targets
            let target_var_name = match target.as_ref() {
                Expr::Var(name) => Some(name.clone()),
                Expr::ArrayVar(name) => Some(format!("@{}", name)),
                Expr::HashVar(name) => Some(format!("%{}", name)),
                _ => None,
            };
            if let Some(target_var) = target_var_name {
                let tmp_value_name =
                    format!("__mutsu_tmp_method_dec_{}", self.code.constants.len());
                let tmp_result_name = format!(
                    "__mutsu_tmp_method_dec_result_{}",
                    self.code.constants.len()
                );
                let tmp_value_idx = self.code.add_constant(Value::str(tmp_value_name.clone()));
                let tmp_result_idx = self.code.add_constant(Value::str(tmp_result_name.clone()));
                self.compile_expr(expr);
                self.code.emit(OpCode::SetGlobal(tmp_value_idx));
                self.code.emit(OpCode::PostDecrement(tmp_value_idx));
                self.code.emit(OpCode::SetGlobal(tmp_result_idx));
                let assign_expr = Expr::Call {
                    name: Symbol::intern("__mutsu_assign_method_lvalue"),
                    args: vec![
                        Expr::Var(target_var.clone()),
                        Expr::Literal(Value::str(name.resolve())),
                        Expr::ArrayLiteral(args.clone()),
                        Expr::Var(tmp_value_name),
                        Expr::Literal(Value::str(target_var.clone())),
                    ],
                };
                self.compile_expr(&assign_expr);
                self.code.emit(OpCode::Pop);
                self.compile_expr(&Expr::Var(tmp_result_name));
            } else {
                self.compile_expr(&Expr::Call {
                    name: Symbol::intern("__mutsu_incdec_nomatch"),
                    args: vec![Expr::Literal(Value::str_from("postfix:<-->"))],
                });
            }
        } else {
            self.compile_expr(&Expr::Call {
                name: Symbol::intern("__mutsu_incdec_nomatch"),
                args: vec![Expr::Literal(Value::str_from("postfix:<-->"))],
            });
        }
    }

    /// Compile postfix ++/-- on a nested index expression (e.g. `$foo[0][0]++`).
    /// `expr` is the full Index expression (the operand of PostfixOp).
    /// `increment` is true for ++, false for --.
    ///
    /// Strategy:
    /// 1. Read old value into tmp_val
    /// 2. PostIncrement/PostDecrement on tmp_val (returns old value, stores new)
    /// 3. Save old value to tmp_old
    /// 4. Write back tmp_val (which now has new value) via IndexAssign
    /// 5. Return tmp_old (the old value before increment)
    fn compile_nested_postfix_incdec(&mut self, expr: &Expr, increment: bool) {
        if let Expr::Index { target, index, .. } = expr {
            let tmp_val = format!("__mutsu_nested_incdec_val_{}", self.code.constants.len());
            let tmp_val_idx = self.code.add_constant(Value::str(tmp_val.clone()));
            let tmp_old = format!("__mutsu_nested_incdec_old_{}", self.code.constants.len());
            let tmp_old_idx = self.code.add_constant(Value::str(tmp_old.clone()));

            // 1. Read current value and store in tmp_val
            self.compile_expr(expr);
            self.code.emit(OpCode::SetGlobal(tmp_val_idx));
            self.code.emit(OpCode::Pop);

            // 2. PostIncrement/PostDecrement on tmp_val:
            //    - pushes old value on stack
            //    - sets tmp_val = old +/- 1
            if increment {
                self.code.emit(OpCode::PostIncrement(tmp_val_idx));
            } else {
                self.code.emit(OpCode::PostDecrement(tmp_val_idx));
            }
            // Stack now has old value; tmp_val has new value
            self.code.emit(OpCode::SetGlobal(tmp_old_idx));
            self.code.emit(OpCode::Pop);

            // 3. Write back the new value (tmp_val) via IndexAssign
            let assign_expr = Expr::IndexAssign {
                target: target.clone(),
                index: index.clone(),
                value: Box::new(Expr::Var(tmp_val)),
            };
            self.compile_expr(&assign_expr);
            self.code.emit(OpCode::Pop);

            // 4. Push the old value as the result of the post-increment expression
            self.code.emit(OpCode::GetGlobal(tmp_old_idx));
        }
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
        if let Expr::Index { target, index, .. } = expr {
            let tmp_val = format!("__mutsu_nested_preincdec_val_{}", self.code.constants.len());
            let tmp_val_idx = self.code.add_constant(Value::str(tmp_val.clone()));

            // 1. Read current value and store in tmp_val
            self.compile_expr(expr);
            self.code.emit(OpCode::SetGlobal(tmp_val_idx));
            self.code.emit(OpCode::Pop);

            // 2. PreIncrement/PreDecrement on tmp_val:
            //    - modifies tmp_val in place
            //    - pushes new value on stack
            if increment {
                self.code.emit(OpCode::PreIncrement(tmp_val_idx));
            } else {
                self.code.emit(OpCode::PreDecrement(tmp_val_idx));
            }
            // Stack now has new value; tmp_val also has new value
            // Save new value, we'll push it back at the end
            let tmp_new = format!("__mutsu_nested_preincdec_new_{}", self.code.constants.len());
            let tmp_new_idx = self.code.add_constant(Value::str(tmp_new.clone()));
            self.code.emit(OpCode::SetGlobal(tmp_new_idx));
            self.code.emit(OpCode::Pop);

            // 3. Write back the new value via IndexAssign
            let assign_expr = Expr::IndexAssign {
                target: target.clone(),
                index: index.clone(),
                value: Box::new(Expr::Var(tmp_val)),
            };
            self.compile_expr(&assign_expr);
            self.code.emit(OpCode::Pop);

            // 4. Push the new value as the result
            self.code.emit(OpCode::GetGlobal(tmp_new_idx));
        }
    }
}
