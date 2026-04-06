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
        } else if let Expr::Index { target, index } = expr {
            if let Some(name) = Self::postfix_index_name(target) {
                self.compile_expr(index);
                let name_idx = self.code.add_constant(Value::str(name));
                self.code.emit(OpCode::PostIncrementIndex(name_idx));
            } else {
                // Arbitrary expression target: assign to temp, increment via temp
                let temp_name = format!("__mutsu_tmp_inc_{}", self.code.constants.len());
                let temp_name_idx = self.code.add_constant(Value::str(temp_name.clone()));
                self.compile_expr(target);
                self.code.emit(OpCode::SetGlobal(temp_name_idx));
                self.compile_expr(index);
                self.code.emit(OpCode::PostIncrementIndex(temp_name_idx));
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
        } else if let Expr::Index { target, index } = expr {
            if let Some(name) = Self::postfix_index_name(target) {
                self.compile_expr(index);
                let name_idx = self.code.add_constant(Value::str(name));
                self.code.emit(OpCode::PostDecrementIndex(name_idx));
            } else {
                let temp_name = format!("__mutsu_tmp_dec_{}", self.code.constants.len());
                let temp_name_idx = self.code.add_constant(Value::str(temp_name.clone()));
                self.compile_expr(target);
                self.code.emit(OpCode::SetGlobal(temp_name_idx));
                self.compile_expr(index);
                self.code.emit(OpCode::PostDecrementIndex(temp_name_idx));
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
}
