use super::*;
use crate::symbol::Symbol;

impl Compiler {
    pub(super) fn compile_expr_unary(&mut self, op: &TokenKind, expr: &Expr) {
        match op {
            TokenKind::Minus => {
                self.compile_expr(expr);
                self.code.emit(OpCode::Negate);
            }
            TokenKind::Bang => {
                self.compile_expr(expr);
                self.code.emit(OpCode::Not);
            }
            TokenKind::Question => {
                self.compile_expr(expr);
                self.code.emit(OpCode::BoolCoerce);
            }
            TokenKind::Plus => {
                self.compile_expr(expr);
                self.code.emit(OpCode::NumCoerce);
            }
            TokenKind::Tilde => {
                self.compile_expr(expr);
                self.code.emit(OpCode::StrCoerce);
            }
            TokenKind::Caret => {
                self.compile_expr(expr);
                self.code.emit(OpCode::UptoRange);
            }
            TokenKind::Ident(name) if name == "so" => {
                self.compile_expr(expr);
                self.code.emit(OpCode::BoolCoerce);
            }
            TokenKind::PlusPlus => {
                if let Some(var) = Self::temp_call_var(expr) {
                    // `++temp $c`: `temp $c` temporizes `$c` (saved for restoration
                    // at scope exit) and yields it as an lvalue, so the `++`
                    // increments the live variable. Emit the temp save, then the
                    // pre-increment on the underlying variable.
                    self.emit_temp_save(&var);
                    let name_idx = self.code.add_constant(Value::str(var));
                    self.code.emit(OpCode::PreIncrement(name_idx));
                } else if let Expr::Var(name) = expr {
                    if name.starts_with('!') && name.len() > 1 {
                        self.alloc_local(name);
                    }
                    let name_idx = self.code.add_constant(Value::str(name.clone()));
                    self.code.emit(OpCode::PreIncrement(name_idx));
                } else if let Expr::BareWord(name) = expr {
                    if self.sigilless_locals.contains(name.as_str()) {
                        let name_idx = self.code.add_constant(Value::str(name.clone()));
                        self.code.emit(OpCode::PreIncrement(name_idx));
                    } else {
                        self.compile_expr(&Expr::Call {
                            name: Symbol::intern("__mutsu_incdec_nomatch"),
                            args: vec![Expr::Literal(Value::str_from("prefix:<++>"))],
                        });
                    }
                } else if let Some(var_name) = Self::extract_vardecl_name(expr) {
                    self.compile_expr(expr);
                    self.code.emit(OpCode::Pop);
                    let name_idx = self.code.add_constant(Value::str(var_name));
                    self.code.emit(OpCode::PreIncrement(name_idx));
                } else if let Expr::Index { target, index, .. } = expr {
                    if let Some(name) = Self::postfix_index_name(target) {
                        self.compile_expr(index);
                        let name_idx = self.code.add_constant(Value::str(name));
                        self.code.emit(OpCode::PreIncrementIndex(name_idx));
                    } else {
                        // Nested index (e.g. ++$foo[0][0])
                        self.compile_nested_prefix_incdec(expr, true);
                    }
                } else {
                    self.compile_expr(&Expr::Call {
                        name: Symbol::intern("__mutsu_incdec_nomatch"),
                        args: vec![Expr::Literal(Value::str_from("prefix:<++>"))],
                    });
                }
            }
            TokenKind::MinusMinus => {
                if let Some(var) = Self::temp_call_var(expr) {
                    // `--temp $c`: temporize `$c` then pre-decrement it (see the
                    // `++temp` case above).
                    self.emit_temp_save(&var);
                    let name_idx = self.code.add_constant(Value::str(var));
                    self.code.emit(OpCode::PreDecrement(name_idx));
                } else if let Expr::Var(name) = expr {
                    if name.starts_with('!') && name.len() > 1 {
                        self.alloc_local(name);
                    }
                    let name_idx = self.code.add_constant(Value::str(name.clone()));
                    self.code.emit(OpCode::PreDecrement(name_idx));
                } else if let Expr::BareWord(name) = expr {
                    if self.sigilless_locals.contains(name.as_str()) {
                        let name_idx = self.code.add_constant(Value::str(name.clone()));
                        self.code.emit(OpCode::PreDecrement(name_idx));
                    } else {
                        self.compile_expr(&Expr::Call {
                            name: Symbol::intern("__mutsu_incdec_nomatch"),
                            args: vec![Expr::Literal(Value::str_from("prefix:<-->"))],
                        });
                    }
                } else if let Some(var_name) = Self::extract_vardecl_name(expr) {
                    self.compile_expr(expr);
                    self.code.emit(OpCode::Pop);
                    let name_idx = self.code.add_constant(Value::str(var_name));
                    self.code.emit(OpCode::PreDecrement(name_idx));
                } else if let Expr::Index { target, index, .. } = expr {
                    if let Some(name) = Self::postfix_index_name(target) {
                        self.compile_expr(index);
                        let name_idx = self.code.add_constant(Value::str(name));
                        self.code.emit(OpCode::PreDecrementIndex(name_idx));
                    } else {
                        // Nested index (e.g. --$foo[0][0])
                        self.compile_nested_prefix_incdec(expr, false);
                    }
                } else {
                    self.compile_expr(&Expr::Call {
                        name: Symbol::intern("__mutsu_incdec_nomatch"),
                        args: vec![Expr::Literal(Value::str_from("prefix:<-->"))],
                    });
                }
            }
            TokenKind::IntBitNeg => {
                self.compile_expr(expr);
                self.code.emit(OpCode::IntBitNeg);
            }
            TokenKind::BoolBitNeg => {
                self.compile_expr(expr);
                self.code.emit(OpCode::BoolBitNeg);
            }
            TokenKind::StrBitNeg => {
                self.compile_expr(expr);
                self.code.emit(OpCode::StrBitNeg);
            }
            TokenKind::Pipe => {
                self.compile_expr(expr);
                self.code.emit(OpCode::MakeSlip);
            }
            _ => {
                self.compile_expr(expr);
                let op_name = format!("prefix:<{}>", super::helpers::token_kind_to_op_name(op));
                let name_idx = self.code.add_constant(Value::str(op_name));
                self.code.emit(OpCode::CallFunc {
                    name_idx,
                    arity: 1,
                    arg_sources_idx: None,
                });
            }
        }
    }

    /// If `expr` is `temp $var` parsed as a `Call("temp", [Var])` (i.e. `temp`
    /// used as an lvalue expression rather than a statement, such as the operand
    /// of `++`/`--`), return the underlying simple variable name.
    fn temp_call_var(expr: &Expr) -> Option<String> {
        if let Expr::Call { name, args } = expr
            && name.resolve() == "temp"
            && args.len() == 1
            && let Expr::Var(var) = &args[0]
        {
            return Some(var.clone());
        }
        None
    }

    /// Emit a `temp` save for the named scalar variable: its current value is
    /// pushed onto the let-saves stack and restored at the enclosing scope's exit.
    fn emit_temp_save(&mut self, var: &str) {
        let name_idx = self.code.add_constant(Value::str(var.to_string()));
        self.code.emit(OpCode::LetSave {
            name_idx,
            index_mode: false,
            is_temp: true,
        });
    }
}
