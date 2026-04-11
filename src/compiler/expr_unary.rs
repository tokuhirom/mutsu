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
                if let Expr::Var(name) = expr {
                    let name_idx = self.code.add_constant(Value::str(name.clone()));
                    self.code.emit(OpCode::PreIncrement(name_idx));
                } else if let Some(var_name) = Self::extract_vardecl_name(expr) {
                    self.compile_expr(expr);
                    self.code.emit(OpCode::Pop);
                    let name_idx = self.code.add_constant(Value::str(var_name));
                    self.code.emit(OpCode::PreIncrement(name_idx));
                } else if let Expr::Index { target, index } = expr {
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
                if let Expr::Var(name) = expr {
                    let name_idx = self.code.add_constant(Value::str(name.clone()));
                    self.code.emit(OpCode::PreDecrement(name_idx));
                } else if let Some(var_name) = Self::extract_vardecl_name(expr) {
                    self.compile_expr(expr);
                    self.code.emit(OpCode::Pop);
                    let name_idx = self.code.add_constant(Value::str(var_name));
                    self.code.emit(OpCode::PreDecrement(name_idx));
                } else if let Expr::Index { target, index } = expr {
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
}
