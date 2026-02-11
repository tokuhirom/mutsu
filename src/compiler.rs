use crate::ast::{AssignOp, Expr, Stmt};
use crate::lexer::TokenKind;
use crate::opcode::{CompiledCode, OpCode};
use crate::value::Value;

pub(crate) struct Compiler {
    code: CompiledCode,
}

impl Compiler {
    pub(crate) fn new() -> Self {
        Self {
            code: CompiledCode::new(),
        }
    }

    pub(crate) fn compile(mut self, stmts: &[Stmt]) -> CompiledCode {
        for stmt in stmts {
            self.compile_stmt(stmt);
        }
        self.code
    }

    fn compile_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => {
                self.compile_expr(expr);
                self.code.emit(OpCode::Pop);
            }
            // Stmt::Block must go through the interpreter's run_block()
            // which handles ENTER/LEAVE phaser splitting.
            Stmt::Block(_) => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::InterpretStmt(idx));
            }
            Stmt::Say(exprs) => {
                for expr in exprs {
                    self.compile_expr(expr);
                }
                self.code.emit(OpCode::Say(exprs.len() as u32));
            }
            Stmt::Print(exprs) => {
                for expr in exprs {
                    self.compile_expr(expr);
                }
                self.code.emit(OpCode::Print(exprs.len() as u32));
            }
            Stmt::VarDecl { name, expr } => {
                self.compile_expr(expr);
                let name_idx = self.code.add_constant(Value::Str(name.clone()));
                self.code.emit(OpCode::SetGlobal(name_idx));
            }
            // Assign has edge cases (*PID read-only, MatchAssign, Bind)
            // that are best handled by the interpreter for now.
            Stmt::Assign { .. } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::InterpretStmt(idx));
            }
            Stmt::If {
                cond,
                then_branch,
                else_branch,
            } => {
                self.compile_expr(cond);
                // JumpIfFalse -> else
                let jump_else = self.code.emit(OpCode::JumpIfFalse(0));
                for s in then_branch {
                    self.compile_stmt(s);
                }
                if else_branch.is_empty() {
                    self.code.patch_jump(jump_else);
                } else {
                    let jump_end = self.code.emit(OpCode::Jump(0));
                    self.code.patch_jump(jump_else);
                    for s in else_branch {
                        self.compile_stmt(s);
                    }
                    self.code.patch_jump(jump_end);
                }
            }
            Stmt::Return(expr) => {
                self.compile_expr(expr);
                self.code.emit(OpCode::Return);
            }
            // Fallback for everything else
            _ => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::InterpretStmt(idx));
            }
        }
    }

    fn compile_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Literal(v) => match v {
                Value::Nil => {
                    self.code.emit(OpCode::LoadNil);
                }
                Value::Bool(true) => {
                    self.code.emit(OpCode::LoadTrue);
                }
                Value::Bool(false) => {
                    self.code.emit(OpCode::LoadFalse);
                }
                _ => {
                    let idx = self.code.add_constant(v.clone());
                    self.code.emit(OpCode::LoadConst(idx));
                }
            },
            Expr::Var(name) => {
                let name_idx = self.code.add_constant(Value::Str(name.clone()));
                self.code.emit(OpCode::GetGlobal(name_idx));
            }
            Expr::Unary { op, expr } => match op {
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
                _ => {
                    self.fallback_expr(&Expr::Unary {
                        op: op.clone(),
                        expr: expr.clone(),
                    });
                }
            },
            Expr::Binary { left, op, right } => {
                // Short-circuit operators
                match op {
                    TokenKind::AndAnd => {
                        self.compile_expr(left);
                        self.code.emit(OpCode::Dup);
                        let jump_end = self.code.emit(OpCode::JumpIfFalse(0));
                        self.code.emit(OpCode::Pop);
                        self.compile_expr(right);
                        self.code.patch_jump(jump_end);
                        return;
                    }
                    TokenKind::OrOr | TokenKind::OrWord => {
                        self.compile_expr(left);
                        self.code.emit(OpCode::Dup);
                        let jump_end = self.code.emit(OpCode::JumpIfTrue(0));
                        self.code.emit(OpCode::Pop);
                        self.compile_expr(right);
                        self.code.patch_jump(jump_end);
                        return;
                    }
                    TokenKind::SlashSlash | TokenKind::OrElse => {
                        // defined-or: if left is not nil, keep it; else evaluate right
                        self.compile_expr(left);
                        self.code.emit(OpCode::Dup);
                        let jump_end = self.code.emit(OpCode::JumpIfNotNil(0));
                        self.code.emit(OpCode::Pop);
                        self.compile_expr(right);
                        self.code.patch_jump(jump_end);
                        return;
                    }
                    TokenKind::AndThen => {
                        // andthen: if left is nil, result is Nil; else evaluate right
                        self.compile_expr(left);
                        self.code.emit(OpCode::Dup);
                        let jump_nil = self.code.emit(OpCode::JumpIfNil(0));
                        self.code.emit(OpCode::Pop);
                        self.compile_expr(right);
                        self.code.patch_jump(jump_nil);
                        return;
                    }
                    _ => {}
                }

                // Check if this is a simple binary op we can compile
                if let Some(opcode) = Self::binary_opcode(op) {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.code.emit(opcode);
                } else {
                    // Fallback for unsupported binary operators
                    self.fallback_expr(&Expr::Binary {
                        left: left.clone(),
                        op: op.clone(),
                        right: right.clone(),
                    });
                }
            }
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => {
                self.compile_expr(cond);
                let jump_else = self.code.emit(OpCode::JumpIfFalse(0));
                self.compile_expr(then_expr);
                let jump_end = self.code.emit(OpCode::Jump(0));
                self.code.patch_jump(jump_else);
                self.compile_expr(else_expr);
                self.code.patch_jump(jump_end);
            }
            Expr::ArrayLiteral(elems) => {
                for elem in elems {
                    self.compile_expr(elem);
                }
                self.code.emit(OpCode::MakeArray(elems.len() as u32));
            }
            // Fallback for all other expression types
            _ => {
                self.fallback_expr(expr);
            }
        }
    }

    fn fallback_expr(&mut self, expr: &Expr) {
        let idx = self.code.add_expr(expr.clone());
        self.code.emit(OpCode::InterpretExpr(idx));
    }

    fn binary_opcode(op: &TokenKind) -> Option<OpCode> {
        match op {
            // Arithmetic
            TokenKind::Plus => Some(OpCode::Add),
            TokenKind::Minus => Some(OpCode::Sub),
            TokenKind::Star => Some(OpCode::Mul),
            TokenKind::Slash => Some(OpCode::Div),
            TokenKind::Percent => Some(OpCode::Mod),
            TokenKind::StarStar => Some(OpCode::Pow),
            // String
            TokenKind::Tilde => Some(OpCode::Concat),
            // Numeric comparison
            TokenKind::EqEq => Some(OpCode::NumEq),
            TokenKind::BangEq => Some(OpCode::NumNe),
            TokenKind::Lt => Some(OpCode::NumLt),
            TokenKind::Lte => Some(OpCode::NumLe),
            TokenKind::Gt => Some(OpCode::NumGt),
            TokenKind::Gte => Some(OpCode::NumGe),
            // String comparison
            TokenKind::Ident(name) if name == "eq" => Some(OpCode::StrEq),
            TokenKind::Ident(name) if name == "ne" => Some(OpCode::StrNe),
            _ => None,
        }
    }
}
