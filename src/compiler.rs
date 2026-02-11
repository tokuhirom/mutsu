use crate::ast::{AssignOp, CallArg, Expr, PhaserKind, Stmt};
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
            Stmt::Block(stmts) => {
                if Self::has_phasers(stmts) {
                    // Fall back for blocks with phasers
                    let idx = self.code.add_stmt(stmt.clone());
                    self.code.emit(OpCode::InterpretStmt(idx));
                } else {
                    for s in stmts {
                        self.compile_stmt(s);
                    }
                }
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
            Stmt::Assign {
                name,
                expr,
                op: AssignOp::Assign,
            } if name != "*PID" => {
                self.compile_expr(expr);
                let name_idx = self.code.add_constant(Value::Str(name.clone()));
                self.code.emit(OpCode::SetGlobal(name_idx));
            }
            Stmt::If {
                cond,
                then_branch,
                else_branch,
            } => {
                self.compile_expr(cond);
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
            Stmt::While { cond, body, label } if !Self::has_phasers(body) => {
                let loop_idx = self.code.emit(OpCode::WhileLoop {
                    cond_end: 0,
                    body_end: 0,
                    label: label.clone(),
                });
                self.compile_expr(cond);
                self.code.patch_while_cond_end(loop_idx);
                for s in body {
                    self.compile_stmt(s);
                }
                self.code.patch_loop_end(loop_idx);
            }
            Stmt::For {
                iterable,
                param,
                body,
                label,
            } if !Self::has_phasers(body) => {
                self.compile_expr(iterable);
                let param_idx = param
                    .as_ref()
                    .map(|p| self.code.add_constant(Value::Str(p.clone())));
                let loop_idx = self.code.emit(OpCode::ForLoop {
                    param_idx,
                    body_end: 0,
                    label: label.clone(),
                });
                for s in body {
                    self.compile_stmt(s);
                }
                self.code.patch_loop_end(loop_idx);
            }
            // C-style loop (non-repeat, no phasers)
            Stmt::Loop {
                init,
                cond,
                step,
                body,
                repeat,
                label,
            } if !*repeat && !Self::has_phasers(body) => {
                // Compile init statement (if any) before the loop opcode
                if let Some(init_stmt) = init {
                    self.compile_stmt(init_stmt);
                }
                // Layout: [CStyleLoop] [cond..] [body..] [step..]
                let loop_idx = self.code.emit(OpCode::CStyleLoop {
                    cond_end: 0,
                    step_start: 0,
                    body_end: 0,
                    label: label.clone(),
                });
                // Compile condition (or push True if none)
                if let Some(cond_expr) = cond {
                    self.compile_expr(cond_expr);
                } else {
                    self.code.emit(OpCode::LoadTrue);
                }
                self.code.patch_cstyle_cond_end(loop_idx);
                // Compile body
                for s in body {
                    self.compile_stmt(s);
                }
                self.code.patch_cstyle_step_start(loop_idx);
                // Compile step (if any)
                if let Some(step_expr) = step {
                    self.compile_expr(step_expr);
                    self.code.emit(OpCode::Pop);
                }
                self.code.patch_loop_end(loop_idx);
            }
            // Statement-level call: compile positional args only.
            // Fall back if named args or Block/AnonSub args exist (exec_call
            // inspects raw expressions for throws-like, lives-ok, etc.).
            Stmt::Call { name, args }
                if args.iter().all(|a| match a {
                    CallArg::Positional(expr) => !Self::needs_raw_expr(expr),
                    _ => false,
                }) =>
            {
                let arity = args.len() as u32;
                for arg in args {
                    if let CallArg::Positional(expr) = arg {
                        self.compile_expr(expr);
                    }
                }
                let name_idx = self.code.add_constant(Value::Str(name.clone()));
                self.code.emit(OpCode::ExecCall { name_idx, arity });
            }
            // Loop control
            Stmt::Last(label) => {
                self.code.emit(OpCode::Last(label.clone()));
            }
            Stmt::Next(label) => {
                self.code.emit(OpCode::Next(label.clone()));
            }
            Stmt::Redo => {
                self.code.emit(OpCode::Redo);
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
                        self.compile_expr(left);
                        self.code.emit(OpCode::Dup);
                        let jump_end = self.code.emit(OpCode::JumpIfNotNil(0));
                        self.code.emit(OpCode::Pop);
                        self.compile_expr(right);
                        self.code.patch_jump(jump_end);
                        return;
                    }
                    TokenKind::AndThen => {
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

                if let Some(opcode) = Self::binary_opcode(op) {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.code.emit(opcode);
                } else {
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
            // Expression-level function call
            Expr::Call { name, args } => {
                let arity = args.len() as u32;
                for arg in args {
                    self.compile_expr(arg);
                }
                let name_idx = self.code.add_constant(Value::Str(name.clone()));
                self.code.emit(OpCode::CallFunc { name_idx, arity });
            }
            // Method call: fall back if target is a variable (method may mutate via
            // update_instance_target, which needs the Expr name for writeback).
            Expr::MethodCall { target, name, args }
                if !Self::is_mutable_target(target) =>
            {
                self.compile_expr(target);
                let arity = args.len() as u32;
                for arg in args {
                    self.compile_expr(arg);
                }
                let name_idx = self.code.add_constant(Value::Str(name.clone()));
                self.code.emit(OpCode::CallMethod { name_idx, arity });
            }
            // Indexing
            Expr::Index { target, index } => {
                self.compile_expr(target);
                self.compile_expr(index);
                self.code.emit(OpCode::Index);
            }
            // String interpolation
            Expr::StringInterpolation(parts) => {
                let n = parts.len() as u32;
                for part in parts {
                    self.compile_expr(part);
                }
                self.code.emit(OpCode::StringConcat(n));
            }
            // Postfix ++ on variable
            Expr::PostfixOp { op, expr } if matches!(op, TokenKind::PlusPlus) => {
                if let Expr::Var(name) = expr.as_ref() {
                    let name_idx = self.code.add_constant(Value::Str(name.clone()));
                    self.code.emit(OpCode::PostIncrement(name_idx));
                } else {
                    self.fallback_expr(&Expr::PostfixOp {
                        op: op.clone(),
                        expr: expr.clone(),
                    });
                }
            }
            // Postfix -- on variable
            Expr::PostfixOp { op, expr } if matches!(op, TokenKind::MinusMinus) => {
                if let Expr::Var(name) = expr.as_ref() {
                    let name_idx = self.code.add_constant(Value::Str(name.clone()));
                    self.code.emit(OpCode::PostDecrement(name_idx));
                } else {
                    self.fallback_expr(&Expr::PostfixOp {
                        op: op.clone(),
                        expr: expr.clone(),
                    });
                }
            }
            // Assignment as expression
            Expr::AssignExpr { name, expr } => {
                self.compile_expr(expr);
                let name_idx = self.code.add_constant(Value::Str(name.clone()));
                self.code.emit(OpCode::AssignExpr(name_idx));
            }
            // Hash literal
            Expr::Hash(pairs) => {
                let n = pairs.len() as u32;
                for (key, val_opt) in pairs {
                    // Push key as string constant
                    let key_idx = self.code.add_constant(Value::Str(key.clone()));
                    self.code.emit(OpCode::LoadConst(key_idx));
                    // Push value (or Nil if none)
                    if let Some(val_expr) = val_opt {
                        self.compile_expr(val_expr);
                    } else {
                        self.code.emit(OpCode::LoadNil);
                    }
                }
                self.code.emit(OpCode::MakeHash(n));
            }
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
            TokenKind::Plus => Some(OpCode::Add),
            TokenKind::Minus => Some(OpCode::Sub),
            TokenKind::Star => Some(OpCode::Mul),
            TokenKind::Slash => Some(OpCode::Div),
            TokenKind::Percent => Some(OpCode::Mod),
            TokenKind::StarStar => Some(OpCode::Pow),
            TokenKind::Tilde => Some(OpCode::Concat),
            TokenKind::EqEq => Some(OpCode::NumEq),
            TokenKind::BangEq => Some(OpCode::NumNe),
            TokenKind::Lt => Some(OpCode::NumLt),
            TokenKind::Lte => Some(OpCode::NumLe),
            TokenKind::Gt => Some(OpCode::NumGt),
            TokenKind::Gte => Some(OpCode::NumGe),
            TokenKind::Ident(name) if name == "eq" => Some(OpCode::StrEq),
            TokenKind::Ident(name) if name == "ne" => Some(OpCode::StrNe),
            _ => None,
        }
    }

    /// Returns true if the expression must be kept as a raw Expr for
    /// exec_call handlers (e.g. throws-like needs Expr::Block to run code).
    fn needs_raw_expr(expr: &Expr) -> bool {
        matches!(expr, Expr::Block(_) | Expr::AnonSub(_))
    }

    /// Returns true if the expression is a variable that methods may mutate
    /// (requiring update_instance_target writeback to the env).
    fn is_mutable_target(expr: &Expr) -> bool {
        matches!(
            expr,
            Expr::Var(_) | Expr::ArrayVar(_) | Expr::HashVar(_)
        )
    }

    fn has_phasers(stmts: &[Stmt]) -> bool {
        stmts
            .iter()
            .any(|s| matches!(s, Stmt::Phaser { kind, .. } if matches!(kind, PhaserKind::Enter | PhaserKind::Leave | PhaserKind::First | PhaserKind::Next | PhaserKind::Last)))
    }
}
