use std::collections::HashMap;

use crate::ast::{AssignOp, CallArg, Expr, PhaserKind, Stmt};
use crate::lexer::TokenKind;
use crate::opcode::{CompiledCode, CompiledFunction, OpCode};
use crate::value::Value;

pub(crate) struct Compiler {
    code: CompiledCode,
    local_map: HashMap<String, u32>,
    compiled_functions: HashMap<String, CompiledFunction>,
    current_package: String,
}

impl Compiler {
    pub(crate) fn new() -> Self {
        Self {
            code: CompiledCode::new(),
            local_map: HashMap::new(),
            compiled_functions: HashMap::new(),
            current_package: "GLOBAL".to_string(),
        }
    }

    fn alloc_local(&mut self, name: &str) -> u32 {
        if let Some(&slot) = self.local_map.get(name) {
            return slot;
        }
        let slot = self.code.locals.len() as u32;
        self.code.locals.push(name.to_string());
        self.local_map.insert(name.to_string(), slot);
        slot
    }

    pub(crate) fn compile(
        mut self,
        stmts: &[Stmt],
    ) -> (CompiledCode, HashMap<String, CompiledFunction>) {
        for stmt in stmts {
            self.compile_stmt(stmt);
        }
        (self.code, self.compiled_functions)
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
                    self.code.emit(OpCode::RunBlockStmt(idx));
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
            Stmt::Note(exprs) => {
                for expr in exprs {
                    self.compile_expr(expr);
                }
                // Note goes to stderr; for now compile as Say (the interpreter handles it)
                self.code.emit(OpCode::Say(exprs.len() as u32));
            }
            Stmt::VarDecl {
                name,
                expr,
                type_constraint,
            } => {
                self.compile_expr(expr);
                if let Some(tc) = type_constraint {
                    let tc_idx = self.code.add_constant(Value::Str(tc.clone()));
                    self.code.emit(OpCode::TypeCheck(tc_idx));
                }
                let slot = self.alloc_local(name);
                self.code.emit(OpCode::SetLocal(slot));
            }
            Stmt::Assign {
                name,
                expr,
                op: AssignOp::Assign | AssignOp::Bind,
            } if name != "*PID" => {
                self.compile_expr(expr);
                if let Some(&slot) = self.local_map.get(name.as_str()) {
                    self.code.emit(OpCode::SetLocal(slot));
                } else {
                    let name_idx = self.code.add_constant(Value::Str(name.clone()));
                    self.code.emit(OpCode::SetGlobal(name_idx));
                }
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
                params,
                body,
                label,
            } if !Self::has_phasers(body) && params.is_empty() => {
                self.compile_expr(iterable);
                let param_idx = param
                    .as_ref()
                    .map(|p| self.code.add_constant(Value::Str(p.clone())));
                let param_local = param.as_ref().map(|p| self.alloc_local(p));
                let loop_idx = self.code.emit(OpCode::ForLoop {
                    param_idx,
                    param_local,
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
            Stmt::While { .. } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RunWhileStmt(idx));
            }
            Stmt::For { .. } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RunForStmt(idx));
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
            Stmt::Call { args, .. } => {
                for arg in args {
                    match arg {
                        CallArg::Positional(expr) if !Self::needs_raw_expr(expr) => {
                            self.compile_expr(expr)
                        }
                        CallArg::Positional(_) => {}
                        CallArg::Named {
                            value: Some(expr), ..
                        } if !Self::needs_raw_expr(expr) => self.compile_expr(expr),
                        CallArg::Named { value: Some(_), .. } => {}
                        CallArg::Named { value: None, .. } => {}
                    }
                }
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::ExecCallMixed(idx));
            }
            // Loop control
            Stmt::Last(label) => {
                self.code.emit(OpCode::Last(label.clone()));
            }
            Stmt::Next(label) => {
                self.code.emit(OpCode::Next(label.clone()));
            }
            Stmt::Redo(label) => {
                self.code.emit(OpCode::Redo(label.clone()));
            }
            Stmt::Return(expr) => {
                self.compile_expr(expr);
                self.code.emit(OpCode::Return);
            }
            Stmt::Die(expr) => {
                self.compile_expr(expr);
                self.code.emit(OpCode::Die);
            }
            Stmt::Proceed => {
                self.code.emit(OpCode::Proceed);
            }
            Stmt::Succeed => {
                self.code.emit(OpCode::Succeed);
            }
            // MatchAssign (~~=): coerce value to string
            Stmt::Assign {
                name,
                expr,
                op: AssignOp::MatchAssign,
            } if name != "*PID" => {
                self.compile_expr(expr);
                self.code.emit(OpCode::StrCoerce);
                if let Some(&slot) = self.local_map.get(name.as_str()) {
                    self.code.emit(OpCode::SetLocal(slot));
                } else {
                    let name_idx = self.code.add_constant(Value::Str(name.clone()));
                    self.code.emit(OpCode::SetGlobal(name_idx));
                }
            }
            Stmt::Assign { .. } => {
                self.code.emit(OpCode::AssignReadOnly);
            }
            // Given/When/Default
            Stmt::Given { topic, body } if !Self::has_phasers(body) => {
                self.compile_expr(topic);
                let given_idx = self.code.emit(OpCode::Given { body_end: 0 });
                for s in body {
                    self.compile_stmt(s);
                }
                self.code.patch_body_end(given_idx);
            }
            Stmt::When { cond, body } if !Self::has_phasers(body) => {
                self.compile_expr(cond);
                let when_idx = self.code.emit(OpCode::When { body_end: 0 });
                for s in body {
                    self.compile_stmt(s);
                }
                self.code.patch_body_end(when_idx);
            }
            Stmt::Default(body) if !Self::has_phasers(body) => {
                let default_idx = self.code.emit(OpCode::Default { body_end: 0 });
                for s in body {
                    self.compile_stmt(s);
                }
                self.code.patch_body_end(default_idx);
            }
            Stmt::Given { .. } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RunGivenStmt(idx));
            }
            Stmt::When { .. } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RunWhenStmt(idx));
            }
            Stmt::Default(_) => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RunDefaultStmt(idx));
            }
            // Repeat loop (repeat while / repeat until)
            Stmt::Loop {
                init,
                cond,
                step,
                body,
                repeat,
                label,
            } if *repeat && !Self::has_phasers(body) => {
                if let Some(init_stmt) = init {
                    self.compile_stmt(init_stmt);
                }
                // Layout: [RepeatLoop] [body..] [cond..]
                let loop_idx = self.code.emit(OpCode::RepeatLoop {
                    cond_end: 0,
                    body_end: 0,
                    label: label.clone(),
                });
                // Compile body
                for s in body {
                    self.compile_stmt(s);
                }
                self.code.patch_repeat_cond_end(loop_idx);
                // Compile condition (or push True if none)
                if let Some(cond_expr) = cond {
                    self.compile_expr(cond_expr);
                } else {
                    self.code.emit(OpCode::LoadTrue);
                }
                // Compile step (if any)
                if let Some(step_expr) = step {
                    self.compile_expr(step_expr);
                    self.code.emit(OpCode::Pop);
                }
                self.code.patch_loop_end(loop_idx);
            }
            Stmt::Loop { .. } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RunLoopStmt(idx));
            }
            // --- No-ops: these statements are handled elsewhere ---
            // CATCH/CONTROL are handled by try expressions, not standalone
            Stmt::Catch(_) | Stmt::Control(_) => {}
            // HasDecl/MethodDecl/DoesDecl outside class context are no-ops
            Stmt::HasDecl { .. } | Stmt::MethodDecl { .. } | Stmt::DoesDecl { .. } => {}

            // --- Take (gather/take) ---
            Stmt::Take(expr) => {
                self.compile_expr(expr);
                self.code.emit(OpCode::Take);
            }

            // --- React: just run the body block ---
            Stmt::React { body } if !Self::has_phasers(body) => {
                for s in body {
                    self.compile_stmt(s);
                }
            }
            Stmt::React { .. } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RunReactStmt(idx));
            }

            // --- Package scope ---
            Stmt::Package { name, body } if !Self::has_phasers(body) => {
                let name_idx = self.code.add_constant(Value::Str(name.clone()));
                let pkg_idx = self.code.emit(OpCode::PackageScope {
                    name_idx,
                    body_end: 0,
                });
                let saved_package = self.current_package.clone();
                self.current_package = name.clone();
                for s in body {
                    self.compile_stmt(s);
                }
                self.current_package = saved_package;
                self.code.patch_body_end(pkg_idx);
            }
            Stmt::Package { .. } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RunPackageStmt(idx));
            }

            // --- Phaser (BEGIN/END) ---
            Stmt::Phaser {
                kind: PhaserKind::Begin,
                body,
            } => {
                // BEGIN: compile body inline (runs immediately)
                for s in body {
                    self.compile_stmt(s);
                }
            }
            Stmt::Phaser {
                kind: PhaserKind::End,
                body,
            } => {
                // END: store body in stmt pool for deferred execution
                let end_stmt = Stmt::Phaser {
                    kind: PhaserKind::End,
                    body: body.clone(),
                };
                let idx = self.code.add_stmt(end_stmt);
                self.code.emit(OpCode::PhaserEnd(idx));
            }
            Stmt::Phaser { .. } => {}

            // --- SubDecl: delegate to interpreter AND compile body ---
            Stmt::SubDecl {
                name,
                params,
                param_defs,
                body,
                multi,
            } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RegisterSub(idx));
                // Also compile the body to bytecode for VM-native dispatch
                self.compile_sub_body(name, params, param_defs, body, *multi);
            }
            Stmt::TokenDecl { .. } | Stmt::RuleDecl { .. } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RegisterToken(idx));
            }
            Stmt::ProtoDecl { .. } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RegisterProtoSub(idx));
            }
            Stmt::ProtoToken { .. } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RegisterProtoToken(idx));
            }
            Stmt::Use { module, arg } if module == "lib" && arg.is_some() => {
                if let Some(expr) = arg {
                    self.compile_expr(expr);
                    self.code.emit(OpCode::UseLibPath);
                }
            }
            Stmt::Use { module, arg } if module == "lib" && arg.is_none() => {}
            Stmt::Use { module, .. }
                if module == "v6"
                    || module == "Test"
                    || module.starts_with("Test::")
                    || module == "customtrait"
                    || module == "isms" => {}
            Stmt::Use { module, .. } => {
                let name_idx = self.code.add_constant(Value::Str(module.clone()));
                self.code.emit(OpCode::UseModule(name_idx));
            }
            Stmt::EnumDecl { .. } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RegisterEnum(idx));
            }
            Stmt::ClassDecl { .. } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RegisterClass(idx));
            }
            Stmt::RoleDecl { .. } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RegisterRole(idx));
            }
            Stmt::SubsetDecl { .. } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RegisterSubset(idx));
            }
            Stmt::Subtest { .. } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RunSubtest(idx));
            }
            Stmt::Whenever { .. } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RunWhenever(idx));
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
                if let Some(&slot) = self.local_map.get(name.as_str()) {
                    self.code.emit(OpCode::GetLocal(slot));
                } else {
                    let name_idx = self.code.add_constant(Value::Str(name.clone()));
                    self.code.emit(OpCode::GetGlobal(name_idx));
                }
            }
            Expr::ArrayVar(name) => {
                let name_idx = self.code.add_constant(Value::Str(format!("@{}", name)));
                self.code.emit(OpCode::GetArrayVar(name_idx));
            }
            Expr::HashVar(name) => {
                let name_idx = self.code.add_constant(Value::Str(format!("%{}", name)));
                self.code.emit(OpCode::GetHashVar(name_idx));
            }
            Expr::BareWord(name) => {
                // Check if this bare word is a local variable (e.g., from constant declaration)
                if let Some(&slot) = self.local_map.get(name.as_str()) {
                    self.code.emit(OpCode::GetLocal(slot));
                } else {
                    let name_idx = self.code.add_constant(Value::Str(name.clone()));
                    self.code.emit(OpCode::GetBareWord(name_idx));
                }
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
                    if let Expr::Var(name) = expr.as_ref() {
                        let name_idx = self.code.add_constant(Value::Str(name.clone()));
                        self.code.emit(OpCode::PreIncrement(name_idx));
                    } else {
                        let idx = self.code.add_expr(Expr::Unary {
                            op: op.clone(),
                            expr: expr.clone(),
                        });
                        self.code.emit(OpCode::RunUnaryExpr(idx));
                    }
                }
                TokenKind::MinusMinus => {
                    if let Expr::Var(name) = expr.as_ref() {
                        let name_idx = self.code.add_constant(Value::Str(name.clone()));
                        self.code.emit(OpCode::PreDecrement(name_idx));
                    } else {
                        let idx = self.code.add_expr(Expr::Unary {
                            op: op.clone(),
                            expr: expr.clone(),
                        });
                        self.code.emit(OpCode::RunUnaryExpr(idx));
                    }
                }
                _ => {
                    let idx = self.code.add_expr(Expr::Unary {
                        op: op.clone(),
                        expr: expr.clone(),
                    });
                    self.code.emit(OpCode::RunUnaryExpr(idx));
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
                    TokenKind::NotAndThen => {
                        self.compile_expr(left);
                        let jump_eval_right = self.code.emit(OpCode::JumpIfNil(0));
                        self.code.emit(OpCode::Pop);
                        self.code.emit(OpCode::LoadNil);
                        let jump_end = self.code.emit(OpCode::Jump(0));
                        self.code.patch_jump(jump_eval_right);
                        self.code.emit(OpCode::Pop);
                        self.compile_expr(right);
                        self.code.patch_jump(jump_end);
                        return;
                    }
                    TokenKind::SmartMatch | TokenKind::BangTilde => {
                        self.compile_expr(left);
                        let sm_idx = self.code.emit(OpCode::SmartMatchExpr {
                            rhs_end: 0,
                            negate: matches!(op, TokenKind::BangTilde),
                        });
                        self.compile_expr(right);
                        self.code.patch_smart_match_rhs_end(sm_idx);
                        return;
                    }
                    _ => {}
                }

                if let Some(opcode) = Self::binary_opcode(op) {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    self.code.emit(opcode);
                } else {
                    self.compile_expr(left);
                    self.compile_expr(right);
                    let token_idx = self.code.add_token(op.clone());
                    self.code.emit(OpCode::RunBinaryToken(token_idx));
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
                // Rewrite undefine($var) → $var = Nil (assign Nil to the variable)
                if name == "undefine" && args.len() == 1 {
                    let var_name = match &args[0] {
                        Expr::Var(n) => Some(n.clone()),
                        Expr::ArrayVar(n) => Some(format!("@{}", n)),
                        Expr::HashVar(n) => Some(format!("%{}", n)),
                        _ => None,
                    };
                    if let Some(vname) = var_name {
                        // Push Nil and assign to the variable
                        self.code.emit(OpCode::LoadNil);
                        let name_idx = self.code.add_constant(Value::Str(vname));
                        self.code.emit(OpCode::AssignExpr(name_idx));
                    } else {
                        self.code.emit(OpCode::LoadNil);
                    }
                }
                // Rewrite shift(@arr)/pop(@arr) → @arr.shift()/@arr.pop() for mutability
                else if matches!(name.as_str(), "shift" | "pop")
                    && args.len() == 1
                    && matches!(args[0], Expr::ArrayVar(_) | Expr::Var(_))
                {
                    let method_call = Expr::MethodCall {
                        target: Box::new(args[0].clone()),
                        name: name.clone(),
                        args: Vec::new(),
                        modifier: None,
                    };
                    self.compile_expr(&method_call);
                }
                // Rewrite push(@arr, val...)/unshift(@arr, val...)/append/prepend → @arr.push(val...)
                else if matches!(name.as_str(), "push" | "unshift" | "append" | "prepend")
                    && args.len() >= 2
                    && matches!(args[0], Expr::ArrayVar(_) | Expr::Var(_))
                {
                    let method_call = Expr::MethodCall {
                        target: Box::new(args[0].clone()),
                        name: name.clone(),
                        args: args[1..].to_vec(),
                        modifier: None,
                    };
                    self.compile_expr(&method_call);
                } else {
                    let arity = args.len() as u32;
                    for arg in args {
                        self.compile_expr(arg);
                    }
                    let name_idx = self.code.add_constant(Value::Str(name.clone()));
                    self.code.emit(OpCode::CallFunc { name_idx, arity });
                }
            }
            // Method call on mutable variable target (needs writeback)
            Expr::MethodCall {
                target, name, args, ..
            } if matches!(
                target.as_ref(),
                Expr::Var(_) | Expr::ArrayVar(_) | Expr::HashVar(_) | Expr::CodeVar(_)
            ) =>
            {
                let target_name = match target.as_ref() {
                    Expr::Var(n) => n.clone(),
                    Expr::ArrayVar(n) => format!("@{}", n),
                    Expr::HashVar(n) => format!("%{}", n),
                    Expr::CodeVar(n) => format!("&{}", n),
                    _ => unreachable!(),
                };
                self.compile_expr(target);
                let arity = args.len() as u32;
                for arg in args {
                    self.compile_method_arg(arg);
                }
                let name_idx = self.code.add_constant(Value::Str(name.clone()));
                let target_name_idx = self.code.add_constant(Value::Str(target_name));
                self.code.emit(OpCode::CallMethodMut {
                    name_idx,
                    arity,
                    target_name_idx,
                });
            }
            // Method call on non-variable target (no writeback needed)
            Expr::MethodCall {
                target, name, args, ..
            } => {
                self.compile_expr(target);
                let arity = args.len() as u32;
                for arg in args {
                    self.compile_method_arg(arg);
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
                    let idx = self.code.add_expr(Expr::PostfixOp {
                        op: op.clone(),
                        expr: expr.clone(),
                    });
                    self.code.emit(OpCode::RunPostfixExpr(idx));
                }
            }
            // Postfix -- on variable
            Expr::PostfixOp { op, expr } if matches!(op, TokenKind::MinusMinus) => {
                if let Expr::Var(name) = expr.as_ref() {
                    let name_idx = self.code.add_constant(Value::Str(name.clone()));
                    self.code.emit(OpCode::PostDecrement(name_idx));
                } else {
                    let idx = self.code.add_expr(Expr::PostfixOp {
                        op: op.clone(),
                        expr: expr.clone(),
                    });
                    self.code.emit(OpCode::RunPostfixExpr(idx));
                }
            }
            // Assignment as expression
            Expr::AssignExpr { name, expr } => {
                self.compile_expr(expr);
                if let Some(&slot) = self.local_map.get(name.as_str()) {
                    self.code.emit(OpCode::AssignExprLocal(slot));
                } else {
                    let name_idx = self.code.add_constant(Value::Str(name.clone()));
                    self.code.emit(OpCode::AssignExpr(name_idx));
                }
            }
            // Capture variable ($0, $1, etc.)
            Expr::CaptureVar(name) => {
                let name_idx = self.code.add_constant(Value::Str(format!("<{}>", name)));
                self.code.emit(OpCode::GetCaptureVar(name_idx));
            }
            // Code variable (&foo)
            Expr::CodeVar(name) => {
                let name_idx = self.code.add_constant(Value::Str(name.clone()));
                self.code.emit(OpCode::GetCodeVar(name_idx));
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
            // Environment variable access (%*ENV<key>)
            Expr::EnvIndex(key) => {
                let key_idx = self.code.add_constant(Value::Str(key.clone()));
                self.code.emit(OpCode::GetEnvIndex(key_idx));
            }
            // Exists check (:exists)
            Expr::Exists(inner) => match inner.as_ref() {
                Expr::EnvIndex(key) => {
                    let key_idx = self.code.add_constant(Value::Str(key.clone()));
                    self.code.emit(OpCode::ExistsEnvIndex(key_idx));
                }
                _ => {
                    self.compile_expr(inner);
                    self.code.emit(OpCode::ExistsExpr);
                }
            },
            // Reduction ([+] @arr)
            Expr::Reduction { op, expr } => {
                self.compile_expr(expr);
                let op_idx = self.code.add_constant(Value::Str(op.clone()));
                self.code.emit(OpCode::Reduction(op_idx));
            }
            // __ROUTINE__ magic
            Expr::RoutineMagic => {
                self.code.emit(OpCode::RoutineMagic);
            }
            // __BLOCK__ magic
            Expr::BlockMagic => {
                self.code.emit(OpCode::BlockMagic);
            }
            // s/// substitution
            Expr::Subst {
                pattern,
                replacement,
            } => {
                let pattern_idx = self.code.add_constant(Value::Str(pattern.clone()));
                let replacement_idx = self.code.add_constant(Value::Str(replacement.clone()));
                self.code.emit(OpCode::Subst {
                    pattern_idx,
                    replacement_idx,
                });
            }
            // HyperOp (>>op<<): compile sub-expressions, delegate operation
            Expr::HyperOp {
                op,
                left,
                right,
                dwim_left,
                dwim_right,
            } => {
                self.compile_expr(left);
                self.compile_expr(right);
                let op_idx = self.code.add_constant(Value::Str(op.clone()));
                self.code.emit(OpCode::HyperOp {
                    op_idx,
                    dwim_left: *dwim_left,
                    dwim_right: *dwim_right,
                });
            }
            // MetaOp (Rop, Xop, Zop): compile sub-expressions, delegate operation
            Expr::MetaOp {
                meta,
                op,
                left,
                right,
            } => {
                self.compile_expr(left);
                self.compile_expr(right);
                let meta_idx = self.code.add_constant(Value::Str(meta.clone()));
                let op_idx = self.code.add_constant(Value::Str(op.clone()));
                self.code.emit(OpCode::MetaOp { meta_idx, op_idx });
            }
            // InfixFunc (atan2, sprintf): compile sub-expressions, delegate operation
            Expr::InfixFunc {
                name,
                left,
                right,
                modifier,
            } => {
                self.compile_expr(left);
                for r in right {
                    self.compile_expr(r);
                }
                let name_idx = self.code.add_constant(Value::Str(name.clone()));
                let modifier_idx = modifier
                    .as_ref()
                    .map(|m| self.code.add_constant(Value::Str(m.clone())));
                self.code.emit(OpCode::InfixFunc {
                    name_idx,
                    right_arity: right.len() as u32,
                    modifier_idx,
                });
            }
            // Try/Catch: compile to TryCatch opcode (only if no CONTROL blocks)
            Expr::Try { body, catch } if !body.iter().any(|s| matches!(s, Stmt::Control(_))) => {
                self.compile_try(body, catch);
            }
            Expr::Try { .. } => {
                let idx = self.code.add_expr(expr.clone());
                self.code.emit(OpCode::RunTryExpr(idx));
            }
            Expr::DoBlock { .. } => {
                let idx = self.code.add_expr(expr.clone());
                self.code.emit(OpCode::RunDoBlockExpr(idx));
            }
            Expr::DoStmt(_) => {
                let idx = self.code.add_expr(expr.clone());
                self.code.emit(OpCode::RunDoStmtExpr(idx));
            }
            Expr::Gather(_) => {
                let idx = self.code.add_expr(expr.clone());
                self.code.emit(OpCode::RunGatherExpr(idx));
            }
            Expr::CallOn { .. } => {
                let idx = self.code.add_expr(expr.clone());
                self.code.emit(OpCode::RunCallOnExpr(idx));
            }
            Expr::AnonSub(_) => {
                let idx = self.code.add_expr(expr.clone());
                self.code.emit(OpCode::RunAnonSubExpr(idx));
            }
            Expr::AnonSubParams { .. } => {
                let idx = self.code.add_expr(expr.clone());
                self.code.emit(OpCode::RunAnonSubParamsExpr(idx));
            }
            Expr::Lambda { .. } => {
                let idx = self.code.add_expr(expr.clone());
                self.code.emit(OpCode::RunLambdaExpr(idx));
            }
            Expr::IndexAssign { .. } => {
                let idx = self.code.add_expr(expr.clone());
                self.code.emit(OpCode::RunIndexAssignExpr(idx));
            }
            Expr::ControlFlow { kind, label } => match kind {
                crate::ast::ControlFlowKind::Last => {
                    self.code.emit(OpCode::Last(label.clone()));
                }
                crate::ast::ControlFlowKind::Next => {
                    self.code.emit(OpCode::Next(label.clone()));
                }
                crate::ast::ControlFlowKind::Redo => {
                    self.code.emit(OpCode::Redo(label.clone()));
                }
            },
            // Block inlining: compile inline if no placeholders
            Expr::Block(stmts) => {
                if Self::has_block_placeholders(stmts) {
                    let idx = self.code.add_expr(expr.clone());
                    self.code.emit(OpCode::RunBlockExpr(idx));
                } else {
                    self.compile_block_inline(stmts);
                }
            }
            // Remaining expression forms not yet bytecode-native.
            Expr::PostfixOp { .. } => {
                let idx = self.code.add_expr(expr.clone());
                self.code.emit(OpCode::RunPostfixExpr(idx));
            }
        }
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
            TokenKind::Ident(name) if name == "lt" => Some(OpCode::StrLt),
            TokenKind::Ident(name) if name == "gt" => Some(OpCode::StrGt),
            TokenKind::Ident(name) if name == "le" => Some(OpCode::StrLe),
            TokenKind::Ident(name) if name == "ge" => Some(OpCode::StrGe),
            TokenKind::DotDot => Some(OpCode::MakeRange),
            TokenKind::DotDotCaret => Some(OpCode::MakeRangeExcl),
            TokenKind::CaretDotDot => Some(OpCode::MakeRangeExclStart),
            TokenKind::CaretDotDotCaret => Some(OpCode::MakeRangeExclBoth),
            // Smart match (~~, !~~): handled via interpreter fallback (needs $_ binding)
            // Three-way comparison
            TokenKind::LtEqGt => Some(OpCode::Spaceship),
            TokenKind::Ident(name) if name == "cmp" => Some(OpCode::Cmp),
            TokenKind::Ident(name) if name == "leg" => Some(OpCode::Leg),
            // Identity/value equality
            TokenKind::EqEqEq => Some(OpCode::StrictEq),
            TokenKind::Ident(name) if name == "eqv" => Some(OpCode::Eqv),
            // Divisibility
            TokenKind::PercentPercent => Some(OpCode::DivisibleBy),
            // Keyword math
            TokenKind::Ident(name) if name == "div" => Some(OpCode::IntDiv),
            TokenKind::Ident(name) if name == "mod" => Some(OpCode::IntMod),
            TokenKind::Ident(name) if name == "gcd" => Some(OpCode::Gcd),
            TokenKind::Ident(name) if name == "lcm" => Some(OpCode::Lcm),
            // Repetition
            TokenKind::Ident(name) if name == "x" => Some(OpCode::StringRepeat),
            TokenKind::Ident(name) if name == "but" => Some(OpCode::ButMixin),
            TokenKind::Ident(name) if name == "xx" => Some(OpCode::ListRepeat),
            // Pair
            TokenKind::FatArrow => Some(OpCode::MakePair),
            // Bitwise
            TokenKind::BitAnd => Some(OpCode::BitAnd),
            TokenKind::BitOr => Some(OpCode::BitOr),
            TokenKind::BitXor => Some(OpCode::BitXor),
            TokenKind::BitShiftLeft => Some(OpCode::BitShiftLeft),
            TokenKind::BitShiftRight => Some(OpCode::BitShiftRight),
            // Set operations
            TokenKind::SetElem => Some(OpCode::SetElem),
            TokenKind::SetCont => Some(OpCode::SetCont),
            TokenKind::SetUnion => Some(OpCode::SetUnion),
            TokenKind::SetIntersect => Some(OpCode::SetIntersect),
            TokenKind::SetDiff => Some(OpCode::SetDiff),
            TokenKind::SetSymDiff => Some(OpCode::SetSymDiff),
            TokenKind::SetSubset => Some(OpCode::SetSubset),
            TokenKind::SetSuperset => Some(OpCode::SetSuperset),
            TokenKind::SetStrictSubset => Some(OpCode::SetStrictSubset),
            TokenKind::SetStrictSuperset => Some(OpCode::SetStrictSuperset),
            TokenKind::Pipe => Some(OpCode::JunctionAny),
            TokenKind::Ampersand => Some(OpCode::JunctionAll),
            TokenKind::Caret => Some(OpCode::JunctionOne),
            TokenKind::DotDotDot => Some(OpCode::Sequence { exclude_end: false }),
            TokenKind::DotDotDotCaret => Some(OpCode::Sequence { exclude_end: true }),
            _ => None,
        }
    }

    /// Returns true if the expression must be kept as a raw Expr for
    /// exec_call handlers (e.g. throws-like needs Expr::Block to run code).
    fn needs_raw_expr(expr: &Expr) -> bool {
        match expr {
            Expr::Block(_) => true,
            // Hash literals containing block values need raw expr for is_run lambda matchers
            Expr::Hash(pairs) => pairs
                .iter()
                .any(|(_, v)| v.as_ref().is_some_and(Self::needs_raw_expr)),
            _ => false,
        }
    }

    fn has_phasers(stmts: &[Stmt]) -> bool {
        stmts
            .iter()
            .any(|s| matches!(s, Stmt::Phaser { kind, .. } if matches!(kind, PhaserKind::Enter | PhaserKind::Leave | PhaserKind::First | PhaserKind::Next | PhaserKind::Last)))
    }

    /// Check if a block body contains placeholder variables ($^a, $^b, etc.)
    fn has_block_placeholders(stmts: &[Stmt]) -> bool {
        for stmt in stmts {
            if Self::stmt_has_placeholder(stmt) {
                return true;
            }
        }
        false
    }

    /// Compile a method call argument. Named args (AssignExpr) are
    /// compiled as Pair values so they survive VM execution.
    fn compile_method_arg(&mut self, arg: &Expr) {
        if let Expr::AssignExpr { name, expr } = arg {
            self.compile_expr(&Expr::Literal(Value::Str(name.clone())));
            self.compile_expr(expr);
            self.code.emit(OpCode::MakePair);
        } else {
            self.compile_expr(arg);
        }
    }

    fn stmt_has_placeholder(stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Expr(e) | Stmt::Return(e) | Stmt::Die(e) | Stmt::Take(e) => {
                Self::expr_has_placeholder(e)
            }
            Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => {
                Self::expr_has_placeholder(expr)
            }
            Stmt::Say(es) | Stmt::Print(es) | Stmt::Note(es) => {
                es.iter().any(Self::expr_has_placeholder)
            }
            Stmt::If {
                cond,
                then_branch,
                else_branch,
            } => {
                Self::expr_has_placeholder(cond)
                    || then_branch.iter().any(Self::stmt_has_placeholder)
                    || else_branch.iter().any(Self::stmt_has_placeholder)
            }
            Stmt::Block(stmts) => stmts.iter().any(Self::stmt_has_placeholder),
            _ => false,
        }
    }

    fn expr_has_placeholder(expr: &Expr) -> bool {
        match expr {
            Expr::Var(name) => name.starts_with("$^"),
            Expr::Binary { left, right, .. } => {
                Self::expr_has_placeholder(left) || Self::expr_has_placeholder(right)
            }
            Expr::Unary { expr, .. } => Self::expr_has_placeholder(expr),
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => {
                Self::expr_has_placeholder(cond)
                    || Self::expr_has_placeholder(then_expr)
                    || Self::expr_has_placeholder(else_expr)
            }
            Expr::Call { args, .. } => args.iter().any(Self::expr_has_placeholder),
            Expr::MethodCall { target, args, .. } => {
                Self::expr_has_placeholder(target) || args.iter().any(Self::expr_has_placeholder)
            }
            Expr::Index { target, index } | Expr::IndexAssign { target, index, .. } => {
                Self::expr_has_placeholder(target) || Self::expr_has_placeholder(index)
            }
            Expr::StringInterpolation(parts) | Expr::ArrayLiteral(parts) => {
                parts.iter().any(Self::expr_has_placeholder)
            }
            _ => false,
        }
    }

    /// Compile a SubDecl body to a CompiledFunction and store it.
    fn compile_sub_body(
        &mut self,
        name: &str,
        params: &[String],
        param_defs: &[crate::ast::ParamDef],
        body: &[Stmt],
        multi: bool,
    ) {
        let mut sub_compiler = Compiler::new();
        // Pre-allocate locals for parameters
        for param in params {
            sub_compiler.alloc_local(param);
        }
        // Also allocate from param_defs in case param names differ
        for pd in param_defs {
            if !pd.name.is_empty() {
                sub_compiler.alloc_local(&pd.name);
            }
        }
        // Compile body statements; last Stmt::Expr should NOT emit Pop (implicit return)
        for (i, stmt) in body.iter().enumerate() {
            let is_last = i == body.len() - 1;
            if is_last && let Stmt::Expr(expr) = stmt {
                sub_compiler.compile_expr(expr);
                // Don't emit Pop — leave value on stack as implicit return
                continue;
            }
            sub_compiler.compile_stmt(stmt);
        }

        let key = if multi {
            let arity = param_defs.len();
            let type_sig: Vec<String> = param_defs
                .iter()
                .map(|pd| pd.type_constraint.clone().unwrap_or_default())
                .collect();
            format!(
                "{}::{}{}",
                self.current_package,
                name,
                if !type_sig.is_empty() {
                    format!("/{}:{}", arity, type_sig.join(","))
                } else {
                    format!("/{}", arity)
                }
            )
        } else {
            format!("{}::{}", self.current_package, name)
        };

        let cf = CompiledFunction {
            code: sub_compiler.code,
            params: params.to_vec(),
            param_defs: param_defs.to_vec(),
        };
        self.compiled_functions.insert(key, cf);
    }

    /// Compile Expr::Try { body, catch } to TryCatch opcode.
    fn compile_try(&mut self, body: &[Stmt], catch: &Option<Vec<Stmt>>) {
        // Separate CATCH/CONTROL blocks from body
        let mut main_stmts = Vec::new();
        let mut catch_stmts = catch.clone();
        for stmt in body {
            if let Stmt::Catch(catch_body) = stmt {
                catch_stmts = Some(catch_body.clone());
            } else {
                main_stmts.push(stmt.clone());
            }
        }
        // Emit TryCatch placeholder
        let try_idx = self.code.emit(OpCode::TryCatch {
            catch_start: 0,
            body_end: 0,
        });
        // Compile main body (last Stmt::Expr leaves value on stack)
        for (i, stmt) in main_stmts.iter().enumerate() {
            let is_last = i == main_stmts.len() - 1;
            if is_last && let Stmt::Expr(expr) = stmt {
                self.compile_expr(expr);
                continue;
            }
            self.compile_stmt(stmt);
        }
        if main_stmts.is_empty() {
            self.code.emit(OpCode::LoadNil);
        }
        // Jump over catch on success
        let jump_end = self.code.emit(OpCode::Jump(0));
        // Patch catch_start
        self.code.patch_try_catch_start(try_idx);
        // Compile catch block
        if let Some(ref catch_body) = catch_stmts {
            for stmt in catch_body {
                self.compile_stmt(stmt);
            }
        }
        self.code.emit(OpCode::LoadNil); // catch result is Nil
        // Patch body_end and jump_end
        self.code.patch_try_body_end(try_idx);
        self.code.patch_jump(jump_end);
    }

    /// Compile a block inline (for blocks without placeholders).
    fn compile_block_inline(&mut self, stmts: &[Stmt]) {
        if stmts.is_empty() {
            self.code.emit(OpCode::LoadNil);
            return;
        }
        for (i, stmt) in stmts.iter().enumerate() {
            let is_last = i == stmts.len() - 1;
            if is_last && let Stmt::Expr(expr) = stmt {
                self.compile_expr(expr);
                // Don't emit Pop — leave value on stack as block's return value
                return;
            }
            self.compile_stmt(stmt);
        }
        // If last statement wasn't an expression, push Nil
        self.code.emit(OpCode::LoadNil);
    }
}
