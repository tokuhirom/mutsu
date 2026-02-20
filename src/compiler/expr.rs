use super::*;

impl Compiler {
    pub(super) fn compile_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Whatever => {
                let idx = self.code.add_constant(Value::Num(f64::INFINITY));
                self.code.emit(OpCode::LoadConst(idx));
            }
            Expr::HyperWhatever => {
                let idx = self.code.add_constant(Value::HyperWhatever);
                self.code.emit(OpCode::LoadConst(idx));
            }
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
                // Compile-time package/module variables
                if name == "?PACKAGE" || name == "?MODULE" {
                    let idx = self
                        .code
                        .add_constant(Value::Package(self.current_package.clone()));
                    self.code.emit(OpCode::LoadConst(idx));
                } else if let Some(&slot) = self.local_map.get(name.as_str()) {
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
                    } else if let Some(var_name) = Self::extract_vardecl_name(expr) {
                        // ++state $ — compile the VarDecl, then increment
                        self.compile_expr(expr);
                        self.code.emit(OpCode::Pop);
                        let name_idx = self.code.add_constant(Value::Str(var_name));
                        self.code.emit(OpCode::PreIncrement(name_idx));
                    } else {
                        self.code.emit(OpCode::LoadNil);
                    }
                }
                TokenKind::MinusMinus => {
                    if let Expr::Var(name) = expr.as_ref() {
                        let name_idx = self.code.add_constant(Value::Str(name.clone()));
                        self.code.emit(OpCode::PreDecrement(name_idx));
                    } else if let Some(var_name) = Self::extract_vardecl_name(expr) {
                        self.compile_expr(expr);
                        self.code.emit(OpCode::Pop);
                        let name_idx = self.code.add_constant(Value::Str(var_name));
                        self.code.emit(OpCode::PreDecrement(name_idx));
                    } else {
                        self.code.emit(OpCode::LoadNil);
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
                TokenKind::Pipe => {
                    self.compile_expr(expr);
                    self.code.emit(OpCode::MakeSlip);
                }
                _ => {
                    // Fallback: delegate to interpreter for unsupported prefix operators
                    let prefix_expr = Expr::Unary {
                        op: op.clone(),
                        expr: expr.clone(),
                    };
                    let idx = self.code.add_stmt(Stmt::Expr(prefix_expr));
                    self.code.emit(OpCode::EvalAstExpr(idx));
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
                        let lhs_var = match left.as_ref() {
                            Expr::Var(name) => Some(name.clone()),
                            _ => None,
                        };
                        self.compile_expr(left);
                        let sm_idx = self.code.emit(OpCode::SmartMatchExpr {
                            rhs_end: 0,
                            negate: matches!(op, TokenKind::BangTilde),
                            lhs_var,
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
                    // Fallback: delegate to interpreter for unsupported operators
                    let expr = Expr::Binary {
                        left: left.clone(),
                        op: op.clone(),
                        right: right.clone(),
                    };
                    let idx = self.code.add_stmt(Stmt::Expr(expr));
                    self.code.emit(OpCode::EvalAstExpr(idx));
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
                        Expr::CodeVar(n) => Some(format!("&{}", n)),
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
                }
                // Rewrite indir($path, { ... }) body into a callable block value so
                // call_function("indir", ...) can execute it after switching $*CWD.
                else if name == "indir" && args.len() >= 2 {
                    let mut rewritten_args = args.clone();
                    if let Expr::Block(body) = &rewritten_args[1] {
                        rewritten_args[1] = make_anon_sub(body.clone());
                    }
                    let arity = rewritten_args.len() as u32;
                    for arg in &rewritten_args {
                        self.compile_expr(arg);
                    }
                    let name_idx = self.code.add_constant(Value::Str(name.clone()));
                    self.code.emit(OpCode::CallFunc { name_idx, arity });
                } else {
                    // Rewrite VAR($var)/VAR(@var)/VAR(%var)/VAR(&var) → $var.VAR, etc.
                    if name == "VAR"
                        && args.len() == 1
                        && matches!(
                            args[0],
                            Expr::Var(_) | Expr::ArrayVar(_) | Expr::HashVar(_) | Expr::CodeVar(_)
                        )
                    {
                        let method_call = Expr::MethodCall {
                            target: Box::new(args[0].clone()),
                            name: "VAR".to_string(),
                            args: Vec::new(),
                            modifier: None,
                        };
                        self.compile_expr(&method_call);
                        return;
                    }
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
                target,
                name,
                args,
                modifier,
            } if matches!(
                target.as_ref(),
                Expr::Var(_) | Expr::ArrayVar(_) | Expr::HashVar(_) | Expr::CodeVar(_)
            ) || Self::is_dostmt_vardecl(target) =>
            {
                let target_name = match target.as_ref() {
                    Expr::Var(n) => n.clone(),
                    Expr::ArrayVar(n) => format!("@{}", n),
                    Expr::HashVar(n) => format!("%{}", n),
                    Expr::CodeVar(n) => format!("&{}", n),
                    Expr::DoStmt(stmt) => {
                        if let Stmt::VarDecl { name, .. } = stmt.as_ref() {
                            name.clone()
                        } else {
                            unreachable!()
                        }
                    }
                    _ => unreachable!(),
                };
                self.compile_expr(target);
                let arity = args.len() as u32;
                for arg in args {
                    self.compile_method_arg(arg);
                }
                let name_idx = self.code.add_constant(Value::Str(name.clone()));
                let target_name_idx = self.code.add_constant(Value::Str(target_name));
                let modifier_idx =
                    modifier.map(|m| self.code.add_constant(Value::Str(m.to_string())));
                self.code.emit(OpCode::CallMethodMut {
                    name_idx,
                    arity,
                    target_name_idx,
                    modifier_idx,
                });
            }
            // Method call on non-variable target (no writeback needed)
            Expr::MethodCall {
                target,
                name,
                args,
                modifier,
            } => {
                self.compile_expr(target);
                let arity = args.len() as u32;
                for arg in args {
                    self.compile_method_arg(arg);
                }
                let name_idx = self.code.add_constant(Value::Str(name.clone()));
                let modifier_idx =
                    modifier.map(|m| self.code.add_constant(Value::Str(m.to_string())));
                self.code.emit(OpCode::CallMethod {
                    name_idx,
                    arity,
                    modifier_idx,
                });
            }
            // Hyper method call: target».method(args)
            Expr::HyperMethodCall { target, name, args } => {
                self.compile_expr(target);
                let arity = args.len() as u32;
                for arg in args {
                    self.compile_method_arg(arg);
                }
                let name_idx = self.code.add_constant(Value::Str(name.clone()));
                self.code.emit(OpCode::HyperMethodCall { name_idx, arity });
            }
            // Indexing
            Expr::Index { target, index } => {
                // Special case: %*ENV<key> compiles to GetEnvIndex
                if let Expr::HashVar(name) = target.as_ref() {
                    if name == "*ENV" {
                        if let Expr::Literal(Value::Str(key)) = index.as_ref() {
                            let key_idx = self.code.add_constant(Value::Str(key.clone()));
                            self.code.emit(OpCode::GetEnvIndex(key_idx));
                        } else {
                            // Dynamic key - compile as runtime hash + index
                            self.compile_expr(target);
                            self.compile_expr(index);
                            self.code.emit(OpCode::Index);
                        }
                    } else {
                        self.compile_expr(target);
                        self.compile_expr(index);
                        self.code.emit(OpCode::Index);
                    }
                } else {
                    self.compile_expr(target);
                    self.compile_expr(index);
                    self.code.emit(OpCode::Index);
                }
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
            Expr::PostfixOp {
                op: TokenKind::PlusPlus,
                expr,
            } => {
                if let Expr::Var(name) = expr.as_ref() {
                    let name_idx = self.code.add_constant(Value::Str(name.clone()));
                    self.code.emit(OpCode::PostIncrement(name_idx));
                } else if let Expr::Index { target, index } = expr.as_ref() {
                    if let Some(name) = Self::postfix_index_name(target) {
                        self.compile_expr(index);
                        let name_idx = self.code.add_constant(Value::Str(name));
                        self.code.emit(OpCode::PostIncrementIndex(name_idx));
                    } else {
                        self.code.emit(OpCode::LoadNil);
                    }
                } else {
                    self.code.emit(OpCode::LoadNil);
                }
            }
            // Postfix -- on variable
            Expr::PostfixOp {
                op: TokenKind::MinusMinus,
                expr,
            } => {
                if let Expr::Var(name) = expr.as_ref() {
                    let name_idx = self.code.add_constant(Value::Str(name.clone()));
                    self.code.emit(OpCode::PostDecrement(name_idx));
                } else if let Expr::Index { target, index } = expr.as_ref() {
                    if let Some(name) = Self::postfix_index_name(target) {
                        self.compile_expr(index);
                        let name_idx = self.code.add_constant(Value::Str(name));
                        self.code.emit(OpCode::PostDecrementIndex(name_idx));
                    } else {
                        self.code.emit(OpCode::LoadNil);
                    }
                } else {
                    self.code.emit(OpCode::LoadNil);
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
                Expr::Index { target, index } if matches!(target.as_ref(), Expr::HashVar(name) if name == "*ENV") => {
                    if let Expr::Literal(Value::Str(key)) = index.as_ref() {
                        let key_idx = self.code.add_constant(Value::Str(key.clone()));
                        self.code.emit(OpCode::ExistsEnvIndex(key_idx));
                    } else {
                        self.compile_expr(inner);
                        self.code.emit(OpCode::ExistsExpr);
                    }
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
            // S/// non-destructive substitution
            Expr::NonDestructiveSubst {
                pattern,
                replacement,
            } => {
                let pattern_idx = self.code.add_constant(Value::Str(pattern.clone()));
                let replacement_idx = self.code.add_constant(Value::Str(replacement.clone()));
                self.code.emit(OpCode::NonDestructiveSubst {
                    pattern_idx,
                    replacement_idx,
                });
            }
            // tr/// transliteration
            Expr::Transliterate { from, to } => {
                let from_idx = self.code.add_constant(Value::Str(from.clone()));
                let to_idx = self.code.add_constant(Value::Str(to.clone()));
                self.code.emit(OpCode::Transliterate { from_idx, to_idx });
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
            Expr::Try { body, catch } => {
                self.compile_try(body, catch);
            }
            Expr::DoBlock { .. } => {
                if let Expr::DoBlock { body, label } = expr {
                    self.compile_do_block_expr(body, label);
                }
            }
            Expr::DoStmt(stmt) => match stmt.as_ref() {
                Stmt::If {
                    cond,
                    then_branch,
                    else_branch,
                } if Self::do_if_branch_supported(then_branch)
                    && Self::do_if_branch_supported(else_branch) =>
                {
                    self.compile_do_if_expr(cond, then_branch, else_branch);
                }
                Stmt::Given { topic, body } => {
                    self.compile_expr(topic);
                    let given_idx = self.code.emit(OpCode::DoGivenExpr { body_end: 0 });
                    for s in body {
                        self.compile_stmt(s);
                    }
                    self.code.patch_body_end(given_idx);
                }
                Stmt::Assign { name, expr, .. } => {
                    // do $var = expr → returns the assigned value
                    self.compile_expr(expr);
                    // Duplicate the value so we can assign AND return it
                    self.code.emit(OpCode::Dup);
                    self.emit_set_named_var(name);
                }
                Stmt::VarDecl {
                    name,
                    expr,
                    is_state,
                    ..
                } => {
                    // my $x = expr in expression context → declare, assign, return value
                    self.compile_expr(expr);
                    if *is_state {
                        let slot = self.alloc_local(name);
                        let key =
                            format!("__state_{}", STATE_COUNTER.fetch_add(1, Ordering::Relaxed));
                        let key_idx = self.code.add_constant(Value::Str(key.clone()));
                        self.code.state_locals.push((slot as usize, key.clone()));
                        self.code.emit(OpCode::StateVarInit(slot, key_idx));
                        self.code.emit(OpCode::GetLocal(slot));
                    } else {
                        self.code.emit(OpCode::Dup);
                        self.emit_set_named_var(name);
                    }
                }
                Stmt::Expr(inner_expr) => {
                    self.compile_expr(inner_expr);
                }
                Stmt::For {
                    iterable,
                    param,
                    params,
                    body,
                    label,
                } => {
                    self.compile_do_for_expr(iterable, param, params, body, label);
                }
                Stmt::ClassDecl { name, .. } => {
                    // Register the class and return the type object
                    self.compile_stmt(stmt);
                    let name_idx = self.code.add_constant(Value::Str(name.clone()));
                    self.code.emit(OpCode::GetBareWord(name_idx));
                }
                _ => {
                    self.compile_stmt(stmt);
                    self.code.emit(OpCode::LoadNil);
                }
            },
            Expr::Gather(_) => {
                if let Expr::Gather(body) = expr {
                    let idx = self.code.add_stmt(Stmt::Block(body.clone()));
                    self.code.emit(OpCode::MakeGather(idx));
                }
            }
            Expr::CallOn { target, args } => {
                if let Expr::CodeVar(name) = target.as_ref() {
                    for arg in args {
                        self.compile_expr(arg);
                    }
                    let name_idx = self.code.add_constant(Value::Str(name.clone()));
                    self.code.emit(OpCode::CallOnCodeVar {
                        name_idx,
                        arity: args.len() as u32,
                    });
                } else {
                    self.compile_expr(target);
                    for arg in args {
                        self.compile_expr(arg);
                    }
                    self.code.emit(OpCode::CallOnValue {
                        arity: args.len() as u32,
                    });
                }
            }
            Expr::AnonSub(body) => {
                let idx = self.code.add_stmt(Stmt::Block(body.clone()));
                self.code.emit(OpCode::MakeAnonSub(idx));
            }
            Expr::AnonSubParams { params, body } => {
                let idx = self.code.add_stmt(Stmt::SubDecl {
                    name: String::new(),
                    params: params.clone(),
                    param_defs: Vec::new(),
                    body: body.clone(),
                    multi: false,
                    is_export: false,
                });
                self.code.emit(OpCode::MakeAnonSubParams(idx));
            }
            Expr::Lambda { param, body } => {
                let idx = self.code.add_stmt(Stmt::SubDecl {
                    name: String::new(),
                    params: if param.is_empty() {
                        Vec::new()
                    } else {
                        vec![param.clone()]
                    },
                    param_defs: Vec::new(),
                    body: body.clone(),
                    multi: false,
                    is_export: false,
                });
                self.code.emit(OpCode::MakeLambda(idx));
            }
            Expr::IndexAssign {
                target,
                index,
                value,
            } => {
                if let Some(name) = Self::index_assign_target_name(target) {
                    self.compile_expr(value);
                    self.compile_expr(index);
                    let name_idx = self.code.add_constant(Value::Str(name));
                    self.code.emit(OpCode::IndexAssignExprNamed(name_idx));
                } else if let Some((name, inner_index)) = Self::index_assign_nested_target(target) {
                    self.compile_expr(value);
                    self.compile_expr(index);
                    self.compile_expr(inner_index);
                    let name_idx = self.code.add_constant(Value::Str(name));
                    self.code.emit(OpCode::IndexAssignExprNested(name_idx));
                } else {
                    self.code.emit(OpCode::IndexAssignInvalid);
                }
            }
            Expr::IndirectTypeLookup(inner) => {
                self.compile_expr(inner);
                self.code.emit(OpCode::IndirectTypeLookup);
            }
            Expr::IndirectCodeLookup { package, name } => {
                self.compile_expr(package);
                let name_idx = self.code.add_constant(Value::Str(name.clone()));
                self.code.emit(OpCode::IndirectCodeLookup(name_idx));
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
                    let idx = self.code.add_stmt(Stmt::Block(stmts.clone()));
                    self.code.emit(OpCode::MakeBlockClosure(idx));
                } else {
                    self.compile_block_inline(stmts);
                }
            }
            // Remaining expression forms not yet bytecode-native.
            Expr::PostfixOp { .. } => {
                self.code.emit(OpCode::LoadNil);
            }
        }
    }

    pub(super) fn binary_opcode(op: &TokenKind) -> Option<OpCode> {
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
            TokenKind::Ident(name) if name == "before" => Some(OpCode::Before),
            TokenKind::Ident(name) if name == "after" => Some(OpCode::After),
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
            TokenKind::Ident(name) if name == "=~=" => Some(OpCode::ApproxEq),
            TokenKind::Ident(name) if name == "=:=" => Some(OpCode::ContainerEq),
            // Divisibility
            TokenKind::PercentPercent => Some(OpCode::DivisibleBy),
            TokenKind::BangPercentPercent => Some(OpCode::NotDivisibleBy),
            // Keyword math
            TokenKind::Ident(name) if name == "div" => Some(OpCode::IntDiv),
            TokenKind::Ident(name) if name == "mod" => Some(OpCode::IntMod),
            TokenKind::Ident(name) if name == "gcd" => Some(OpCode::Gcd),
            TokenKind::Ident(name) if name == "lcm" => Some(OpCode::Lcm),
            TokenKind::Ident(name) if name == "min" => Some(OpCode::InfixMin),
            TokenKind::Ident(name) if name == "max" => Some(OpCode::InfixMax),
            // Repetition
            TokenKind::Ident(name) if name == "x" => Some(OpCode::StringRepeat),
            TokenKind::Ident(name) if name == "but" => Some(OpCode::ButMixin),
            TokenKind::Ident(name) if name == "isa" => Some(OpCode::Isa),
            TokenKind::Ident(name) if name == "does" => Some(OpCode::Does),
            TokenKind::Ident(name) if name == "xx" => Some(OpCode::ListRepeat),
            TokenKind::Ident(name) if name == "o" => Some(OpCode::FunctionCompose),
            // Pair
            TokenKind::FatArrow => Some(OpCode::MakePair),
            // Bitwise
            TokenKind::BitAnd => Some(OpCode::BitAnd),
            TokenKind::BitOr => Some(OpCode::BitOr),
            TokenKind::Ident(name) if name == "?|" => Some(OpCode::BoolBitOr),
            TokenKind::Ident(name) if name == "?&" => Some(OpCode::BoolBitAnd),
            TokenKind::Ident(name) if name == "?^" => Some(OpCode::BoolBitXor),
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
}
