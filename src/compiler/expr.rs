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
            // m/regex/ — compile as $_ ~~ /regex/, matching against $_
            Expr::MatchRegex(v) => {
                self.compile_match_regex(v);
            }
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
                    let name_idx = self
                        .code
                        .add_constant(Value::Str(self.qualify_variable_name(name)));
                    self.code.emit(OpCode::GetGlobal(name_idx));
                }
            }
            Expr::ArrayVar(name) => {
                let sigiled = format!("@{}", name);
                let var_name = if self.local_map.contains_key(sigiled.as_str()) {
                    sigiled
                } else {
                    self.qualify_variable_name(&format!("@{}", name))
                };
                let name_idx = self.code.add_constant(Value::Str(var_name));
                self.code.emit(OpCode::GetArrayVar(name_idx));
            }
            Expr::HashVar(name) => {
                let sigiled = format!("%{}", name);
                let var_name = if self.local_map.contains_key(sigiled.as_str()) {
                    sigiled
                } else {
                    self.qualify_variable_name(&format!("%{}", name))
                };
                let name_idx = self.code.add_constant(Value::Str(var_name));
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
            Expr::PseudoStash(name) => {
                let name_idx = self.code.add_constant(Value::Str(name.clone()));
                self.code.emit(OpCode::GetPseudoStash(name_idx));
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
                // Detect `funcname |capture` pattern (listop call with capture slip)
                if *op == TokenKind::Pipe
                    && matches!(right.as_ref(), Expr::BareWord(_))
                    && let Expr::BareWord(name) = left.as_ref()
                {
                    // Compile the slip arg (the capture variable)
                    self.compile_expr(right);
                    let name_idx = self.code.add_constant(Value::Str(name.clone()));
                    self.code.emit(OpCode::CallFuncSlip {
                        name_idx,
                        regular_arity: 0,
                        arg_sources_idx: None,
                    });
                    return;
                }
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
                        // a || b: result is a if truthy, else b
                        self.compile_expr(left);
                        self.code.emit(OpCode::Dup);
                        let jump_cleanup = self.code.emit(OpCode::JumpIfTrue(0));
                        // Falsy path: pop both copies, evaluate right
                        self.code.emit(OpCode::Pop);
                        self.code.emit(OpCode::Pop);
                        self.compile_expr(right);
                        let jump_end = self.code.emit(OpCode::Jump(0));
                        // Truthy path: pop the dup, keep the original
                        self.code.patch_jump(jump_cleanup);
                        self.code.emit(OpCode::Pop);
                        self.code.patch_jump(jump_end);
                        return;
                    }
                    TokenKind::SlashSlash | TokenKind::OrElse => {
                        // a // b: result is a if not nil, else b
                        self.compile_expr(left);
                        self.code.emit(OpCode::Dup);
                        let jump_cleanup = self.code.emit(OpCode::JumpIfNotNil(0));
                        // Nil path: pop both copies, evaluate right
                        self.code.emit(OpCode::Pop);
                        self.code.emit(OpCode::Pop);
                        self.compile_expr(right);
                        let jump_end = self.code.emit(OpCode::Jump(0));
                        // Not-nil path: pop the dup, keep the original
                        self.code.patch_jump(jump_cleanup);
                        self.code.emit(OpCode::Pop);
                        self.code.patch_jump(jump_end);
                        return;
                    }
                    TokenKind::AndThen => {
                        // a andthen b: result is b if a.defined, else a
                        self.compile_expr(left);
                        self.code.emit(OpCode::Dup);
                        self.code.emit(OpCode::CallDefined);
                        let jump_undef = self.code.emit(OpCode::JumpIfFalse(0));
                        // Defined path: pop original, evaluate right
                        self.code.emit(OpCode::Pop);
                        self.compile_expr(right);
                        let jump_end = self.code.emit(OpCode::Jump(0));
                        // Undefined path: keep original
                        self.code.patch_jump(jump_undef);
                        self.code.patch_jump(jump_end);
                        return;
                    }
                    TokenKind::NotAndThen => {
                        // a notandthen b: result is b if a is NOT defined, else Nil
                        self.compile_expr(left);
                        self.code.emit(OpCode::Dup);
                        self.code.emit(OpCode::CallDefined);
                        let jump_undef = self.code.emit(OpCode::JumpIfFalse(0));
                        // Defined path: pop original, push Nil
                        self.code.emit(OpCode::Pop);
                        self.code.emit(OpCode::LoadNil);
                        let jump_end = self.code.emit(OpCode::Jump(0));
                        // Undefined path: pop original, evaluate right
                        self.code.patch_jump(jump_undef);
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
                        // When RHS is m/regex/, unwrap to the regex value since
                        // SmartMatchExpr already handles the matching against LHS
                        match right.as_ref() {
                            Expr::MatchRegex(v) => {
                                let idx = self.code.add_constant(v.clone());
                                self.code.emit(OpCode::LoadConst(idx));
                            }
                            _ => self.compile_expr(right),
                        }
                        self.code.patch_smart_match_rhs_end(sm_idx);
                        return;
                    }
                    // Sequential metaoperators: S&, S|, S^
                    TokenKind::Ident(op) if op == "S&" => {
                        self.compile_expr(left);
                        self.compile_expr(right);
                        self.code.emit(OpCode::JunctionAll);
                        return;
                    }
                    TokenKind::Ident(op) if op == "S|" => {
                        self.compile_expr(left);
                        self.compile_expr(right);
                        self.code.emit(OpCode::JunctionAny);
                        return;
                    }
                    TokenKind::Ident(op) if op == "S^" => {
                        self.compile_expr(left);
                        self.compile_expr(right);
                        self.code.emit(OpCode::JunctionOne);
                        return;
                    }
                    _ => {}
                }

                if let Some(opcode) = Self::binary_opcode(op) {
                    if matches!(op, TokenKind::Ident(name) if name == "does")
                        && let Expr::Var(name) = left.as_ref()
                    {
                        self.compile_expr(left);
                        self.compile_expr(right);
                        let name_idx = self.code.add_constant(Value::Str(name.clone()));
                        self.code.emit(OpCode::DoesVar(name_idx));
                        return;
                    }
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
            Expr::BracketArray(elems) => {
                // When a single ArrayLiteral is the sole element (e.g., [<a b c>]
                // or [(1,2,3)]), flatten it so items become direct array elements.
                // Multiple comma-separated items stay as-is: [(0,1),(2,3)] → 2 Lists.
                if elems.len() == 1 {
                    if let Expr::ArrayLiteral(items) = &elems[0] {
                        for item in items {
                            self.compile_expr(item);
                        }
                        self.code.emit(OpCode::MakeRealArray(items.len() as u32));
                    } else {
                        self.compile_expr(&elems[0]);
                        self.code.emit(OpCode::MakeRealArray(1));
                    }
                } else {
                    for elem in elems {
                        self.compile_expr(elem);
                    }
                    self.code.emit(OpCode::MakeRealArray(elems.len() as u32));
                }
            }
            Expr::CaptureLiteral(items) => {
                // Compile all items onto the stack. At runtime, MakeCapture
                // separates Pair values (named) from non-Pair (positional).
                // Slip (|expr) items are compiled with a special marker.
                for item in items {
                    self.compile_expr(item);
                }
                self.code.emit(OpCode::MakeCapture(items.len() as u32));
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
                        quoted: false,
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
                        quoted: false,
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
                    let arg_sources_idx = self.add_arg_sources_constant(&rewritten_args);
                    for arg in &rewritten_args {
                        self.compile_call_arg(arg);
                    }
                    let name_idx = self.code.add_constant(Value::Str(name.clone()));
                    self.code.emit(OpCode::CallFunc {
                        name_idx,
                        arity,
                        arg_sources_idx,
                    });
                }
                // Rewrite cas($target, $expected, $new) into:
                // do {
                //   my $__cas_seen = $target;
                //   if $__cas_seen == $expected { $target = $new }
                //   $__cas_seen
                // }
                else if name == "cas" && args.len() == 3 {
                    let assign_stmt = match &args[0] {
                        Expr::Var(target_name) => Some(Stmt::Assign {
                            name: target_name.clone(),
                            expr: args[2].clone(),
                            op: AssignOp::Assign,
                        }),
                        Expr::Index { target, index } => Some(Stmt::Expr(Expr::IndexAssign {
                            target: target.clone(),
                            index: index.clone(),
                            value: Box::new(args[2].clone()),
                        })),
                        _ => None,
                    };
                    if let Some(assign_stmt) = assign_stmt {
                        let seen_name = format!(
                            "__mutsu_cas_seen_{}",
                            STATE_COUNTER.fetch_add(1, Ordering::Relaxed)
                        );
                        let cas_expr = Expr::DoBlock {
                            body: vec![
                                Stmt::VarDecl {
                                    name: seen_name.clone(),
                                    expr: args[0].clone(),
                                    type_constraint: None,
                                    is_state: false,
                                    is_our: false,
                                },
                                Stmt::If {
                                    cond: Expr::Binary {
                                        left: Box::new(Expr::Var(seen_name.clone())),
                                        op: TokenKind::EqEq,
                                        right: Box::new(args[1].clone()),
                                    },
                                    then_branch: vec![assign_stmt],
                                    else_branch: vec![],
                                },
                                Stmt::Expr(Expr::Var(seen_name)),
                            ],
                            label: None,
                        };
                        self.compile_expr(&cas_expr);
                    } else {
                        let arity = args.len() as u32;
                        let arg_sources_idx = self.add_arg_sources_constant(args);
                        for arg in args {
                            self.compile_expr(arg);
                        }
                        let name_idx = self.code.add_constant(Value::Str(name.clone()));
                        self.code.emit(OpCode::CallFunc {
                            name_idx,
                            arity,
                            arg_sources_idx,
                        });
                    }
                }
                // Rewrite cas($var, &fn) to assignment expression:
                // $var = fn($var)
                else if name == "cas" && args.len() == 2 {
                    let var_name = match &args[0] {
                        Expr::Var(n) => Some(n.clone()),
                        Expr::ArrayVar(n) => Some(format!("@{}", n)),
                        Expr::HashVar(n) => Some(format!("%{}", n)),
                        Expr::CodeVar(n) => Some(format!("&{}", n)),
                        _ => None,
                    };
                    if let Some(vname) = var_name {
                        let assign_expr = Expr::AssignExpr {
                            name: vname,
                            expr: Box::new(Expr::CallOn {
                                target: Box::new(args[1].clone()),
                                args: vec![args[0].clone()],
                            }),
                        };
                        self.compile_expr(&assign_expr);
                    } else {
                        let arity = args.len() as u32;
                        let arg_sources_idx = self.add_arg_sources_constant(args);
                        for arg in args {
                            self.compile_expr(arg);
                        }
                        let name_idx = self.code.add_constant(Value::Str(name.clone()));
                        self.code.emit(OpCode::CallFunc {
                            name_idx,
                            arity,
                            arg_sources_idx,
                        });
                    }
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
                            quoted: false,
                        };
                        self.compile_expr(&method_call);
                        return;
                    }
                    // Check for capture slip args (|c)
                    let has_slip = args.iter().any(|arg| {
                        matches!(
                            arg,
                            Expr::Unary {
                                op: TokenKind::Pipe,
                                ..
                            }
                        )
                    });
                    if has_slip {
                        // Compile regular args first, then the slip arg
                        let mut regular_count = 0u32;
                        for arg in args {
                            if let Expr::Unary {
                                op: TokenKind::Pipe,
                                ..
                            } = arg
                            {
                                // skip slip args in first pass
                            } else {
                                self.compile_call_arg(arg);
                                regular_count += 1;
                            }
                        }
                        // Compile the slip arg (only the inner expr)
                        for arg in args {
                            if let Expr::Unary {
                                op: TokenKind::Pipe,
                                expr,
                            } = arg
                            {
                                self.compile_expr(expr);
                                break; // only one slip supported
                            }
                        }
                        let name_idx = self.code.add_constant(Value::Str(name.clone()));
                        self.code.emit(OpCode::CallFuncSlip {
                            name_idx,
                            regular_arity: regular_count,
                            arg_sources_idx: None,
                        });
                    } else {
                        let arity = args.len() as u32;
                        let arg_sources_idx = self.add_arg_sources_constant(args);
                        for arg in args {
                            self.compile_call_arg(arg);
                        }
                        let name_idx = self.code.add_constant(Value::Str(name.clone()));
                        self.code.emit(OpCode::CallFunc {
                            name_idx,
                            arity,
                            arg_sources_idx,
                        });
                    }
                }
            }
            // Method call on mutable variable target (needs writeback)
            Expr::MethodCall {
                target,
                name,
                args,
                modifier,
                quoted,
            } if matches!(
                target.as_ref(),
                Expr::Var(_)
                    | Expr::ArrayVar(_)
                    | Expr::HashVar(_)
                    | Expr::CodeVar(_)
                    | Expr::BareWord(_)
            ) || Self::is_dostmt_vardecl(target) =>
            {
                let target_name = match target.as_ref() {
                    Expr::Var(n) => n.clone(),
                    Expr::ArrayVar(n) => format!("@{}", n),
                    Expr::HashVar(n) => format!("%{}", n),
                    Expr::CodeVar(n) => format!("&{}", n),
                    Expr::BareWord(n) => n.clone(),
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
                    quoted: *quoted,
                });
            }
            // Method call on non-variable target (no writeback needed)
            Expr::MethodCall {
                target,
                name,
                args,
                modifier,
                quoted,
            } => {
                // Lower index :delete adverb to dedicated delete opcodes.
                if name == "DELETE-KEY"
                    && args.is_empty()
                    && modifier.is_none()
                    && let Expr::Index {
                        target: delete_target,
                        index: delete_index,
                    } = target.as_ref()
                {
                    if let Some(var_name) = Self::postfix_index_name(delete_target) {
                        self.compile_expr(delete_index);
                        let name_idx = self.code.add_constant(Value::Str(var_name));
                        self.code.emit(OpCode::DeleteIndexNamed(name_idx));
                    } else {
                        self.compile_expr(delete_target);
                        self.compile_expr(delete_index);
                        self.code.emit(OpCode::DeleteIndexExpr);
                    }
                    return;
                }
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
                    quoted: *quoted,
                });
            }
            // Dynamic method call: target."$name"(args)
            Expr::DynamicMethodCall {
                target,
                name_expr,
                args,
            } => {
                self.compile_expr(target);
                self.compile_expr(name_expr);
                let arity = args.len() as u32;
                for arg in args {
                    self.compile_method_arg(arg);
                }
                self.code.emit(OpCode::CallMethodDynamic { arity });
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
                        self.compile_expr(target);
                        self.compile_expr(index);
                        self.code.emit(OpCode::ExistsIndexExpr);
                    }
                }
                Expr::Index { target, index } => {
                    self.compile_expr(target);
                    self.compile_expr(index);
                    self.code.emit(OpCode::ExistsIndexExpr);
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
                samemark,
            } => {
                let pattern_idx = self.code.add_constant(Value::Str(pattern.clone()));
                let replacement_idx = self.code.add_constant(Value::Str(replacement.clone()));
                self.code.emit(OpCode::Subst {
                    pattern_idx,
                    replacement_idx,
                    samemark: *samemark,
                });
            }
            // S/// non-destructive substitution
            Expr::NonDestructiveSubst {
                pattern,
                replacement,
                samemark,
            } => {
                let pattern_idx = self.code.add_constant(Value::Str(pattern.clone()));
                let replacement_idx = self.code.add_constant(Value::Str(replacement.clone()));
                self.code.emit(OpCode::NonDestructiveSubst {
                    pattern_idx,
                    replacement_idx,
                    samemark: *samemark,
                });
            }
            // tr/// transliteration
            Expr::Transliterate {
                from,
                to,
                delete,
                complement,
                squash,
            } => {
                let from_idx = self.code.add_constant(Value::Str(from.clone()));
                let to_idx = self.code.add_constant(Value::Str(to.clone()));
                self.code.emit(OpCode::Transliterate {
                    from_idx,
                    to_idx,
                    delete: *delete,
                    complement: *complement,
                    squash: *squash,
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
                    self.compile_block_inline(body);
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
                    let is_dynamic = self.var_is_dynamic(name);
                    // my $x = expr in expression context → declare, assign, return value
                    if *is_state {
                        self.compile_expr(expr);
                        let slot = self.alloc_local(name);
                        let key = format!("__state_{}::{}", self.current_package, name);
                        let key_idx = self.code.add_constant(Value::Str(key.clone()));
                        self.code.state_locals.push((slot as usize, key.clone()));
                        self.code.emit(OpCode::StateVarInit(slot, key_idx));
                        self.code.emit(OpCode::GetLocal(slot));
                    } else if name.starts_with('@') || name.starts_with('%') {
                        let marker_name = format!(
                            "__do_decl_init_{}",
                            STATE_COUNTER.fetch_add(1, Ordering::Relaxed)
                        );
                        let marker_slot = self.alloc_local(&marker_name);
                        // Container declarations in expression position (`my @a`) should
                        // initialize once per declaration site and then keep using the
                        // current lexical value on repeated evaluation (e.g. in loops).
                        self.code.emit(OpCode::GetLocal(marker_slot));
                        let jump_have_value = self.code.emit(OpCode::JumpIfNotNil(0));
                        self.code.emit(OpCode::Pop);
                        self.compile_expr(expr);
                        let name_idx = self.code.add_constant(Value::Str(name.clone()));
                        self.code.emit(OpCode::SetGlobal(name_idx));
                        // Read back the coerced value (SetGlobal coerces list→hash for %)
                        let name_idx2 = self.code.add_constant(Value::Str(name.clone()));
                        if name.starts_with('@') {
                            self.code.emit(OpCode::GetArrayVar(name_idx2));
                        } else {
                            self.code.emit(OpCode::GetHashVar(name_idx2));
                        }
                        self.code.emit(OpCode::Dup);
                        self.code.emit(OpCode::SetLocal(marker_slot));
                        let jump_end = self.code.emit(OpCode::Jump(0));
                        self.code.patch_jump(jump_have_value);
                        self.code.emit(OpCode::Pop);
                        let name_idx = self.code.add_constant(Value::Str(name.clone()));
                        if name.starts_with('@') {
                            self.code.emit(OpCode::GetArrayVar(name_idx));
                        } else {
                            self.code.emit(OpCode::GetHashVar(name_idx));
                        }
                        self.code.patch_jump(jump_end);
                    } else {
                        self.compile_expr(expr);
                        self.code.emit(OpCode::Dup);
                        self.emit_set_named_var(name);
                    }
                    let name_idx = self.code.add_constant(Value::Str(name.clone()));
                    self.code.emit(OpCode::SetVarDynamic {
                        name_idx,
                        dynamic: is_dynamic,
                    });
                }
                Stmt::Expr(inner_expr) => {
                    self.compile_expr(inner_expr);
                }
                Stmt::For {
                    iterable,
                    param,
                    param_def,
                    params,
                    body,
                    label,
                } => {
                    self.compile_do_for_expr(
                        iterable,
                        param,
                        param_def.as_ref(),
                        params,
                        body,
                        label,
                    );
                }
                Stmt::ClassDecl {
                    name, name_expr, ..
                } => {
                    // Register the class and return the type object
                    self.compile_stmt(stmt);
                    if let Some(expr) = name_expr {
                        self.compile_expr(expr);
                        self.code.emit(OpCode::IndirectTypeLookup);
                    } else {
                        let name_idx = self.code.add_constant(Value::Str(name.clone()));
                        self.code.emit(OpCode::GetBareWord(name_idx));
                    }
                }
                Stmt::RoleDecl { name, .. } => {
                    // Register the role and return the role type object.
                    self.compile_stmt(stmt);
                    let name_idx = self.code.add_constant(Value::Str(name.clone()));
                    self.code.emit(OpCode::GetBareWord(name_idx));
                }
                Stmt::Package { name, .. } => {
                    // Register the package and return the type object.
                    self.compile_stmt(stmt);
                    let name_idx = self.code.add_constant(Value::Str(name.clone()));
                    self.code.emit(OpCode::GetBareWord(name_idx));
                }
                Stmt::EnumDecl { name, .. } if name.is_empty() => {
                    // Anonymous enum: RegisterEnum pushes the Map result
                    self.compile_stmt(stmt);
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
                    let arg_sources_idx = self.add_arg_sources_constant(args);
                    for arg in args {
                        self.compile_expr(arg);
                    }
                    let name_idx = self.code.add_constant(Value::Str(name.clone()));
                    self.code.emit(OpCode::CallOnCodeVar {
                        name_idx,
                        arity: args.len() as u32,
                        arg_sources_idx,
                    });
                } else {
                    self.compile_expr(target);
                    let arg_sources_idx = self.add_arg_sources_constant(args);
                    for arg in args {
                        self.compile_expr(arg);
                    }
                    self.code.emit(OpCode::CallOnValue {
                        arity: args.len() as u32,
                        arg_sources_idx,
                    });
                }
            }
            Expr::AnonSub(body) => {
                let idx = self.code.add_stmt(Stmt::Block(body.clone()));
                self.code.emit(OpCode::MakeAnonSub(idx));
            }
            Expr::AnonSubParams {
                params,
                param_defs,
                return_type,
                body,
            } => {
                // Validate for placeholder conflicts
                if let Some(err_val) = self.check_placeholder_conflicts(params, body, None) {
                    let idx = self.code.add_constant(err_val);
                    self.code.emit(OpCode::LoadConst(idx));
                    self.code.emit(OpCode::Die);
                    return;
                }
                let idx = self.code.add_stmt(Stmt::SubDecl {
                    name: String::new(),
                    name_expr: None,
                    params: params.clone(),
                    param_defs: param_defs.clone(),
                    return_type: return_type.clone(),
                    signature_alternates: Vec::new(),
                    body: body.clone(),
                    multi: false,
                    is_export: false,
                    is_test_assertion: false,
                    supersede: false,
                });
                self.code.emit(OpCode::MakeAnonSubParams(idx));
            }
            Expr::Lambda { param, body } => {
                let idx = self.code.add_stmt(Stmt::SubDecl {
                    name: String::new(),
                    name_expr: None,
                    params: if param.is_empty() {
                        Vec::new()
                    } else {
                        vec![param.clone()]
                    },
                    param_defs: Vec::new(),
                    return_type: None,
                    signature_alternates: Vec::new(),
                    body: body.clone(),
                    multi: false,
                    is_export: false,
                    is_test_assertion: false,
                    supersede: false,
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
            TokenKind::BangEqEqEq => Some(OpCode::StrictNe),
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

    /// Compile a regex value as `$_ ~~ /regex/`, so it matches against $_
    /// and sets $/ with the match result.
    fn compile_match_regex(&mut self, v: &Value) {
        let lhs_var = Some("_".to_string());
        // Load $_ as the LHS
        let name_idx = self
            .code
            .add_constant(Value::Str(self.qualify_variable_name("_")));
        self.code.emit(OpCode::GetGlobal(name_idx));
        // SmartMatchExpr will set $_ to LHS, run RHS, then smartmatch
        let sm_idx = self.code.emit(OpCode::SmartMatchExpr {
            rhs_end: 0,
            negate: false,
            lhs_var,
        });
        // RHS: load the regex constant
        let idx = self.code.add_constant(v.clone());
        self.code.emit(OpCode::LoadConst(idx));
        self.code.patch_smart_match_rhs_end(sm_idx);
    }
}
